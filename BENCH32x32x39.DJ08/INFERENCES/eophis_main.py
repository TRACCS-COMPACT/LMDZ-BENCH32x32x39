# eophis API
import eophis
from eophis import Freqs, Domains
# other modules
import argparse
import os

def earth_info():
    """
    Return arguments for a Tunnel object that will configure coupling. Return run info for namelist
    """

    # namelist info
    with open("run.def") as f:
        d = dict(line.strip().split("=", 1) for line in f if "=" in line and not line.lstrip().startswith("#"))
        nday = int(d["nday"])
    with open("gcm.def") as f:
        d = dict(line.strip().split("=", 1) for line in f if "=" in line and not line.lstrip().startswith("#"))
        day_step = int(d["day_step"])
        iphysiq = int(d["iphysiq"])

    step = int( 24*3600 / day_step * iphysiq )
    ite_end = int(24*3600*nday / step)
    ite_0 = 1

    tunnel_config = list()
    tunnel_config.append( { 'label' : 'TO_LMDZ', \
                            'grids' : { 'lmdz' :  {'npts' : (32,33)} }, \
                            'exchs' : [ {'freq' : step, 'grd' : 'lmdz', 'lvl' : 39, 'in' : ['pres','temp','q_spec'], 'out' : ['theta_w'] } ] }
                        )

    return tunnel_config, step, ite_end, ite_0


def preproduction():
    eophis.info('========= EOPHIS: Pre-Production =========')
    eophis.info('  Aim: write coupling namelist\n')

    # earth info
    tunnel_config, step, it_end, it_0 = earth_info()
    total_time = (it_end - it_0 + 1) * step

    # tunnel registration (lazy) compulsory to update namelist
    eophis.register_tunnels( tunnel_config )
    
    # write updated namelist
    eophis.write_coupling_namelist( simulation_time=total_time )


def production():
    eophis.info('========= EOPHIS: Production =========')
    eophis.info('  Aim: execute coupled simulation\n')

    #  Earth Coupling
    # ++++++++++++++++
    tunnel_config, step, it_end, it_0 = earth_info()
    niter = it_end - it_0 + 1
    total_time = niter * step

    # tunnel registration (lazy)
    earth, = eophis.register_tunnels( tunnel_config )

    # link all tunnels (beware, dormant errors will likely appear here)
    eophis.open_tunnels()

    #  Models
    # ++++++++
    from models import sub_wet_temp

    #  Assemble
    # ++++++++++
    @eophis.all_in_all_out(geo_model=earth, step=step, niter=niter)
    def loop_core(**inputs):
        outputs = {}
        outputs['theta_w'] = sub_wet_temp( inputs['pres'], inputs['temp'], inputs['q_spec'] )
        return outputs

    #  Run
    # +++++
    eophis.starter(loop_core)
    
# ============================ #
if __name__=='__main__':
    """ Switch between preproduction and production mode. Call different instructions depending on the selected mode. """

    parser = argparse.ArgumentParser()
    parser.add_argument('--exec', dest='exec', type=str, default='prod', help='Execution type: preprod or prod')
    args = parser.parse_args()

    eophis.set_mode(args.exec)
    
    if args.exec == 'preprod':
        preproduction()
    elif args.exec == 'prod':
        production()
    else:
        eophis.abort(f'Unknown execution mode {args.exec}, use "preprod" or "prod"')
