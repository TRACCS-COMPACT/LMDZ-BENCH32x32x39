import os
import argparse
import numpy as np
import xarray as xr
import cmocean

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import cartopy.util as cutil

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.colors as colors
import matplotlib
matplotlib.use('Agg')

def make_plot(data,lon,lat,infos,output):
    # args
    title, cmap, norm, tfs = infos
    data = tfs(data)
    # figure
    plt.figure(figsize=(12, 8))
    ax = plt.axes(projection=ccrs.PlateCarree())
    ax.add_feature(cfeature.COASTLINE, zorder=100, edgecolor='k')
    # color map
    pcm = ax.pcolormesh(lon, lat, data, cmap=cmap, norm=norm, transform=ccrs.PlateCarree())
    cbar = plt.colorbar(pcm, ax=ax, orientation='vertical', pad=0.05, shrink=0.5)
    plt.title(title)
    # write fig
    plt.savefig(output, bbox_inches='tight')
    plt.close()


def main(filepath, var_name, fig_name, infos, freq):

    # read files
    try:
        ds = xr.open_dataset(filepath)
    except:
        print(f'FAILED TO READ {filepath}')
        return

    print(f'Plotting {var_name}')

    # get fields
    lon = ds.lon.values
    lat = ds.lat.values
    var = getattr(ds,var_name)
    if len(var.shape) == 3:
        var_val = var.values[2,:,:]
    elif len(var.shape) == 4:
        var_val = var.values[2,0,:,:]

    # plot
    plotpath = fig_name + '_' + config +'_' + freq + '.png'
    make_plot(var_val,lon,lat,infos,plotpath)



if __name__=="__main__":

    # Config name
    # -----------
    config = 'BENCH'

    # snapshots
    # ---------
    
    # wet-bulb temperature thetaw
    infos = [ 'Wet-bulb Temp. (K)' , cmocean.cm.thermal , colors.Normalize(vmin=230.0, vmax=300.0), lambda x: x ]
    main( filepath='histday.nc' , var_name='thetaw' , fig_name='Theta_w', infos=infos , freq='d03_surf' )


