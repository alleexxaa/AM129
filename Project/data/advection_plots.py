
import numpy as np
import matplotlib.pyplot as plt


def readInitialFile(file):
    x_vals = []
    y_vals = []
    with open(file, 'r') as dataFile:
        first_line = True
        for line in dataFile:
            if first_line:
                first_line = False
                continue
            
            columns = line.split()
            
            if len(columns) >= 2:
                x1 = columns[0] 
                y1 = columns[1]
                x = float(x1)
                y = float(y1)
                x_vals.append(x)
                y_vals.append(y)
                #print(x)
                #print(y)
                
            else:
                print('something wrong happened')
    return x_vals, y_vals

def readLaterFile(file):
    yf_vals = []
    with open(file, 'r') as dataFile:
        first_line = True
        for line in dataFile:
            if first_line:
                first_line = False
                continue
            
            columns = line.split()
            
            if len(columns) >= 2:
                y1 = columns[1:]
                #print(y1)
                #whatType = type(y1)
                #y = float(y1[1])
                #print(whatType)
                #print(y1)
                for i in range(len(y1)):
                    y = float(y1[i])
                    #print(y)
                    yf_vals.append(y)
                #print(x)
                #print(y)
                
            else:
                print('something wrong happened')
    return yf_vals#, whatType



def plotF6_32Graphs(xCont,yCont1, yCont2, yCont3, yCont4, xDont,yDont1, yDont2, yDont3, yDont4):

    labels = [
        'Continuous Initial', 'Continuous Upwind Final', 'Continuous Centered Final', 'Continuous LW Final', 
    'Discontinuous Initial', 'Discontinuous Upwind Final', 'Discontinuous Centered Final', 'Discontinuous LW Final'
    ]

    # Create subplots
    fig, axs = plt.subplots(nrows=3, ncols=2, figsize=(10, 8))
    
    # Plots the continuous side (left side)
    axs[0,0].plot(xCont, yCont1, label=labels[0], linestyle='--', color='black')
    axs[0,0].plot(xCont, yCont2, label=labels[1], color='deeppink')
    axs[0,0].set_title('Initial and Upwind Continuous at t=0.98 and N=32')
    axs[0,0].legend()

    axs[1,0].plot(xCont, yCont1, label=labels[0], linestyle='--', color='black')
    axs[1,0].plot(xCont, yCont3, label=labels[2], color='deeppink')
    axs[1,0].set_title('Initial and Centered Continuous at t=0.98 and N=32')
    axs[1,0].legend()

    axs[2,0].plot(xCont, yCont1, label=labels[0], linestyle='--', color='black')
    axs[2,0].plot(xCont, yCont4, label=labels[3], color='deeppink')
    axs[2,0].set_title('Initial and Lax Wendroff Continuous at t=0.98 and N=32')
    axs[2,0].legend()

   
    # Plots the discontinuous side (right side)
    axs[0,1].plot(xDont, yDont1, label=labels[4], linestyle='--', color='black')
    axs[0,1].plot(xDont, yDont2, label=labels[5], color='deeppink')
    axs[0,1].set_title('Initial and Upwind Discontinuous at t=0.98 and N=32')
    axs[0,1].legend()

    axs[1,1].plot(xDont, yDont1, label=labels[4], linestyle='--', color='black')
    axs[1,1].plot(xDont, yDont3, label=labels[6], color='deeppink')
    axs[1,1].set_title('Initial and Centered Discontinuous at t=0.98 and N=32')
    axs[1,1].legend()

    axs[2,1].plot(xDont, yDont1, label=labels[4], linestyle='--', color='black')
    axs[2,1].plot(xDont, yDont4, label=labels[7], color='deeppink')
    axs[2,1].set_title('Initial and Lax Wendroff Discontinuous at t=0.98 and N=32')
    axs[2,1].legend()

    # Adjust layout
    plt.tight_layout()

    # Save plot
    plt.savefig('Advection32.png')

    # Show the plots
    plt.show()


def plotF6_128Graphs(xCont,yCont1, yCont2, yCont3, yCont4, xDont,yDont1, yDont2, yDont3, yDont4):

    labels = [
        'Continuous Initial', 'Continuous Upwind Final', 'Continuous Centered Final', 'Continuous LW Final', 
    'Discontinuous Initial', 'Discontinuous Upwind Final', 'Discontinuous Centered Final', 'Discontinuous LW Final'
    ]

    # Create subplots
    fig, axs = plt.subplots(nrows=3, ncols=2, figsize=(10, 8))
    
    # Plots the continuous side (left side)
    axs[0,0].plot(xCont, yCont1, label=labels[0], linestyle='--', color='black')
    axs[0,0].plot(xCont, yCont2, label=labels[1], color='deeppink')
    axs[0,0].set_title('Initial and Upwind Continuous at t=0.91 and N=128')
    axs[0,0].legend()

    axs[1,0].plot(xCont, yCont1, label=labels[0], linestyle='--', color='black')
    axs[1,0].plot(xCont, yCont3, label=labels[2], color='deeppink')
    axs[1,0].set_title('Initial and Centered Continuous at t=0.91 and N=128')
    axs[1,0].legend()

    axs[2,0].plot(xCont, yCont1, label=labels[0], linestyle='--', color='black')
    axs[2,0].plot(xCont, yCont4, label=labels[3], color='deeppink')
    axs[2,0].set_title('Initial and Lax Wendroff Continuous at t=0.91 and N=128')
    axs[2,0].legend()

   
    # Plots the discontinuous side (right side)
    axs[0,1].plot(xDont, yDont1, label=labels[4], linestyle='--', color='black')
    axs[0,1].plot(xDont, yDont2, label=labels[5], color='deeppink')
    axs[0,1].set_title('Initial and Upwind Discontinuous at t=0.91 and N=128')
    axs[0,1].legend()

    axs[1,1].plot(xDont, yDont1, label=labels[4], linestyle='--', color='black')
    axs[1,1].plot(xDont, yDont3, label=labels[6], color='deeppink')
    axs[1,1].set_title('Initial and Centered Discontinuous at t=0.91 and N=128')
    axs[1,1].legend()

    axs[2,1].plot(xDont, yDont1, label=labels[4], linestyle='--', color='black')
    axs[2,1].plot(xDont, yDont4, label=labels[7], color='deeppink')
    axs[2,1].set_title('Initial and Lax Wendroff Discontinuous at t=0.91 and N=128')
    axs[2,1].legend()

    # Adjust layout
    plt.tight_layout()

    # Save plot
    plt.savefig('Advection128.png')

    # Show the plots
    plt.show()



def plotDiffSols(x, y2, y5, y8, y1, N):
   
    fig, axs = plt.subplots(nrows=2, ncols=2, figsize=(10, 8))
    
    axs[0,0].plot(x, y2, linestyle='-', color='limegreen')
    axs[0,0].set_title('N= '+ str(N) + 'tmax = 0.2')
    

    axs[1,0].plot(x, y5, linestyle='-', color='limegreen')
    axs[1,0].set_title('N= '+ str(N) +'tmax = 0.5')
   

    axs[0,1].plot(x, y8, linestyle='-', color='limegreen')
    axs[0,1].set_title('N= '+ str(N) +'tmax = 0.8')
   

    axs[1,1].plot(x, y1, linestyle='-', color='limegreen')
    axs[1,1].set_title('N= '+ str(N) + 'tmax = 1.0')
 


    # Adjust layout
    plt.tight_layout()

    # Save plot
    plt.savefig('Diffusion' +str(N)+'.png')

    # Show the plots
    plt.show()




if __name__=="__main__":
    
    filename = 'python_parameter.init'

    with open(filename, 'r') as file:
        line = file.readline().strip()
        key, value = map(str.strip, line.split(' '))
        N_Dict = {key: value}
        N_as_int = int(N_Dict['sim_N'])
        N_Dict = {key:N_as_int}
        use_N = N_Dict['sim_N']

    
    if N_Dict['sim_N'] == 32:
        print('I am N=32')
        #read continuous files (32)
  
        xinitialCont_values, yinitialCont_values = readInitialFile('CICs.dat')
        yContUpwind32_values = readLaterFile('upwindsCont_100000032.dat')
        yContCentered32_values = readLaterFile('centeredCont_100000032.dat')
        yContLaxW32_values = readLaterFile('LaxWenCont_100000032.dat')

        #read discontinuous files (32)
        xinitialDont_values, yinitialDont_values = readInitialFile('discont_ICs.dat')
        yDontUpwind32_values = readLaterFile('upwindsDiscont_100000032.dat')
        yDontCentered32_values = readLaterFile('centeredDiscont_100000032.dat')
        yDontLaxW32_values = readLaterFile('LaxWenDiscont_100000032.dat')

        #print(xinitialCont_values)
        #print(yContUpwind32_values)
        # plot graphs for N=32
        plotF6_32Graphs(xinitialCont_values, yinitialCont_values, yContUpwind32_values, yContCentered32_values, yContLaxW32_values, \
            xinitialDont_values, yinitialDont_values, yDontUpwind32_values, yDontCentered32_values, yDontLaxW32_values)

    elif N_Dict['sim_N'] == 128:
        #read continuous files (128)
        print('I am N=128')
        xinitialCont128_values, yinitialCont128_values = readInitialFile('CICs.dat')
        yContUpwind128_values = readLaterFile('upwindsCont_100000117.dat')
        yContCentered128_values = readLaterFile('centeredCont_100000117.dat')
        yContLaxW128_values = readLaterFile('LaxWenCont_100000117.dat')

        #read discontinuous files (128)
        xinitialDont128_values, yinitialDont128_values = readInitialFile('discont_ICs.dat')
        yDontUpwind128_values = readLaterFile('upwindsDiscont_100000117.dat')
        yDontCentered128_values = readLaterFile('centeredCont_100000117.dat')
        yDontLaxW128_values = readLaterFile('LaxWenCont_100000117.dat')

        plotF6_128Graphs(xinitialCont128_values, yinitialCont128_values, yContUpwind128_values, yContCentered128_values, yContLaxW128_values, \
            xinitialDont128_values, yinitialDont128_values, yDontUpwind128_values, yDontCentered128_values, yDontLaxW128_values)

    else:
        print('*cries*')


    
    xDiff, yDiff2 = readInitialFile('DifSols_100000001.dat')
    #print (yDiff2)
    #print(len(xDiff))
    #print(xDiff_list)
    yDiff5 = readLaterFile('DifSols_100000002.dat')
    yDiff8 = readLaterFile('DifSols_100000003.dat')
    yDiff1 = readLaterFile('DifSols_100000004.dat')

    plotDiffSols(xDiff, yDiff2, yDiff5, yDiff8, yDiff1, use_N)

    