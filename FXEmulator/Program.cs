using FriedPixelWindow;
using SFML.Graphics;

namespace FXEmulator;

public static class Program
{

	public static Emulator emu = new Emulator();

	public static void Main()
	{
		emu.data[0xfffc] = 0x00;
		emu.data[0xfffd] = 0x06;

        emu.data.AddCodeAt(0x0600,
			0xa2, 0x00,			//	LDX #$00
			0xa9, 0x01,			//	LDA #$01
								//LOOP:
			0x9d, 0x00, 0x02,	//	STA $0200, X
			0x9d, 0x00, 0x03,	//	STA $0300, X
			0x9d, 0x00, 0x04,	//	STA $0400, X
			0x9d, 0x00, 0x05,	//	STA $0500, X
			0xe8,               //	INX
            0x69, 0x00,			//	ADC #$00


            0xe0, 0x00,			//	CPX #$00
			0xd0, 0xed,			//	BNE LOOP

			0xa9, 0x02,			//	LDA #$02
			0x8d, 0x50, 0x03,   //	STA $0350

            0xa9, 0x03,         //	LDA #$03
            0x8d, 0x51, 0x03,	//	STA $0352

            0x00				//	BRK
        );

        emu.Debug = false;
		emu.reset();
		emu.step(7); //reset sequence takes 7 clock cycles


        var appManager = new PixelManager();

        var window = new PixelWindow(32,32, 15, "Big pixels", appManager,
            fixedTimestep: 20, framerateLimit: 300);

        Thread thread = new Thread(new ThreadStart(Stepper));
        thread.Start();

        window.Run();
	}
	public static void Stepper() 
	{
        Thread.Sleep(100);

        while (!emu.CheckFlag(sf.I))
		{
			emu.step();
		}
	}

}
class PixelManager : IPixelWindowAppManager
{

    public void OnLoad(RenderWindow renderWindow)
    {
        // On load function - runs once at start.
        // The SFML render window provides ability to set up events and input (maybe store a reference to it for later use in your update functions)
    }

    public void Update(float frameTime)
    {
        // Update function - process update logic to run every frame here
    }

    public void FixedUpdate(float timeStep)
    {
        // Fixed update function - process logic to run every fixed timestep here
    }

    public void Render(PixelData pixelData, float frameTime)
    {
        // Render function - set pixel data for the current frame here
        // Randomised pixels shown as example.

        pixelData.Clear();
        pixelData.SetRawData(Program.emu.data.Skip(0x0200).Take(1024).ToArray());
    }
}
