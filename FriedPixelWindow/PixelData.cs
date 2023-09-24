namespace FriedPixelWindow
{
    public class PixelData
    {
        /// <summary>
        /// The width of the pixel grid
        /// </summary>
        public uint Width { get; private set; }

        /// <summary>
        /// The height of the pixel grid
        /// </summary>
        public uint Height { get; private set; }

        public uint PixelScale { get; private set; }

        /// <summary>
        /// Raw RGBA data. The alpha component is not used, except for being set to 255 for full opacity.
        /// This object is used for rendering with SFML by updating render texture data.
        /// </summary>
        public byte[] RawData { get; private set; }

        /// <summary>
        /// Creates a new pixel data instance. Only the <see cref="PixelWindow"/> should need to do this.
        /// </summary>
        /// <param name="width">The width of the pixel grid</param>
        /// <param name="height">The height of the pixel grid</param>
        public PixelData(uint width, uint height,uint pixelScale)
        {
            Width = width;
            Height = height;
            PixelScale = pixelScale;
            RawData = new byte[4 * width * height]; // 4 bytes per pixel (R, G, B, A)
        }

        // For a given X and Y coordinate, gets the index of the 1 dimensional array.
        private uint GetIndexFromXY(uint x, uint y) => 4 * ((y * Width) + x);

        private uint GetIndexFromPixelIndex(uint pixelIndex)
        {
            uint y = (pixelIndex / (Width * PixelScale));
            uint x = (pixelIndex % (Width * PixelScale));
            return GetIndexFromXY(x, y);
        }

        /// <summary>
        /// Gets or sets the pixel data at the specified coordinates
        /// </summary>
        /// <param name="x">The column of the pixel</param>
        /// <param name="y">The row of the pixel</param>
        /// <returns>Pixel data as a tuple of 3 bytes - one for each R, G, and B component</returns>
        public (byte r, byte g, byte b) this[uint x, uint y]
        {
            get
            {
                var i = GetIndexFromXY(x, y);
                return (RawData[i], RawData[i + 1], RawData[i + 2]);
            }
            set
            {
                var i = GetIndexFromXY(x, y);
                RawData[i] = value.r;
                RawData[i + 1] = value.g;
                RawData[i + 2] = value.b;
                RawData[i + 3] = 255; // We don't care about opacity, set alpha to opaque
            }
        }

        /// <summary>
        /// Clears all bytes in the pixel data to 0
        /// </summary>
        public void Clear()
        {
            Array.Clear(RawData);
        }

        /// <summary>
        /// sets the raw data directly
        /// </summary>
        /// <param name="newData"></param>
        /// <exception cref="ArgumentException"></exception>
        public void SetRawData(byte[] newData) 
        {
            var t16 = (newData.Length * (4));
            for (int i = 0; i < t16; i+=4)
            {
                (byte R, byte G, byte B, byte A) = GetColorFromIndex(newData[i/4]);
                RawData[i + 0] = R;
                RawData[i + 1] = G;
                RawData[i + 2] = B;
                RawData[i + 3] = A;
            }
        }

        public void SetRawDataSpan(Span<byte> newData)
        {
            var t16 = (newData.Length * (4));
            for (int i = 0; i < t16; i += 4)
            {
                (byte R, byte G, byte B, byte A) = GetColorFromIndex(newData[i / 4]);
                RawData[i + 0] = R;
                RawData[i + 1] = G;
                RawData[i + 2] = B;
                RawData[i + 3] = A;
            }
        }

        public (byte R, byte G, byte B, byte A) GetColorFromIndex(byte ColorIndex)
        {
            switch (ColorIndex & 0x0F) //mask only care about lower bits
            {
                case 0x00: //black
                    return (0, 0, 0, 255);
                case 0x01: //white
                    return (255, 255, 255, 255);
                case 0x02: //red
                    return (255, 0, 0, 255);
                case 0x03: //cyan
                    return (0, 255, 255, 255);
                case 0x04: //purple
                    return (128, 0, 128, 255);
                case 0x05: //green
                    return (0, 128, 0, 255);
                case 0x06: //blue
                    return (0, 0, 255, 255);
                case 0x07: //yellow
                    return (255, 215, 0, 255);
                case 0x08: //orange
                    return (255, 140, 0, 255);
                case 0x09: //brown
                    return (139, 69, 19, 255);
                case 0x0a: //light red
                    return (205, 92, 92, 255);
                case 0x0b: //dark grey
                    return (105, 105, 105, 255);
                case 0x0c: //grey
                    return (150, 150, 150, 255);
                case 0x0d: //light green
                    return (50, 205, 50, 255);
                case 0x0e: //light blue
                    return (100, 149, 237, 255);
                case 0x0f: //light grey
                    return (192, 192, 192, 255);
                default:
                    return (255, 255, 255, 255);
                    break;
            }
        }
    }
}