package it.unibo.qarobotexecutor;

import com.pi4j.io.gpio.GpioController;
import com.pi4j.io.gpio.GpioPinDigitalOutput;
import com.pi4j.io.gpio.PinState;
import com.pi4j.io.gpio.RaspiPin;

public final class LedUtils {

		
	final static GpioController controller = com.pi4j.io.gpio.GpioFactory.getInstance();
    final static GpioPinDigitalOutput myLed = controller.provisionDigitalOutputPin(RaspiPin.GPIO_06, PinState.LOW);
	
    public static String check(){
    	return "VIVOOOOO";
    }
    
    public static void startBlinking(){
    	myLed.blink(250);
    }
    public static void stopBlinking(){
    	myLed.blink(0);
    	myLed.low();
    }
}
