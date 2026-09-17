import java.time.Duration;
import java.util.ArrayList;

import javax.print.Doc;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.ui.ExpectedCondition;
import org.openqa.selenium.support.ui.WebDriverWait;

public class DataScienceToolbox {
	
	// module-info NICHT erzeugen!!! bzw. löschen
	// FirefoxDriver no longer supported in JRE 17+   --> switch to ChromeDriver

	
	public static WebDriver driver;
	
	public static void main (String [] args) throws InterruptedException {
	
		
		
		// CASE 1: babynames
		
		ArrayList<String> names = new ArrayList<String>();
		
		names.add("Angela");
		names.add("Hans");
		names.add("Susanne");
		names.add("Peter");
		names.add("X123-EAX");
		
		
		for (String name : names) {
			
			String url = "https://www.gpeters.com/names/baby-names.php?name=";
	    	
	    	try {
		    	Document doc = Jsoup.connect(url+name).get(); 	
		    	String genderInfo = doc.select("div[class='gender-result']").select("h1").first().text();
		    	System.out.println(name);
		    	System.out.println(genderInfo);
	    	}
	    	catch (Exception e) {
	    		System.out.println("name not found");
	    	}
	    	
	    	Thread.sleep(200);
			
		}
				
		
		
		
		
		
		/// CASE 2: flixtrain
    	
		String driverPath = Paths.get(System.getProperty("user.dir"))
                .getParent()
                .resolve("geckodriver.exe")
                .toString();

    	System.setProperty("webdriver.gecko.driver", driverPath);

    	driver = new FirefoxDriver();
	
    
    	ArrayList<String> cities = new ArrayList<String>();
        
    	cities.add("40d8f682-8646-11e6-9066-549f350fcb0c"); // berlin
    	cities.add("40d90407-8646-11e6-9066-549f350fcb0c"); // frankfurt
    	cities.add("40d901a5-8646-11e6-9066-549f350fcb0c"); // münchen
    	cities.add("40d91025-8646-11e6-9066-549f350fcb0c"); // köln
    	cities.add("40d917f9-8646-11e6-9066-549f350fcb0c"); // leipzig
    	
    	boolean cookieFlag = true;
    
    	for (String c1 : cities) {
    		for (String c2 : cities) {
    			if (!c1.equals(c2)) {
    			String url = "https://shop.flixbus.de/search?"
    	    			+ "departureCity="+c1+"&"
    	    			+ "arrivalCity="+c2+"&"
    	    			+ "rideDate=21.07.2026&"
    	    			+ "adult=1";
    	    	
    	    
    	    	driver.get(url);
    	    	waitForLoad(driver);
    	    	Thread.sleep(10000);
    	    	
    	    	
    	    	if (cookieFlag) {
		    		
		    		// move the mouse (relative to the center of the window/body) x+160, y+159 and click
		    		// this will depend on your screen size
		    		
		    		WebElement body = driver.findElement(By.tagName("body"));
		    		Actions actions = new Actions(driver);
		    		actions.moveToElement(body, 160, 70).click().perform();
		    		
    		    	Thread.sleep(2000);
		    		cookieFlag = false;
		    	}
		    	
    	    	
    	    	
    	    	
    	    	Document doc = Jsoup.parse(driver.getPageSource());

    	    	Elements prices = doc.select("span[class='SearchResult__price___QpySa']");
    	    	for (Element p : prices) {
    	    		System.out.println(p.text());
    	    	}
    	    	System.out.println("----------------------------");
    	    	
    			}
    		}
    	}
    	
    		
    	driver.close();
    			
	}//main
	
	
	
	public static void waitForLoad(WebDriver driver) {
        ExpectedCondition<Boolean> pageLoadCondition = new
                ExpectedCondition<Boolean>() {
                    public Boolean apply(WebDriver driver) {
                        return ((JavascriptExecutor)driver).executeScript("return document.readyState").equals("complete");
                    }
                };
        WebDriverWait wait = new WebDriverWait(driver, 10);
        wait.until(pageLoadCondition);
	}

	
}