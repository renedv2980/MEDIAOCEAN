*          DATA SET TAGEN77    AT LEVEL 014 AS OF 10/09/15                      
*PHASE T70277C,*                                                                
         TITLE 'T70277 - TABLES FOR YEAR 2 ACTRA 2015-2016'                     
T70277   CSECT                                                                  
         DC    AL4(USETBLS-T70277)                                              
         DC    AL4(USELUT-T70277)                                               
         DC    AL4(MAJLUT-T70277)                                               
         DC    AL4(AFMCOLS-T70277)                                              
         DC    AL4(RADCOLS-T70277)                                              
         DC    AL4(OFFCOLS-T70277)                                              
         DC    AL4(ONCOLS-T70277)                                               
         DC    AL4(MSWEET-T70277)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
*                                  1202:SESSION FEE                             
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'77150'            PP,SD,CAR                                    
         DC    F'77150'            SOC,GD                                       
         DC    F'56250'            VO,SS                                        
         DC    F'24350'            GS                                           
         DC    F'115725'           SA = PP+50%                                  
         DC    F'77150'            DEM                                          
         DC    F'47250'            E                                            
         DC    F'31500'            GE                                           
         DC    F'77150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'77150'            PP,SD,CAR                                    
         DC    F'77150'            SOC,GD                                       
         DC    F'56250'            VO,SS                                        
         DC    F'24350'            GS                                           
         DC    F'115725'           SA = PP+50%                                  
         DC    F'77150'            DEM                                          
         DC    F'47250'            E                                            
         DC    F'31500'            GE                                           
         DC    F'77150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'77150'            PP,SD,CAR                                    
         DC    F'77150'            SOC,GD                                       
         DC    F'56250'            VO,SS                                        
         DC    F'24350'            GS                                           
         DC    F'115725'           SA = PP+50%                                  
         DC    F'77150'            DEM                                          
         DC    F'47250'            E                                            
         DC    F'31500'            GE                                           
         DC    F'77150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'77150'            PP,SD,CAR                                    
         DC    F'77150'            SOC,GD                                       
         DC    F'56250'            VO,SS                                        
         DC    F'24350'            GS                                           
         DC    F'115725'           SA = PP+50%                                  
         DC    F'77150'            DEM                                          
         DC    F'47250'            E                                            
         DC    F'31500'            GE                                           
         DC    F'77150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'77150'            PP,SD,CAR                                    
         DC    F'77150'            SOC,GD                                       
         DC    F'56250'            VO,SS                                        
         DC    F'24350'            GS                                           
         DC    F'115725'           SA = PP+50%                                  
         DC    F'77150'            DEM                                          
         DC    F'47250'            E                                            
         DC    F'31500'            GE                                           
         DC    F'77150'            ST, SAME AS SOC                              
         SPACE 1                                                                
*--------------------------------------------------------------------*          
         DC    AL1(8,40,0,0)       1820:SESSION FEE NEW MEDIA VIDEO             
         DC    F'77150'            PP,SD,CAR                                    
         DC    F'77150'            SOC,GD                                       
         DC    F'56250'            VO,SS                                        
         DC    F'24350'            GROUP SINGER                                 
         DC    F'115725'           SA = PP+50%                                  
         DC    F'77150'                                                         
         DC    F'47250'            BACKGROUND                                   
         DC    F'31500'            GROUP BACKGROUND                             
         DC    F'77150'            ST, SAME AS SOC                              
         EJECT                                                                  
DORTTBL  DC    AL1(12,40,0,0)      NATIONAL TV                                  
         DC    F'77150'            PP,SD,CAR                                    
         DC    F'77150'            SOC,GD                                       
         DC    F'56250'            VO,SS                                        
         DC    F'24350'            GS                                           
         DC    F'115725'           SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77150'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'77150'                                                         
         DC    F'77150'                                                         
         DC    F'56250'                                                         
         DC    F'24350'                                                         
         DC    F'115725'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77150'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'77150'                                                         
         DC    F'77150'                                                         
         DC    F'56250'                                                         
         DC    F'24350'                                                         
         DC    F'115725'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77150'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'77150'                                                         
         DC    F'77150'                                                         
         DC    F'56250'                                                         
         DC    F'24350'                                                         
         DC    F'115725'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77150'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'77150'                                                         
         DC    F'77150'                                                         
         DC    F'56250'                                                         
         DC    F'24350'                                                         
         DC    F'115725'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77150'            ST SAME AS SOC                               
         SPACE 1                                                                
*                                  2101:SESSION AND RESIDUAL FEES               
BSCRTBL  DC    AL1(24,12,0,0)      NATIONAL RADIO                               
         DC    F'60075'            SV,SS                                        
         DC    F'45075'            MV,GS                                        
         SPACE 1                                                                
*                                  406:RADIO SESSION AND RESIDUAL FEES          
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'30650'                                                         
         DC    F'23050'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'40500'                                                         
         DC    F'30300'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'45950'                                                         
         DC    F'34575'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'52075'                                                         
         DC    F'39125'                                                         
         SPACE 1                                                                
*--------------------------------------------------------------------*          
         DC    AL1(33,12,0,0)      1820:SESSION FEE NEW MEDIA AUDIO             
         DC    F'60075'            SV,SS                                        
         DC    F'45075'            MV,GS                                        
         EJECT                                                                  
*                                  1804:TABLE A                                 
WSCTBL   DC    AL1(48,40,1,5)      TV WILDSPOT UNITS 1-5                        
         DC    F'61030'            PP,CAR,SD                                    
         DC    F'44730'            SOC,GD                                       
         DC    F'29870'            VO,SS                                        
         DC    F'17890'            GS                                           
         DC    F'61030'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44730'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'62630'            PP,CAR,SD                                    
         DC    F'45745'            SOC,GD                                       
         DC    F'30970'            VO,SS                                        
         DC    F'18435'            GS                                           
         DC    F'62630'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45745'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'64275'            PP,CAR,SD                                    
         DC    F'46795'            SOC,GD                                       
         DC    F'32015'            VO,SS                                        
         DC    F'19075'            GS                                           
         DC    F'64275'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46795'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'65920'            PP,CAR,SD                                    
         DC    F'47820'            SOC,GD                                       
         DC    F'33085'            VO,SS                                        
         DC    F'19680'            GS                                           
         DC    F'65920'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47820'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'67530'            PP,CAR,SD                                    
         DC    F'48820'            SOC,GD                                       
         DC    F'34115'            VO,SS                                        
         DC    F'20300'            GS                                           
         DC    F'67530'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48820'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'69160'            PP,CAR,SD                                    
         DC    F'49840'            SOC,GD                                       
         DC    F'35180'            VO,SS                                        
         DC    F'20890'            GS                                           
         DC    F'69160'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49840'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'70795'            PP,CAR,SD                                    
         DC    F'50815'            SOC,GD                                       
         DC    F'36220'            VO,SS                                        
         DC    F'21295'            GS                                           
         DC    F'70795'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50815'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'72395'            PP,CAR,SD                                    
         DC    F'51850'            SOC,GD                                       
         DC    F'37300'            VO,SS                                        
         DC    F'21785'            GS                                           
         DC    F'72395'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51850'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'74085'            PP,CAR,SD                                    
         DC    F'52915'            SOC,GD                                       
         DC    F'38450'            VO,SS                                        
         DC    F'22170'            GS                                           
         DC    F'74085'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52915'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'76225'            PP,CAR,SD                                    
         DC    F'53830'            SOC,GD                                       
         DC    F'39480'            VO,SS                                        
         DC    F'22635'            GS                                           
         DC    F'76225'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53830'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'77300'            PP,CAR,SD                                    
         DC    F'54905'            SOC,GD                                       
         DC    F'40505'            VO,SS                                        
         DC    F'23090'            GS                                           
         DC    F'77300'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54905'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'78915'            PP,CAR,SD                                    
         DC    F'55920'            SOC,GD                                       
         DC    F'41605'            VO,SS                                        
         DC    F'23450'            GS                                           
         DC    F'78915'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'81305'            PP,CAR,SD                                    
         DC    F'56920'            SOC,GD                                       
         DC    F'42645'            VO,SS                                        
         DC    F'23965'            GS                                           
         DC    F'81305'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'82895'            PP,CAR,SD                                    
         DC    F'57930'            SOC,GD                                       
         DC    F'43685'            VO,SS                                        
         DC    F'24390'            GS                                           
         DC    F'82895'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57930'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'83740'            PP,CAR,SD                                    
         DC    F'58955'            SOC,GD                                       
         DC    F'44775'            VO,SS                                        
         DC    F'24800'            GS                                           
         DC    F'83740'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58955'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'85445'            PP,CAR,SD                                    
         DC    F'59965'            SOC,GD                                       
         DC    F'45830'            VO,SS                                        
         DC    F'25220'            GS                                           
         DC    F'85445'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59965'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'87075'            PP,CAR,SD                                    
         DC    F'60775'            SOC,GD                                       
         DC    F'46925'            VO,SS                                        
         DC    F'25695'            GS                                           
         DC    F'87075'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60775'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'88705'            PP,CAR,SD                                    
         DC    F'61645'            SOC,GD                                       
         DC    F'48020'            VO,SS                                        
         DC    F'26105'            GS                                           
         DC    F'88705'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61645'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'90360'            PP,CAR,SD                                    
         DC    F'62475'            SOC,GD                                       
         DC    F'49055'            VO,SS                                        
         DC    F'26560'            GS                                           
         DC    F'90360'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'91955'            PP,CAR,SD                                    
         DC    F'63195'            SOC,GD                                       
         DC    F'50090'            VO,SS                                        
         DC    F'27055'            GS                                           
         DC    F'91955'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63195'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'93560'            PP,CAR,SD                                    
         DC    F'64125'            SOC,GD                                       
         DC    F'51235'            VO,SS                                        
         DC    F'27360'            GS                                           
         DC    F'93560'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64125'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'95185'            PP,CAR,SD                                    
         DC    F'65015'            SOC,GD                                       
         DC    F'51830'            VO,SS                                        
         DC    F'27895'            GS                                           
         DC    F'95185'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65015'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'96865'            PP,CAR,SD                                    
         DC    F'65815'            SOC,GD                                       
         DC    F'52480'            VO,SS                                        
         DC    F'28200'            GS                                           
         DC    F'96865'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65815'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'98460'            PP,CAR,SD                                    
         DC    F'66605'            SOC,GD                                       
         DC    F'53115'            VO,SS                                        
         DC    F'28580'            GS                                           
         DC    F'98460'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66605'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'100120'           PP,CAR,SD                                    
         DC    F'67500'            SOC,GD                                       
         DC    F'53760'            VO,SS                                        
         DC    F'28955'            GS                                           
         DC    F'100120'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'101775'           PP,CAR,SD                                    
         DC    F'68295'            SOC,GD                                       
         DC    F'54410'            VO,SS                                        
         DC    F'29350'            GS                                           
         DC    F'101775'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68295'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'103000'           PP,CAR,SD                                    
         DC    F'69160'            SOC,GD                                       
         DC    F'55020'            VO,SS                                        
         DC    F'29700'            GS                                           
         DC    F'103000'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69160'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'104140'           PP,CAR,SD                                    
         DC    F'69975'            SOC,GD                                       
         DC    F'55700'            VO,SS                                        
         DC    F'30100'            GS                                           
         DC    F'104140'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69975'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'105425'           PP,CAR,SD                                    
         DC    F'70795'            SOC,GD                                       
         DC    F'56345'            VO,SS                                        
         DC    F'30445'            GS                                           
         DC    F'105425'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70795'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'106670'           PP,CAR,SD                                    
         DC    F'71635'            SOC,GD                                       
         DC    F'56980'            VO,SS                                        
         DC    F'30895'            GS                                           
         DC    F'106670'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71635'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'107920'           PP,CAR,SD                                    
         DC    F'72400'            SOC,GD                                       
         DC    F'57620'            VO,SS                                        
         DC    F'31255'            GS                                           
         DC    F'107920'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72400'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'109150'           PP,CAR,SD                                    
         DC    F'73040'            SOC,GD                                       
         DC    F'58100'            VO,SS                                        
         DC    F'31595'            GS                                           
         DC    F'109150'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73040'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'110350'           PP,CAR,SD                                    
         DC    F'73640'            SOC,GD                                       
         DC    F'58535'            VO,SS                                        
         DC    F'31990'            GS                                           
         DC    F'110350'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73640'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'111615'           PP,CAR,SD                                    
         DC    F'74350'            SOC,GD                                       
         DC    F'59065'            VO,SS                                        
         DC    F'32360'            GS                                           
         DC    F'111615'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'112810'           PP,CAR,SD                                    
         DC    F'74895'            SOC,GD                                       
         DC    F'59445'            VO,SS                                        
         DC    F'32715'            GS                                           
         DC    F'112810'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74895'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'114060'           PP,CAR,SD                                    
         DC    F'75520'            SOC,GD                                       
         DC    F'59940'            VO,SS                                        
         DC    F'33130'            GS                                           
         DC    F'114060'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75520'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'114895'           PP,CAR,SD                                    
         DC    F'75930'            SOC,GD                                       
         DC    F'60360'            VO,SS                                        
         DC    F'33405'            GS                                           
         DC    F'114895'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75930'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'115700'           PP,CAR,SD                                    
         DC    F'76755'            SOC,GD                                       
         DC    F'60840'            VO,SS                                        
         DC    F'33755'            GS                                           
         DC    F'115700'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76755'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'116525'           PP,CAR,SD                                    
         DC    F'77380'            SOC,GD                                       
         DC    F'61270'            VO,SS                                        
         DC    F'34030'            GS                                           
         DC    F'116525'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77380'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'117370'           PP,CAR,SD                                    
         DC    F'78005'            SOC,GD                                       
         DC    F'61690'            VO,SS                                        
         DC    F'34395'            GS                                           
         DC    F'117370'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78005'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'118170'           PP,CAR,SD                                    
         DC    F'78575'            SOC,GD                                       
         DC    F'62180'            VO,SS                                        
         DC    F'34675'            GS                                           
         DC    F'118170'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78575'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'119050'           PP,CAR,SD                                    
         DC    F'79210'            SOC,GD                                       
         DC    F'62660'            VO,SS                                        
         DC    F'35055'            GS                                           
         DC    F'119050'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79210'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'119870'           PP,CAR,SD                                    
         DC    F'79835'            SOC,GD                                       
         DC    F'63105'            VO,SS                                        
         DC    F'35365'            GS                                           
         DC    F'119870'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79835'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'120710'           PP,CAR,SD                                    
         DC    F'80440'            SOC,GD                                       
         DC    F'63565'            VO,SS                                        
         DC    F'35660'            GS                                           
         DC    F'120710'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'80440'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'121535'           PP,CAR,SD                                    
         DC    F'81065'            SOC,GD                                       
         DC    F'64030'            VO,SS                                        
         DC    F'35970'            GS                                           
         DC    F'121535'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'81065'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'122380'           PP,CAR,SD                                    
         DC    F'81665'            SOC,GD                                       
         DC    F'64505'            VO,SS                                        
         DC    F'36290'            GS                                           
         DC    F'122380'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'81665'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'159'              PP,CAR,SD                                    
         DC    F'122'              SOC,GD                                       
         DC    F'91'               VO,SS                                        
         DC    F'49'               GS                                           
         DC    F'159'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'122'              ST, SAME AS SOC                              
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
*                                  1805:TABLE B                                 
         DC    AL1(60,40,1,5)      TV WSP&NET SPC/NETWORK UNITS 1-5             
         DC    F'85340'            PP,CAR,SD                                    
         DC    F'62620'            SOC,GD                                       
         DC    F'39825'            VO,SS                                        
         DC    F'22785'            GS                                           
         DC    F'85340'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62620'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'87625'            PP,CAR,SD                                    
         DC    F'63990'            SOC,GD                                       
         DC    F'41260'            VO,SS                                        
         DC    F'23495'            GS                                           
         DC    F'87625'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63990'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'89910'            PP,CAR,SD                                    
         DC    F'65410'            SOC,GD                                       
         DC    F'42720'            VO,SS                                        
         DC    F'24260'            GS                                           
         DC    F'89910'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65410'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'92245'            PP,CAR,SD                                    
         DC    F'66840'            SOC,GD                                       
         DC    F'44130'            VO,SS                                        
         DC    F'25015'            GS                                           
         DC    F'92245'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66840'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'94540'            PP,CAR,SD                                    
         DC    F'68285'            SOC,GD                                       
         DC    F'45665'            VO,SS                                        
         DC    F'25805'            GS                                           
         DC    F'94540'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'96750'            PP,CAR,SD                                    
         DC    F'69675'            SOC,GD                                       
         DC    F'47050'            VO,SS                                        
         DC    F'26515'            GS                                           
         DC    F'96750'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69675'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'99095'            PP,CAR,SD                                    
         DC    F'71080'            SOC,GD                                       
         DC    F'48500'            VO,SS                                        
         DC    F'27100'            GS                                           
         DC    F'99095'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71080'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'101340'           PP,CAR,SD                                    
         DC    F'72495'            SOC,GD                                       
         DC    F'49950'            VO,SS                                        
         DC    F'27700'            GS                                           
         DC    F'101340'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72495'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'103600'           PP,CAR,SD                                    
         DC    F'73870'            SOC,GD                                       
         DC    F'51405'            VO,SS                                        
         DC    F'28295'            GS                                           
         DC    F'103600'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73870'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'105935'           PP,CAR,SD                                    
         DC    F'75340'            SOC,GD                                       
         DC    F'52915'            VO,SS                                        
         DC    F'28895'            GS                                           
         DC    F'105935'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75340'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'108205'           PP,CAR,SD                                    
         DC    F'76750'            SOC,GD                                       
         DC    F'54305'            VO,SS                                        
         DC    F'29450'            GS                                           
         DC    F'108205'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76750'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'110475'           PP,CAR,SD                                    
         DC    F'78090'            SOC,GD                                       
         DC    F'55700'            VO,SS                                        
         DC    F'30015'            GS                                           
         DC    F'110475'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78090'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'112730'           PP,CAR,SD                                    
         DC    F'79540'            SOC,GD                                       
         DC    F'57135'            VO,SS                                        
         DC    F'30645'            GS                                           
         DC    F'112730'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79540'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'115050'           PP,CAR,SD                                    
         DC    F'80955'            SOC,GD                                       
         DC    F'58535'            VO,SS                                        
         DC    F'31200'            GS                                           
         DC    F'115050'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'80955'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'117270'           PP,CAR,SD                                    
         DC    F'82385'            SOC,GD                                       
         DC    F'59960'            VO,SS                                        
         DC    F'31735'            GS                                           
         DC    F'117270'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'82385'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'119580'           PP,CAR,SD                                    
         DC    F'83740'            SOC,GD                                       
         DC    F'61415'            VO,SS                                        
         DC    F'32550'            GS                                           
         DC    F'119580'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'83740'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'121910'           PP,CAR,SD                                    
         DC    F'84965'            SOC,GD                                       
         DC    F'62715'            VO,SS                                        
         DC    F'32860'            GS                                           
         DC    F'121910'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'84965'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'124220'           PP,CAR,SD                                    
         DC    F'86160'            SOC,GD                                       
         DC    F'64145'            VO,SS                                        
         DC    F'33360'            GS                                           
         DC    F'124220'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'86160'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'126450'           PP,CAR,SD                                    
         DC    F'87370'            SOC,GD                                       
         DC    F'65600'            VO,SS                                        
         DC    F'33835'            GS                                           
         DC    F'126450'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87370'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'128690'           PP,CAR,SD                                    
         DC    F'88590'            SOC,GD                                       
         DC    F'66970'            VO,SS                                        
         DC    F'34360'            GS                                           
         DC    F'128690'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'88590'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'131025'           PP,CAR,SD                                    
         DC    F'89775'            SOC,GD                                       
         DC    F'68380'            VO,SS                                        
         DC    F'34885'            GS                                           
         DC    F'131025'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'89775'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'133265'           PP,CAR,SD                                    
         DC    F'90920'            SOC,GD                                       
         DC    F'69280'            VO,SS                                        
         DC    F'35390'            GS                                           
         DC    F'133265'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'135535'           PP,CAR,SD                                    
         DC    F'92135'            SOC,GD                                       
         DC    F'70190'            VO,SS                                        
         DC    F'35895'            GS                                           
         DC    F'135535'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'92135'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'137695'           PP,CAR,SD                                    
         DC    F'93325'            SOC,GD                                       
         DC    F'71040'            VO,SS                                        
         DC    F'36385'            GS                                           
         DC    F'137695'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93325'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'139980'           PP,CAR,SD                                    
         DC    F'94575'            SOC,GD                                       
         DC    F'71920'            VO,SS                                        
         DC    F'36850'            GS                                           
         DC    F'139980'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'94575'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'142220'           PP,CAR,SD                                    
         DC    F'95725'            SOC,GD                                       
         DC    F'72795'            VO,SS                                        
         DC    F'37395'            GS                                           
         DC    F'142220'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'95725'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'144025'           PP,CAR,SD                                    
         DC    F'96910'            SOC,GD                                       
         DC    F'73635'            VO,SS                                        
         DC    F'37915'            GS                                           
         DC    F'144025'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96910'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'145690'           PP,CAR,SD                                    
         DC    F'98010'            SOC,GD                                       
         DC    F'74470'            VO,SS                                        
         DC    F'38330'            GS                                           
         DC    F'145690'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'98010'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'147400'           PP,CAR,SD                                    
         DC    F'99175'            SOC,GD                                       
         DC    F'75380'            VO,SS                                        
         DC    F'38800'            GS                                           
         DC    F'147400'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'99175'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'149200'           PP,CAR,SD                                    
         DC    F'99960'            SOC,GD                                       
         DC    F'76220'            VO,SS                                        
         DC    F'39255'            GS                                           
         DC    F'149200'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'99960'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'150910'           PP,CAR,SD                                    
         DC    F'101465'           SOC,GD                                       
         DC    F'77155'            VO,SS                                        
         DC    F'39730'            GS                                           
         DC    F'150910'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'101465'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'152645'           PP,CAR,SD                                    
         DC    F'102390'           SOC,GD                                       
         DC    F'77715'            VO,SS                                        
         DC    F'40240'            GS                                           
         DC    F'152645'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'102390'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'154365'           PP,CAR,SD                                    
         DC    F'103265'           SOC,GD                                       
         DC    F'78295'            VO,SS                                        
         DC    F'40715'            GS                                           
         DC    F'154365'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'103265'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'156095'           PP,CAR,SD                                    
         DC    F'104110'           SOC,GD                                       
         DC    F'78855'            VO,SS                                        
         DC    F'41185'            GS                                           
         DC    F'156095'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'104110'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'157875'           PP,CAR,SD                                    
         DC    F'104965'           SOC,GD                                       
         DC    F'79420'            VO,SS                                        
         DC    F'41630'            GS                                           
         DC    F'157875'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'104965'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'159555'           PP,CAR,SD                                    
         DC    F'105855'           SOC,GD                                       
         DC    F'80040'            VO,SS                                        
         DC    F'42095'            GS                                           
         DC    F'159555'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'105855'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'160750'           PP,CAR,SD                                    
         DC    F'106670'           SOC,GD                                       
         DC    F'80605'            VO,SS                                        
         DC    F'42435'            GS                                           
         DC    F'160750'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'106670'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'161865'           PP,CAR,SD                                    
         DC    F'107505'           SOC,GD                                       
         DC    F'81175'            VO,SS                                        
         DC    F'42890'            GS                                           
         DC    F'161865'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'107505'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'163025'           PP,CAR,SD                                    
         DC    F'108410'           SOC,GD                                       
         DC    F'81720'            VO,SS                                        
         DC    F'43300'            GS                                           
         DC    F'163025'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'108410'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'164245'           PP,CAR,SD                                    
         DC    F'109230'           SOC,GD                                       
         DC    F'82325'            VO,SS                                        
         DC    F'43680'            GS                                           
         DC    F'164245'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'109230'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'165355'           PP,CAR,SD                                    
         DC    F'109995'           SOC,GD                                       
         DC    F'82930'            VO,SS                                        
         DC    F'44115'            GS                                           
         DC    F'165355'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'109995'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'166580'           PP,CAR,SD                                    
         DC    F'110890'           SOC,GD                                       
         DC    F'83425'            VO,SS                                        
         DC    F'44460'            GS                                           
         DC    F'166580'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'110890'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'167690'           PP,CAR,SD                                    
         DC    F'111665'           SOC,GD                                       
         DC    F'84010'            VO,SS                                        
         DC    F'44775'            GS                                           
         DC    F'167690'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'111665'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'168810'           PP,CAR,SD                                    
         DC    F'112445'           SOC,GD                                       
         DC    F'84545'            VO,SS                                        
         DC    F'45205'            GS                                           
         DC    F'168810'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'112445'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'170010'           PP,CAR,SD                                    
         DC    F'113360'           SOC,GD                                       
         DC    F'85085'            VO,SS                                        
         DC    F'45485'            GS                                           
         DC    F'170010'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'113360'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'171155'           PP,CAR,SD                                    
         DC    F'114165'           SOC,GD                                       
         DC    F'85680'            VO,SS                                        
         DC    F'45910'            GS                                           
         DC    F'171155'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'114165'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'221'              PP,CAR,SD                                    
         DC    F'170'              SOC,GD                                       
         DC    F'112'              VO,SS                                        
         DC    F'59'               GS                                           
         DC    F'221'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'170'              ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  906:FEE DETAINED IN AUDITION                 
CAUTTBL  DC    AL1(68,44,0,0)      AUDITION INTERVIEW - TV                      
         DC    F'8150'             PP,CAR,SD                                    
         DC    F'8150'             SOC,GD                                       
         DC    F'8150'             VO,SS                                        
         DC    F'8150'             GS                                           
         DC    F'8150'             SA                                           
         DC    F'8150'             DEM                                          
         DC    F'8150'             E                                            
         DC    F'8150'             GE                                           
         DC    F'8150'             ST                                           
         DC    F'8150'             EVERYBODY ELSE-US,SI,SB,PT                   
         SPACE 1                                                                
CAURTBL  DC    AL1(69,12,0,0)      AUDITION INTERVIEW - RADIO                   
         DC    F'8150'             SS,SV                                        
         DC    F'8150'             MV,GS                                        
         EJECT                                                                  
*                                  1203.C:DEMO AND TEST COMMERCIAL              
CDMTBL   DC    AL1(70,44,2,2)      TV PRESENTATION DEMO 2                       
         DC    F'30000'            PP,CAR,SD                                    
         DC    F'30000'            SOC,GD                                       
         DC    F'30000'            VO,SS                                        
         DC    F'30000'            GS                                           
         DC    F'30000'            SA                                           
         DC    F'30000'            DEM                                          
         DC    F'30000'            E                                            
         DC    F'30000'            GE                                           
         DC    F'30000'            ST                                           
         DC    F'30000'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'15100'            PP,CAR,SD                                    
         DC    F'15100'            SOC,GD                                       
         DC    F'15100'            VO,SS                                        
         DC    F'15100'            GS                                           
         DC    F'15100'            SA                                           
         DC    F'15100'            DEM                                          
         DC    F'15100'            E                                            
         DC    F'15100'            GE                                           
         DC    F'15100'            ST                                           
         DC    F'15100'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
*                                  1202:SESSION FEE                             
BSCHTBL  DC    AL1(80,40,0,0)      NATIONAL TV  - HOURLY RATE                   
         DC    F'9800'             PP,SD,CAR                                    
         DC    F'9800'             SOC,GD                                       
         DC    F'8150'             VO,SS                                        
         DC    F'8150'             GS                                           
         DC    F'14700'            SA = PP+50%                                  
         DC    F'9800'             DEM                                          
         DC    F'6050'             E                                            
         DC    F'4000'             GE                                           
         DC    F'9800'             ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(81,40,0,0)      NATIONAL TV  - OVERTIME HOURLY RATE          
         DC    F'12750'            PP,SD,CAR                                    
         DC    F'12750'            SOC,GD                                       
         DC    F'10250'            VO,SS                                        
         DC    F'10250'            GS                                           
         DC    F'19125'            SA = PP+50%                                  
         DC    F'12750'            DEM                                          
         DC    F'7250'             E                                            
         DC    F'5150'             GE                                           
         DC    F'12750'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(82,40,0,0)      NATIONAL TV  - DBLTIME HOURLY RATE           
         DC    F'14400'            PP,SD,CAR                                    
         DC    F'14400'            SOC,GD                                       
         DC    F'12750'            VO,SS                                        
         DC    F'12750'            GS                                           
         DC    F'21600'            SA = PP+50%                                  
         DC    F'14400'            DEM                                          
         DC    F'9050'             E                                            
         DC    F'5900'             GE                                           
         DC    F'14400'            ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  1820:NEW MEDIA                               
CNMTBL   DC    AL1(100,40,0,0)     NEW MEDIA VIDEO 4 WEEKS - 35%                
         DC    F'27000'            PP,SD,CAR                                    
         DC    F'27000'            SOC,GD                                       
         DC    F'19700'            VO,SS                                        
         DC    F'8500'             GS                                           
         DC    F'40500'            SA = PP+50%                                  
         DC    F'27000'            DEM                                          
         DC    F'16550'            E                                            
         DC    F'11050'            GE                                           
         DC    F'27000'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(101,12,0,0)     NEW MEDIA AUDIO 4 WEEKS - 35%                
         DC    F'21050'            SS,SV                                        
         DC    F'15800'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(110,40,0,0)     NEW MEDIA VIDEO 8 WEEKS - 50%                
         DC    F'38600'            PP,SD,CAR                                    
         DC    F'38600'            SOC,GD                                       
         DC    F'28150'            VO,SS                                        
         DC    F'12200'            GS                                           
         DC    F'57900'            SA = PP+50%                                  
         DC    F'38600'            DEM                                          
         DC    F'23650'            E                                            
         DC    F'15750'            GE                                           
         DC    F'38600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(111,12,0,0)     NEW MEDIA AUDIO 8 WEEKS - 50%                
         DC    F'30050'            SS,SV                                        
         DC    F'22550'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(120,40,0,0)     NEW MEDIA VIDEO 26 WEEKS - 75%               
         DC    F'57850'            PP,SD,CAR                                    
         DC    F'57850'            SOC,GD                                       
         DC    F'42200'            VO,SS                                        
         DC    F'18250'            GS                                           
         DC    F'86775'            SA = PP+50%                                  
         DC    F'57850'            DEM                                          
         DC    F'35450'            E                                            
         DC    F'23650'            GE                                           
         DC    F'57850'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(121,12,0,0)     NEW MEDIA AUDIO 26 WEEKS - 75%               
         DC    F'45100'            SS,SV                                        
         DC    F'33850'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(130,40,0,0)     NEW MEDIA VIDEO 1 YEAR - 100%                
         DC    F'77150'            PP,SD,CAR                                    
         DC    F'77150'            SOC,GD                                       
         DC    F'56250'            VO,SS                                        
         DC    F'24350'            GS                                           
         DC    F'115725'           SA = PP+50%                                  
         DC    F'77150'            DEM                                          
         DC    F'47250'            E                                            
         DC    F'31500'            GE                                           
         DC    F'77150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(131,12,0,0)     NEW MEDIA AUDIO 1 YEAR - 100%                
         DC    F'60075'            SS,SV                                        
         DC    F'45075'            MV,GS                                        
         SPACE 1                                                                
CNMATBL  DC    AL1(140,12,0,0)     NEW MEDIA AUDIO 4 WEEKS ADDTL CUTS           
         DC    F'10550'            SS,SV                                        
         DC    F'7900'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(141,12,0,0)     NEW MEDIA AUDIO 8 WEEKS ADDTL CUTS           
         DC    F'15050'            SS,SV                                        
         DC    F'11300'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(142,12,0,0)     NEW MEDIA AUDIO 26 WEEKS ADDTL CUTS          
         DC    F'22550'            SS,SV                                        
         DC    F'16950'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(143,12,0,0)     NEW MEDIA AUDIO 1 YEAR ADDTL CUTS            
         DC    F'30050'            SS,SV                                        
         DC    F'22550'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(144,12,0,0)     BSC NEW MEDIA AUDIO ADDTL CUTS               
         DC    F'30050'            SS,SV                                        
         DC    F'22550'            MV,GS                                        
         SPACE 1                                                                
*                                  1820:NEW MEDIA                               
NMCTBL   DC    AL1(150,40,0,0)     NEW MEDIA VIDEO 3 MONTHS                     
         DC    F'17500'            PP,SD,CAR                                    
         DC    F'17500'            SOC,GD                                       
         DC    F'13100'            VO,SS                                        
         DC    F'5600'             GS                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'17500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(151,12,0,0)     NEW MEDIA AUDIO 3 MONTHS                     
         DC    F'13800'            SS,SV                                        
         DC    F'10400'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(160,40,0,0)     NEW MEDIA VIDEO 4-6 MONTHS                   
         DC    F'32500'            PP,SD,CAR                                    
         DC    F'32500'            SOC,GD                                       
         DC    F'23800'            VO,SS                                        
         DC    F'10300'            GS                                           
         DC    F'0'                SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'32500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(161,12,0,0)     NEW MEDIA AUDIO 4-6 MONTHS                   
         DC    F'25400'            SS,SV                                        
         DC    F'19000'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(170,40,0,0)     NEW MEDIA VIDEO 7-12 MONTHS                  
         DC    F'45000'            PP,SD,CAR                                    
         DC    F'45000'            SOC,GD                                       
         DC    F'32800'            VO,SS                                        
         DC    F'14000'            GS                                           
         DC    F'0'                SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'45000'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(171,12,0,0)     NEW MEDIA AUDIO 7-12 MONTHS                  
         DC    F'34700'            SS,SV                                        
         DC    F'26000'            MV,GS                                        
         SPACE 1                                                                
         DC    X'FF'               *** END OF USE RATE TABLES ***               
         EJECT                                                                  
*--------------------------------------------------------------------*          
* AS PER DIANE RAMOS, (NOV 3, 2011), "IRRELEVANT FOR TP AS WE DO NOT *          
* HAVE ANY CLIENTS WHO DO L&R COMMERCIALS."                          *          
*--------------------------------------------------------------------*          
         DC    AL1(5,36,0,0)       LOCAL AND REGIONAL TV 1                      
         DC    F'48590'                                                         
         DC    F'46860'                                                         
         DC    F'24070'                                                         
         DC    F'18060'                                                         
         DC    F'72890'                                                         
         DC    F'0'                                                             
         DC    F'27530'                                                         
         DC    F'27530'                                                         
         SPACE 1                                                                
         DC    AL1(6,36,0,0)       LOCAL AND REGIONAL TV 2                      
         DC    F'40310'                                                         
         DC    F'38710'                                                         
         DC    F'14250'                                                         
         DC    F'8550'                                                          
         DC    F'60470'                                                         
         DC    F'0'                                                             
         DC    F'13870'                                                         
         DC    F'13870'                                                         
         SPACE 1                                                                
         DC    AL1(7,36,0,0)       LOCAL AND REGIONAL TV 3                      
         DC    F'34920'                                                         
         DC    F'33240'                                                         
         DC    F'11090'                                                         
         DC    F'6630'                                                          
         DC    F'52380'                                                         
         DC    F'0'                                                             
         DC    F'13870'                                                         
         DC    F'13870'                                                         
         SPACE 1                                                                
*--------------------------------------------------------------------*          
* AS PER DIANE RAMOS, (NOV 3, 2011), "IRRELEVANT FOR TP AS WE DO NOT *          
* HAVE ANY CLIENTS WHO DO L&R COMMERCIALS."                          *          
*--------------------------------------------------------------------*          
         DC    AL1(17,24,0,0)      LOCAL AND REGIONAL TV 1                      
         DC    F'48690'                                                         
         DC    F'46860'                                                         
         DC    F'24070'                                                         
         DC    F'18060'                                                         
         DC    F'73040'                                                         
         SPACE 1                                                                
         DC    AL1(18,24,0,0)      LOCAL AND REGIONAL TV 2                      
         DC    F'40310'                                                         
         DC    F'38710'                                                         
         DC    F'14250'                                                         
         DC    F'8550'                                                          
         DC    F'60470'                                                         
         SPACE 1                                                                
         DC    AL1(19,24,0,0)      LOCAL AND REGIONAL TV 3                      
         DC    F'34920'                                                         
         DC    F'33240'                                                         
         DC    F'11090'                                                         
         DC    F'6630'                                                          
         DC    F'52380'                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
* AS PER DIANE RAMOS, (NOV 3, 2011), "IRRELEVANT FOR TP AS WE DO NOT *          
* HAVE ANY CLIENTS WHO DO L&R COMMERCIALS."                          *          
*--------------------------------------------------------------------*          
         DC    AL1(29,12,0,0)      LOCAL AND REGIONAL RADIO 1                   
         DC    F'39410'                                                         
         DC    F'29715'                                                         
         SPACE 1                                                                
         DC    AL1(30,12,0,0)      LOCAL AND REGIONAL RADIO 2                   
         DC    F'25610'                                                         
         DC    F'15390'                                                         
         SPACE 1                                                                
         DC    AL1(31,12,0,0)      LOCAL AND REGIONAL RADIO 3                   
         DC    F'22910'                                                         
         DC    F'13745'                                                         
         SPACE 1                                                                
*              USE LOOK UP TABLES                                               
         SPACE 2                                                                
USELUT   DS    0CL5                                                             
         DC    AL1(00,UBSC,ALL,ACT,0,0,0,TV+NEWMEDIA)    TV SESSION             
         DC    AL1(80,UBSC,ALL,ACT,0,0,0,TV)    TV SESSION HOURLY RATE          
         DC    AL1(81,UBSC,ALL,ACT,0,0,0,TV)    TV SESSION HOURLY OT            
         DC    AL1(82,UBSC,ALL,ACT,0,0,0,TV)    TV SESSION HOURLY DT            
         DC    AL1(24,UBSC,ALL,ACT,0,0,0,RADIO+NEWMEDIA) RADIO SESSION          
         DC    AL1(12,UDOR,ALL,ACT,0,0,0,TV)    TV DORMANCY                     
         DC    AL1(48,UWSC,ALL,ACT,0,0,0,TV)    TV WILDSPOT                     
         DC    AL1(60,UWSM,ALL,ACT,0,0,0,TV)    TV WILDSPOT COMBINED            
         DC    AL1(60,UNET,ALL,ACT,0,0,0,TV)    TV NETWORK                      
         DC    AL1(68,UCAU,ALL,ACT,0,0,0,TV)    AUDITION                        
         DC    AL1(69,UCAU,ALL,ACT,0,0,0,RADIO) AUDITION                        
         DC    AL1(70,UCDM,ALL,ACT,0,0,0,TV)    TV PRESENTATION DEMO            
         DC    AL1(100,UCNM,UCNM4W,ACT,0,0,0,NEWMEDIA) NEW MEDIA 4 WK           
         DC    AL1(110,UCNM,UCNM8W,ACT,0,0,0,NEWMEDIA) NEW MEDIA 8 WK           
         DC    AL1(120,UCNM,UCNM26W,ACT,0,0,0,NEWMEDIA) NEW MEDIA 26 WK         
         DC    AL1(130,UCNM,UCNM1Y,ACT,0,0,0,NEWMEDIA) NEW MEDIA 1 YR           
         DC    AL1(140,UCNM,UCNM4W,ACT,0,0,0,NEWMEDIA) 4 WK ADDTL CUTS          
         DC    AL1(141,UCNM,UCNM8W,ACT,0,0,0,NEWMEDIA) 8 WK ADDTL CUTS          
         DC    AL1(142,UCNM,UCNM26W,ACT,0,0,0,NEWMEDIA) 26 WK ADDT CUTS         
         DC    AL1(143,UCNM,UCNM1Y,ACT,0,0,0,NEWMEDIA) 1 YR ADDTL CUTS          
         DC    AL1(144,UBSC,ALL,ACT,0,0,0,NEWMEDIA) AUDIO ADDTL CUTS            
         DC    AL1(150,UNMC,UNMC3M,ACT,0,0,0,NEWMEDIA) NEW MEDIA 4 WK           
         DC    AL1(160,UNMC,UNMC6M,ACT,0,0,0,NEWMEDIA) NEW MEDIA 8 WK           
         DC    AL1(170,UNMC,UNMC1Y,ACT,0,0,0,NEWMEDIA) NEW MEDIA 1 YR           
         DC    X'FF'                                                            
         EJECT                                                                  
*              PERCENTAGE TABLES FOR MULTITRACKING & SWEETENING                 
         SPACE 3                                                                
MSWEET   DS    0CL3                                                             
         DC    X'FF',AL2(50)                            ALL OTHERS              
         SPACE 3                                                                
*              MAJORS TABLE                                                     
         SPACE 3                                                                
MAJLUT   DS    0CL2                                                             
         DC    AL1(0,0)                                                         
         DC    AL1(1,NY)                                                        
         DC    AL1(2,CHI)                                                       
         DC    AL1(2,LA)                                                        
         DC    AL1(3,NY+CHI)                                                    
         DC    AL1(3,NY+LA)                                                     
         DC    AL1(3,CHI+LA)                                                    
         DC    AL1(4,NY+CHI+LA)                                                 
         EJECT                                                                  
*              COLUMN TABLES - TV - ON CAMERA                                   
         SPACE 1                                                                
ONCOLS   DS    0CL2                                                             
         DC    AL1(1,CTPP)         PRINCIPAL PERFORMER                          
         DC    AL1(1,CTSD)         SOLO DANCER                                  
         DC    AL1(1,CTCAR)        CARTOONIST                                   
         DC    AL1(2,CTSOC)        SILENT ON-CAMERA                             
         DC    AL1(2,CTGD)         GROUP DANCER                                 
         DC    AL1(3,CTSS)         SOLO SINGER                                  
         DC    AL1(3,CTSV)         SOLO VOICE                                   
         DC    AL1(4,CTGS)         GROUP SINGER                                 
         DC    AL1(4,CTMV)         MULTIPLE VOIC                                
         DC    AL1(5,CTSA)         SPECIALTY ACT                                
         DC    AL1(6,CTDEM)        DEMONSTRATOR                                 
         DC    AL1(7,CTE)          EXTRA                                        
         DC    AL1(8,CTGE)         GROUP EXTRA                                  
         DC    AL1(9,CTST)         STUNT PERFORMER                              
         DC    AL1(10,CTUS)        UNDERSTUDY                                   
         DC    AL1(10,CTSI)        STANDIN                                      
         DC    AL1(10,CTSB)        STANDBY                                      
         DC    AL1(10,CTPT)        PUPPETEER                                    
*                                  IF DEFINE ROW >=18, CHANGE SYSCALC           
         DC    AL1(60,CTSSM)       SOLO/DUE WITH MULTI                          
         DC    AL1(60,CTSS1)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS2)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS3)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS4)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTGSM)       GROUP SINGERS WITH MULTI                     
         DC    AL1(60,CTGSS)       GROUP SINGERS SWEETENING                     
         DC    AL1(60,CTZZZ)       OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    AL1(60,CTZZ)        OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - TV - OFF CAMERA                                  
         SPACE 1                                                                
OFFCOLS  DS    0CL2                                                             
         DC    AL1(1,CTCAR)        CARTOONIST                                   
         DC    AL1(2,CTGD)         GROUP DANCER                                 
         DC    AL1(3,CTSS)         SOLO SINGER                                  
         DC    AL1(3,CTVO)         VOICE OVER                                   
         DC    AL1(3,CTSV)         SINGLE OR SINGER VOICE                       
         DC    AL1(4,CTGS)         GROUP SINGER                                 
         DC    AL1(4,CTMV)         MULTIPLE VOICE                               
         DC    AL1(5,CTSA)         SPECIALTY ACT                                
*                                  IF DEFINE ROW >=18, CHANGE SYSCALC           
         DC    AL1(60,CTSSM)       SOLO/DUE WITH MULTI                          
         DC    AL1(60,CTSS1)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS2)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS3)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS4)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTGSM)       GROUP SINGERS WITH MULTI                     
         DC    AL1(60,CTGSS)       GROUP SINGERS SWEETENING                     
         DC    AL1(60,CTZZZ)       OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    AL1(60,CTZZ)        OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - RADIO                                            
         SPACE 1                                                                
RADCOLS  DS    0CL2                                                             
         DC    AL1(1,CTSV)         SINGLE OR SINGER VOICE                       
         DC    AL1(1,CTSS)         SOLO SINGER                                  
         DC    AL1(2,CTGS)         GROUP SINGER                                 
         DC    AL1(2,CTMV)         MULTIPLE VOICE                               
*                                  IF DEFINE ROW >=9, CHANGE SYSCALC            
         DC    AL1(60,CTVO)        VOICE OVER                                   
         DC    AL1(60,CTSSM)       SOLO/DUE WITH MULTI                          
         DC    AL1(60,CTSS1)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS2)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS3)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS4)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTGSM)       GROUP SINGERS WITH MULTI                     
         DC    AL1(60,CTGSS)       GROUP SINGERS SWEETENING                     
         DC    AL1(60,CTGD)        GROUP DANCER                                 
         DC    AL1(60,CTSA)        SPECIALTY ACT                                
         DC    AL1(60,CTCAR)       CARTOONIST                                   
         DC    AL1(60,CTZZZ)       OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    AL1(60,CTZZ)        OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - AFM                                              
         SPACE 3                                                                
AFMCOLS  DS    0CL2                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TASYSEQUS                                                      
         EJECT                                                                  
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014TAGEN77   10/09/15'                                      
         END                                                                    
