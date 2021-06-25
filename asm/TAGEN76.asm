*          DATA SET TAGEN76    AT LEVEL 020 AS OF 10/09/15                      
*PHASE T70276C,*                                                                
         TITLE 'T70276 - TABLES FOR YEAR 1 ACTRA 2014-2015'                     
T70276   CSECT                                                                  
         DC    AL4(USETBLS-T70276)                                              
         DC    AL4(USELUT-T70276)                                               
         DC    AL4(MAJLUT-T70276)                                               
         DC    AL4(AFMCOLS-T70276)                                              
         DC    AL4(RADCOLS-T70276)                                              
         DC    AL4(OFFCOLS-T70276)                                              
         DC    AL4(ONCOLS-T70276)                                               
         DC    AL4(MSWEET-T70276)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
*                                  1202:SESSION FEE                             
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'75650'            PP,SD,CAR                                    
         DC    F'75650'            SOC,GD                                       
         DC    F'55150'            VO,SS                                        
         DC    F'23850'            GS                                           
         DC    F'113475'           SA = PP+50%                                  
         DC    F'75650'            DEM                                          
         DC    F'46300'            E                                            
         DC    F'30900'            GE                                           
         DC    F'75650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'75650'            PP,SD,CAR                                    
         DC    F'75650'            SOC,GD                                       
         DC    F'55150'            VO,SS                                        
         DC    F'23850'            GS                                           
         DC    F'113475'           SA = PP+50%                                  
         DC    F'75650'            DEM                                          
         DC    F'46300'            E                                            
         DC    F'30900'            GE                                           
         DC    F'75650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'75650'            PP,SD,CAR                                    
         DC    F'75650'            SOC,GD                                       
         DC    F'55150'            VO,SS                                        
         DC    F'23850'            GS                                           
         DC    F'113475'           SA = PP+50%                                  
         DC    F'75650'            DEM                                          
         DC    F'46300'            E                                            
         DC    F'30900'            GE                                           
         DC    F'75650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'75650'            PP,SD,CAR                                    
         DC    F'75650'            SOC,GD                                       
         DC    F'55150'            VO,SS                                        
         DC    F'23850'            GS                                           
         DC    F'113475'           SA = PP+50%                                  
         DC    F'75650'            DEM                                          
         DC    F'46300'            E                                            
         DC    F'30900'            GE                                           
         DC    F'75650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'75650'            PP,SD,CAR                                    
         DC    F'75650'            SOC,GD                                       
         DC    F'55150'            VO,SS                                        
         DC    F'23850'            GS                                           
         DC    F'113475'           SA = PP+50%                                  
         DC    F'75650'            DEM                                          
         DC    F'46300'            E                                            
         DC    F'30900'            GE                                           
         DC    F'75650'            ST, SAME AS SOC                              
         SPACE 1                                                                
*--------------------------------------------------------------------*          
         DC    AL1(8,40,0,0)       1820:SESSION FEE NEW MEDIA VIDEO             
         DC    F'75650'            PP,SD,CAR                                    
         DC    F'75650'            SOC,GD                                       
         DC    F'55150'            VO,SS                                        
         DC    F'23850'            GROUP SINGER                                 
         DC    F'113475'           SA = PP+50%                                  
         DC    F'75650'                                                         
         DC    F'46300'            BACKGROUND                                   
         DC    F'30900'            GROUP BACKGROUND                             
         DC    F'75650'            ST, SAME AS SOC                              
         EJECT                                                                  
DORTTBL  DC    AL1(12,40,0,0)      NATIONAL TV                                  
         DC    F'75650'            PP,SD,CAR                                    
         DC    F'75650'            SOC,GD                                       
         DC    F'55150'            VO,SS                                        
         DC    F'23850'            GS                                           
         DC    F'113475'           SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75650'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'75650'                                                         
         DC    F'75650'                                                         
         DC    F'55150'                                                         
         DC    F'23850'                                                         
         DC    F'113475'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75650'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'75650'                                                         
         DC    F'75650'                                                         
         DC    F'55150'                                                         
         DC    F'23850'                                                         
         DC    F'113475'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75650'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'75650'                                                         
         DC    F'75650'                                                         
         DC    F'55150'                                                         
         DC    F'23850'                                                         
         DC    F'113475'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75650'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'75650'                                                         
         DC    F'75650'                                                         
         DC    F'55150'                                                         
         DC    F'23850'                                                         
         DC    F'113475'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75650'            ST SAME AS SOC                               
         SPACE 1                                                                
*                                  2101:SESSION AND RESIDUAL FEES               
BSCRTBL  DC    AL1(24,12,0,0)      NATIONAL RADIO                               
         DC    F'58900'            SV,SS                                        
         DC    F'44200'            MV,GS                                        
         SPACE 1                                                                
*                                  406:RADIO SESSION AND RESIDUAL FEES          
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'30050'                                                         
         DC    F'22600'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'39700'                                                         
         DC    F'29700'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'45050'                                                         
         DC    F'33900'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'51050'                                                         
         DC    F'38350'                                                         
         SPACE 1                                                                
*--------------------------------------------------------------------*          
         DC    AL1(33,12,0,0)      1820:SESSION FEE NEW MEDIA AUDIO             
         DC    F'58900'            SV,SS                                        
         DC    F'44200'            MV,GS                                        
         EJECT                                                                  
*                                  1804:TABLE A                                 
WSCTBL   DC    AL1(48,40,1,5)      TV WILDSPOT UNITS 1-5                        
         DC    F'59835'            PP,CAR,SD                                    
         DC    F'43855'            SOC,GD                                       
         DC    F'29285'            VO,SS                                        
         DC    F'17540'            GS                                           
         DC    F'59835'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43855'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'61400'            PP,CAR,SD                                    
         DC    F'44850'            SOC,GD                                       
         DC    F'30365'            VO,SS                                        
         DC    F'18075'            GS                                           
         DC    F'61400'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44850'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'63015'            PP,CAR,SD                                    
         DC    F'45875'            SOC,GD                                       
         DC    F'31385'            VO,SS                                        
         DC    F'18700'            GS                                           
         DC    F'63015'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45875'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'64625'            PP,CAR,SD                                    
         DC    F'46880'            SOC,GD                                       
         DC    F'32435'            VO,SS                                        
         DC    F'19295'            GS                                           
         DC    F'64625'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46880'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'66205'            PP,CAR,SD                                    
         DC    F'47865'            SOC,GD                                       
         DC    F'33445'            VO,SS                                        
         DC    F'19900'            GS                                           
         DC    F'66205'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47865'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'67805'            PP,CAR,SD                                    
         DC    F'48865'            SOC,GD                                       
         DC    F'34490'            VO,SS                                        
         DC    F'20480'            GS                                           
         DC    F'67805'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48865'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'69405'            PP,CAR,SD                                    
         DC    F'49820'            SOC,GD                                       
         DC    F'35510'            VO,SS                                        
         DC    F'20875'            GS                                           
         DC    F'69405'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49820'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'70975'            PP,CAR,SD                                    
         DC    F'50835'            SOC,GD                                       
         DC    F'36570'            VO,SS                                        
         DC    F'21360'            GS                                           
         DC    F'70975'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50835'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'72630'            PP,CAR,SD                                    
         DC    F'51875'            SOC,GD                                       
         DC    F'37695'            VO,SS                                        
         DC    F'21735'            GS                                           
         DC    F'72630'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51875'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'74730'            PP,CAR,SD                                    
         DC    F'52775'            SOC,GD                                       
         DC    F'38705'            VO,SS                                        
         DC    F'22190'            GS                                           
         DC    F'74730'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52775'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'75785'            PP,CAR,SD                                    
         DC    F'53830'            SOC,GD                                       
         DC    F'39710'            VO,SS                                        
         DC    F'22635'            GS                                           
         DC    F'75785'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53830'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'77370'            PP,CAR,SD                                    
         DC    F'54825'            SOC,GD                                       
         DC    F'40790'            VO,SS                                        
         DC    F'22990'            GS                                           
         DC    F'77370'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54825'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'79710'            PP,CAR,SD                                    
         DC    F'55805'            SOC,GD                                       
         DC    F'41810'            VO,SS                                        
         DC    F'23495'            GS                                           
         DC    F'79710'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55805'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'81270'            PP,CAR,SD                                    
         DC    F'56795'            SOC,GD                                       
         DC    F'42830'            VO,SS                                        
         DC    F'23910'            GS                                           
         DC    F'81270'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56795'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'82100'            PP,CAR,SD                                    
         DC    F'57800'            SOC,GD                                       
         DC    F'43895'            VO,SS                                        
         DC    F'24315'            GS                                           
         DC    F'82100'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57800'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'83770'            PP,CAR,SD                                    
         DC    F'58790'            SOC,GD                                       
         DC    F'44930'            VO,SS                                        
         DC    F'24725'            GS                                           
         DC    F'83770'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58790'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'85370'            PP,CAR,SD                                    
         DC    F'59585'            SOC,GD                                       
         DC    F'46005'            VO,SS                                        
         DC    F'25190'            GS                                           
         DC    F'85370'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59585'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'86965'            PP,CAR,SD                                    
         DC    F'60435'            SOC,GD                                       
         DC    F'47080'            VO,SS                                        
         DC    F'25595'            GS                                           
         DC    F'86965'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60435'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'88590'            PP,CAR,SD                                    
         DC    F'61250'            SOC,GD                                       
         DC    F'48095'            VO,SS                                        
         DC    F'26040'            GS                                           
         DC    F'88590'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'90150'            PP,CAR,SD                                    
         DC    F'61955'            SOC,GD                                       
         DC    F'49110'            VO,SS                                        
         DC    F'26525'            GS                                           
         DC    F'90150'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61955'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'91725'            PP,CAR,SD                                    
         DC    F'62870'            SOC,GD                                       
         DC    F'50230'            VO,SS                                        
         DC    F'26825'            GS                                           
         DC    F'91725'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62870'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'93320'            PP,CAR,SD                                    
         DC    F'63740'            SOC,GD                                       
         DC    F'50815'            VO,SS                                        
         DC    F'27350'            GS                                           
         DC    F'93320'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63740'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'94965'            PP,CAR,SD                                    
         DC    F'64525'            SOC,GD                                       
         DC    F'51450'            VO,SS                                        
         DC    F'27645'            GS                                           
         DC    F'94965'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64525'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'96530'            PP,CAR,SD                                    
         DC    F'65300'            SOC,GD                                       
         DC    F'52075'            VO,SS                                        
         DC    F'28020'            GS                                           
         DC    F'96530'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65300'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'98155'            PP,CAR,SD                                    
         DC    F'66175'            SOC,GD                                       
         DC    F'52705'            VO,SS                                        
         DC    F'28385'            GS                                           
         DC    F'98155'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66175'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'99780'            PP,CAR,SD                                    
         DC    F'66955'            SOC,GD                                       
         DC    F'53345'            VO,SS                                        
         DC    F'28775'            GS                                           
         DC    F'99780'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66955'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'100980'           PP,CAR,SD                                    
         DC    F'67805'            SOC,GD                                       
         DC    F'53940'            VO,SS                                        
         DC    F'29120'            GS                                           
         DC    F'100980'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67805'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'102100'           PP,CAR,SD                                    
         DC    F'68605'            SOC,GD                                       
         DC    F'54610'            VO,SS                                        
         DC    F'29510'            GS                                           
         DC    F'102100'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68605'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'103360'           PP,CAR,SD                                    
         DC    F'69405'            SOC,GD                                       
         DC    F'55240'            VO,SS                                        
         DC    F'29850'            GS                                           
         DC    F'103360'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69405'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'104580'           PP,CAR,SD                                    
         DC    F'70230'            SOC,GD                                       
         DC    F'55865'            VO,SS                                        
         DC    F'30290'            GS                                           
         DC    F'104580'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70230'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'105805'           PP,CAR,SD                                    
         DC    F'70980'            SOC,GD                                       
         DC    F'56490'            VO,SS                                        
         DC    F'30640'            GS                                           
         DC    F'105805'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70980'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'107010'           PP,CAR,SD                                    
         DC    F'71610'            SOC,GD                                       
         DC    F'56960'            VO,SS                                        
         DC    F'30975'            GS                                           
         DC    F'107010'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71610'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'108185'           PP,CAR,SD                                    
         DC    F'72195'            SOC,GD                                       
         DC    F'57385'            VO,SS                                        
         DC    F'31365'            GS                                           
         DC    F'108185'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72195'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'109425'           PP,CAR,SD                                    
         DC    F'72890'            SOC,GD                                       
         DC    F'57905'            VO,SS                                        
         DC    F'31725'            GS                                           
         DC    F'109425'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72890'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'110600'           PP,CAR,SD                                    
         DC    F'73425'            SOC,GD                                       
         DC    F'58280'            VO,SS                                        
         DC    F'32075'            GS                                           
         DC    F'110600'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73425'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'111825'           PP,CAR,SD                                    
         DC    F'74040'            SOC,GD                                       
         DC    F'58765'            VO,SS                                        
         DC    F'32480'            GS                                           
         DC    F'111825'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74040'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'112640'           PP,CAR,SD                                    
         DC    F'74440'            SOC,GD                                       
         DC    F'59175'            VO,SS                                        
         DC    F'32750'            GS                                           
         DC    F'112640'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74440'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'113430'           PP,CAR,SD                                    
         DC    F'75250'            SOC,GD                                       
         DC    F'59645'            VO,SS                                        
         DC    F'33095'            GS                                           
         DC    F'113430'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'114240'           PP,CAR,SD                                    
         DC    F'75865'            SOC,GD                                       
         DC    F'60070'            VO,SS                                        
         DC    F'33365'            GS                                           
         DC    F'114240'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75865'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'115070'           PP,CAR,SD                                    
         DC    F'76475'            SOC,GD                                       
         DC    F'60480'            VO,SS                                        
         DC    F'33720'            GS                                           
         DC    F'115070'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'115855'           PP,CAR,SD                                    
         DC    F'77035'            SOC,GD                                       
         DC    F'60960'            VO,SS                                        
         DC    F'33995'            GS                                           
         DC    F'115855'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77035'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'116715'           PP,CAR,SD                                    
         DC    F'77655'            SOC,GD                                       
         DC    F'61430'            VO,SS                                        
         DC    F'34370'            GS                                           
         DC    F'116715'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77655'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'117520'           PP,CAR,SD                                    
         DC    F'78270'            SOC,GD                                       
         DC    F'61870'            VO,SS                                        
         DC    F'34670'            GS                                           
         DC    F'117520'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78270'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'118345'           PP,CAR,SD                                    
         DC    F'78865'            SOC,GD                                       
         DC    F'62320'            VO,SS                                        
         DC    F'34960'            GS                                           
         DC    F'118345'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78865'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'119150'           PP,CAR,SD                                    
         DC    F'79475'            SOC,GD                                       
         DC    F'62775'            VO,SS                                        
         DC    F'35265'            GS                                           
         DC    F'119150'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'119980'           PP,CAR,SD                                    
         DC    F'80065'            SOC,GD                                       
         DC    F'63240'            VO,SS                                        
         DC    F'35580'            GS                                           
         DC    F'119980'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'80065'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'156'              PP,CAR,SD                                    
         DC    F'120'              SOC,GD                                       
         DC    F'89'               VO,SS                                        
         DC    F'48'               GS                                           
         DC    F'156'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'120'              ST, SAME AS SOC                              
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
*                                  1805:TABLE B                                 
         DC    AL1(60,40,1,5)      TV WSP&NET SPC/NETWORK UNITS 1-5             
         DC    F'83665'            PP,CAR,SD                                    
         DC    F'61390'            SOC,GD                                       
         DC    F'39045'            VO,SS                                        
         DC    F'22340'            GS                                           
         DC    F'83665'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61390'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'85905'            PP,CAR,SD                                    
         DC    F'62735'            SOC,GD                                       
         DC    F'40450'            VO,SS                                        
         DC    F'23035'            GS                                           
         DC    F'85905'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62735'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'88145'            PP,CAR,SD                                    
         DC    F'64125'            SOC,GD                                       
         DC    F'41880'            VO,SS                                        
         DC    F'23785'            GS                                           
         DC    F'88145'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64125'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'90435'            PP,CAR,SD                                    
         DC    F'65530'            SOC,GD                                       
         DC    F'43265'            VO,SS                                        
         DC    F'24525'            GS                                           
         DC    F'90435'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65530'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'92685'            PP,CAR,SD                                    
         DC    F'66945'            SOC,GD                                       
         DC    F'44770'            VO,SS                                        
         DC    F'25300'            GS                                           
         DC    F'92685'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66945'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'94855'            PP,CAR,SD                                    
         DC    F'68310'            SOC,GD                                       
         DC    F'46125'            VO,SS                                        
         DC    F'25995'            GS                                           
         DC    F'94855'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68310'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'97150'            PP,CAR,SD                                    
         DC    F'69685'            SOC,GD                                       
         DC    F'47550'            VO,SS                                        
         DC    F'26570'            GS                                           
         DC    F'97150'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69685'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'99355'            PP,CAR,SD                                    
         DC    F'71075'            SOC,GD                                       
         DC    F'48970'            VO,SS                                        
         DC    F'27155'            GS                                           
         DC    F'99355'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71075'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'101570'           PP,CAR,SD                                    
         DC    F'72420'            SOC,GD                                       
         DC    F'50395'            VO,SS                                        
         DC    F'27740'            GS                                           
         DC    F'101570'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72420'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'103860'           PP,CAR,SD                                    
         DC    F'73865'            SOC,GD                                       
         DC    F'51875'            VO,SS                                        
         DC    F'28330'            GS                                           
         DC    F'103860'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73865'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'106085'           PP,CAR,SD                                    
         DC    F'75245'            SOC,GD                                       
         DC    F'53240'            VO,SS                                        
         DC    F'28875'            GS                                           
         DC    F'106085'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75245'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'108310'           PP,CAR,SD                                    
         DC    F'76560'            SOC,GD                                       
         DC    F'54610'            VO,SS                                        
         DC    F'29425'            GS                                           
         DC    F'108310'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76560'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'110520'           PP,CAR,SD                                    
         DC    F'77980'            SOC,GD                                       
         DC    F'56015'            VO,SS                                        
         DC    F'30045'            GS                                           
         DC    F'110520'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77980'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'112795'           PP,CAR,SD                                    
         DC    F'79370'            SOC,GD                                       
         DC    F'57385'            VO,SS                                        
         DC    F'30590'            GS                                           
         DC    F'112795'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79370'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'114970'           PP,CAR,SD                                    
         DC    F'80770'            SOC,GD                                       
         DC    F'58785'            VO,SS                                        
         DC    F'31115'            GS                                           
         DC    F'114970'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'80770'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'117235'           PP,CAR,SD                                    
         DC    F'82100'            SOC,GD                                       
         DC    F'60210'            VO,SS                                        
         DC    F'31910'            GS                                           
         DC    F'117235'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'82100'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'119520'           PP,CAR,SD                                    
         DC    F'83300'            SOC,GD                                       
         DC    F'61485'            VO,SS                                        
         DC    F'32215'            GS                                           
         DC    F'119520'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'83300'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'121785'           PP,CAR,SD                                    
         DC    F'84470'            SOC,GD                                       
         DC    F'62885'            VO,SS                                        
         DC    F'32705'            GS                                           
         DC    F'121785'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'84470'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'123970'           PP,CAR,SD                                    
         DC    F'85655'            SOC,GD                                       
         DC    F'64315'            VO,SS                                        
         DC    F'33170'            GS                                           
         DC    F'123970'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'85655'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'126165'           PP,CAR,SD                                    
         DC    F'86855'            SOC,GD                                       
         DC    F'65655'            VO,SS                                        
         DC    F'33685'            GS                                           
         DC    F'126165'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'86855'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'128455'           PP,CAR,SD                                    
         DC    F'88015'            SOC,GD                                       
         DC    F'67040'            VO,SS                                        
         DC    F'34200'            GS                                           
         DC    F'128455'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'88015'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'130650'           PP,CAR,SD                                    
         DC    F'89135'            SOC,GD                                       
         DC    F'67920'            VO,SS                                        
         DC    F'34695'            GS                                           
         DC    F'130650'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'89135'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'132875'           PP,CAR,SD                                    
         DC    F'90330'            SOC,GD                                       
         DC    F'68815'            VO,SS                                        
         DC    F'35190'            GS                                           
         DC    F'132875'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90330'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'134995'           PP,CAR,SD                                    
         DC    F'91495'            SOC,GD                                       
         DC    F'69645'            VO,SS                                        
         DC    F'35670'            GS                                           
         DC    F'134995'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'91495'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'137235'           PP,CAR,SD                                    
         DC    F'92720'            SOC,GD                                       
         DC    F'70510'            VO,SS                                        
         DC    F'36125'            GS                                           
         DC    F'137235'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'92720'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'139430'           PP,CAR,SD                                    
         DC    F'93850'            SOC,GD                                       
         DC    F'71370'            VO,SS                                        
         DC    F'36660'            GS                                           
         DC    F'139430'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93850'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'141200'           PP,CAR,SD                                    
         DC    F'95010'            SOC,GD                                       
         DC    F'72190'            VO,SS                                        
         DC    F'37170'            GS                                           
         DC    F'141200'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'95010'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'142835'           PP,CAR,SD                                    
         DC    F'96090'            SOC,GD                                       
         DC    F'73010'            VO,SS                                        
         DC    F'37580'            GS                                           
         DC    F'142835'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96090'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'144510'           PP,CAR,SD                                    
         DC    F'97230'            SOC,GD                                       
         DC    F'73900'            VO,SS                                        
         DC    F'38040'            GS                                           
         DC    F'144510'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'97230'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'146275'           PP,CAR,SD                                    
         DC    F'98000'            SOC,GD                                       
         DC    F'74725'            VO,SS                                        
         DC    F'38485'            GS                                           
         DC    F'146275'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'98000'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'147950'           PP,CAR,SD                                    
         DC    F'99475'            SOC,GD                                       
         DC    F'75640'            VO,SS                                        
         DC    F'38950'            GS                                           
         DC    F'147950'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'99475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'149650'           PP,CAR,SD                                    
         DC    F'100380'           SOC,GD                                       
         DC    F'76190'            VO,SS                                        
         DC    F'39450'            GS                                           
         DC    F'149650'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'100380'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'151340'           PP,CAR,SD                                    
         DC    F'101240'           SOC,GD                                       
         DC    F'76760'            VO,SS                                        
         DC    F'39915'            GS                                           
         DC    F'151340'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'101240'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'153035'           PP,CAR,SD                                    
         DC    F'102070'           SOC,GD                                       
         DC    F'77310'            VO,SS                                        
         DC    F'40375'            GS                                           
         DC    F'153035'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'102070'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'154780'           PP,CAR,SD                                    
         DC    F'102905'           SOC,GD                                       
         DC    F'77865'            VO,SS                                        
         DC    F'40815'            GS                                           
         DC    F'154780'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'102905'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'156425'           PP,CAR,SD                                    
         DC    F'103780'           SOC,GD                                       
         DC    F'78470'            VO,SS                                        
         DC    F'41270'            GS                                           
         DC    F'156425'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'103780'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'157600'           PP,CAR,SD                                    
         DC    F'104580'           SOC,GD                                       
         DC    F'79025'            VO,SS                                        
         DC    F'41605'            GS                                           
         DC    F'157600'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'104580'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'158690'           PP,CAR,SD                                    
         DC    F'105395'           SOC,GD                                       
         DC    F'79585'            VO,SS                                        
         DC    F'42050'            GS                                           
         DC    F'158690'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'105395'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'159830'           PP,CAR,SD                                    
         DC    F'106285'           SOC,GD                                       
         DC    F'80120'            VO,SS                                        
         DC    F'42450'            GS                                           
         DC    F'159830'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'106285'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'161025'           PP,CAR,SD                                    
         DC    F'107090'           SOC,GD                                       
         DC    F'80710'            VO,SS                                        
         DC    F'42825'            GS                                           
         DC    F'161025'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'107090'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'162115'           PP,CAR,SD                                    
         DC    F'107840'           SOC,GD                                       
         DC    F'81305'            VO,SS                                        
         DC    F'43250'            GS                                           
         DC    F'162115'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'107840'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'163315'           PP,CAR,SD                                    
         DC    F'108715'           SOC,GD                                       
         DC    F'81790'            VO,SS                                        
         DC    F'43590'            GS                                           
         DC    F'163315'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'108715'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'164400'           PP,CAR,SD                                    
         DC    F'109475'           SOC,GD                                       
         DC    F'82365'            VO,SS                                        
         DC    F'43895'            GS                                           
         DC    F'164400'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'109475'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'165500'           PP,CAR,SD                                    
         DC    F'110240'           SOC,GD                                       
         DC    F'82885'            VO,SS                                        
         DC    F'44320'            GS                                           
         DC    F'165500'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'110240'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'166675'           PP,CAR,SD                                    
         DC    F'111135'           SOC,GD                                       
         DC    F'83415'            VO,SS                                        
         DC    F'44595'            GS                                           
         DC    F'166675'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'111135'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'167800'           PP,CAR,SD                                    
         DC    F'111925'           SOC,GD                                       
         DC    F'84000'            VO,SS                                        
         DC    F'45010'            GS                                           
         DC    F'167800'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'111925'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'217'              PP,CAR,SD                                    
         DC    F'167'              SOC,GD                                       
         DC    F'110'              VO,SS                                        
         DC    F'58'               GS                                           
         DC    F'217'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'167'              ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  906:FEE DETAINED IN AUDITION                 
CAUTTBL  DC    AL1(68,44,0,0)      AUDITION INTERVIEW - TV                      
         DC    F'8000'             PP,CAR,SD                                    
         DC    F'8000'             SOC,GD                                       
         DC    F'8000'             VO,SS                                        
         DC    F'8000'             GS                                           
         DC    F'8000'             SA                                           
         DC    F'8000'             DEM                                          
         DC    F'8000'             E                                            
         DC    F'8000'             GE                                           
         DC    F'8000'             ST                                           
         DC    F'8000'             EVERYBODY ELSE-US,SI,SB,PT                   
         SPACE 1                                                                
CAURTBL  DC    AL1(69,12,0,0)      AUDITION INTERVIEW - RADIO                   
         DC    F'8000'             SS,SV                                        
         DC    F'8000'             MV,GS                                        
         EJECT                                                                  
*                                  1203.C:DEMO AND TEST COMMERCIAL              
CDMTBL   DC    AL1(70,44,2,2)      TV PRESENTATION DEMO 2                       
         DC    F'29400'            PP,CAR,SD                                    
         DC    F'29400'            SOC,GD                                       
         DC    F'29400'            VO,SS                                        
         DC    F'29400'            GS                                           
         DC    F'29400'            SA                                           
         DC    F'29400'            DEM                                          
         DC    F'29400'            E                                            
         DC    F'29400'            GE                                           
         DC    F'29400'            ST                                           
         DC    F'29400'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'14800'            PP,CAR,SD                                    
         DC    F'14800'            SOC,GD                                       
         DC    F'14800'            VO,SS                                        
         DC    F'14800'            GS                                           
         DC    F'14800'            SA                                           
         DC    F'14800'            DEM                                          
         DC    F'14800'            E                                            
         DC    F'14800'            GE                                           
         DC    F'14800'            ST                                           
         DC    F'14800'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
*                                  1202:SESSION FEE                             
BSCHTBL  DC    AL1(80,40,0,0)      NATIONAL TV  - HOURLY RATE                   
         DC    F'9600'             PP,SD,CAR                                    
         DC    F'9600'             SOC,GD                                       
         DC    F'8000'             VO,SS                                        
         DC    F'8000'             GS                                           
         DC    F'14400'            SA = PP+50%                                  
         DC    F'9600'             DEM                                          
         DC    F'5950'             E                                            
         DC    F'3900'             GE                                           
         DC    F'9600'             ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(81,40,0,0)      NATIONAL TV  - OVERTIME HOURLY RATE          
         DC    F'12500'            PP,SD,CAR                                    
         DC    F'12500'            SOC,GD                                       
         DC    F'10050'            VO,SS                                        
         DC    F'10050'            GS                                           
         DC    F'18750'            SA = PP+50%                                  
         DC    F'12500'            DEM                                          
         DC    F'7100'             E                                            
         DC    F'5050'             GE                                           
         DC    F'12500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(82,40,0,0)      NATIONAL TV  - DBLTIME HOURLY RATE           
         DC    F'14100'            PP,SD,CAR                                    
         DC    F'14100'            SOC,GD                                       
         DC    F'12500'            VO,SS                                        
         DC    F'12500'            GS                                           
         DC    F'21150'            SA = PP+50%                                  
         DC    F'14100'            DEM                                          
         DC    F'8850'             E                                            
         DC    F'5800'             GE                                           
         DC    F'14100'            ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  1820:NEW MEDIA                               
CNMTBL   DC    AL1(100,40,0,0)     NEW MEDIA VIDEO 4 WEEKS - 35%                
         DC    F'26500'            PP,SD,CAR                                    
         DC    F'26500'            SOC,GD                                       
         DC    F'19300'            VO,SS                                        
         DC    F'8350'             GS                                           
         DC    F'39750'            SA = PP+50%                                  
         DC    F'26500'            DEM                                          
         DC    F'16200'            E                                            
         DC    F'10800'            GE                                           
         DC    F'26500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(101,12,0,0)     NEW MEDIA AUDIO 4 WEEKS - 35%                
         DC    F'20600'            SS,SV                                        
         DC    F'15450'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(110,40,0,0)     NEW MEDIA VIDEO 8 WEEKS - 50%                
         DC    F'37850'            PP,SD,CAR                                    
         DC    F'37850'            SOC,GD                                       
         DC    F'27600'            VO,SS                                        
         DC    F'11950'            GS                                           
         DC    F'56775'            SA = PP+50%                                  
         DC    F'37850'            DEM                                          
         DC    F'23150'            E                                            
         DC    F'15450'            GE                                           
         DC    F'37850'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(111,12,0,0)     NEW MEDIA AUDIO 8 WEEKS - 50%                
         DC    F'29450'            SS,SV                                        
         DC    F'22100'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(120,40,0,0)     NEW MEDIA VIDEO 26 WEEKS - 75%               
         DC    F'56750'            PP,SD,CAR                                    
         DC    F'56750'            SOC,GD                                       
         DC    F'41350'            VO,SS                                        
         DC    F'17900'            GS                                           
         DC    F'85125'            SA = PP+50%                                  
         DC    F'56750'            DEM                                          
         DC    F'34750'            E                                            
         DC    F'23200'            GE                                           
         DC    F'56750'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(121,12,0,0)     NEW MEDIA AUDIO 26 WEEKS - 75%               
         DC    F'44200'            SS,SV                                        
         DC    F'33150'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(130,40,0,0)     NEW MEDIA VIDEO 1 YEAR - 100%                
         DC    F'75650'            PP,SD,CAR                                    
         DC    F'75650'            SOC,GD                                       
         DC    F'55150'            VO,SS                                        
         DC    F'23850'            GS                                           
         DC    F'113475'           SA = PP+50%                                  
         DC    F'75650'            DEM                                          
         DC    F'46300'            E                                            
         DC    F'30900'            GE                                           
         DC    F'75650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(131,12,0,0)     NEW MEDIA AUDIO 1 YEAR - 100%                
         DC    F'58900'            SS,SV                                        
         DC    F'44200'            MV,GS                                        
         SPACE 1                                                                
CNMATBL  DC    AL1(140,12,0,0)     NEW MEDIA AUDIO 4 WEEKS ADDTL CUTS           
         DC    F'10300'            SS,SV                                        
         DC    F'7750'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(141,12,0,0)     NEW MEDIA AUDIO 8 WEEKS ADDTL CUTS           
         DC    F'14750'            SS,SV                                        
         DC    F'11050'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(142,12,0,0)     NEW MEDIA AUDIO 26 WEEKS ADDTL CUTS          
         DC    F'22100'            SS,SV                                        
         DC    F'16600'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(143,12,0,0)     NEW MEDIA AUDIO 1 YEAR ADDTL CUTS            
         DC    F'29450'            SS,SV                                        
         DC    F'22100'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(144,12,0,0)     BSC NEW MEDIA AUDIO ADDTL CUTS               
         DC    F'29450'            SS,SV                                        
         DC    F'22100'            MV,GS                                        
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
         DC    AL1(4,CTMV)         MULTIPLE VOICE                               
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
**PAN#1  DC    CL21'020TAGEN76   10/09/15'                                      
         END                                                                    
