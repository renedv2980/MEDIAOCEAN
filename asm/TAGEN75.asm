*          DATA SET TAGEN75    AT LEVEL 023 AS OF 10/09/15                      
*PHASE T70275C,*                                                                
         TITLE 'T70275 - TABLES FOR YEAR 3 ACTRA 2013-2014'                     
T70275   CSECT                                                                  
         DC    AL4(USETBLS-T70275)                                              
         DC    AL4(USELUT-T70275)                                               
         DC    AL4(MAJLUT-T70275)                                               
         DC    AL4(AFMCOLS-T70275)                                              
         DC    AL4(RADCOLS-T70275)                                              
         DC    AL4(OFFCOLS-T70275)                                              
         DC    AL4(ONCOLS-T70275)                                               
         DC    AL4(MSWEET-T70275)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
*                                  1202:SESSION FEE                             
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'74150'            PP,SD,CAR                                    
         DC    F'74150'            SOC,GD                                       
         DC    F'54050'            VO,SS                                        
         DC    F'23400'            GS                                           
         DC    F'111225'           SA = PP+50%                                  
         DC    F'74150'            DEM                                          
         DC    F'45400'            E                                            
         DC    F'30300'            GE                                           
         DC    F'74150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'74150'            PP,SD,CAR                                    
         DC    F'74150'            SOC,GD                                       
         DC    F'54050'            VO,SS                                        
         DC    F'23400'            GS                                           
         DC    F'111225'           SA = PP+50%                                  
         DC    F'74150'            DEM                                          
         DC    F'45400'            E                                            
         DC    F'30300'            GE                                           
         DC    F'74150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'74150'            PP,SD,CAR                                    
         DC    F'74150'            SOC,GD                                       
         DC    F'54050'            VO,SS                                        
         DC    F'23400'            GS                                           
         DC    F'111225'           SA = PP+50%                                  
         DC    F'74150'            DEM                                          
         DC    F'45400'            E                                            
         DC    F'30300'            GE                                           
         DC    F'74150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'74150'            PP,SD,CAR                                    
         DC    F'74150'            SOC,GD                                       
         DC    F'54050'            VO,SS                                        
         DC    F'23400'            GS                                           
         DC    F'111225'           SA = PP+50%                                  
         DC    F'74150'            DEM                                          
         DC    F'45400'            E                                            
         DC    F'30300'            GE                                           
         DC    F'74150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'74150'            PP,SD,CAR                                    
         DC    F'74150'            SOC,GD                                       
         DC    F'54050'            VO,SS                                        
         DC    F'23400'            GS                                           
         DC    F'111225'           SA = PP+50%                                  
         DC    F'74150'            DEM                                          
         DC    F'45400'            E                                            
         DC    F'30300'            GE                                           
         DC    F'74150'            ST, SAME AS SOC                              
         SPACE 1                                                                
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
         DC    AL1(8,40,0,0)       1820:SESSION FEE NEW MEDIA VIDEO             
         DC    F'36200'            PP,SD,CAR                                    
         DC    F'36200'            SOC,GD                                       
         DC    F'26400'            VO,SS                                        
         DC    F'11450'            GROUP SINGER                                 
         DC    F'54300'            SA = PP+50%                                  
         DC    F'36200'                                                         
         DC    F'22150'            BACKGROUND                                   
         DC    F'14800'            GROUP BACKGROUND                             
         DC    F'36200'            ST, SAME AS SOC                              
         EJECT                                                                  
DORTTBL  DC    AL1(12,40,0,0)      NATIONAL TV                                  
         DC    F'74150'            PP,SD,CAR                                    
         DC    F'74150'            SOC,GD                                       
         DC    F'54050'            VO,SS                                        
         DC    F'23400'            GS                                           
         DC    F'111225'           SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74150'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'74150'                                                         
         DC    F'74150'                                                         
         DC    F'54050'                                                         
         DC    F'23400'                                                         
         DC    F'111225'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74150'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'74150'                                                         
         DC    F'74150'                                                         
         DC    F'54050'                                                         
         DC    F'23400'                                                         
         DC    F'111225'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74150'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'74150'                                                         
         DC    F'74150'                                                         
         DC    F'54050'                                                         
         DC    F'23400'                                                         
         DC    F'111225'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74150'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'74150'                                                         
         DC    F'74150'                                                         
         DC    F'54050'                                                         
         DC    F'23400'                                                         
         DC    F'111225'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74150'            ST SAME AS SOC                               
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
*                                  2101:SESSION AND RESIDUAL FEES               
BSCRTBL  DC    AL1(24,12,0,0)      NATIONAL RADIO                               
         DC    F'57750'            SV,SS                                        
         DC    F'43325'            MV,GS                                        
         SPACE 1                                                                
*                                  406:RADIO SESSION AND RESIDUAL FEES          
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'29450'                                                         
         DC    F'22150'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'38925'                                                         
         DC    F'29125'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'44175'                                                         
         DC    F'33225'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'50050'                                                         
         DC    F'37600'                                                         
         SPACE 1                                                                
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
*--------------------------------------------------------------------*          
         DC    AL1(33,12,0,0)      1820:SESSION FEE NEW MEDIA AUDIO             
         DC    F'28175'            SV,SS                                        
         DC    F'21150'            MV,GS                                        
         EJECT                                                                  
*                                  1804:TABLE A                                 
WSCTBL   DC    AL1(48,40,1,1)      TV WILDSPOT UNITS 1                          
         DC    F'58660'            PP,CAR,SD                                    
         DC    F'42995'            SOC,GD                                       
         DC    F'28710'            VO,SS                                        
         DC    F'17195'            GS                                           
         DC    F'58660'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42995'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,2,2)      TV WILDSPOT UNITS 2                          
         DC    F'58660'            PP,CAR,SD                                    
         DC    F'42995'            SOC,GD                                       
         DC    F'28710'            VO,SS                                        
         DC    F'17195'            GS                                           
         DC    F'58660'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42995'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,3,3)      TV WILDSPOT UNITS 3                          
         DC    F'58660'            PP,CAR,SD                                    
         DC    F'42995'            SOC,GD                                       
         DC    F'28710'            VO,SS                                        
         DC    F'17195'            GS                                           
         DC    F'58660'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42995'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,4,4)      TV WILDSPOT UNITS 4                          
         DC    F'58660'            PP,CAR,SD                                    
         DC    F'42995'            SOC,GD                                       
         DC    F'28710'            VO,SS                                        
         DC    F'17195'            GS                                           
         DC    F'58660'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42995'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,5,5)      TV WILDSPOT UNITS 5                          
         DC    F'58660'            PP,CAR,SD                                    
         DC    F'42995'            SOC,GD                                       
         DC    F'28710'            VO,SS                                        
         DC    F'17195'            GS                                           
         DC    F'58660'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42995'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'60195'            PP,CAR,SD                                    
         DC    F'43970'            SOC,GD                                       
         DC    F'29770'            VO,SS                                        
         DC    F'17720'            GS                                           
         DC    F'60195'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43970'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'61780'            PP,CAR,SD                                    
         DC    F'44975'            SOC,GD                                       
         DC    F'30770'            VO,SS                                        
         DC    F'18335'            GS                                           
         DC    F'61780'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44975'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'63360'            PP,CAR,SD                                    
         DC    F'45960'            SOC,GD                                       
         DC    F'31800'            VO,SS                                        
         DC    F'18915'            GS                                           
         DC    F'63360'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45960'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'64905'            PP,CAR,SD                                    
         DC    F'46925'            SOC,GD                                       
         DC    F'32790'            VO,SS                                        
         DC    F'19510'            GS                                           
         DC    F'64905'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46925'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'66475'            PP,CAR,SD                                    
         DC    F'47905'            SOC,GD                                       
         DC    F'33815'            VO,SS                                        
         DC    F'20080'            GS                                           
         DC    F'66575'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47905'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'68045'            PP,CAR,SD                                    
         DC    F'48845'            SOC,GD                                       
         DC    F'34815'            VO,SS                                        
         DC    F'20465'            GS                                           
         DC    F'68045'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48845'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'69585'            PP,CAR,SD                                    
         DC    F'49840'            SOC,GD                                       
         DC    F'35855'            VO,SS                                        
         DC    F'20940'            GS                                           
         DC    F'69585'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49840'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'71205'            PP,CAR,SD                                    
         DC    F'50860'            SOC,GD                                       
         DC    F'36955'            VO,SS                                        
         DC    F'21310'            GS                                           
         DC    F'71205'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50860'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'73265'            PP,CAR,SD                                    
         DC    F'51740'            SOC,GD                                       
         DC    F'37945'            VO,SS                                        
         DC    F'21755'            GS                                           
         DC    F'73265'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51740'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'74300'            PP,CAR,SD                                    
         DC    F'52775'            SOC,GD                                       
         DC    F'38930'            VO,SS                                        
         DC    F'22190'            GS                                           
         DC    F'74300'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52775'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'75855'            PP,CAR,SD                                    
         DC    F'53750'            SOC,GD                                       
         DC    F'39990'            VO,SS                                        
         DC    F'22540'            GS                                           
         DC    F'75855'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53750'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'78145'            PP,CAR,SD                                    
         DC    F'54710'            SOC,GD                                       
         DC    F'40990'            VO,SS                                        
         DC    F'23035'            GS                                           
         DC    F'78145'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54710'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'79675'            PP,CAR,SD                                    
         DC    F'55680'            SOC,GD                                       
         DC    F'41990'            VO,SS                                        
         DC    F'23440'            GS                                           
         DC    F'79675'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55680'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'80490'            PP,CAR,SD                                    
         DC    F'56665'            SOC,GD                                       
         DC    F'43035'            VO,SS                                        
         DC    F'23840'            GS                                           
         DC    F'80490'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56665'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'82125'            PP,CAR,SD                                    
         DC    F'57635'            SOC,GD                                       
         DC    F'44050'            VO,SS                                        
         DC    F'24240'            GS                                           
         DC    F'82125'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57635'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'83695'            PP,CAR,SD                                    
         DC    F'58415'            SOC,GD                                       
         DC    F'45105'            VO,SS                                        
         DC    F'24695'            GS                                           
         DC    F'83695'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58415'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'85260'            PP,CAR,SD                                    
         DC    F'59250'            SOC,GD                                       
         DC    F'46155'            VO,SS                                        
         DC    F'25095'            GS                                           
         DC    F'85260'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'86855'            PP,CAR,SD                                    
         DC    F'60050'            SOC,GD                                       
         DC    F'47150'            VO,SS                                        
         DC    F'25530'            GS                                           
         DC    F'86855'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60050'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'88380'            PP,CAR,SD                                    
         DC    F'60740'            SOC,GD                                       
         DC    F'48145'            VO,SS                                        
         DC    F'26005'            GS                                           
         DC    F'88380'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60740'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'89925'            PP,CAR,SD                                    
         DC    F'61635'            SOC,GD                                       
         DC    F'49245'            VO,SS                                        
         DC    F'26300'            GS                                           
         DC    F'89925'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61635'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'91490'            PP,CAR,SD                                    
         DC    F'62490'            SOC,GD                                       
         DC    F'49820'            VO,SS                                        
         DC    F'26815'            GS                                           
         DC    F'91490'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62490'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'93105'            PP,CAR,SD                                    
         DC    F'63260'            SOC,GD                                       
         DC    F'50440'            VO,SS                                        
         DC    F'27105'            GS                                           
         DC    F'93105'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63260'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'94635'            PP,CAR,SD                                    
         DC    F'64020'            SOC,GD                                       
         DC    F'51055'            VO,SS                                        
         DC    F'27470'            GS                                           
         DC    F'94635'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64020'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'96230'            PP,CAR,SD                                    
         DC    F'64875'            SOC,GD                                       
         DC    F'51670'            VO,SS                                        
         DC    F'27830'            GS                                           
         DC    F'96230'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64875'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'97825'            PP,CAR,SD                                    
         DC    F'65640'            SOC,GD                                       
         DC    F'52300'            VO,SS                                        
         DC    F'28210'            GS                                           
         DC    F'97825'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65640'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'99000'            PP,CAR,SD                                    
         DC    F'66475'            SOC,GD                                       
         DC    F'52880'            VO,SS                                        
         DC    F'28550'            GS                                           
         DC    F'99000'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'100100'           PP,CAR,SD                                    
         DC    F'67260'            SOC,GD                                       
         DC    F'53540'            VO,SS                                        
         DC    F'28930'            GS                                           
         DC    F'100100'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67260'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'101335'           PP,CAR,SD                                    
         DC    F'68045'            SOC,GD                                       
         DC    F'54155'            VO,SS                                        
         DC    F'29265'            GS                                           
         DC    F'101335'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68045'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'102530'           PP,CAR,SD                                    
         DC    F'68855'            SOC,GD                                       
         DC    F'54770'            VO,SS                                        
         DC    F'29695'            GS                                           
         DC    F'102530'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68855'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'103730'           PP,CAR,SD                                    
         DC    F'69590'            SOC,GD                                       
         DC    F'55380'            VO,SS                                        
         DC    F'30040'            GS                                           
         DC    F'103730'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69590'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'104910'           PP,CAR,SD                                    
         DC    F'70205'            SOC,GD                                       
         DC    F'55845'            VO,SS                                        
         DC    F'30370'            GS                                           
         DC    F'104910'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70205'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'106065'           PP,CAR,SD                                    
         DC    F'70780'            SOC,GD                                       
         DC    F'56260'            VO,SS                                        
         DC    F'30750'            GS                                           
         DC    F'106065'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70780'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'107280'           PP,CAR,SD                                    
         DC    F'71460'            SOC,GD                                       
         DC    F'56770'            VO,SS                                        
         DC    F'31105'            GS                                           
         DC    F'107280'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71460'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'108430'           PP,CAR,SD                                    
         DC    F'71985'            SOC,GD                                       
         DC    F'57135'            VO,SS                                        
         DC    F'31445'            GS                                           
         DC    F'108430'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71985'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'109630'           PP,CAR,SD                                    
         DC    F'72590'            SOC,GD                                       
         DC    F'57615'            VO,SS                                        
         DC    F'31845'            GS                                           
         DC    F'109630'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72590'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'110430'           PP,CAR,SD                                    
         DC    F'72980'            SOC,GD                                       
         DC    F'58015'            VO,SS                                        
         DC    F'32110'            GS                                           
         DC    F'110430'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72980'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'111205'           PP,CAR,SD                                    
         DC    F'73775'            SOC,GD                                       
         DC    F'58475'            VO,SS                                        
         DC    F'32445'            GS                                           
         DC    F'111205'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73775'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'112000'           PP,CAR,SD                                    
         DC    F'74375'            SOC,GD                                       
         DC    F'58890'            VO,SS                                        
         DC    F'32710'            GS                                           
         DC    F'112000'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74375'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'112815'           PP,CAR,SD                                    
         DC    F'74975'            SOC,GD                                       
         DC    F'59295'            VO,SS                                        
         DC    F'33060'            GS                                           
         DC    F'112815'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74975'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'113585'           PP,CAR,SD                                    
         DC    F'75525'            SOC,GD                                       
         DC    F'59765'            VO,SS                                        
         DC    F'33330'            GS                                           
         DC    F'113585'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75525'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'114425'           PP,CAR,SD                                    
         DC    F'76130'            SOC,GD                                       
         DC    F'60225'            VO,SS                                        
         DC    F'33695'            GS                                           
         DC    F'114425'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76130'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'115215'           PP,CAR,SD                                    
         DC    F'76735'            SOC,GD                                       
         DC    F'60655'            VO,SS                                        
         DC    F'33990'            GS                                           
         DC    F'115215'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76735'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'116025'           PP,CAR,SD                                    
         DC    F'77320'            SOC,GD                                       
         DC    F'61100'            VO,SS                                        
         DC    F'34275'            GS                                           
         DC    F'116025'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77320'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'116815'           PP,CAR,SD                                    
         DC    F'77915'            SOC,GD                                       
         DC    F'61545'            VO,SS                                        
         DC    F'34575'            GS                                           
         DC    F'116815'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77915'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'117625'           PP,CAR,SD                                    
         DC    F'78495'            SOC,GD                                       
         DC    F'62000'            VO,SS                                        
         DC    F'34880'            GS                                           
         DC    F'117625'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78495'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'153'              PP,CAR,SD                                    
         DC    F'118'              SOC,GD                                       
         DC    F'87'               VO,SS                                        
         DC    F'47'               GS                                           
         DC    F'153'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'118'              ST, SAME AS SOC                              
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
*                                  1805:TABLE B                                 
         DC    AL1(60,40,1,1)      TV WSP&NET SPC/NETWORK UNITS 1               
         DC    F'82025'            PP,CAR,SD                                    
         DC    F'60185'            SOC,GD                                       
         DC    F'38280'            VO,SS                                        
         DC    F'21900'            GS                                           
         DC    F'82025'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60185'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,2,2)      TV WSP&NET SPC/NETWORK UNITS 2               
         DC    F'82025'            PP,CAR,SD                                    
         DC    F'60185'            SOC,GD                                       
         DC    F'38280'            VO,SS                                        
         DC    F'21900'            GS                                           
         DC    F'82025'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60185'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,3,3)      TV WSP&NET SPC/NETWORK UNITS 3               
         DC    F'82025'            PP,CAR,SD                                    
         DC    F'60185'            SOC,GD                                       
         DC    F'38280'            VO,SS                                        
         DC    F'21900'            GS                                           
         DC    F'82025'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60185'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,4,4)      TV WSP&NET SPC/NETWORK UNIT 4                
         DC    F'82025'            PP,CAR,SD                                    
         DC    F'60185'            SOC,GD                                       
         DC    F'38280'            VO,SS                                        
         DC    F'21900'            GS                                           
         DC    F'82025'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60185'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,5,5)      TV WSP&NET SPC/NETWORK UNIT 5                
         DC    F'82025'            PP,CAR,SD                                    
         DC    F'60185'            SOC,GD                                       
         DC    F'38280'            VO,SS                                        
         DC    F'21900'            GS                                           
         DC    F'82025'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60185'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'84220'            PP,CAR,SD                                    
         DC    F'61505'            SOC,GD                                       
         DC    F'39655'            VO,SS                                        
         DC    F'22585'            GS                                           
         DC    F'84220'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61505'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'86415'            PP,CAR,SD                                    
         DC    F'62870'            SOC,GD                                       
         DC    F'41060'            VO,SS                                        
         DC    F'23320'            GS                                           
         DC    F'86415'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62870'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'88660'            PP,CAR,SD                                    
         DC    F'64245'            SOC,GD                                       
         DC    F'42415'            VO,SS                                        
         DC    F'24045'            GS                                           
         DC    F'88660'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64245'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'90870'            PP,CAR,SD                                    
         DC    F'65630'            SOC,GD                                       
         DC    F'43890'            VO,SS                                        
         DC    F'24805'            GS                                           
         DC    F'90870'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65690'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'92995'            PP,CAR,SD                                    
         DC    F'66970'            SOC,GD                                       
         DC    F'45220'            VO,SS                                        
         DC    F'25485'            GS                                           
         DC    F'92995'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66970'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'95245'            PP,CAR,SD                                    
         DC    F'68320'            SOC,GD                                       
         DC    F'46620'            VO,SS                                        
         DC    F'26050'            GS                                           
         DC    F'95245'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68320'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'97405'            PP,CAR,SD                                    
         DC    F'69680'            SOC,GD                                       
         DC    F'48010'            VO,SS                                        
         DC    F'26625'            GS                                           
         DC    F'97405'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69680'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'99580'            PP,CAR,SD                                    
         DC    F'71000'            SOC,GD                                       
         DC    F'49405'            VO,SS                                        
         DC    F'27195'            GS                                           
         DC    F'99580'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71000'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'101825'           PP,CAR,SD                                    
         DC    F'72415'            SOC,GD                                       
         DC    F'50860'            VO,SS                                        
         DC    F'27775'            GS                                           
         DC    F'101825'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72415'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'104005'           PP,CAR,SD                                    
         DC    F'73770'            SOC,GD                                       
         DC    F'52195'            VO,SS                                        
         DC    F'28310'            GS                                           
         DC    F'104005'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73770'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'106185'           PP,CAR,SD                                    
         DC    F'75060'            SOC,GD                                       
         DC    F'53540'            VO,SS                                        
         DC    F'28850'            GS                                           
         DC    F'106185'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'108355'           PP,CAR,SD                                    
         DC    F'76450'            SOC,GD                                       
         DC    F'54915'            VO,SS                                        
         DC    F'29455'            GS                                           
         DC    F'108355'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76450'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'110585'           PP,CAR,SD                                    
         DC    F'77815'            SOC,GD                                       
         DC    F'56260'            VO,SS                                        
         DC    F'29990'            GS                                           
         DC    F'110585'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77815'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'112715'           PP,CAR,SD                                    
         DC    F'79185'            SOC,GD                                       
         DC    F'57630'            VO,SS                                        
         DC    F'30505'            GS                                           
         DC    F'112715'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79185'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'114935'           PP,CAR,SD                                    
         DC    F'80490'            SOC,GD                                       
         DC    F'59030'            VO,SS                                        
         DC    F'31285'            GS                                           
         DC    F'114935'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'80490'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'117175'           PP,CAR,SD                                    
         DC    F'81665'            SOC,GD                                       
         DC    F'60280'            VO,SS                                        
         DC    F'31585'            GS                                           
         DC    F'117175'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'81665'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'119395'           PP,CAR,SD                                    
         DC    F'82815'            SOC,GD                                       
         DC    F'61650'            VO,SS                                        
         DC    F'32065'            GS                                           
         DC    F'119395'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'82815'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'121540'           PP,CAR,SD                                    
         DC    F'83975'            SOC,GD                                       
         DC    F'63055'            VO,SS                                        
         DC    F'32520'            GS                                           
         DC    F'121540'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'32520'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'123690'           PP,CAR,SD                                    
         DC    F'85150'            SOC,GD                                       
         DC    F'64370'            VO,SS                                        
         DC    F'33025'            GS                                           
         DC    F'123690'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'85150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'125935'           PP,CAR,SD                                    
         DC    F'86290'            SOC,GD                                       
         DC    F'65725'            VO,SS                                        
         DC    F'33530'            GS                                           
         DC    F'125935'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'86290'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'128090'           PP,CAR,SD                                    
         DC    F'87385'            SOC,GD                                       
         DC    F'66590'            VO,SS                                        
         DC    F'34015'            GS                                           
         DC    F'128090'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87385'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'130270'           PP,CAR,SD                                    
         DC    F'88560'            SOC,GD                                       
         DC    F'67465'            VO,SS                                        
         DC    F'34500'            GS                                           
         DC    F'130270'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'88560'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'132350'           PP,CAR,SD                                    
         DC    F'89700'            SOC,GD                                       
         DC    F'68280'            VO,SS                                        
         DC    F'34970'            GS                                           
         DC    F'132350'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'89700'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'134545'           PP,CAR,SD                                    
         DC    F'90900'            SOC,GD                                       
         DC    F'69125'            VO,SS                                        
         DC    F'35415'            GS                                           
         DC    F'134545'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90900'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'136695'           PP,CAR,SD                                    
         DC    F'92010'            SOC,GD                                       
         DC    F'69970'            VO,SS                                        
         DC    F'35940'            GS                                           
         DC    F'136695'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'92010'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'138430'           PP,CAR,SD                                    
         DC    F'93145'            SOC,GD                                       
         DC    F'70775'            VO,SS                                        
         DC    F'36440'            GS                                           
         DC    F'138430'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93145'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'140035'           PP,CAR,SD                                    
         DC    F'94205'            SOC,GD                                       
         DC    F'71580'            VO,SS                                        
         DC    F'36845'            GS                                           
         DC    F'140035'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'94205'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'141675'           PP,CAR,SD                                    
         DC    F'95325'            SOC,GD                                       
         DC    F'72450'            VO,SS                                        
         DC    F'37295'            GS                                           
         DC    F'141675'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'95325'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'143405'           PP,CAR,SD                                    
         DC    F'96080'            SOC,GD                                       
         DC    F'73260'            VO,SS                                        
         DC    F'37730'            GS                                           
         DC    F'143405'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96080'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'145050'           PP,CAR,SD                                    
         DC    F'97525'            SOC,GD                                       
         DC    F'74155'            VO,SS                                        
         DC    F'38185'            GS                                           
         DC    F'145050'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'97525'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'146715'           PP,CAR,SD                                    
         DC    F'98410'            SOC,GD                                       
         DC    F'74695'            VO,SS                                        
         DC    F'38675'            GS                                           
         DC    F'146715'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'98410'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'148375'           PP,CAR,SD                                    
         DC    F'99255'            SOC,GD                                       
         DC    F'75255'            VO,SS                                        
         DC    F'39130'            GS                                           
         DC    F'148375'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'99255'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'150035'           PP,CAR,SD                                    
         DC    F'100070'           SOC,GD                                       
         DC    F'75795'            VO,SS                                        
         DC    F'39585'            GS                                           
         DC    F'150035'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'100070'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'151745'           PP,CAR,SD                                    
         DC    F'100885'           SOC,GD                                       
         DC    F'76340'            VO,SS                                        
         DC    F'40015'            GS                                           
         DC    F'151745'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'100885'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'153360'           PP,CAR,SD                                    
         DC    F'101745'           SOC,GD                                       
         DC    F'76930'            VO,SS                                        
         DC    F'40460'            GS                                           
         DC    F'153360'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'101745'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'154510'           PP,CAR,SD                                    
         DC    F'102530'           SOC,GD                                       
         DC    F'77475'            VO,SS                                        
         DC    F'40790'            GS                                           
         DC    F'154510'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'102530'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'155580'           PP,CAR,SD                                    
         DC    F'103330'           SOC,GD                                       
         DC    F'78025'            VO,SS                                        
         DC    F'41225'            GS                                           
         DC    F'155580'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'103330'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'156695'           PP,CAR,SD                                    
         DC    F'104200'           SOC,GD                                       
         DC    F'78550'            VO,SS                                        
         DC    F'41620'            GS                                           
         DC    F'156695'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'104200'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'157870'           PP,CAR,SD                                    
         DC    F'104990'           SOC,GD                                       
         DC    F'79125'            VO,SS                                        
         DC    F'41985'            GS                                           
         DC    F'157870'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'104990'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'158935'           PP,CAR,SD                                    
         DC    F'105725'           SOC,GD                                       
         DC    F'79710'            VO,SS                                        
         DC    F'42400'            GS                                           
         DC    F'158935'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'105725'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'160115'           PP,CAR,SD                                    
         DC    F'106585'           SOC,GD                                       
         DC    F'80185'            VO,SS                                        
         DC    F'42735'            GS                                           
         DC    F'160115'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'106585'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'161175'           PP,CAR,SD                                    
         DC    F'107330'           SOC,GD                                       
         DC    F'80750'            VO,SS                                        
         DC    F'43035'            GS                                           
         DC    F'161175'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'107330'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'162255'           PP,CAR,SD                                    
         DC    F'108080'           SOC,GD                                       
         DC    F'81260'            VO,SS                                        
         DC    F'43450'            GS                                           
         DC    F'162255'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'108080'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'163405'           PP,CAR,SD                                    
         DC    F'108955'           SOC,GD                                       
         DC    F'81780'            VO,SS                                        
         DC    F'43720'            GS                                           
         DC    F'163405'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'108955'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'164510'           PP,CAR,SD                                    
         DC    F'109730'           SOC,GD                                       
         DC    F'82355'            VO,SS                                        
         DC    F'44125'            GS                                           
         DC    F'164510'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'109730'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'213'              PP,CAR,SD                                    
         DC    F'164'              SOC,GD                                       
         DC    F'108'              VO,SS                                        
         DC    F'57'               GS                                           
         DC    F'213'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'164'              ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  906:FEE DETAINED IN AUDITION                 
CAUTTBL  DC    AL1(68,44,0,0)      AUDITION INTERVIEW - TV                      
         DC    F'7850'             PP,CAR,SD                                    
         DC    F'7850'             SOC,GD                                       
         DC    F'7850'             VO,SS                                        
         DC    F'7850'             GS                                           
         DC    F'7850'             SA                                           
         DC    F'7850'             DEM                                          
         DC    F'7850'             E                                            
         DC    F'7850'             GE                                           
         DC    F'7850'             ST                                           
         DC    F'7850'             EVERYBODY ELSE-US,SI,SB,PT                   
         SPACE 1                                                                
CAURTBL  DC    AL1(69,12,0,0)      AUDITION INTERVIEW - RADIO                   
         DC    F'7850'             SS,SV                                        
         DC    F'7850'             MV,GS                                        
         EJECT                                                                  
*                                  1203.C:DEMO AND TEST COMMERCIAL              
CDMTBL   DC    AL1(70,44,2,2)      TV PRESENTATION DEMO 2                       
         DC    F'28800'            PP,CAR,SD                                    
         DC    F'28800'            SOC,GD                                       
         DC    F'28800'            VO,SS                                        
         DC    F'28800'            GS                                           
         DC    F'28800'            SA                                           
         DC    F'28800'            DEM                                          
         DC    F'28800'            E                                            
         DC    F'28800'            GE                                           
         DC    F'28800'            ST                                           
         DC    F'28800'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'14500'            PP,CAR,SD                                    
         DC    F'14500'            SOC,GD                                       
         DC    F'14500'            VO,SS                                        
         DC    F'14500'            GS                                           
         DC    F'14500'            SA                                           
         DC    F'14500'            DEM                                          
         DC    F'14500'            E                                            
         DC    F'14500'            GE                                           
         DC    F'14500'            ST                                           
         DC    F'14500'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
*                                  1202:SESSION FEE                             
BSCHTBL  DC    AL1(80,40,0,0)      NATIONAL TV  - HOURLY RATE                   
         DC    F'9400'             PP,SD,CAR                                    
         DC    F'9400'             SOC,GD                                       
         DC    F'7850'             VO,SS                                        
         DC    F'7850'             GS                                           
         DC    F'14100'            SA = PP+50%                                  
         DC    F'9400'             DEM                                          
         DC    F'5850'             E                                            
         DC    F'3800'             GE                                           
         DC    F'9400'             ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(81,40,0,0)      NATIONAL TV  - OVERTIME HOURLY RATE          
         DC    F'12250'            PP,SD,CAR                                    
         DC    F'12250'            SOC,GD                                       
         DC    F'9850'             VO,SS                                        
         DC    F'9850'             GS                                           
         DC    F'18375'            SA = PP+50%                                  
         DC    F'12250'            DEM                                          
         DC    F'6950'             E                                            
         DC    F'4950'             GE                                           
         DC    F'12250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(82,40,0,0)      NATIONAL TV  - DBLTIME HOURLY RATE           
         DC    F'13800'            PP,SD,CAR                                    
         DC    F'13800'            SOC,GD                                       
         DC    F'12250'            VO,SS                                        
         DC    F'12250'            GS                                           
         DC    F'20700'            SA = PP+50%                                  
         DC    F'13800'            DEM                                          
         DC    F'8700'             E                                            
         DC    F'5700'             GE                                           
         DC    F'13800'            ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  1820:NEW MEDIA                               
CNMTBL   DC    AL1(100,40,0,0)     NEW MEDIA VIDEO 4 WEEKS - 35%                
         DC    F'12675'            PP,SD,CAR                                    
         DC    F'12675'            SOC,GD                                       
         DC    F'9250'             VO,SS                                        
         DC    F'4000'             GS                                           
         DC    F'19000'            SA = PP+50%                                  
         DC    F'12675'            DEM                                          
         DC    F'7750'             E                                            
         DC    F'5175'             GE                                           
         DC    F'12675'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(101,12,0,0)     NEW MEDIA AUDIO 4 WEEKS - 35%                
         DC    F'9850'             SS,SV                                        
         DC    F'7400'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(110,40,0,0)     NEW MEDIA VIDEO 8 WEEKS - 50%                
         DC    F'18100'            PP,SD,CAR                                    
         DC    F'18100'            SOC,GD                                       
         DC    F'13200'            VO,SS                                        
         DC    F'5725'             GS                                           
         DC    F'27150'            SA = PP+50%                                  
         DC    F'18100'            DEM                                          
         DC    F'11075'            E                                            
         DC    F'7400'             GE                                           
         DC    F'18100'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(111,12,0,0)     NEW MEDIA AUDIO 8 WEEKS - 50%                
         DC    F'14100'            SS,SV                                        
         DC    F'10575'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(120,40,0,0)     NEW MEDIA VIDEO 26 WEEKS - 75%               
         DC    F'27150'            PP,SD,CAR                                    
         DC    F'27150'            SOC,GD                                       
         DC    F'19800'            VO,SS                                        
         DC    F'8600'             GS                                           
         DC    F'40725'            SA = PP+50%                                  
         DC    F'27150'            DEM                                          
         DC    F'16625'            E                                            
         DC    F'11100'            GE                                           
         DC    F'27150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(121,12,0,0)     NEW MEDIA AUDIO 26 WEEKS - 75%               
         DC    F'21125'            SS,SV                                        
         DC    F'15875'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(130,40,0,0)     NEW MEDIA VIDEO 1 YEAR - 100%                
         DC    F'36200'            PP,SD,CAR                                    
         DC    F'36200'            SOC,GD                                       
         DC    F'26400'            VO,SS                                        
         DC    F'11450'            GS                                           
         DC    F'54300'            SA = PP+50%                                  
         DC    F'36200'            DEM                                          
         DC    F'22150'            E                                            
         DC    F'14800'            GE                                           
         DC    F'36200'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(131,12,0,0)     NEW MEDIA AUDIO 1 YEAR - 100%                
         DC    F'28175'            SS,SV                                        
         DC    F'21150'            MV,GS                                        
         SPACE 1                                                                
CNMATBL  DC    AL1(140,12,0,0)     NEW MEDIA AUDIO 4 WEEKS ADDTL CUTS           
         DC    F'4935'             SS,SV                                        
         DC    F'3700'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(141,12,0,0)     NEW MEDIA AUDIO 8 WEEKS ADDTL CUTS           
         DC    F'7050'             SS,SV                                        
         DC    F'5300'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(142,12,0,0)     NEW MEDIA AUDIO 26 WEEKS ADDTL CUTS          
         DC    F'10575'            SS,SV                                        
         DC    F'7950'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(143,12,0,0)     NEW MEDIA AUDIO 1 YEAR ADDTL CUTS            
         DC    F'14100'            SS,SV                                        
         DC    F'10600'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(144,12,0,0)     BSC NEW MEDIA AUDIO ADDTL CUTS               
         DC    F'14100'            SS,SV                                        
         DC    F'10600'            MV,GS                                        
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
**PAN#1  DC    CL21'023TAGEN75   10/09/15'                                      
         END                                                                    
