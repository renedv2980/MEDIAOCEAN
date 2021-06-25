*          DATA SET TAGEN70    AT LEVEL 006 AS OF 02/12/09                      
*PHASE T70270A,*                                                                
         TITLE 'T70270 - TABLES FOR YEAR 3 CANADIAN CONTRACT'                   
T70270   CSECT                                                                  
         DC    AL4(USETBLS-T70270)                                              
         DC    AL4(USELUT-T70270)                                               
         DC    AL4(MAJLUT-T70270)                                               
         DC    AL4(AFMCOLS-T70270)                                              
         DC    AL4(RADCOLS-T70270)                                              
         DC    AL4(OFFCOLS-T70270)                                              
         DC    AL4(ONCOLS-T70270)                                               
         DC    AL4(MSWEET-T70270)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'68900'            PP,SD,CAR                                    
         DC    F'68900'            SOC,GD                                       
         DC    F'50250'            VO,SS                                        
         DC    F'21750'            GS                                           
         DC    F'103350'           SA = PP+50%                                  
         DC    F'68900'            DEM                                          
         DC    F'42150'            E                                            
         DC    F'28150'            GE                                           
         DC    F'68900'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'68900'                                                         
         DC    F'68900'                                                         
         DC    F'50250'                                                         
         DC    F'21750'                                                         
         DC    F'103350'                                                        
         DC    F'68900'                                                         
         DC    F'42150'                                                         
         DC    F'28150'                                                         
         DC    F'68900'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'68900'                                                         
         DC    F'68900'                                                         
         DC    F'50250'                                                         
         DC    F'21750'                                                         
         DC    F'103350'                                                        
         DC    F'68900'                                                         
         DC    F'42150'                                                         
         DC    F'28150'                                                         
         DC    F'68900'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'68900'                                                         
         DC    F'68900'                                                         
         DC    F'50250'                                                         
         DC    F'21750'                                                         
         DC    F'103350'                                                        
         DC    F'68900'                                                         
         DC    F'42150'                                                         
         DC    F'28150'                                                         
         DC    F'68900'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'68900'                                                         
         DC    F'68900'                                                         
         DC    F'50250'                                                         
         DC    F'21750'                                                         
         DC    F'103350'                                                        
         DC    F'68900'                                                         
         DC    F'42150'                                                         
         DC    F'28150'                                                         
         DC    F'68900'            ST, SAME AS SOC                              
         SPACE 1                                                                
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
         DC    AL1(8,40,0,0)       VIDEO                                        
         DC    F'33600'                                                         
         DC    F'33600'                                                         
         DC    F'24700'                                                         
         DC    F'11050'                                                         
         DC    F'50400'                                                         
         DC    F'33600'                                                         
         DC    F'21000'                                                         
         DC    F'14150'                                                         
         DC    F'33600'            ST, SAME AS SOC                              
         EJECT                                                                  
DORTTBL  DC    AL1(12,40,0,0)      NATIONAL TV                                  
         DC    F'68900'            PP,SD,CAR                                    
         DC    F'68900'            SOC,GD                                       
         DC    F'50250'            VO,SS                                        
         DC    F'21750'            GS                                           
         DC    F'103350'           SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68900'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'68900'                                                         
         DC    F'68900'                                                         
         DC    F'50250'                                                         
         DC    F'21750'                                                         
         DC    F'103350'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68900'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'68900'                                                         
         DC    F'68900'                                                         
         DC    F'50250'                                                         
         DC    F'21750'                                                         
         DC    F'103350'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68900'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'68900'                                                         
         DC    F'68900'                                                         
         DC    F'50250'                                                         
         DC    F'21750'                                                         
         DC    F'103350'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68900'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'68900'                                                         
         DC    F'68900'                                                         
         DC    F'50250'                                                         
         DC    F'21750'                                                         
         DC    F'103350'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68900'            ST SAME AS SOC                               
         SPACE 1                                                                
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
BSCRTBL  DC    AL1(24,12,0,0)      NATIONAL RADIO                               
         DC    F'53650'            SV,SS                                        
         DC    F'40275'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'27350'                                                         
         DC    F'20550'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'36150'                                                         
         DC    F'27050'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'41025'                                                         
         DC    F'30850'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'46475'                                                         
         DC    F'34900'                                                         
         SPACE 1                                                                
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
         DC    AL1(33,12,0,0)      AUDIO                                        
         DC    F'26200'                                                         
         DC    F'20000'                                                         
         EJECT                                                                  
WSCTBL   DC    AL1(48,40,1,1)      TV WILDSPOT UNITS 1                          
         DC    F'53670'            PP,CAR,SD                                    
         DC    F'39330'            SOC,GD                                       
         DC    F'26265'            VO,SS                                        
         DC    F'15730'            GS                                           
         DC    F'53670'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'39330'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,2,2)      TV WILDSPOT UNITS 2                          
         DC    F'53670'            PP,CAR,SD                                    
         DC    F'39330'            SOC,GD                                       
         DC    F'26265'            VO,SS                                        
         DC    F'15730'            GS                                           
         DC    F'53670'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'39330'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,3,3)      TV WILDSPOT UNITS 3                          
         DC    F'53670'            PP,CAR,SD                                    
         DC    F'39330'            SOC,GD                                       
         DC    F'26265'            VO,SS                                        
         DC    F'15730'            GS                                           
         DC    F'53670'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'39330'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,4,4)      TV WILDSPOT UNITS 4                          
         DC    F'53670'            PP,CAR,SD                                    
         DC    F'39330'            SOC,GD                                       
         DC    F'26265'            VO,SS                                        
         DC    F'15730'            GS                                           
         DC    F'53670'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'39330'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,5,5)      TV WILDSPOT UNITS 5                          
         DC    F'53670'            PP,CAR,SD                                    
         DC    F'39330'            SOC,GD                                       
         DC    F'26265'            VO,SS                                        
         DC    F'15730'            GS                                           
         DC    F'53670'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'39330'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'55070'                                                         
         DC    F'40230'                                                         
         DC    F'27235'                                                         
         DC    F'16215'                                                         
         DC    F'55070'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40230'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'56525'                                                         
         DC    F'41150'                                                         
         DC    F'28155'                                                         
         DC    F'16780'                                                         
         DC    F'56525'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'57965'                                                         
         DC    F'42045'                                                         
         DC    F'29095'                                                         
         DC    F'17305'                                                         
         DC    F'57965'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42045'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'59380'                                                         
         DC    F'42930'                                                         
         DC    F'30000'                                                         
         DC    F'17850'                                                         
         DC    F'59380'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42930'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'60820'                                                         
         DC    F'43825'                                                         
         DC    F'30935'                                                         
         DC    F'18370'                                                         
         DC    F'60820'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43825'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'62250'                                                         
         DC    F'44690'                                                         
         DC    F'31850'                                                         
         DC    F'18725'                                                         
         DC    F'62250'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44690'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'63665'                                                         
         DC    F'45600'                                                         
         DC    F'32800'                                                         
         DC    F'19155'                                                         
         DC    F'63665'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'65145'                                                         
         DC    F'46530'                                                         
         DC    F'33810'                                                         
         DC    F'19500'                                                         
         DC    F'65145'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46530'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'67030'                                                         
         DC    F'47340'                                                         
         DC    F'34715'                                                         
         DC    F'19900'                                                         
         DC    F'67030'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47340'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'67375'                                                         
         DC    F'48285'                                                         
         DC    F'35615'                                                         
         DC    F'20300'                                                         
         DC    F'67975'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'69400'                                                         
         DC    F'49175'                                                         
         DC    F'36590'                                                         
         DC    F'20620'                                                         
         DC    F'69400'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49175'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'71495'                                                         
         DC    F'50055'                                                         
         DC    F'37500'                                                         
         DC    F'21075'                                                         
         DC    F'71495'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50055'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'72890'                                                         
         DC    F'50940'                                                         
         DC    F'38415'                                                         
         DC    F'21445'                                                         
         DC    F'72890'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50940'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'73635'                                                         
         DC    F'51840'                                                         
         DC    F'39370'                                                         
         DC    F'21815'                                                         
         DC    F'73635'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51840'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'75130'                                                         
         DC    F'52730'                                                         
         DC    F'40300'                                                         
         DC    F'22180'                                                         
         DC    F'75130'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52730'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'76575'                                                         
         DC    F'53440'                                                         
         DC    F'41265'                                                         
         DC    F'22595'                                                         
         DC    F'76575'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53440'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'78000'                                                         
         DC    F'54205'                                                         
         DC    F'42225'                                                         
         DC    F'22965'                                                         
         DC    F'78000'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54205'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'79460'                                                         
         DC    F'54935'                                                         
         DC    F'43140'                                                         
         DC    F'23355'                                                         
         DC    F'79460'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54935'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'80855'                                                         
         DC    F'55570'                                                         
         DC    F'44045'                                                         
         DC    F'23790'                                                         
         DC    F'80855'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55570'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'82270'                                                         
         DC    F'56390'                                                         
         DC    F'45055'                                                         
         DC    F'24065'                                                         
         DC    F'82270'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56390'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'83705'                                                         
         DC    F'57170'                                                         
         DC    F'45580'                                                         
         DC    F'24530'                                                         
         DC    F'83705'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57170'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'85180'                                                         
         DC    F'57875'                                                         
         DC    F'46145'                                                         
         DC    F'24800'                                                         
         DC    F'85180'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57875'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'86580'                                                         
         DC    F'58570'                                                         
         DC    F'46710'                                                         
         DC    F'25130'                                                         
         DC    F'86580'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58570'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'88040'                                                         
         DC    F'59355'                                                         
         DC    F'47270'                                                         
         DC    F'25460'                                                         
         DC    F'88040'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59355'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'89495'                                                         
         DC    F'60055'                                                         
         DC    F'47850'                                                         
         DC    F'25810'                                                         
         DC    F'89495'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60055'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'90575'                                                         
         DC    F'60820'                                                         
         DC    F'48375'                                                         
         DC    F'26120'                                                         
         DC    F'90575'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60820'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'91580'                                                         
         DC    F'61535'                                                         
         DC    F'48980'                                                         
         DC    F'26465'                                                         
         DC    F'91580'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61535'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'92710'                                                         
         DC    F'62250'                                                         
         DC    F'49545'                                                         
         DC    F'26775'                                                         
         DC    F'92710'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'93805'                                                         
         DC    F'62990'                                                         
         DC    F'50105'                                                         
         DC    F'27170'                                                         
         DC    F'93805'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62990'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'94900'                                                         
         DC    F'63670'                                                         
         DC    F'50665'                                                         
         DC    F'27480'                                                         
         DC    F'94900'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63670'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'95980'                                                         
         DC    F'64230'                                                         
         DC    F'51090'                                                         
         DC    F'27785'                                                         
         DC    F'95980'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64230'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'97035'                                                         
         DC    F'64755'                                                         
         DC    F'51470'                                                         
         DC    F'28135'                                                         
         DC    F'97035'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64755'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'98145'                                                         
         DC    F'65375'                                                         
         DC    F'51940'                                                         
         DC    F'28455'                                                         
         DC    F'98145'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65375'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'99200'                                                         
         DC    F'65855'                                                         
         DC    F'52270'                                                         
         DC    F'28770'                                                         
         DC    F'99200'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65855'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'100295'                                                        
         DC    F'66415'                                                         
         DC    F'52710'                                                         
         DC    F'29135'                                                         
         DC    F'100295'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66415'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'101025'                                                        
         DC    F'66770'                                                         
         DC    F'53075'                                                         
         DC    F'29375'                                                         
         DC    F'101025'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66770'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'101740'                                                        
         DC    F'67495'                                                         
         DC    F'53500'                                                         
         DC    F'29685'                                                         
         DC    F'101740'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67495'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'102470'                                                        
         DC    F'68045'                                                         
         DC    F'53880'                                                         
         DC    F'29920'                                                         
         DC    F'102470'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68045'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'103215'                                                        
         DC    F'68590'                                                         
         DC    F'54250'                                                         
         DC    F'30250'                                                         
         DC    F'103215'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68590'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'103915'                                                        
         DC    F'69100'                                                         
         DC    F'54675'                                                         
         DC    F'30490'                                                         
         DC    F'103915'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69100'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'104685'                                                        
         DC    F'69655'                                                         
         DC    F'55095'                                                         
         DC    F'30830'                                                         
         DC    F'104685'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69655'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'105410'                                                        
         DC    F'70205'                                                         
         DC    F'55490'                                                         
         DC    F'31095'                                                         
         DC    F'105410'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70205'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'106150'                                                        
         DC    F'70740'                                                         
         DC    F'55900'                                                         
         DC    F'31360'                                                         
         DC    F'106150'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70740'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'106870'                                                        
         DC    F'71280'                                                         
         DC    F'56305'                                                         
         DC    F'31630'                                                         
         DC    F'106870'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71280'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'107610'                                                        
         DC    F'71810'                                                         
         DC    F'56725'                                                         
         DC    F'31910'                                                         
         DC    F'107610'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71810'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'140'                                                           
         DC    F'108'                                                           
         DC    F'80'                                                            
         DC    F'43'                                                            
         DC    F'140'                                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'108'               ST, SAME AS SOC                             
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
         DC    AL1(60,40,1,1)      TV WSP&NET SPC/NETWORK UNITS 1               
         DC    F'75045'            PP,CAR,SD                                    
         DC    F'55060'            SOC,GD                                       
         DC    F'35020'            VO,SS                                        
         DC    F'20035'            GS                                           
         DC    F'75045'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,2,2)      TV WSP&NET SPC/NETWORK UNITS 2               
         DC    F'75045'                                                         
         DC    F'55060'                                                         
         DC    F'35020'                                                         
         DC    F'20035'                                                         
         DC    F'75045'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,3,3)      TV WSP&NET SPC/NETWORK UNITS 3               
         DC    F'75045'                                                         
         DC    F'55060'                                                         
         DC    F'35020'                                                         
         DC    F'20035'                                                         
         DC    F'75045'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,4,4)      TV WSP&NET SPC/NETWORK UNIT 4                
         DC    F'75045'                                                         
         DC    F'55060'                                                         
         DC    F'35020'                                                         
         DC    F'20035'                                                         
         DC    F'75045'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,5,5)      TV WSP&NET SPC/NETWORK UNIT 5                
         DC    F'75045'                                                         
         DC    F'55060'                                                         
         DC    F'35020'                                                         
         DC    F'20035'                                                         
         DC    F'75045'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'77050'                                                         
         DC    F'56270'                                                         
         DC    F'36280'                                                         
         DC    F'20665'                                                         
         DC    F'77050'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56270'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'79060'                                                         
         DC    F'57520'                                                         
         DC    F'37570'                                                         
         DC    F'21335'                                                         
         DC    F'79060'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57520'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'81115'                                                         
         DC    F'58775'                                                         
         DC    F'38805'                                                         
         DC    F'22000'                                                         
         DC    F'81115'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58775'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'83140'                                                         
         DC    F'60045'                                                         
         DC    F'40155'                                                         
         DC    F'22695'                                                         
         DC    F'83140'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60045'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'85075'                                                         
         DC    F'61265'                                                         
         DC    F'41370'                                                         
         DC    F'23320'                                                         
         DC    F'85075'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61265'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'87135'                                                         
         DC    F'62505'                                                         
         DC    F'42655'                                                         
         DC    F'23835'                                                         
         DC    F'87135'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62505'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'89110'                                                         
         DC    F'63745'                                                         
         DC    F'43925'                                                         
         DC    F'24355'                                                         
         DC    F'89110'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63745'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'91100'                                                         
         DC    F'64955'                                                         
         DC    F'45200'                                                         
         DC    F'24880'                                                         
         DC    F'91100'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64955'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'93155'                                                         
         DC    F'66250'                                                         
         DC    F'46530'                                                         
         DC    F'25415'                                                         
         DC    F'93155'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'95155'                                                         
         DC    F'67490'                                                         
         DC    F'47750'                                                         
         DC    F'25900'                                                         
         DC    F'95155'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67490'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'97150'                                                         
         DC    F'68670'                                                         
         DC    F'48980'                                                         
         DC    F'26395'                                                         
         DC    F'97150'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68670'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'99130'                                                         
         DC    F'69940'                                                         
         DC    F'50240'                                                         
         DC    F'26945'                                                         
         DC    F'99130'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69940'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'101175'                                                        
         DC    F'71190'                                                         
         DC    F'51470'                                                         
         DC    F'27435'                                                         
         DC    F'101175'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71190'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'103120'                                                        
         DC    F'72445'                                                         
         DC    F'52725'                                                         
         DC    F'27910'                                                         
         DC    F'103120'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72445'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'105150'                                                        
         DC    F'73635'                                                         
         DC    F'54005'                                                         
         DC    F'28620'                                                         
         DC    F'105150'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73635'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'107195'                                                        
         DC    F'74715'                                                         
         DC    F'55150'                                                         
         DC    F'28900'                                                         
         DC    F'107195'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'109195'                                                        
         DC    F'75765'                                                         
         DC    F'56405'                                                         
         DC    F'29335'                                                         
         DC    F'109195'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75765'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'111195'                                                        
         DC    F'76825'                                                         
         DC    F'57685'                                                         
         DC    F'29750'                                                         
         DC    F'111195'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76825'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'113160'                                                        
         DC    F'77905'                                                         
         DC    F'58890'                                                         
         DC    F'30215'                                                         
         DC    F'113160'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77905'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'115220'                                                        
         DC    F'78945'                                                         
         DC    F'60125'                                                         
         DC    F'30670'                                                         
         DC    F'115220'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78945'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'117185'                                                        
         DC    F'79945'                                                         
         DC    F'60920'                                                         
         DC    F'31120'                                                         
         DC    F'117185'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79945'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'119185'                                                        
         DC    F'81025'                                                         
         DC    F'61725'                                                         
         DC    F'31565'                                                         
         DC    F'119185'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'81025'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'121080'                                                        
         DC    F'82060'                                                         
         DC    F'62470'                                                         
         DC    F'31995'                                                         
         DC    F'121080'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'82060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'123095'                                                        
         DC    F'83160'                                                         
         DC    F'63240'                                                         
         DC    F'32395'                                                         
         DC    F'123095'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'83160'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'125055'                                                        
         DC    F'84175'                                                         
         DC    F'64015'                                                         
         DC    F'32880'                                                         
         DC    F'125055'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'84175'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'126645'                                                        
         DC    F'85220'                                                         
         DC    F'64750'                                                         
         DC    F'33340'                                                         
         DC    F'126645'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'85220'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'128115'                                                        
         DC    F'86185'                                                         
         DC    F'65485'                                                         
         DC    F'33710'                                                         
         DC    F'128115'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'86185'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'129615'                                                        
         DC    F'87210'                                                         
         DC    F'66285'                                                         
         DC    F'34120'                                                         
         DC    F'129615'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87210'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'131200'                                                        
         DC    F'87900'                                                         
         DC    F'67025'                                                         
         DC    F'34515'                                                         
         DC    F'131200'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87900'           ST, SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'132700'                                                        
         DC    F'89225'                                                         
         DC    F'67840'                                                         
         DC    F'34935'                                                         
         DC    F'132700'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'89225'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'134225'                                                        
         DC    F'90035'                                                         
         DC    F'68340'                                                         
         DC    F'35380'                                                         
         DC    F'134225'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90035'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'135745'                                                        
         DC    F'90810'                                                         
         DC    F'68850'                                                         
         DC    F'35800'                                                         
         DC    F'135745'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90810'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'137265'                                                        
         DC    F'91555'                                                         
         DC    F'69340'                                                         
         DC    F'36220'                                                         
         DC    F'137265'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'91555'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'138830'                                                        
         DC    F'92300'                                                         
         DC    F'69845'                                                         
         DC    F'36610'                                                         
         DC    F'138830'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'92300'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'140305'                                                        
         DC    F'93085'                                                         
         DC    F'70385'                                                         
         DC    F'37015'                                                         
         DC    F'140305'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93085'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'141360'                                                        
         DC    F'93805'                                                         
         DC    F'70880'                                                         
         DC    F'37320'                                                         
         DC    F'141360'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93805'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'142340'                                                        
         DC    F'94535'                                                         
         DC    F'71385'                                                         
         DC    F'37715'                                                         
         DC    F'142340'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'94535'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'143355'                                                        
         DC    F'95330'                                                         
         DC    F'71865'                                                         
         DC    F'38080'                                                         
         DC    F'143355'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'95330'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'144435'                                                        
         DC    F'96050'                                                         
         DC    F'72385'                                                         
         DC    F'38410'                                                         
         DC    F'144435'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96050'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'145410'                                                        
         DC    F'96725'                                                         
         DC    F'72925'                                                         
         DC    F'38790'                                                         
         DC    F'145410'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96725'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'146485'                                                        
         DC    F'97515'                                                         
         DC    F'73360'                                                         
         DC    F'39100'                                                         
         DC    F'146485'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'97515'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'147455'                                                        
         DC    F'98190'                                                         
         DC    F'73875'                                                         
         DC    F'39370'                                                         
         DC    F'147455'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'98190'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'148445'                                                        
         DC    F'98885'                                                         
         DC    F'74345'                                                         
         DC    F'39750'                                                         
         DC    F'148445'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'98885'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'149495'                                                        
         DC    F'99680'                                                         
         DC    F'74820'                                                         
         DC    F'40000'                                                         
         DC    F'149495'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'99680'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'150510'                                                        
         DC    F'100390'                                                        
         DC    F'75345'                                                         
         DC    F'40370'                                                         
         DC    F'150510'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'100390'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'194'                                                           
         DC    F'149'                                                           
         DC    F'99'                                                            
         DC    F'54'                                                            
         DC    F'194'                                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'149'              ST, SAME AS SOC                              
         SPACE 1                                                                
CAUTTBL  DC    AL1(68,44,0,0)      AUDITION INTERVIEW - TV                      
         DC    F'7250'             PP,CAR,SD                                    
         DC    F'7250'             SOC,GD                                       
         DC    F'7250'             VO,SS                                        
         DC    F'7250'             GS                                           
         DC    F'7250'             SA                                           
         DC    F'7250'             DEM                                          
         DC    F'7250'             E                                            
         DC    F'7250'             GE                                           
         DC    F'7250'             ST                                           
         DC    F'7250'             EVERYBODY ELSE-US,SI,SB,PT                   
         SPACE 1                                                                
CAURTBL  DC    AL1(69,12,0,0)      AUDITION INTERVIEW - RADIO                   
         DC    F'7250'             SS,SV                                        
         DC    F'7250'             MV,GS                                        
         EJECT                                                                  
CDMTBL   DC    AL1(70,44,2,2)      TV PRESENTATION DEMO 2                       
         DC    F'26750'            PP,CAR,SD                                    
         DC    F'26750'            SOC,GD                                       
         DC    F'26750'            VO,SS                                        
         DC    F'26750'            GS                                           
         DC    F'26750'            SA                                           
         DC    F'26750'            DEM                                          
         DC    F'26750'            E                                            
         DC    F'26750'            GE                                           
         DC    F'26750'            ST                                           
         DC    F'26750'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'13450'            PP,CAR,SD                                    
         DC    F'13450'            SOC,GD                                       
         DC    F'13450'            VO,SS                                        
         DC    F'13450'            GS                                           
         DC    F'13450'            SA                                           
         DC    F'13450'            DEM                                          
         DC    F'13450'            E                                            
         DC    F'13450'            GE                                           
         DC    F'13450'            ST                                           
         DC    F'13450'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
BSCHTBL  DC    AL1(80,40,0,0)      NATIONAL TV  - HOURLY RATE                   
         DC    F'8750'             PP,SD,CAR                                    
         DC    F'8750'             SOC,GD                                       
         DC    F'7250'             VO,SS                                        
         DC    F'7250'             GS                                           
         DC    F'13150'            SA = PP+50%                                  
         DC    F'8750'             DEM                                          
         DC    F'5400'             E                                            
         DC    F'3500'             GE                                           
         DC    F'8750'             ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(81,40,0,0)      NATIONAL TV  - OVERTIME HOURLY RATE          
         DC    F'11350'            PP,SD,CAR                                    
         DC    F'11350'            SOC,GD                                       
         DC    F'9100'             VO,SS                                        
         DC    F'9100'             GS                                           
         DC    F'17050'            SA = PP+50%                                  
         DC    F'11350'            DEM                                          
         DC    F'6500'             E                                            
         DC    F'4450'             GE                                           
         DC    F'11350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(82,40,0,0)      NATIONAL TV  - DBLTIME HOURLY RATE           
         DC    F'12800'            PP,SD,CAR                                    
         DC    F'12800'            SOC,GD                                       
         DC    F'11350'            VO,SS                                        
         DC    F'11350'            GS                                           
         DC    F'19200'            SA = PP+50%                                  
         DC    F'12800'            DEM                                          
         DC    F'8100'             E                                            
         DC    F'5400'             GE                                           
         DC    F'12800'            ST, SAME AS SOC                              
         SPACE 1                                                                
CNMTBL   DC    AL1(100,40,0,0)     NEW MEDIA VIDEO 4 WEEKS                      
         DC    F'11750'            PP,SD,CAR                                    
         DC    F'11750'            SOC,GD                                       
         DC    F'8650'             VO,SS                                        
         DC    F'3875'             GS                                           
         DC    F'17625'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'11750'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(101,12,0,0)     NEW MEDIA AUDIO 4 WEEKS                      
         DC    F'9150'             SS,SV                                        
         DC    F'7000'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(110,40,0,0)     NEW MEDIA VIDEO 8 WEEKS                      
         DC    F'16800'            PP,SD,CAR                                    
         DC    F'16800'            SOC,GD                                       
         DC    F'12350'            VO,SS                                        
         DC    F'5525'             GS                                           
         DC    F'25200'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'16800'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(111,12,0,0)     NEW MEDIA AUDIO 8 WEEKS                      
         DC    F'13100'            SS,SV                                        
         DC    F'10000'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(120,40,0,0)     NEW MEDIA VIDEO 26 WEEKS                     
         DC    F'25200'            PP,SD,CAR                                    
         DC    F'25200'            SOC,GD                                       
         DC    F'18525'            VO,SS                                        
         DC    F'8300'             GS                                           
         DC    F'37800'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'25200'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(121,12,0,0)     NEW MEDIA AUDIO 26 WEEKS                     
         DC    F'19650'            SS,SV                                        
         DC    F'15000'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(130,40,0,0)     NEW MEDIA VIDEO 1 YEAR                       
         DC    F'33600'            PP,SD,CAR                                    
         DC    F'33600'            SOC,GD                                       
         DC    F'24700'            VO,SS                                        
         DC    F'11050'            GS                                           
         DC    F'50400'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'33600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(131,12,0,0)     NEW MEDIA AUDIO 1 YEAR                       
         DC    F'26200'            SS,SV                                        
         DC    F'20000'            MV,GS                                        
         SPACE 1                                                                
CNMATBL  DC    AL1(140,12,0,0)     NEW MEDIA AUDIO 4 WEEKS ADDTL CUTS           
         DC    F'4575'             SS,SV                                        
         DC    F'3500'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(141,12,0,0)     NEW MEDIA AUDIO 8 WEEKS ADDTL CUTS           
         DC    F'6550'             SS,SV                                        
         DC    F'5000'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(142,12,0,0)     NEW MEDIA AUDIO 26 WEEKS ADDTL CUTS          
         DC    F'9825'             SS,SV                                        
         DC    F'7500'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(143,12,0,0)     NEW MEDIA AUDIO 1 YEAR ADDTL CUTS            
         DC    F'13100'            SS,SV                                        
         DC    F'10000'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(144,12,0,0)     BSC NEW MEDIA AUDIO ADDTL CUTS               
         DC    F'13100'            SS,SV                                        
         DC    F'10000'            MV,GS                                        
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
         DC    AL1(4,CTGS)         GROUP SINGER                                 
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
         DC    AL1(4,CTGS)         GROUP SINGER                                 
         DC    AL1(5,CTSA)         SPECIALTY ACT                                
*                                  IF DEFINE ROW >=18, CHANGE SYSCALC           
         DC    AL1(60,CTSSM)       SOLO/DUE WITH MULTI                          
         DC    AL1(60,CTSS1)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS2)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS3)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS4)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTGSM)       GROUP SINGERS WITH MULTI                     
         DC    AL1(60,CTGSS)       GROUP SINGERS SWEETENING                     
         DC    AL1(60,CTSV)        SINGLE OR SINGER VOICE                       
         DC    AL1(60,CTMV)        MULTIPLE VOICE                               
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
**PAN#1  DC    CL21'006TAGEN70   02/12/09'                                      
         END                                                                    
