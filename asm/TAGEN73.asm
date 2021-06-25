*          DATA SET TAGEN73    AT LEVEL 008 AS OF 06/26/12                      
*PHASE T70273E,*                                                                
         TITLE 'T70273 - TABLES FOR YEAR 1 ACTRA 2011-2012'                     
T70273   CSECT                                                                  
         DC    AL4(USETBLS-T70273)                                              
         DC    AL4(USELUT-T70273)                                               
         DC    AL4(MAJLUT-T70273)                                               
         DC    AL4(AFMCOLS-T70273)                                              
         DC    AL4(RADCOLS-T70273)                                              
         DC    AL4(OFFCOLS-T70273)                                              
         DC    AL4(ONCOLS-T70273)                                               
         DC    AL4(MSWEET-T70273)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
*                                  1202:SESSION FEE                             
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'70600'            PP,SD,CAR                                    
         DC    F'70600'            SOC,GD                                       
         DC    F'51450'            VO,SS                                        
         DC    F'22300'            GS                                           
         DC    F'105900'           SA = PP+50%                                  
         DC    F'70600'            DEM                                          
         DC    F'43200'            E                                            
         DC    F'28850'            GE                                           
         DC    F'70600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'70600'            PP,SD,CAR                                    
         DC    F'70600'            SOC,GD                                       
         DC    F'51450'            VO,SS                                        
         DC    F'22300'            GS                                           
         DC    F'105900'           SA = PP+50%                                  
         DC    F'70600'            DEM                                          
         DC    F'43200'            E                                            
         DC    F'28850'            GE                                           
         DC    F'70600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'70600'            PP,SD,CAR                                    
         DC    F'70600'            SOC,GD                                       
         DC    F'51450'            VO,SS                                        
         DC    F'22300'            GS                                           
         DC    F'105900'           SA = PP+50%                                  
         DC    F'70600'            DEM                                          
         DC    F'43200'            E                                            
         DC    F'28850'            GE                                           
         DC    F'70600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'70600'            PP,SD,CAR                                    
         DC    F'70600'            SOC,GD                                       
         DC    F'51450'            VO,SS                                        
         DC    F'22300'            GS                                           
         DC    F'105900'           SA = PP+50%                                  
         DC    F'70600'            DEM                                          
         DC    F'43200'            E                                            
         DC    F'28850'            GE                                           
         DC    F'70600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'70600'            PP,SD,CAR                                    
         DC    F'70600'            SOC,GD                                       
         DC    F'51450'            VO,SS                                        
         DC    F'22300'            GS                                           
         DC    F'105900'           SA = PP+50%                                  
         DC    F'70600'            DEM                                          
         DC    F'43200'            E                                            
         DC    F'28850'            GE                                           
         DC    F'70600'            ST, SAME AS SOC                              
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
         DC    AL1(8,40,0,0)       VIDEO                                        
         DC    F'35300'            PP,SD,CAR                                    
         DC    F'35300'            SOC,GD                                       
         DC    F'25750'            VO,SS                                        
         DC    F'11150'            GROUP SINGER                                 
         DC    F'50400'            SA = PP+50%                                  
         DC    F'35300'                                                         
         DC    F'21600'            BACKGROUND                                   
         DC    F'14450'            GROUP BACKGROUND                             
         DC    F'35300'            ST, SAME AS SOC                              
         EJECT                                                                  
DORTTBL  DC    AL1(12,40,0,0)      NATIONAL TV                                  
         DC    F'70600'            PP,SD,CAR                                    
         DC    F'70600'            SOC,GD                                       
         DC    F'51450'            VO,SS                                        
         DC    F'22300'            GS                                           
         DC    F'105900'           SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70600'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'70600'                                                         
         DC    F'70600'                                                         
         DC    F'51450'                                                         
         DC    F'22300'                                                         
         DC    F'105900'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70600'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'70600'                                                         
         DC    F'70600'                                                         
         DC    F'51450'                                                         
         DC    F'22300'                                                         
         DC    F'105900'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70600'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'70600'                                                         
         DC    F'70600'                                                         
         DC    F'51450'                                                         
         DC    F'22300'                                                         
         DC    F'105900'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70600'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'70600'                                                         
         DC    F'70600'                                                         
         DC    F'51450'                                                         
         DC    F'22300'                                                         
         DC    F'105900'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70600'            ST SAME AS SOC                               
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
         DC    F'54975'            SV,SS                                        
         DC    F'41250'            MV,GS                                        
         SPACE 1                                                                
*                                  406:RADIO SESSION AND RESIDUAL FEES          
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'28025'                                                         
         DC    F'21075'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'37050'                                                         
         DC    F'27725'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'42050'                                                         
         DC    F'31625'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'47625'                                                         
         DC    F'35775'                                                         
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
         DC    AL1(33,12,0,0)      AUDIO                                        
         DC    F'27500'                                                         
         DC    F'20625'                                                         
         EJECT                                                                  
*                                  1804:TABLE A                                 
WSCTBL   DC    AL1(48,40,1,1)      TV WILDSPOT UNITS 1                          
         DC    F'55835'            PP,CAR,SD                                    
         DC    F'40920'            SOC,GD                                       
         DC    F'27325'            VO,SS                                        
         DC    F'16365'            GS                                           
         DC    F'55835'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,2,2)      TV WILDSPOT UNITS 2                          
         DC    F'55835'            PP,CAR,SD                                    
         DC    F'40920'            SOC,GD                                       
         DC    F'27325'            VO,SS                                        
         DC    F'16365'            GS                                           
         DC    F'55835'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,3,3)      TV WILDSPOT UNITS 3                          
         DC    F'55835'            PP,CAR,SD                                    
         DC    F'40920'            SOC,GD                                       
         DC    F'27325'            VO,SS                                        
         DC    F'16365'            GS                                           
         DC    F'55835'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,4,4)      TV WILDSPOT UNITS 4                          
         DC    F'55835'            PP,CAR,SD                                    
         DC    F'40920'            SOC,GD                                       
         DC    F'27325'            VO,SS                                        
         DC    F'16365'            GS                                           
         DC    F'55835'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,5,5)      TV WILDSPOT UNITS 5                          
         DC    F'55835'            PP,CAR,SD                                    
         DC    F'40920'            SOC,GD                                       
         DC    F'27325'            VO,SS                                        
         DC    F'16365'            GS                                           
         DC    F'55835'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'57295'            PP,CAR,SD                                    
         DC    F'41855'            SOC,GD                                       
         DC    F'28335'            VO,SS                                        
         DC    F'16870'            GS                                           
         DC    F'57295'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41855'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'58805'            PP,CAR,SD                                    
         DC    F'42810'            SOC,GD                                       
         DC    F'29290'            VO,SS                                        
         DC    F'17455'            GS                                           
         DC    F'58805'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42810'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'60305'            PP,CAR,SD                                    
         DC    F'43745'            SOC,GD                                       
         DC    F'30270'            VO,SS                                        
         DC    F'18005'            GS                                           
         DC    F'60305'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43745'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'61775'            PP,CAR,SD                                    
         DC    F'44665'            SOC,GD                                       
         DC    F'31210'            VO,SS                                        
         DC    F'18570'            GS                                           
         DC    F'61775'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44665'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'63275'            PP,CAR,SD                                    
         DC    F'45595'            SOC,GD                                       
         DC    F'32185'            VO,SS                                        
         DC    F'19110'            GS                                           
         DC    F'63275'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45595'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'64765'            PP,CAR,SD                                    
         DC    F'46495'            SOC,GD                                       
         DC    F'33135'            VO,SS                                        
         DC    F'19480'            GS                                           
         DC    F'64765'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46495'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'66235'            PP,CAR,SD                                    
         DC    F'47440'            SOC,GD                                       
         DC    F'34125'            VO,SS                                        
         DC    F'19930'            GS                                           
         DC    F'66235'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47440'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'67775'            PP,CAR,SD                                    
         DC    F'48410'            SOC,GD                                       
         DC    F'35175'            VO,SS                                        
         DC    F'20285'            GS                                           
         DC    F'67775'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48410'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'69735'            PP,CAR,SD                                    
         DC    F'49250'            SOC,GD                                       
         DC    F'36115'            VO,SS                                        
         DC    F'20705'            GS                                           
         DC    F'69735'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'70720'            PP,CAR,SD                                    
         DC    F'50235'            SOC,GD                                       
         DC    F'37055'            VO,SS                                        
         DC    F'21120'            GS                                           
         DC    F'70720'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50235'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'72200'            PP,CAR,SD                                    
         DC    F'51160'            SOC,GD                                       
         DC    F'38065'            VO,SS                                        
         DC    F'21455'            GS                                           
         DC    F'72200'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51160'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'74380'            PP,CAR,SD                                    
         DC    F'52075'            SOC,GD                                       
         DC    F'39015'            VO,SS                                        
         DC    F'21925'            GS                                           
         DC    F'74380'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52075'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'75835'            PP,CAR,SD                                    
         DC    F'52995'            SOC,GD                                       
         DC    F'39965'            VO,SS                                        
         DC    F'22310'            GS                                           
         DC    F'75835'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52995'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'76610'            PP,CAR,SD                                    
         DC    F'53935'            SOC,GD                                       
         DC    F'40960'            VO,SS                                        
         DC    F'22695'            GS                                           
         DC    F'76610'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53935'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'78165'            PP,CAR,SD                                    
         DC    F'54860'            SOC,GD                                       
         DC    F'41925'            VO,SS                                        
         DC    F'23075'            GS                                           
         DC    F'78165'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54860'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'79665'            PP,CAR,SD                                    
         DC    F'55600'            SOC,GD                                       
         DC    F'42930'            VO,SS                                        
         DC    F'23505'            GS                                           
         DC    F'79665'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'81150'            PP,CAR,SD                                    
         DC    F'56395'            SOC,GD                                       
         DC    F'43930'            VO,SS                                        
         DC    F'23890'            GS                                           
         DC    F'81150'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56395'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'82670'            PP,CAR,SD                                    
         DC    F'57155'            SOC,GD                                       
         DC    F'44880'            VO,SS                                        
         DC    F'24300'            GS                                           
         DC    F'82670'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57155'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'84120'            PP,CAR,SD                                    
         DC    F'57815'            SOC,GD                                       
         DC    F'45825'            VO,SS                                        
         DC    F'24750'            GS                                           
         DC    F'84120'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57815'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'85590'            PP,CAR,SD                                    
         DC    F'58665'            SOC,GD                                       
         DC    F'46875'            VO,SS                                        
         DC    F'25035'            GS                                           
         DC    F'85590'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58665'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'87085'            PP,CAR,SD                                    
         DC    F'59480'            SOC,GD                                       
         DC    F'47420'            VO,SS                                        
         DC    F'25520'            GS                                           
         DC    F'87085'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59480'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'88620'            PP,CAR,SD                                    
         DC    F'60210'            SOC,GD                                       
         DC    F'48010'            VO,SS                                        
         DC    F'25800'            GS                                           
         DC    F'88620'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60210'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'90075'            PP,CAR,SD                                    
         DC    F'60935'            SOC,GD                                       
         DC    F'48595'            VO,SS                                        
         DC    F'26145'            GS                                           
         DC    F'90075'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60935'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'91595'            PP,CAR,SD                                    
         DC    F'61750'            SOC,GD                                       
         DC    F'49180'            VO,SS                                        
         DC    F'26490'            GS                                           
         DC    F'91595'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61750'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'93110'            PP,CAR,SD                                    
         DC    F'62480'            SOC,GD                                       
         DC    F'49780'            VO,SS                                        
         DC    F'26850'            GS                                           
         DC    F'93110'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62480'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'94230'            PP,CAR,SD                                    
         DC    F'63275'            SOC,GD                                       
         DC    F'50330'            VO,SS                                        
         DC    F'27175'            GS                                           
         DC    F'94230'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63275'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'95280'            PP,CAR,SD                                    
         DC    F'64020'            SOC,GD                                       
         DC    F'50960'            VO,SS                                        
         DC    F'27535'            GS                                           
         DC    F'95280'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64020'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'96455'            PP,CAR,SD                                    
         DC    F'64765'            SOC,GD                                       
         DC    F'51545'            VO,SS                                        
         DC    F'27855'            GS                                           
         DC    F'96455'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64765'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'97590'            PP,CAR,SD                                    
         DC    F'65535'            SOC,GD                                       
         DC    F'52130'            VO,SS                                        
         DC    F'28265'            GS                                           
         DC    F'97590'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65535'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'98730'            PP,CAR,SD                                    
         DC    F'66240'            SOC,GD                                       
         DC    F'52710'            VO,SS                                        
         DC    F'28590'            GS                                           
         DC    F'98730'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66240'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'99855'            PP,CAR,SD                                    
         DC    F'66825'            SOC,GD                                       
         DC    F'53155'            VO,SS                                        
         DC    F'28905'            GS                                           
         DC    F'99855'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66825'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'100955'           PP,CAR,SD                                    
         DC    F'67370'            SOC,GD                                       
         DC    F'53550'            VO,SS                                        
         DC    F'29270'            GS                                           
         DC    F'100955'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67370'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'102110'           PP,CAR,SD                                    
         DC    F'68015'            SOC,GD                                       
         DC    F'54035'            VO,SS                                        
         DC    F'29605'            GS                                           
         DC    F'102110'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68015'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'103205'           PP,CAR,SD                                    
         DC    F'68515'            SOC,GD                                       
         DC    F'54380'            VO,SS                                        
         DC    F'29930'            GS                                           
         DC    F'103205'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68515'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'104345'           PP,CAR,SD                                    
         DC    F'69095'            SOC,GD                                       
         DC    F'54840'            VO,SS                                        
         DC    F'30310'            GS                                           
         DC    F'104345'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69095'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'105105'           PP,CAR,SD                                    
         DC    F'69465'            SOC,GD                                       
         DC    F'55220'            VO,SS                                        
         DC    F'30560'            GS                                           
         DC    F'105105'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69465'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'105850'           PP,CAR,SD                                    
         DC    F'70220'            SOC,GD                                       
         DC    F'55660'            VO,SS                                        
         DC    F'30885'            GS                                           
         DC    F'105850'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70220'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'106605'           PP,CAR,SD                                    
         DC    F'70790'            SOC,GD                                       
         DC    F'56055'            VO,SS                                        
         DC    F'31130'            GS                                           
         DC    F'106605'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70790'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'107380'           PP,CAR,SD                                    
         DC    F'71360'            SOC,GD                                       
         DC    F'56440'            VO,SS                                        
         DC    F'31470'            GS                                           
         DC    F'107380'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71360'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'108110'           PP,CAR,SD                                    
         DC    F'71890'            SOC,GD                                       
         DC    F'56885'            VO,SS                                        
         DC    F'31720'            GS                                           
         DC    F'108110'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71890'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'108910'           PP,CAR,SD                                    
         DC    F'72465'            SOC,GD                                       
         DC    F'57320'            VO,SS                                        
         DC    F'32075'            GS                                           
         DC    F'108910'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72465'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'109665'           PP,CAR,SD                                    
         DC    F'73040'            SOC,GD                                       
         DC    F'57730'            VO,SS                                        
         DC    F'32350'            GS                                           
         DC    F'109665'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73040'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'110435'           PP,CAR,SD                                    
         DC    F'73595'            SOC,GD                                       
         DC    F'58155'            VO,SS                                        
         DC    F'32625'            GS                                           
         DC    F'110435'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73595'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'111185'           PP,CAR,SD                                    
         DC    F'74160'            SOC,GD                                       
         DC    F'58580'            VO,SS                                        
         DC    F'32905'            GS                                           
         DC    F'111185'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74160'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'111955'           PP,CAR,SD                                    
         DC    F'74710'            SOC,GD                                       
         DC    F'59015'            VO,SS                                        
         DC    F'33200'            GS                                           
         DC    F'111955'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74710'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'145'              PP,CAR,SD                                    
         DC    F'112'              SOC,GD                                       
         DC    F'83'               VO,SS                                        
         DC    F'45'               GS                                           
         DC    F'145'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'112'              ST, SAME AS SOC                              
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
*                                  1805:TABLE B                                 
         DC    AL1(60,40,1,1)      TV WSP&NET SPC/NETWORK UNITS 1               
         DC    F'78075'            PP,CAR,SD                                    
         DC    F'57285'            SOC,GD                                       
         DC    F'36435'            VO,SS                                        
         DC    F'20845'            GS                                           
         DC    F'78075'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,2,2)      TV WSP&NET SPC/NETWORK UNITS 2               
         DC    F'78075'            PP,CAR,SD                                    
         DC    F'57285'            SOC,GD                                       
         DC    F'36435'            VO,SS                                        
         DC    F'20845'            GS                                           
         DC    F'78075'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,3,3)      TV WSP&NET SPC/NETWORK UNITS 3               
         DC    F'78075'            PP,CAR,SD                                    
         DC    F'57285'            SOC,GD                                       
         DC    F'36435'            VO,SS                                        
         DC    F'20845'            GS                                           
         DC    F'78075'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,4,4)      TV WSP&NET SPC/NETWORK UNIT 4                
         DC    F'78075'            PP,CAR,SD                                    
         DC    F'57285'            SOC,GD                                       
         DC    F'36435'            VO,SS                                        
         DC    F'20845'            GS                                           
         DC    F'78075'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,5,5)      TV WSP&NET SPC/NETWORK UNIT 5                
         DC    F'78075'            PP,CAR,SD                                    
         DC    F'57285'            SOC,GD                                       
         DC    F'36435'            VO,SS                                        
         DC    F'20845'            GS                                           
         DC    F'78075'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'80160'            PP,CAR,SD                                    
         DC    F'58540'            SOC,GD                                       
         DC    F'37745'            VO,SS                                        
         DC    F'21500'            GS                                           
         DC    F'80160'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58540'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'82250'            PP,CAR,SD                                    
         DC    F'59840'            SOC,GD                                       
         DC    F'39085'            VO,SS                                        
         DC    F'22195'            GS                                           
         DC    F'82250'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59840'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'84390'            PP,CAR,SD                                    
         DC    F'61150'            SOC,GD                                       
         DC    F'40370'            VO,SS                                        
         DC    F'22890'            GS                                           
         DC    F'84390'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'86495'            PP,CAR,SD                                    
         DC    F'62470'            SOC,GD                                       
         DC    F'41775'            VO,SS                                        
         DC    F'23610'            GS                                           
         DC    F'86495'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62470'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'88510'            PP,CAR,SD                                    
         DC    F'63740'            SOC,GD                                       
         DC    F'43040'            VO,SS                                        
         DC    F'24260'            GS                                           
         DC    F'88510'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63740'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'90655'            PP,CAR,SD                                    
         DC    F'65030'            SOC,GD                                       
         DC    F'44375'            VO,SS                                        
         DC    F'24795'            GS                                           
         DC    F'90655'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65030'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'92710'            PP,CAR,SD                                    
         DC    F'66320'            SOC,GD                                       
         DC    F'45700'            VO,SS                                        
         DC    F'25340'            GS                                           
         DC    F'92710'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66320'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'94780'            PP,CAR,SD                                    
         DC    F'67580'            SOC,GD                                       
         DC    F'47025'            VO,SS                                        
         DC    F'25885'            GS                                           
         DC    F'94780'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67580'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'96915'            PP,CAR,SD                                    
         DC    F'68925'            SOC,GD                                       
         DC    F'48410'            VO,SS                                        
         DC    F'26440'            GS                                           
         DC    F'96915'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68925'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'98995'            PP,CAR,SD                                    
         DC    F'70215'            SOC,GD                                       
         DC    F'49680'            VO,SS                                        
         DC    F'26945'            GS                                           
         DC    F'98995'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70215'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'101070'           PP,CAR,SD                                    
         DC    F'71445'            SOC,GD                                       
         DC    F'50960'            VO,SS                                        
         DC    F'27460'            GS                                           
         DC    F'101070'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71445'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'103130'           PP,CAR,SD                                    
         DC    F'72765'            SOC,GD                                       
         DC    F'52270'            VO,SS                                        
         DC    F'28035'            GS                                           
         DC    F'103130'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72765'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'105260'           PP,CAR,SD                                    
         DC    F'74065'            SOC,GD                                       
         DC    F'53550'            VO,SS                                        
         DC    F'28545'            GS                                           
         DC    F'105260'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74065'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'107285'           PP,CAR,SD                                    
         DC    F'75370'            SOC,GD                                       
         DC    F'54855'            VO,SS                                        
         DC    F'29035'            GS                                           
         DC    F'107285'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75370'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'109395'           PP,CAR,SD                                    
         DC    F'76610'            SOC,GD                                       
         DC    F'56185'            VO,SS                                        
         DC    F'29775'            GS                                           
         DC    F'109395'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76610'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'111525'           PP,CAR,SD                                    
         DC    F'77730'            SOC,GD                                       
         DC    F'57375'            VO,SS                                        
         DC    F'30065'            GS                                           
         DC    F'111525'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77730'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'113605'           PP,CAR,SD                                    
         DC    F'78825'            SOC,GD                                       
         DC    F'58680'            VO,SS                                        
         DC    F'30520'            GS                                           
         DC    F'113605'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78825'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'115685'           PP,CAR,SD                                    
         DC    F'79925'            SOC,GD                                       
         DC    F'60015'            VO,SS                                        
         DC    F'30950'            GS                                           
         DC    F'115685'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79925'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'117730'           PP,CAR,SD                                    
         DC    F'81050'            SOC,GD                                       
         DC    F'61270'            VO,SS                                        
         DC    F'31435'            GS                                           
         DC    F'117730'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'81050'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'119870'           PP,CAR,SD                                    
         DC    F'82130'            SOC,GD                                       
         DC    F'62555'            VO,SS                                        
         DC    F'31910'            GS                                           
         DC    F'119870'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'82130'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'121915'           PP,CAR,SD                                    
         DC    F'83175'            SOC,GD                                       
         DC    F'63380'            VO,SS                                        
         DC    F'32375'            GS                                           
         DC    F'121915'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'83175'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'123995'           PP,CAR,SD                                    
         DC    F'84295'            SOC,GD                                       
         DC    F'64215'            VO,SS                                        
         DC    F'32840'            GS                                           
         DC    F'123995'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'84295'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'125970'           PP,CAR,SD                                    
         DC    F'85375'            SOC,GD                                       
         DC    F'64990'            VO,SS                                        
         DC    F'33285'            GS                                           
         DC    F'125970'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'85375'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'128065'           PP,CAR,SD                                    
         DC    F'86520'            SOC,GD                                       
         DC    F'65795'            VO,SS                                        
         DC    F'33705'            GS                                           
         DC    F'128065'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'86520'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'130105'           PP,CAR,SD                                    
         DC    F'87575'            SOC,GD                                       
         DC    F'66600'            VO,SS                                        
         DC    F'34210'            GS                                           
         DC    F'130105'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87575'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'131760'           PP,CAR,SD                                    
         DC    F'88660'            SOC,GD                                       
         DC    F'67365'            VO,SS                                        
         DC    F'34685'            GS                                           
         DC    F'131760'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'88660'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'133290'           PP,CAR,SD                                    
         DC    F'89665'            SOC,GD                                       
         DC    F'68130'            VO,SS                                        
         DC    F'35070'            GS                                           
         DC    F'133290'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'89665'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'134850'           PP,CAR,SD                                    
         DC    F'90730'            SOC,GD                                       
         DC    F'68960'            VO,SS                                        
         DC    F'35500'            GS                                           
         DC    F'134850'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90730'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'136495'           PP,CAR,SD                                    
         DC    F'91450'            SOC,GD                                       
         DC    F'69730'            VO,SS                                        
         DC    F'35910'            GS                                           
         DC    F'136495'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'91450'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'138060'           PP,CAR,SD                                    
         DC    F'92825'            SOC,GD                                       
         DC    F'70580'            VO,SS                                        
         DC    F'36345'            GS                                           
         DC    F'138060'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'92825'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'139645'           PP,CAR,SD                                    
         DC    F'93670'            SOC,GD                                       
         DC    F'71100'            VO,SS                                        
         DC    F'36810'            GS                                           
         DC    F'139645'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93670'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'141225'           PP,CAR,SD                                    
         DC    F'94475'            SOC,GD                                       
         DC    F'71630'            VO,SS                                        
         DC    F'37245'            GS                                           
         DC    F'141225'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'94475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'142805'           PP,CAR,SD                                    
         DC    F'95250'            SOC,GD                                       
         DC    F'72140'            VO,SS                                        
         DC    F'37680'            GS                                           
         DC    F'142805'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'95250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'144435'           PP,CAR,SD                                    
         DC    F'96025'            SOC,GD                                       
         DC    F'72665'            VO,SS                                        
         DC    F'38090'            GS                                           
         DC    F'144435'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96025'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'145970'           PP,CAR,SD                                    
         DC    F'96845'            SOC,GD                                       
         DC    F'73225'            VO,SS                                        
         DC    F'38510'            GS                                           
         DC    F'145970'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96845'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'147065'           PP,CAR,SD                                    
         DC    F'97590'            SOC,GD                                       
         DC    F'73740'            VO,SS                                        
         DC    F'38825'            GS                                           
         DC    F'147065'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'97590'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'148085'           PP,CAR,SD                                    
         DC    F'98350'            SOC,GD                                       
         DC    F'74265'            VO,SS                                        
         DC    F'39240'            GS                                           
         DC    F'148085'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'98350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'149145'           PP,CAR,SD                                    
         DC    F'99180'            SOC,GD                                       
         DC    F'74765'            VO,SS                                        
         DC    F'39615'            GS                                           
         DC    F'149145'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'99180'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'150265'           PP,CAR,SD                                    
         DC    F'99930'            SOC,GD                                       
         DC    F'75310'            VO,SS                                        
         DC    F'39960'            GS                                           
         DC    F'150265'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'99930'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'151280'           PP,CAR,SD                                    
         DC    F'100630'           SOC,GD                                       
         DC    F'75870'            VO,SS                                        
         DC    F'40355'            GS                                           
         DC    F'151280'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'100630'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'152400'           PP,CAR,SD                                    
         DC    F'101450'           SOC,GD                                       
         DC    F'76320'            VO,SS                                        
         DC    F'40680'            GS                                           
         DC    F'152400'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'101450'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'153410'           PP,CAR,SD                                    
         DC    F'102155'           SOC,GD                                       
         DC    F'76860'            VO,SS                                        
         DC    F'40960'            GS                                           
         DC    F'153410'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'102155'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'154440'           PP,CAR,SD                                    
         DC    F'102875'           SOC,GD                                       
         DC    F'77345'            VO,SS                                        
         DC    F'41355'            GS                                           
         DC    F'154440'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'102875'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'155530'           PP,CAR,SD                                    
         DC    F'103705'           SOC,GD                                       
         DC    F'77840'            VO,SS                                        
         DC    F'41615'            GS                                           
         DC    F'155530'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'103705'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'156585'           PP,CAR,SD                                    
         DC    F'104445'           SOC,GD                                       
         DC    F'78385'            VO,SS                                        
         DC    F'42000'            GS                                           
         DC    F'156585'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'104445'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'203'              PP,CAR,SD                                    
         DC    F'156'              SOC,GD                                       
         DC    F'102'              VO,SS                                        
         DC    F'55'               GS                                           
         DC    F'203'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'156'              ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  906:FEE DETAINED IN AUDITION                 
CAUTTBL  DC    AL1(68,44,0,0)      AUDITION INTERVIEW - TV                      
         DC    F'7450'             PP,CAR,SD                                    
         DC    F'7450'             SOC,GD                                       
         DC    F'7450'             VO,SS                                        
         DC    F'7450'             GS                                           
         DC    F'7450'             SA                                           
         DC    F'7450'             DEM                                          
         DC    F'7450'             E                                            
         DC    F'7450'             GE                                           
         DC    F'7450'             ST                                           
         DC    F'7450'             EVERYBODY ELSE-US,SI,SB,PT                   
         SPACE 1                                                                
CAURTBL  DC    AL1(69,12,0,0)      AUDITION INTERVIEW - RADIO                   
         DC    F'7450'             SS,SV                                        
         DC    F'7450'             MV,GS                                        
         EJECT                                                                  
*                                  1203.C:DEMO AND TEST COMMERCIAL              
CDMTBL   DC    AL1(70,44,2,2)      TV PRESENTATION DEMO 2                       
         DC    F'27400'            PP,CAR,SD                                    
         DC    F'27400'            SOC,GD                                       
         DC    F'27400'            VO,SS                                        
         DC    F'27400'            GS                                           
         DC    F'27400'            SA                                           
         DC    F'27400'            DEM                                          
         DC    F'27400'            E                                            
         DC    F'27400'            GE                                           
         DC    F'27400'            ST                                           
         DC    F'27400'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'13800'            PP,CAR,SD                                    
         DC    F'13800'            SOC,GD                                       
         DC    F'13800'            VO,SS                                        
         DC    F'13800'            GS                                           
         DC    F'13800'            SA                                           
         DC    F'13800'            DEM                                          
         DC    F'13800'            E                                            
         DC    F'13800'            GE                                           
         DC    F'13800'            ST                                           
         DC    F'13800'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
*                                  1202:SESSION FEE                             
BSCHTBL  DC    AL1(80,40,0,0)      NATIONAL TV  - HOURLY RATE                   
         DC    F'8950'             PP,SD,CAR                                    
         DC    F'8950'             SOC,GD                                       
         DC    F'7450'             VO,SS                                        
         DC    F'7450'             GS                                           
         DC    F'13450'            SA = PP+50%                                  
         DC    F'8950'             DEM                                          
         DC    F'5550'             E                                            
         DC    F'3600'             GE                                           
         DC    F'8950'             ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(81,40,0,0)      NATIONAL TV  - OVERTIME HOURLY RATE          
         DC    F'11650'            PP,SD,CAR                                    
         DC    F'11650'            SOC,GD                                       
         DC    F'9350'             VO,SS                                        
         DC    F'9350'             GS                                           
         DC    F'17500'            SA = PP+50%                                  
         DC    F'11650'            DEM                                          
         DC    F'6650'             E                                            
         DC    F'4700'             GE                                           
         DC    F'11650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(82,40,0,0)      NATIONAL TV  - DBLTIME HOURLY RATE           
         DC    F'13100'            PP,SD,CAR                                    
         DC    F'13100'            SOC,GD                                       
         DC    F'11650'            VO,SS                                        
         DC    F'11650'            GS                                           
         DC    F'19650'            SA = PP+50%                                  
         DC    F'13100'            DEM                                          
         DC    F'8300'             E                                            
         DC    F'5400'             GE                                           
         DC    F'13100'            ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  1820:NEW MEDIA                               
CNMTBL   DC    AL1(100,40,0,0)     NEW MEDIA VIDEO 4 WEEKS - 35%                
         DC    F'12350'            PP,SD,CAR                                    
         DC    F'12350'            SOC,GD                                       
         DC    F'9025'             VO,SS                                        
         DC    F'3900'             GS                                           
         DC    F'18525'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'12350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(101,12,0,0)     NEW MEDIA AUDIO 4 WEEKS - 35%                
         DC    F'9625'             SS,SV                                        
         DC    F'7225'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(110,40,0,0)     NEW MEDIA VIDEO 8 WEEKS - 50%                
         DC    F'17650'            PP,SD,CAR                                    
         DC    F'17650'            SOC,GD                                       
         DC    F'12875'            VO,SS                                        
         DC    F'5575'             GS                                           
         DC    F'26475'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'17650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(111,12,0,0)     NEW MEDIA AUDIO 8 WEEKS - 50%                
         DC    F'13750'            SS,SV                                        
         DC    F'10325'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(120,40,0,0)     NEW MEDIA VIDEO 26 WEEKS - 75%               
         DC    F'26475'            PP,SD,CAR                                    
         DC    F'26475'            SOC,GD                                       
         DC    F'19325'            VO,SS                                        
         DC    F'8375'             GS                                           
         DC    F'39725'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'26475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(121,12,0,0)     NEW MEDIA AUDIO 26 WEEKS - 75%               
         DC    F'20625'            SS,SV                                        
         DC    F'15475'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(130,40,0,0)     NEW MEDIA VIDEO 1 YEAR - 100%                
         DC    F'35300'            PP,SD,CAR                                    
         DC    F'35300'            SOC,GD                                       
         DC    F'25750'            VO,SS                                        
         DC    F'11150'            GS                                           
         DC    F'52950'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'35300'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(131,12,0,0)     NEW MEDIA AUDIO 1 YEAR - 100%                
         DC    F'27500'            SS,SV                                        
         DC    F'20625'            MV,GS                                        
         SPACE 1                                                                
CNMATBL  DC    AL1(140,12,0,0)     NEW MEDIA AUDIO 4 WEEKS ADDTL CUTS           
         DC    F'4825'             SS,SV                                        
         DC    F'3625'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(141,12,0,0)     NEW MEDIA AUDIO 8 WEEKS ADDTL CUTS           
         DC    F'6875'             SS,SV                                        
         DC    F'5175'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(142,12,0,0)     NEW MEDIA AUDIO 26 WEEKS ADDTL CUTS          
         DC    F'10325'            SS,SV                                        
         DC    F'7750'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(143,12,0,0)     NEW MEDIA AUDIO 1 YEAR ADDTL CUTS            
         DC    F'13750'            SS,SV                                        
         DC    F'10325'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(144,12,0,0)     BSC NEW MEDIA AUDIO ADDTL CUTS               
         DC    F'13750'            SS,SV                                        
         DC    F'10325'            MV,GS                                        
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
**PAN#1  DC    CL21'008TAGEN73   06/26/12'                                      
         END                                                                    
