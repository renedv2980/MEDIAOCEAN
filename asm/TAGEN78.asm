*          DATA SET TAGEN78    AT LEVEL 035 AS OF 10/09/15                      
*PHASE T70278C,*                                                                
         TITLE 'T70278 - TABLES FOR YEAR 3 ACTRA 2016-2017'                     
T70278   CSECT                                                                  
         DC    AL4(USETBLS-T70278)                                              
         DC    AL4(USELUT-T70278)                                               
         DC    AL4(MAJLUT-T70278)                                               
         DC    AL4(AFMCOLS-T70278)                                              
         DC    AL4(RADCOLS-T70278)                                              
         DC    AL4(OFFCOLS-T70278)                                              
         DC    AL4(ONCOLS-T70278)                                               
         DC    AL4(MSWEET-T70278)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
*                                  1202:SESSION FEE                             
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'78700'            PP,SD,CAR                                    
         DC    F'78700'            SOC,GD                                       
         DC    F'57400'            VO,SS                                        
         DC    F'24850'            GS                                           
         DC    F'118050'           SA = PP+50%                                  
         DC    F'78700'            DEM                                          
         DC    F'48200'            E                                            
         DC    F'32150'            GE                                           
         DC    F'78700'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'78700'            PP,SD,CAR                                    
         DC    F'78700'            SOC,GD                                       
         DC    F'57400'            VO,SS                                        
         DC    F'24850'            GS                                           
         DC    F'118050'           SA = PP+50%                                  
         DC    F'78700'            DEM                                          
         DC    F'48200'            E                                            
         DC    F'32150'            GE                                           
         DC    F'78700'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'78700'            PP,SD,CAR                                    
         DC    F'78700'            SOC,GD                                       
         DC    F'57400'            VO,SS                                        
         DC    F'24850'            GS                                           
         DC    F'118050'           SA = PP+50%                                  
         DC    F'78700'            DEM                                          
         DC    F'48200'            E                                            
         DC    F'32150'            GE                                           
         DC    F'78700'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'78700'            PP,SD,CAR                                    
         DC    F'78700'            SOC,GD                                       
         DC    F'57400'            VO,SS                                        
         DC    F'24850'            GS                                           
         DC    F'118050'           SA = PP+50%                                  
         DC    F'78700'            DEM                                          
         DC    F'48200'            E                                            
         DC    F'32150'            GE                                           
         DC    F'78700'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'78700'            PP,SD,CAR                                    
         DC    F'78700'            SOC,GD                                       
         DC    F'57400'            VO,SS                                        
         DC    F'24850'            GS                                           
         DC    F'118050'           SA = PP+50%                                  
         DC    F'78700'            DEM                                          
         DC    F'48200'            E                                            
         DC    F'32150'            GE                                           
         DC    F'78700'            ST, SAME AS SOC                              
         SPACE 1                                                                
*--------------------------------------------------------------------*          
         DC    AL1(8,40,0,0)       1820:SESSION FEE NEW MEDIA VIDEO             
         DC    F'78700'            PP,SD,CAR                                    
         DC    F'78700'            SOC,GD                                       
         DC    F'57400'            VO,SS                                        
         DC    F'24850'            GROUP SINGER                                 
         DC    F'118050'           SA = PP+50%                                  
         DC    F'78700'                                                         
         DC    F'48200'            BACKGROUND                                   
         DC    F'32150'            GROUP BACKGROUND                             
         DC    F'78700'            ST, SAME AS SOC                              
         EJECT                                                                  
DORTTBL  DC    AL1(12,40,0,0)      NATIONAL TV                                  
         DC    F'78700'            PP,SD,CAR                                    
         DC    F'78700'            SOC,GD                                       
         DC    F'57400'            VO,SS                                        
         DC    F'24850'            GS                                           
         DC    F'118050'           SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78700'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'78700'                                                         
         DC    F'78700'                                                         
         DC    F'57400'                                                         
         DC    F'24850'                                                         
         DC    F'118050'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78700'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'78700'                                                         
         DC    F'78700'                                                         
         DC    F'57400'                                                         
         DC    F'24850'                                                         
         DC    F'118050'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78700'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'78700'                                                         
         DC    F'78700'                                                         
         DC    F'57400'                                                         
         DC    F'24850'                                                         
         DC    F'118050'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78700'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'78700'                                                         
         DC    F'78700'                                                         
         DC    F'57400'                                                         
         DC    F'24850'                                                         
         DC    F'118050'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78700'            ST SAME AS SOC                               
         SPACE 1                                                                
*                                  2101:SESSION AND RESIDUAL FEES               
BSCRTBL  DC    AL1(24,12,0,0)      NATIONAL RADIO                               
         DC    F'61275'            SV,SS                                        
         DC    F'45975'            MV,GS                                        
         SPACE 1                                                                
*                                  406:RADIO SESSION AND RESIDUAL FEES          
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'31275'                                                         
         DC    F'23500'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'41300'                                                         
         DC    F'30900'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'46875'                                                         
         DC    F'35275'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'53125'                                                         
         DC    F'39900'                                                         
         SPACE 1                                                                
*--------------------------------------------------------------------*          
         DC    AL1(33,12,0,0)      1820:SESSION FEE NEW MEDIA AUDIO             
         DC    F'61275'            SV,SS                                        
         DC    F'45975'            MV,GS                                        
         EJECT                                                                  
*                                  1804:TABLE A                                 
WSCTBL   DC    AL1(48,40,1,5)      TV WILDSPOT UNITS 1-5                        
         DC    F'62250'            PP,CAR,SD                                    
         DC    F'45625'            SOC,GD                                       
         DC    F'30465'            VO,SS                                        
         DC    F'18250'            GS                                           
         DC    F'62250'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45625'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'63885'            PP,CAR,SD                                    
         DC    F'46660'            SOC,GD                                       
         DC    F'31590'            VO,SS                                        
         DC    F'18805'            GS                                           
         DC    F'63885'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46660'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'65560'            PP,CAR,SD                                    
         DC    F'47730'            SOC,GD                                       
         DC    F'32655'            VO,SS                                        
         DC    F'19455'            GS                                           
         DC    F'65560'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47730'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'67240'            PP,CAR,SD                                    
         DC    F'48775'            SOC,GD                                       
         DC    F'33745'            VO,SS                                        
         DC    F'20075'            GS                                           
         DC    F'67240'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48775'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'68880'            PP,CAR,SD                                    
         DC    F'49795'            SOC,GD                                       
         DC    F'34795'            VO,SS                                        
         DC    F'20705'            GS                                           
         DC    F'68880'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49795'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'70545'            PP,CAR,SD                                    
         DC    F'50835'            SOC,GD                                       
         DC    F'35885'            VO,SS                                        
         DC    F'21310'            GS                                           
         DC    F'70545'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50835'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'72210'            PP,CAR,SD                                    
         DC    F'51830'            SOC,GD                                       
         DC    F'36945'            VO,SS                                        
         DC    F'21720'            GS                                           
         DC    F'72210'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51830'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'73845'            PP,CAR,SD                                    
         DC    F'52885'            SOC,GD                                       
         DC    F'38045'            VO,SS                                        
         DC    F'22220'            GS                                           
         DC    F'73845'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52885'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'75565'            PP,CAR,SD                                    
         DC    F'53975'            SOC,GD                                       
         DC    F'39220'            VO,SS                                        
         DC    F'22615'            GS                                           
         DC    F'75565'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53975'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'77750'            PP,CAR,SD                                    
         DC    F'54905'            SOC,GD                                       
         DC    F'40270'            VO,SS                                        
         DC    F'23090'            GS                                           
         DC    F'77750'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54905'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'78845'            PP,CAR,SD                                    
         DC    F'56005'            SOC,GD                                       
         DC    F'41315'            VO,SS                                        
         DC    F'23550'            GS                                           
         DC    F'78845'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56005'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'80495'            PP,CAR,SD                                    
         DC    F'57040'            SOC,GD                                       
         DC    F'42435'            VO,SS                                        
         DC    F'23920'            GS                                           
         DC    F'80495'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57040'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'82930'            PP,CAR,SD                                    
         DC    F'58060'            SOC,GD                                       
         DC    F'43500'            VO,SS                                        
         DC    F'24445'            GS                                           
         DC    F'82930'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'84555'            PP,CAR,SD                                    
         DC    F'59090'            SOC,GD                                       
         DC    F'44560'            VO,SS                                        
         DC    F'24880'            GS                                           
         DC    F'84555'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59090'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'85415'            PP,CAR,SD                                    
         DC    F'60135'            SOC,GD                                       
         DC    F'45670'            VO,SS                                        
         DC    F'25295'            GS                                           
         DC    F'85415'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60135'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'87155'            PP,CAR,SD                                    
         DC    F'61165'            SOC,GD                                       
         DC    F'46745'            VO,SS                                        
         DC    F'25725'            GS                                           
         DC    F'87155'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61165'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'88815'            PP,CAR,SD                                    
         DC    F'61990'            SOC,GD                                       
         DC    F'47865'            VO,SS                                        
         DC    F'26210'            GS                                           
         DC    F'88815'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61990'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'90480'            PP,CAR,SD                                    
         DC    F'62880'            SOC,GD                                       
         DC    F'48980'            VO,SS                                        
         DC    F'26625'            GS                                           
         DC    F'90480'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62880'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'92165'            PP,CAR,SD                                    
         DC    F'63725'            SOC,GD                                       
         DC    F'50035'            VO,SS                                        
         DC    F'27090'            GS                                           
         DC    F'92165'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63725'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'93795'            PP,CAR,SD                                    
         DC    F'64460'            SOC,GD                                       
         DC    F'51090'            VO,SS                                        
         DC    F'27595'            GS                                           
         DC    F'93795'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64460'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'95430'            PP,CAR,SD                                    
         DC    F'65410'            SOC,GD                                       
         DC    F'52260'            VO,SS                                        
         DC    F'27905'            GS                                           
         DC    F'95430'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65410'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'97090'            PP,CAR,SD                                    
         DC    F'66315'            SOC,GD                                       
         DC    F'52865'            VO,SS                                        
         DC    F'28455'            GS                                           
         DC    F'97090'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66315'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'98800'            PP,CAR,SD                                    
         DC    F'67130'            SOC,GD                                       
         DC    F'53530'            VO,SS                                        
         DC    F'28765'            GS                                           
         DC    F'98800'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67130'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'100430'           PP,CAR,SD                                    
         DC    F'67935'            SOC,GD                                       
         DC    F'54175'            VO,SS                                        
         DC    F'29150'            GS                                           
         DC    F'100430'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67935'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'102120'           PP,CAR,SD                                    
         DC    F'68850'            SOC,GD                                       
         DC    F'54835'            VO,SS                                        
         DC    F'29535'            GS                                           
         DC    F'102120'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68850'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'103810'           PP,CAR,SD                                    
         DC    F'69660'            SOC,GD                                       
         DC    F'55500'            VO,SS                                        
         DC    F'29935'            GS                                           
         DC    F'103810'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69660'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'105060'           PP,CAR,SD                                    
         DC    F'70545'            SOC,GD                                       
         DC    F'56120'            VO,SS                                        
         DC    F'30295'            GS                                           
         DC    F'105060'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70545'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'106225'           PP,CAR,SD                                    
         DC    F'71375'            SOC,GD                                       
         DC    F'56815'            VO,SS                                        
         DC    F'30700'            GS                                           
         DC    F'106225'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71375'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'107535'           PP,CAR,SD                                    
         DC    F'72210'            SOC,GD                                       
         DC    F'57470'            VO,SS                                        
         DC    F'31055'            GS                                           
         DC    F'107535'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72210'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'108805'           PP,CAR,SD                                    
         DC    F'73070'            SOC,GD                                       
         DC    F'58120'            VO,SS                                        
         DC    F'31515'            GS                                           
         DC    F'108805'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73070'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'110080'           PP,CAR,SD                                    
         DC    F'73850'            SOC,GD                                       
         DC    F'58770'            VO,SS                                        
         DC    F'31880'            GS                                           
         DC    F'110080'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73850'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'111335'           PP,CAR,SD                                    
         DC    F'74500'            SOC,GD                                       
         DC    F'59260'            VO,SS                                        
         DC    F'32225'            GS                                           
         DC    F'111335'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'112555'           PP,CAR,SD                                    
         DC    F'75115'            SOC,GD                                       
         DC    F'59705'            VO,SS                                        
         DC    F'32630'            GS                                           
         DC    F'112555'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75115'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'113845'           PP,CAR,SD                                    
         DC    F'75835'            SOC,GD                                       
         DC    F'60245'            VO,SS                                        
         DC    F'33005'            GS                                           
         DC    F'113845'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75835'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'115065'           PP,CAR,SD                                    
         DC    F'76395'            SOC,GD                                       
         DC    F'60635'            VO,SS                                        
         DC    F'33370'            GS                                           
         DC    F'115065'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76395'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'116340'           PP,CAR,SD                                    
         DC    F'77030'            SOC,GD                                       
         DC    F'61140'            VO,SS                                        
         DC    F'33795'            GS                                           
         DC    F'116340'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77030'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'117195'           PP,CAR,SD                                    
         DC    F'77450'            SOC,GD                                       
         DC    F'61565'            VO,SS                                        
         DC    F'34075'            GS                                           
         DC    F'117195'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77450'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'118015'           PP,CAR,SD                                    
         DC    F'78290'            SOC,GD                                       
         DC    F'62055'            VO,SS                                        
         DC    F'34430'            GS                                           
         DC    F'118015'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78290'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'118855'           PP,CAR,SD                                    
         DC    F'78930'            SOC,GD                                       
         DC    F'62495'            VO,SS                                        
         DC    F'34710'            GS                                           
         DC    F'118855'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78930'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'119715'           PP,CAR,SD                                    
         DC    F'79565'            SOC,GD                                       
         DC    F'62925'            VO,SS                                        
         DC    F'35085'            GS                                           
         DC    F'119715'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79565'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'120535'           PP,CAR,SD                                    
         DC    F'80145'            SOC,GD                                       
         DC    F'63425'            VO,SS                                        
         DC    F'35370'            GS                                           
         DC    F'120535'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'80145'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'121430'           PP,CAR,SD                                    
         DC    F'80795'            SOC,GD                                       
         DC    F'63915'            VO,SS                                        
         DC    F'35755'            GS                                           
         DC    F'121430'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'80795'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'122265'           PP,CAR,SD                                    
         DC    F'81430'            SOC,GD                                       
         DC    F'64365'            VO,SS                                        
         DC    F'36070'            GS                                           
         DC    F'122265'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'81430'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'123125'           PP,CAR,SD                                    
         DC    F'82050'            SOC,GD                                       
         DC    F'64835'            VO,SS                                        
         DC    F'36375'            GS                                           
         DC    F'123125'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'82050'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'123965'           PP,CAR,SD                                    
         DC    F'82685'            SOC,GD                                       
         DC    F'65310'            VO,SS                                        
         DC    F'36690'            GS                                           
         DC    F'123965'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'82685'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'124830'           PP,CAR,SD                                    
         DC    F'83300'            SOC,GD                                       
         DC    F'65795'            VO,SS                                        
         DC    F'37015'            GS                                           
         DC    F'124830'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'83300'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'162'              PP,CAR,SD                                    
         DC    F'124'              SOC,GD                                       
         DC    F'93'               VO,SS                                        
         DC    F'50'               GS                                           
         DC    F'162'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'124'              ST, SAME AS SOC                              
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
*                                  1805:TABLE B                                 
         DC    AL1(60,40,1,5)      TV WSP&NET SPC/NETWORK UNITS 1-5             
         DC    F'87045'            PP,CAR,SD                                    
         DC    F'63870'            SOC,GD                                       
         DC    F'40620'            VO,SS                                        
         DC    F'23240'            GS                                           
         DC    F'87045'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63870'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'89380'            PP,CAR,SD                                    
         DC    F'65270'            SOC,GD                                       
         DC    F'42085'            VO,SS                                        
         DC    F'23965'            GS                                           
         DC    F'89380'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65270'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'91710'            PP,CAR,SD                                    
         DC    F'66720'            SOC,GD                                       
         DC    F'43575'            VO,SS                                        
         DC    F'24745'            GS                                           
         DC    F'91710'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66720'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'94090'            PP,CAR,SD                                    
         DC    F'68175'            SOC,GD                                       
         DC    F'45015'            VO,SS                                        
         DC    F'25515'            GS                                           
         DC    F'94090'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68175'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'96430'            PP,CAR,SD                                    
         DC    F'69650'            SOC,GD                                       
         DC    F'46580'            VO,SS                                        
         DC    F'26320'            GS                                           
         DC    F'96430'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'98685'            PP,CAR,SD                                    
         DC    F'71070'            SOC,GD                                       
         DC    F'47990'            VO,SS                                        
         DC    F'27045'            GS                                           
         DC    F'98685'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71070'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'101075'           PP,CAR,SD                                    
         DC    F'72500'            SOC,GD                                       
         DC    F'49470'            VO,SS                                        
         DC    F'27640'            GS                                           
         DC    F'101075'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'103365'           PP,CAR,SD                                    
         DC    F'73945'            SOC,GD                                       
         DC    F'50950'            VO,SS                                        
         DC    F'28255'            GS                                           
         DC    F'103365'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73945'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'105670'           PP,CAR,SD                                    
         DC    F'75345'            SOC,GD                                       
         DC    F'52435'            VO,SS                                        
         DC    F'28860'            GS                                           
         DC    F'105670'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75345'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'108055'           PP,CAR,SD                                    
         DC    F'76845'            SOC,GD                                       
         DC    F'53975'            VO,SS                                        
         DC    F'29475'            GS                                           
         DC    F'108055'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76845'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'110370'           PP,CAR,SD                                    
         DC    F'78285'            SOC,GD                                       
         DC    F'55390'            VO,SS                                        
         DC    F'30040'            GS                                           
         DC    F'110370'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'112685'           PP,CAR,SD                                    
         DC    F'79650'            SOC,GD                                       
         DC    F'56815'            VO,SS                                        
         DC    F'30615'            GS                                           
         DC    F'112685'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'114985'           PP,CAR,SD                                    
         DC    F'81130'            SOC,GD                                       
         DC    F'58280'            VO,SS                                        
         DC    F'31260'            GS                                           
         DC    F'114985'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'81130'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'117350'           PP,CAR,SD                                    
         DC    F'82575'            SOC,GD                                       
         DC    F'59705'            VO,SS                                        
         DC    F'31825'            GS                                           
         DC    F'117350'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'82575'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'119615'           PP,CAR,SD                                    
         DC    F'84035'            SOC,GD                                       
         DC    F'61160'            VO,SS                                        
         DC    F'32370'            GS                                           
         DC    F'119615'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'84035'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'121970'           PP,CAR,SD                                    
         DC    F'85415'            SOC,GD                                       
         DC    F'62645'            VO,SS                                        
         DC    F'33200'            GS                                           
         DC    F'121970'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'85415'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'124350'           PP,CAR,SD                                    
         DC    F'86665'            SOC,GD                                       
         DC    F'63970'            VO,SS                                        
         DC    F'33515'            GS                                           
         DC    F'124350'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'86665'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'126705'           PP,CAR,SD                                    
         DC    F'87885'            SOC,GD                                       
         DC    F'65430'            VO,SS                                        
         DC    F'34025'            GS                                           
         DC    F'126705'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87885'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'128980'           PP,CAR,SD                                    
         DC    F'89115'            SOC,GD                                       
         DC    F'66910'            VO,SS                                        
         DC    F'34510'            GS                                           
         DC    F'128980'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'89115'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'131265'           PP,CAR,SD                                    
         DC    F'90360'            SOC,GD                                       
         DC    F'68310'            VO,SS                                        
         DC    F'35045'            GS                                           
         DC    F'131265'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90360'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'133645'           PP,CAR,SD                                    
         DC    F'91570'            SOC,GD                                       
         DC    F'69750'            VO,SS                                        
         DC    F'35585'            GS                                           
         DC    F'133645'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'91570'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'135930'           PP,CAR,SD                                    
         DC    F'92740'            SOC,GD                                       
         DC    F'70665'            VO,SS                                        
         DC    F'36100'            GS                                           
         DC    F'135930'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'92740'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'138245'           PP,CAR,SD                                    
         DC    F'93980'            SOC,GD                                       
         DC    F'71595'            VO,SS                                        
         DC    F'36615'            GS                                           
         DC    F'138245'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93980'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'140450'           PP,CAR,SD                                    
         DC    F'95190'            SOC,GD                                       
         DC    F'72460'            VO,SS                                        
         DC    F'37115'            GS                                           
         DC    F'140450'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'95190'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'142780'           PP,CAR,SD                                    
         DC    F'96465'            SOC,GD                                       
         DC    F'73360'            VO,SS                                        
         DC    F'37585'            GS                                           
         DC    F'142780'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96465'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'145065'           PP,CAR,SD                                    
         DC    F'97640'            SOC,GD                                       
         DC    F'74250'            VO,SS                                        
         DC    F'38145'            GS                                           
         DC    F'145065'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'97640'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'146905'           PP,CAR,SD                                    
         DC    F'98850'            SOC,GD                                       
         DC    F'75110'            VO,SS                                        
         DC    F'38675'            GS                                           
         DC    F'146905'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'98850'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'148605'           PP,CAR,SD                                    
         DC    F'99970'            SOC,GD                                       
         DC    F'75960'            VO,SS                                        
         DC    F'39095'            GS                                           
         DC    F'148605'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'99970'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'150350'           PP,CAR,SD                                    
         DC    F'101160'           SOC,GD                                       
         DC    F'76890'            VO,SS                                        
         DC    F'39575'            GS                                           
         DC    F'150350'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'101160'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'152185'           PP,CAR,SD                                    
         DC    F'101960'           SOC,GD                                       
         DC    F'77745'            VO,SS                                        
         DC    F'40040'            GS                                           
         DC    F'152185'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'101960'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'153930'           PP,CAR,SD                                    
         DC    F'103495'           SOC,GD                                       
         DC    F'78700'            VO,SS                                        
         DC    F'40525'            GS                                           
         DC    F'153930'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'103495'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'155700'           PP,CAR,SD                                    
         DC    F'104440'           SOC,GD                                       
         DC    F'79270'            VO,SS                                        
         DC    F'41045'            GS                                           
         DC    F'155700'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'104440'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'157450'           PP,CAR,SD                                    
         DC    F'105330'           SOC,GD                                       
         DC    F'79860'            VO,SS                                        
         DC    F'41530'            GS                                           
         DC    F'157450'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'105330'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'159215'           PP,CAR,SD                                    
         DC    F'106190'           SOC,GD                                       
         DC    F'80430'            VO,SS                                        
         DC    F'42010'            GS                                           
         DC    F'159215'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'106190'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'161035'           PP,CAR,SD                                    
         DC    F'107065'           SOC,GD                                       
         DC    F'81010'            VO,SS                                        
         DC    F'42465'            GS                                           
         DC    F'161035'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'107065'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'162745'           PP,CAR,SD                                    
         DC    F'107970'           SOC,GD                                       
         DC    F'81640'            VO,SS                                        
         DC    F'42935'            GS                                           
         DC    F'162745'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'107970'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'163965'           PP,CAR,SD                                    
         DC    F'108805'           SOC,GD                                       
         DC    F'82215'            VO,SS                                        
         DC    F'43285'            GS                                           
         DC    F'163965'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'108805'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'165100'           PP,CAR,SD                                    
         DC    F'109655'           SOC,GD                                       
         DC    F'82800'            VO,SS                                        
         DC    F'43750'            GS                                           
         DC    F'165100'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'109655'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'166285'           PP,CAR,SD                                    
         DC    F'110580'           SOC,GD                                       
         DC    F'83355'            VO,SS                                        
         DC    F'44165'            GS                                           
         DC    F'166285'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'110580'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'167530'           PP,CAR,SD                                    
         DC    F'111415'           SOC,GD                                       
         DC    F'83970'            VO,SS                                        
         DC    F'44555'            GS                                           
         DC    F'167530'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'111415'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'168660'           PP,CAR,SD                                    
         DC    F'112195'           SOC,GD                                       
         DC    F'84590'            VO,SS                                        
         DC    F'44995'            GS                                           
         DC    F'168660'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'112195'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'169910'           PP,CAR,SD                                    
         DC    F'113110'           SOC,GD                                       
         DC    F'85095'            VO,SS                                        
         DC    F'45350'            GS                                           
         DC    F'169910'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'113110'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'171045'           PP,CAR,SD                                    
         DC    F'113900'           SOC,GD                                       
         DC    F'85690'            VO,SS                                        
         DC    F'45670'            GS                                           
         DC    F'171045'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'113900'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'172185'           PP,CAR,SD                                    
         DC    F'114695'           SOC,GD                                       
         DC    F'86235'            VO,SS                                        
         DC    F'46110'            GS                                           
         DC    F'172185'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'114695'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'173410'           PP,CAR,SD                                    
         DC    F'115625'           SOC,GD                                       
         DC    F'86785'            VO,SS                                        
         DC    F'46395'            GS                                           
         DC    F'173410'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'115625'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'174580'           PP,CAR,SD                                    
         DC    F'116450'           SOC,GD                                       
         DC    F'87395'            VO,SS                                        
         DC    F'46830'            GS                                           
         DC    F'174580'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'116450'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'225'              PP,CAR,SD                                    
         DC    F'173'              SOC,GD                                       
         DC    F'114'              VO,SS                                        
         DC    F'60'               GS                                           
         DC    F'225'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'173'              ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  906:FEE DETAINED IN AUDITION                 
CAUTTBL  DC    AL1(68,44,0,0)      AUDITION INTERVIEW - TV                      
         DC    F'8300'             PP,CAR,SD                                    
         DC    F'8300'             SOC,GD                                       
         DC    F'8300'             VO,SS                                        
         DC    F'8300'             GS                                           
         DC    F'8300'             SA                                           
         DC    F'8300'             DEM                                          
         DC    F'8300'             E                                            
         DC    F'8300'             GE                                           
         DC    F'8300'             ST                                           
         DC    F'8300'             EVERYBODY ELSE-US,SI,SB,PT                   
         SPACE 1                                                                
CAURTBL  DC    AL1(69,12,0,0)      AUDITION INTERVIEW - RADIO                   
         DC    F'8300'             SS,SV                                        
         DC    F'8300'             MV,GS                                        
         EJECT                                                                  
*                                  1203.C:DEMO AND TEST COMMERCIAL              
CDMTBL   DC    AL1(70,44,2,2)      TV PRESENTATION DEMO 2                       
         DC    F'30600'            PP,CAR,SD                                    
         DC    F'30600'            SOC,GD                                       
         DC    F'30600'            VO,SS                                        
         DC    F'30600'            GS                                           
         DC    F'30600'            SA                                           
         DC    F'30600'            DEM                                          
         DC    F'30600'            E                                            
         DC    F'30600'            GE                                           
         DC    F'30600'            ST                                           
         DC    F'30600'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'15400'            PP,CAR,SD                                    
         DC    F'15400'            SOC,GD                                       
         DC    F'15400'            VO,SS                                        
         DC    F'15400'            GS                                           
         DC    F'15400'            SA                                           
         DC    F'15400'            DEM                                          
         DC    F'15400'            E                                            
         DC    F'15400'            GE                                           
         DC    F'15400'            ST                                           
         DC    F'15400'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
*                                  1202:SESSION FEE                             
BSCHTBL  DC    AL1(80,40,0,0)      NATIONAL TV  - HOURLY RATE                   
         DC    F'10000'            PP,SD,CAR                                    
         DC    F'10000'            SOC,GD                                       
         DC    F'8300'             VO,SS                                        
         DC    F'8300'             GS                                           
         DC    F'15000'            SA = PP+50%                                  
         DC    F'10000'            DEM                                          
         DC    F'6150'             E                                            
         DC    F'4100'             GE                                           
         DC    F'10000'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(81,40,0,0)      NATIONAL TV  - OVERTIME HOURLY RATE          
         DC    F'13000'            PP,SD,CAR                                    
         DC    F'13000'            SOC,GD                                       
         DC    F'10450'            VO,SS                                        
         DC    F'10450'            GS                                           
         DC    F'19500'            SA = PP+50%                                  
         DC    F'13000'            DEM                                          
         DC    F'7400'             E                                            
         DC    F'5250'             GE                                           
         DC    F'13000'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(82,40,0,0)      NATIONAL TV  - DBLTIME HOURLY RATE           
         DC    F'14700'            PP,SD,CAR                                    
         DC    F'14700'            SOC,GD                                       
         DC    F'13000'            VO,SS                                        
         DC    F'13000'            GS                                           
         DC    F'22050'            SA = PP+50%                                  
         DC    F'14700'            DEM                                          
         DC    F'9250'             E                                            
         DC    F'6000'             GE                                           
         DC    F'14700'            ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  1820:NEW MEDIA                               
CNMTBL   DC    AL1(100,40,0,0)     NEW MEDIA VIDEO 4 WEEKS - 35%                
         DC    F'27550'            PP,SD,CAR                                    
         DC    F'27550'            SOC,GD                                       
         DC    F'20100'            VO,SS                                        
         DC    F'8700'             GS                                           
         DC    F'41325'            SA = PP+50%                                  
         DC    F'27550'            DEM                                          
         DC    F'16850'            E                                            
         DC    F'11250'            GE                                           
         DC    F'27550'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(101,12,0,0)     NEW MEDIA AUDIO 4 WEEKS - 35%                
         DC    F'21450'            SS,SV                                        
         DC    F'16100'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(110,40,0,0)     NEW MEDIA VIDEO 8 WEEKS - 50%                
         DC    F'39350'            PP,SD,CAR                                    
         DC    F'39350'            SOC,GD                                       
         DC    F'28700'            VO,SS                                        
         DC    F'12450'            GS                                           
         DC    F'59025'            SA = PP+50%                                  
         DC    F'39350'            DEM                                          
         DC    F'24100'            E                                            
         DC    F'16100'            GE                                           
         DC    F'39350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(111,12,0,0)     NEW MEDIA AUDIO 8 WEEKS - 50%                
         DC    F'30650'            SS,SV                                        
         DC    F'23000'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(120,40,0,0)     NEW MEDIA VIDEO 26 WEEKS - 75%               
         DC    F'59050'            PP,SD,CAR                                    
         DC    F'59050'            SOC,GD                                       
         DC    F'43050'            VO,SS                                        
         DC    F'18650'            GS                                           
         DC    F'88575'            SA = PP+50%                                  
         DC    F'59050'            DEM                                          
         DC    F'36150'            E                                            
         DC    F'24100'            GE                                           
         DC    F'59050'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(121,12,0,0)     NEW MEDIA AUDIO 26 WEEKS - 75%               
         DC    F'46000'            SS,SV                                        
         DC    F'34500'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(130,40,0,0)     NEW MEDIA VIDEO 1 YEAR - 100%                
         DC    F'78700'            PP,SD,CAR                                    
         DC    F'78700'            SOC,GD                                       
         DC    F'57400'            VO,SS                                        
         DC    F'24850'            GS                                           
         DC    F'118050'           SA = PP+50%                                  
         DC    F'78700'            DEM                                          
         DC    F'48200'            E                                            
         DC    F'32150'            GE                                           
         DC    F'78700'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(131,12,0,0)     NEW MEDIA AUDIO 1 YEAR - 100%                
         DC    F'61275'            SS,SV                                        
         DC    F'45975'            MV,GS                                        
         SPACE 1                                                                
CNMATBL  DC    AL1(140,12,0,0)     NEW MEDIA AUDIO 4 WEEKS ADDTL CUTS           
         DC    F'10750'            SS,SV                                        
         DC    F'8050'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(141,12,0,0)     NEW MEDIA AUDIO 8 WEEKS ADDTL CUTS           
         DC    F'15350'            SS,SV                                        
         DC    F'11500'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(142,12,0,0)     NEW MEDIA AUDIO 26 WEEKS ADDTL CUTS          
         DC    F'23000'            SS,SV                                        
         DC    F'17250'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(143,12,0,0)     NEW MEDIA AUDIO 1 YEAR ADDTL CUTS            
         DC    F'30650'            SS,SV                                        
         DC    F'23000'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(144,12,0,0)     BSC NEW MEDIA AUDIO ADDTL CUTS               
         DC    F'30650'            SS,SV                                        
         DC    F'23000'            MV,GS                                        
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
**PAN#1  DC    CL21'035TAGEN78   10/09/15'                                      
         END                                                                    
