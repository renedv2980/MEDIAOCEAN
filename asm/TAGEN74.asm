*          DATA SET TAGEN74    AT LEVEL 008 AS OF 06/26/12                      
*PHASE T70274E,*                                                                
         TITLE 'T70274 - TABLES FOR YEAR 2 ACTRA 2012-2013'                     
T70274   CSECT                                                                  
         DC    AL4(USETBLS-T70274)                                              
         DC    AL4(USELUT-T70274)                                               
         DC    AL4(MAJLUT-T70274)                                               
         DC    AL4(AFMCOLS-T70274)                                              
         DC    AL4(RADCOLS-T70274)                                              
         DC    AL4(OFFCOLS-T70274)                                              
         DC    AL4(ONCOLS-T70274)                                               
         DC    AL4(MSWEET-T70274)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
*                                  1202:SESSION FEE                             
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'72350'            PP,SD,CAR                                    
         DC    F'72350'            SOC,GD                                       
         DC    F'52750'            VO,SS                                        
         DC    F'22850'            GS                                           
         DC    F'108525'           SA = PP+50%                                  
         DC    F'72350'            DEM                                          
         DC    F'44300'            E                                            
         DC    F'29550'            GE                                           
         DC    F'72350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'72350'            PP,SD,CAR                                    
         DC    F'72350'            SOC,GD                                       
         DC    F'52750'            VO,SS                                        
         DC    F'22850'            GS                                           
         DC    F'108525'           SA = PP+50%                                  
         DC    F'72350'            DEM                                          
         DC    F'44300'            E                                            
         DC    F'29550'            GE                                           
         DC    F'72350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'72350'            PP,SD,CAR                                    
         DC    F'72350'            SOC,GD                                       
         DC    F'52750'            VO,SS                                        
         DC    F'22850'            GS                                           
         DC    F'108525'           SA = PP+50%                                  
         DC    F'72350'            DEM                                          
         DC    F'44300'            E                                            
         DC    F'29550'            GE                                           
         DC    F'72350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'72350'            PP,SD,CAR                                    
         DC    F'72350'            SOC,GD                                       
         DC    F'52750'            VO,SS                                        
         DC    F'22850'            GS                                           
         DC    F'108525'           SA = PP+50%                                  
         DC    F'72350'            DEM                                          
         DC    F'44300'            E                                            
         DC    F'29550'            GE                                           
         DC    F'72350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'72350'            PP,SD,CAR                                    
         DC    F'72350'            SOC,GD                                       
         DC    F'52750'            VO,SS                                        
         DC    F'22850'            GS                                           
         DC    F'108525'           SA = PP+50%                                  
         DC    F'72350'            DEM                                          
         DC    F'44300'            E                                            
         DC    F'29550'            GE                                           
         DC    F'72350'            ST, SAME AS SOC                              
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
         DC    F'72350'            PP,SD,CAR                                    
         DC    F'72350'            SOC,GD                                       
         DC    F'52750'            VO,SS                                        
         DC    F'22850'            GS                                           
         DC    F'108525'           SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72350'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'72350'                                                         
         DC    F'72350'                                                         
         DC    F'52750'                                                         
         DC    F'22850'                                                         
         DC    F'108525'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72350'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'72350'                                                         
         DC    F'72350'                                                         
         DC    F'52750'                                                         
         DC    F'22850'                                                         
         DC    F'108525'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72350'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'72350'                                                         
         DC    F'72350'                                                         
         DC    F'52750'                                                         
         DC    F'22850'                                                         
         DC    F'108525'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72350'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'72350'                                                         
         DC    F'72350'                                                         
         DC    F'52750'                                                         
         DC    F'22850'                                                         
         DC    F'108525'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72350'            ST SAME AS SOC                               
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
         DC    F'56350'            SV,SS                                        
         DC    F'42275'            MV,GS                                        
         SPACE 1                                                                
*                                  406:RADIO SESSION AND RESIDUAL FEES          
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'28725'                                                         
         DC    F'21600'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'37975'                                                         
         DC    F'28425'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'43150'                                                         
         DC    F'38425'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'48825'                                                         
         DC    F'36675'                                                         
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
         DC    F'57230'            PP,CAR,SD                                    
         DC    F'41945'            SOC,GD                                       
         DC    F'28010'            VO,SS                                        
         DC    F'16775'            GS                                           
         DC    F'57230'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41945'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,2,2)      TV WILDSPOT UNITS 2                          
         DC    F'57230'            PP,CAR,SD                                    
         DC    F'41945'            SOC,GD                                       
         DC    F'28010'            VO,SS                                        
         DC    F'16775'            GS                                           
         DC    F'57230'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41945'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,3,3)      TV WILDSPOT UNITS 3                          
         DC    F'57230'            PP,CAR,SD                                    
         DC    F'41945'            SOC,GD                                       
         DC    F'28010'            VO,SS                                        
         DC    F'16775'            GS                                           
         DC    F'57230'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41945'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,4,4)      TV WILDSPOT UNITS 4                          
         DC    F'57230'            PP,CAR,SD                                    
         DC    F'41945'            SOC,GD                                       
         DC    F'28010'            VO,SS                                        
         DC    F'16775'            GS                                           
         DC    F'57230'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41945'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,5,5)      TV WILDSPOT UNITS 5                          
         DC    F'57230'            PP,CAR,SD                                    
         DC    F'41945'            SOC,GD                                       
         DC    F'28010'            VO,SS                                        
         DC    F'16775'            GS                                           
         DC    F'57230'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41945'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'58725'            PP,CAR,SD                                    
         DC    F'42900'            SOC,GD                                       
         DC    F'29045'            VO,SS                                        
         DC    F'17290'            GS                                           
         DC    F'58725'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42900'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'60275'            PP,CAR,SD                                    
         DC    F'43880'            SOC,GD                                       
         DC    F'30020'            VO,SS                                        
         DC    F'17890'            GS                                           
         DC    F'60275'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43880'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'61815'            PP,CAR,SD                                    
         DC    F'44840'            SOC,GD                                       
         DC    F'31025'            VO,SS                                        
         DC    F'18455'            GS                                           
         DC    F'61815'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44840'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'63320'            PP,CAR,SD                                    
         DC    F'45780'            SOC,GD                                       
         DC    F'31990'            VO,SS                                        
         DC    F'19035'            GS                                           
         DC    F'63320'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45780'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'64855'            PP,CAR,SD                                    
         DC    F'46735'            SOC,GD                                       
         DC    F'32990'            VO,SS                                        
         DC    F'19590'            GS                                           
         DC    F'64855'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46735'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'63385'            PP,CAR,SD                                    
         DC    F'47655'            SOC,GD                                       
         DC    F'33965'            VO,SS                                        
         DC    F'19965'            GS                                           
         DC    F'63385'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47655'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'67890'            PP,CAR,SD                                    
         DC    F'48625'            SOC,GD                                       
         DC    F'34980'            VO,SS                                        
         DC    F'20430'            GS                                           
         DC    F'67890'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48625'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'69470'            PP,CAR,SD                                    
         DC    F'49620'            SOC,GD                                       
         DC    F'36055'            VO,SS                                        
         DC    F'20790'            GS                                           
         DC    F'69470'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49620'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'71480'            PP,CAR,SD                                    
         DC    F'50480'            SOC,GD                                       
         DC    F'37020'            VO,SS                                        
         DC    F'21225'            GS                                           
         DC    F'71480'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50480'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'72490'            PP,CAR,SD                                    
         DC    F'51490'            SOC,GD                                       
         DC    F'37980'            VO,SS                                        
         DC    F'21650'            GS                                           
         DC    F'72490'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51490'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'74005'            PP,CAR,SD                                    
         DC    F'52440'            SOC,GD                                       
         DC    F'39015'            VO,SS                                        
         DC    F'21990'            GS                                           
         DC    F'74005'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52440'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'76240'            PP,CAR,SD                                    
         DC    F'53375'            SOC,GD                                       
         DC    F'39990'            VO,SS                                        
         DC    F'22475'            GS                                           
         DC    F'76240'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53375'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'77730'            PP,CAR,SD                                    
         DC    F'54320'            SOC,GD                                       
         DC    F'40965'            VO,SS                                        
         DC    F'22870'            GS                                           
         DC    F'77730'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54320'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'78525'            PP,CAR,SD                                    
         DC    F'55285'            SOC,GD                                       
         DC    F'41985'            VO,SS                                        
         DC    F'23260'            GS                                           
         DC    F'78525'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'80120'            PP,CAR,SD                                    
         DC    F'56230'            SOC,GD                                       
         DC    F'42975'            VO,SS                                        
         DC    F'23650'            GS                                           
         DC    F'80120'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56230'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'81655'            PP,CAR,SD                                    
         DC    F'56990'            SOC,GD                                       
         DC    F'44005'            VO,SS                                        
         DC    F'24095'            GS                                           
         DC    F'81655'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56990'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'83180'            PP,CAR,SD                                    
         DC    F'57805'            SOC,GD                                       
         DC    F'45030'            VO,SS                                        
         DC    F'24485'            GS                                           
         DC    F'83180'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57805'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'84735'            PP,CAR,SD                                    
         DC    F'58585'            SOC,GD                                       
         DC    F'46000'            VO,SS                                        
         DC    F'24905'            GS                                           
         DC    F'84735'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58585'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'86225'            PP,CAR,SD                                    
         DC    F'59260'            SOC,GD                                       
         DC    F'46970'            VO,SS                                        
         DC    F'25370'            GS                                           
         DC    F'86225'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59260'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'87730'            PP,CAR,SD                                    
         DC    F'60130'            SOC,GD                                       
         DC    F'48045'            VO,SS                                        
         DC    F'25660'            GS                                           
         DC    F'87730'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60130'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'89260'            PP,CAR,SD                                    
         DC    F'60965'            SOC,GD                                       
         DC    F'48605'            VO,SS                                        
         DC    F'26160'            GS                                           
         DC    F'89260'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60965'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'90835'            PP,CAR,SD                                    
         DC    F'61715'            SOC,GD                                       
         DC    F'49210'            VO,SS                                        
         DC    F'26445'            GS                                           
         DC    F'90835'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'92325'            PP,CAR,SD                                    
         DC    F'62460'            SOC,GD                                       
         DC    F'49810'            VO,SS                                        
         DC    F'26800'            GS                                           
         DC    F'92325'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62460'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'93885'            PP,CAR,SD                                    
         DC    F'63295'            SOC,GD                                       
         DC    F'50410'            VO,SS                                        
         DC    F'27150'            GS                                           
         DC    F'93885'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63295'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'95440'            PP,CAR,SD                                    
         DC    F'64040'            SOC,GD                                       
         DC    F'51025'            VO,SS                                        
         DC    F'27520'            GS                                           
         DC    F'95440'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64040'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'96585'            PP,CAR,SD                                    
         DC    F'64855'            SOC,GD                                       
         DC    F'51590'            VO,SS                                        
         DC    F'27855'            GS                                           
         DC    F'96585'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64855'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'97660'            PP,CAR,SD                                    
         DC    F'65620'            SOC,GD                                       
         DC    F'52235'            VO,SS                                        
         DC    F'28225'            GS                                           
         DC    F'97660'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65620'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'98865'            PP,CAR,SD                                    
         DC    F'66385'            SOC,GD                                       
         DC    F'52835'            VO,SS                                        
         DC    F'28550'            GS                                           
         DC    F'98865'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66385'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'100030'           PP,CAR,SD                                    
         DC    F'67175'            SOC,GD                                       
         DC    F'53435'            VO,SS                                        
         DC    F'28970'            GS                                           
         DC    F'100030'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67175'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'101200'           PP,CAR,SD                                    
         DC    F'67895'            SOC,GD                                       
         DC    F'54030'            VO,SS                                        
         DC    F'29305'            GS                                           
         DC    F'101200'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67895'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'102350'           PP,CAR,SD                                    
         DC    F'68495'            SOC,GD                                       
         DC    F'54485'            VO,SS                                        
         DC    F'29630'            GS                                           
         DC    F'102350'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68495'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'103480'           PP,CAR,SD                                    
         DC    F'69055'            SOC,GD                                       
         DC    F'54890'            VO,SS                                        
         DC    F'30000'            GS                                           
         DC    F'103480'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69055'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'104665'           PP,CAR,SD                                    
         DC    F'69715'            SOC,GD                                       
         DC    F'55385'            VO,SS                                        
         DC    F'30345'            GS                                           
         DC    F'104665'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'105785'           PP,CAR,SD                                    
         DC    F'70230'            SOC,GD                                       
         DC    F'55740'            VO,SS                                        
         DC    F'30680'            GS                                           
         DC    F'105785'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70230'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'106955'           PP,CAR,SD                                    
         DC    F'70820'            SOC,GD                                       
         DC    F'56210'            VO,SS                                        
         DC    F'31070'            GS                                           
         DC    F'106955'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70820'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'107735'           PP,CAR,SD                                    
         DC    F'71200'            SOC,GD                                       
         DC    F'56600'            VO,SS                                        
         DC    F'31325'            GS                                           
         DC    F'107735'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71200'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'108495'           PP,CAR,SD                                    
         DC    F'71975'            SOC,GD                                       
         DC    F'57050'            VO,SS                                        
         DC    F'31655'            GS                                           
         DC    F'108495'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71975'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'109270'           PP,CAR,SD                                    
         DC    F'72560'            SOC,GD                                       
         DC    F'57455'            VO,SS                                        
         DC    F'31910'            GS                                           
         DC    F'109270'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72560'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'110065'           PP,CAR,SD                                    
         DC    F'73145'            SOC,GD                                       
         DC    F'57850'            VO,SS                                        
         DC    F'32255'            GS                                           
         DC    F'110065'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73145'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'110815'           PP,CAR,SD                                    
         DC    F'73685'            SOC,GD                                       
         DC    F'58305'            VO,SS                                        
         DC    F'32515'            GS                                           
         DC    F'110815'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73685'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'111635'           PP,CAR,SD                                    
         DC    F'74275'            SOC,GD                                       
         DC    F'58755'            VO,SS                                        
         DC    F'32875'            GS                                           
         DC    F'111635'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74275'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'112405'           PP,CAR,SD                                    
         DC    F'74865'            SOC,GD                                       
         DC    F'59175'            VO,SS                                        
         DC    F'33160'            GS                                           
         DC    F'112405'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74865'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'113195'           PP,CAR,SD                                    
         DC    F'75435'            SOC,GD                                       
         DC    F'59610'            VO,SS                                        
         DC    F'33440'            GS                                           
         DC    F'113195'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75435'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'113965'           PP,CAR,SD                                    
         DC    F'76015'            SOC,GD                                       
         DC    F'60045'            VO,SS                                        
         DC    F'33730'            GS                                           
         DC    F'113965'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76015'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'114755'           PP,CAR,SD                                    
         DC    F'76580'            SOC,GD                                       
         DC    F'60490'            VO,SS                                        
         DC    F'34030'            GS                                           
         DC    F'114755'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76580'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'149'              PP,CAR,SD                                    
         DC    F'115'              SOC,GD                                       
         DC    F'85'               VO,SS                                        
         DC    F'46'               GS                                           
         DC    F'149'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'115'              ST, SAME AS SOC                              
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
*                                  1805:TABLE B                                 
         DC    AL1(60,40,1,1)      TV WSP&NET SPC/NETWORK UNITS 1               
         DC    F'80025'            PP,CAR,SD                                    
         DC    F'58715'            SOC,GD                                       
         DC    F'37345'            VO,SS                                        
         DC    F'21365'            GS                                           
         DC    F'80025'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,2,2)      TV WSP&NET SPC/NETWORK UNITS 2               
         DC    F'80025'            PP,CAR,SD                                    
         DC    F'58715'            SOC,GD                                       
         DC    F'37345'            VO,SS                                        
         DC    F'21365'            GS                                           
         DC    F'80025'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,3,3)      TV WSP&NET SPC/NETWORK UNITS 3               
         DC    F'80025'            PP,CAR,SD                                    
         DC    F'58715'            SOC,GD                                       
         DC    F'37345'            VO,SS                                        
         DC    F'21365'            GS                                           
         DC    F'80025'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,4,4)      TV WSP&NET SPC/NETWORK UNIT 4                
         DC    F'80025'            PP,CAR,SD                                    
         DC    F'58715'            SOC,GD                                       
         DC    F'37345'            VO,SS                                        
         DC    F'21365'            GS                                           
         DC    F'80025'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,5,5)      TV WSP&NET SPC/NETWORK UNIT 5                
         DC    F'80025'            PP,CAR,SD                                    
         DC    F'58715'            SOC,GD                                       
         DC    F'37345'            VO,SS                                        
         DC    F'21365'            GS                                           
         DC    F'80025'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'82165'            PP,CAR,SD                                    
         DC    F'60005'            SOC,GD                                       
         DC    F'38690'            VO,SS                                        
         DC    F'22035'            GS                                           
         DC    F'82165'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60005'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'84305'            PP,CAR,SD                                    
         DC    F'61335'            SOC,GD                                       
         DC    F'40060'            VO,SS                                        
         DC    F'22750'            GS                                           
         DC    F'84305'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61335'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'86500'            PP,CAR,SD                                    
         DC    F'62680'            SOC,GD                                       
         DC    F'41380'            VO,SS                                        
         DC    F'23460'            GS                                           
         DC    F'86500'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62680'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'88655'            PP,CAR,SD                                    
         DC    F'64030'            SOC,GD                                       
         DC    F'42820'            VO,SS                                        
         DC    F'24200'            GS                                           
         DC    F'88655'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64030'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'90725'            PP,CAR,SD                                    
         DC    F'65335'            SOC,GD                                       
         DC    F'44115'            VO,SS                                        
         DC    F'24865'            GS                                           
         DC    F'90725'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65335'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'92920'            PP,CAR,SD                                    
         DC    F'66655'            SOC,GD                                       
         DC    F'45485'            VO,SS                                        
         DC    F'25415'            GS                                           
         DC    F'92920'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66655'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'95030'            PP,CAR,SD                                    
         DC    F'67980'            SOC,GD                                       
         DC    F'46840'            VO,SS                                        
         DC    F'25975'            GS                                           
         DC    F'95030'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67980'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'97150'            PP,CAR,SD                                    
         DC    F'69270'            SOC,GD                                       
         DC    F'48200'            VO,SS                                        
         DC    F'26530'            GS                                           
         DC    F'97150'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69270'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'99340'            PP,CAR,SD                                    
         DC    F'70650'            SOC,GD                                       
         DC    F'49620'            VO,SS                                        
         DC    F'27100'            GS                                           
         DC    F'99340'            SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'101470'           PP,CAR,SD                                    
         DC    F'71970'            SOC,GD                                       
         DC    F'50920'            VO,SS                                        
         DC    F'27600'            GS                                           
         DC    F'101470'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71970'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'103595'           PP,CAR,SD                                    
         DC    F'73230'            SOC,GD                                       
         DC    F'52235'            VO,SS                                        
         DC    F'28145'            GS                                           
         DC    F'103595'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73230'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'105710'           PP,CAR,SD                                    
         DC    F'74585'            SOC,GD                                       
         DC    F'53575'            VO,SS                                        
         DC    F'28735'            GS                                           
         DC    F'105710'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74585'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'107890'           PP,CAR,SD                                    
         DC    F'75915'            SOC,GD                                       
         DC    F'54890'            VO,SS                                        
         DC    F'29260'            GS                                           
         DC    F'107890'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75915'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'109965'           PP,CAR,SD                                    
         DC    F'77255'            SOC,GD                                       
         DC    F'56225'            VO,SS                                        
         DC    F'29760'            GS                                           
         DC    F'109965'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77255'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'112130'           PP,CAR,SD                                    
         DC    F'78525'            SOC,GD                                       
         DC    F'57590'            VO,SS                                        
         DC    F'30520'            GS                                           
         DC    F'112130'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78525'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'114315'           PP,CAR,SD                                    
         DC    F'79675'            SOC,GD                                       
         DC    F'58810'            VO,SS                                        
         DC    F'30815'            GS                                           
         DC    F'114315'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79675'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'116445'           PP,CAR,SD                                    
         DC    F'80795'            SOC,GD                                       
         DC    F'60145'            VO,SS                                        
         DC    F'31285'            GS                                           
         DC    F'116445'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'80795'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'118575'           PP,CAR,SD                                    
         DC    F'81925'            SOC,GD                                       
         DC    F'61515'            VO,SS                                        
         DC    F'31725'            GS                                           
         DC    F'118575'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'81925'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'120675'           PP,CAR,SD                                    
         DC    F'83075'            SOC,GD                                       
         DC    F'62800'            VO,SS                                        
         DC    F'32220'            GS                                           
         DC    F'120675'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'83075'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'122865'           PP,CAR,SD                                    
         DC    F'84185'            SOC,GD                                       
         DC    F'64120'            VO,SS                                        
         DC    F'32710'            GS                                           
         DC    F'122865'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'84185'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'124965'           PP,CAR,SD                                    
         DC    F'85255'            SOC,GD                                       
         DC    F'64965'            VO,SS                                        
         DC    F'33185'            GS                                           
         DC    F'124965'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'85255'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'127095'           PP,CAR,SD                                    
         DC    F'86400'            SOC,GD                                       
         DC    F'65820'            VO,SS                                        
         DC    F'33660'            GS                                           
         DC    F'127095'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'86400'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'129120'           PP,CAR,SD                                    
         DC    F'87510'            SOC,GD                                       
         DC    F'66615'            VO,SS                                        
         DC    F'34115'            GS                                           
         DC    F'129120'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87510'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'131265'           PP,CAR,SD                                    
         DC    F'86685'            SOC,GD                                       
         DC    F'67440'            VO,SS                                        
         DC    F'34550'            GS                                           
         DC    F'131265'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'86685'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'133360'           PP,CAR,SD                                    
         DC    F'89765'            SOC,GD                                       
         DC    F'68265'            VO,SS                                        
         DC    F'35065'            GS                                           
         DC    F'133360'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'89765'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'133360'           PP,CAR,SD                                    
         DC    F'90875'            SOC,GD                                       
         DC    F'69050'            VO,SS                                        
         DC    F'35550'            GS                                           
         DC    F'133360'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90875'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'136620'           PP,CAR,SD                                    
         DC    F'91905'            SOC,GD                                       
         DC    F'69835'            VO,SS                                        
         DC    F'35945'            GS                                           
         DC    F'136620'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'91905'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'138220'           PP,CAR,SD                                    
         DC    F'93000'            SOC,GD                                       
         DC    F'70685'            VO,SS                                        
         DC    F'36385'            GS                                           
         DC    F'138220'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93000'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'139905'           PP,CAR,SD                                    
         DC    F'93735'            SOC,GD                                       
         DC    F'71475'            VO,SS                                        
         DC    F'36810'            GS                                           
         DC    F'139905'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93735'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'141510'           PP,CAR,SD                                    
         DC    F'95145'            SOC,GD                                       
         DC    F'72345'            VO,SS                                        
         DC    F'37255'            GS                                           
         DC    F'141510'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'95145'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'143135'           PP,CAR,SD                                    
         DC    F'96010'            SOC,GD                                       
         DC    F'72875'            VO,SS                                        
         DC    F'37730'            GS                                           
         DC    F'143135'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96010'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'144755'           PP,CAR,SD                                    
         DC    F'96835'            SOC,GD                                       
         DC    F'73420'            VO,SS                                        
         DC    F'38175'            GS                                           
         DC    F'144755'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96835'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'146375'           PP,CAR,SD                                    
         DC    F'97630'            SOC,GD                                       
         DC    F'73945'            VO,SS                                        
         DC    F'38620'            GS                                           
         DC    F'146375'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'97630'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'148045'           PP,CAR,SD                                    
         DC    F'98425'            SOC,GD                                       
         DC    F'74480'            VO,SS                                        
         DC    F'39040'            GS                                           
         DC    F'148045'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'98425'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'149620'           PP,CAR,SD                                    
         DC    F'99265'            SOC,GD                                       
         DC    F'75055'            VO,SS                                        
         DC    F'39475'            GS                                           
         DC    F'149620'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'99265'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'150740'           PP,CAR,SD                                    
         DC    F'100030'           SOC,GD                                       
         DC    F'75585'            VO,SS                                        
         DC    F'39795'            GS                                           
         DC    F'150740'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'100030'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'151785'           PP,CAR,SD                                    
         DC    F'100810'           SOC,GD                                       
         DC    F'76120'            VO,SS                                        
         DC    F'40220'            GS                                           
         DC    F'151785'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'100810'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'152875'           PP,CAR,SD                                    
         DC    F'101660'           SOC,GD                                       
         DC    F'76635'            VO,SS                                        
         DC    F'40605'            GS                                           
         DC    F'152875'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'101660'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'154020'           PP,CAR,SD                                    
         DC    F'102430'           SOC,GD                                       
         DC    F'77195'            VO,SS                                        
         DC    F'40960'            GS                                           
         DC    F'154020'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'102430'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'155060'           PP,CAR,SD                                    
         DC    F'103145'           SOC,GD                                       
         DC    F'77765'            VO,SS                                        
         DC    F'41365'            GS                                           
         DC    F'155060'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'103145'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'156210'           PP,CAR,SD                                    
         DC    F'103985'           SOC,GD                                       
         DC    F'78230'            VO,SS                                        
         DC    F'41695'            GS                                           
         DC    F'156210'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'103985'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'157245'           PP,CAR,SD                                    
         DC    F'104710'           SOC,GD                                       
         DC    F'78780'            VO,SS                                        
         DC    F'41985'            GS                                           
         DC    F'157245'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'104710'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'158300'           PP,CAR,SD                                    
         DC    F'105445'           SOC,GD                                       
         DC    F'79280'            VO,SS                                        
         DC    F'42390'            GS                                           
         DC    F'158300'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'105445'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'159420'           PP,CAR,SD                                    
         DC    F'106300'           SOC,GD                                       
         DC    F'79785'            VO,SS                                        
         DC    F'42655'            GS                                           
         DC    F'159420'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'106300'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'160500'           PP,CAR,SD                                    
         DC    F'107055'           SOC,GD                                       
         DC    F'80345'            VO,SS                                        
         DC    F'43050'            GS                                           
         DC    F'160500'           SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'107055'           ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'208'              PP,CAR,SD                                    
         DC    F'160'              SOC,GD                                       
         DC    F'105'              VO,SS                                        
         DC    F'56'               GS                                           
         DC    F'208'              SA, SAME AS PP                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'160'              ST, SAME AS SOC                              
         SPACE 1                                                                
*                                  906:FEE DETAINED IN AUDITION                 
CAUTTBL  DC    AL1(68,44,0,0)      AUDITION INTERVIEW - TV                      
         DC    F'7650'             PP,CAR,SD                                    
         DC    F'7650'             SOC,GD                                       
         DC    F'7650'             VO,SS                                        
         DC    F'7650'             GS                                           
         DC    F'7650'             SA                                           
         DC    F'7650'             DEM                                          
         DC    F'7650'             E                                            
         DC    F'7650'             GE                                           
         DC    F'7650'             ST                                           
         DC    F'7650'             EVERYBODY ELSE-US,SI,SB,PT                   
         SPACE 1                                                                
CAURTBL  DC    AL1(69,12,0,0)      AUDITION INTERVIEW - RADIO                   
         DC    F'7650'             SS,SV                                        
         DC    F'7650'             MV,GS                                        
         EJECT                                                                  
*                                  1203.C:DEMO AND TEST COMMERCIAL              
CDMTBL   DC    AL1(70,44,2,2)      TV PRESENTATION DEMO 2                       
         DC    F'28100'            PP,CAR,SD                                    
         DC    F'28100'            SOC,GD                                       
         DC    F'28100'            VO,SS                                        
         DC    F'28100'            GS                                           
         DC    F'28100'            SA                                           
         DC    F'28100'            DEM                                          
         DC    F'28100'            E                                            
         DC    F'28100'            GE                                           
         DC    F'28100'            ST                                           
         DC    F'28100'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'14150'            PP,CAR,SD                                    
         DC    F'14150'            SOC,GD                                       
         DC    F'14150'            VO,SS                                        
         DC    F'14150'            GS                                           
         DC    F'14150'            SA                                           
         DC    F'14150'            DEM                                          
         DC    F'14150'            E                                            
         DC    F'14150'            GE                                           
         DC    F'14150'            ST                                           
         DC    F'14150'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
*                                  1202:SESSION FEE                             
BSCHTBL  DC    AL1(80,40,0,0)      NATIONAL TV  - HOURLY RATE                   
         DC    F'9150'             PP,SD,CAR                                    
         DC    F'9150'             SOC,GD                                       
         DC    F'7650'             VO,SS                                        
         DC    F'7650'             GS                                           
         DC    F'13725'            SA = PP+50%                                  
         DC    F'9150'             DEM                                          
         DC    F'5700'             E                                            
         DC    F'3700'             GE                                           
         DC    F'9150'             ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(81,40,0,0)      NATIONAL TV  - OVERTIME HOURLY RATE          
         DC    F'11950'            PP,SD,CAR                                    
         DC    F'11950'            SOC,GD                                       
         DC    F'9600'             VO,SS                                        
         DC    F'9600'             GS                                           
         DC    F'17925'            SA = PP+50%                                  
         DC    F'11950'            DEM                                          
         DC    F'6800'             E                                            
         DC    F'4800'             GE                                           
         DC    F'11950'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(82,40,0,0)      NATIONAL TV  - DBLTIME HOURLY RATE           
         DC    F'13450'            PP,SD,CAR                                    
         DC    F'13450'            SOC,GD                                       
         DC    F'11950'            VO,SS                                        
         DC    F'11950'            GS                                           
         DC    F'19650'            SA = PP+50%                                  
         DC    F'13450'            DEM                                          
         DC    F'8500'             E                                            
         DC    F'5550'             GE                                           
         DC    F'13450'            ST, SAME AS SOC                              
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
**PAN#1  DC    CL21'008TAGEN74   06/26/12'                                      
         END                                                                    
