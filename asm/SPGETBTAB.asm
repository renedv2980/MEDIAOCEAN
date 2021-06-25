*          DATA SET SPGETBTAB  AT LEVEL 032 AS OF 02/11/21                      
*PHASE T00A2DC                                                                  
         TITLE 'SPOT/GETBUY TABLE OF 2-CHAR LINE AGENCIES'                      
GETBTAB  CSECT                                                                  
         DS    XL128                                                            
*                                                                               
         ORG   *-128                                                            
         DC    F'3'                ENTRY LENGTH                                 
         DC    A(GETTABX-GETTAB-1) TABLE LENGTH -1                              
******                                                                          
* 2 BYTE AGENCY / 1 BYTE SYSFLAG  - REFER TO GETBTABD IN SPGETBUY               
******                                                                          
GETTAB   DC    0C                                                               
*                                                                               
* BELOW AGENCIES FOR SPOTTT ON TST & MEL                                        
*                                                                               
         DC    C'T1',C'T'          TCH1                                         
         DC    C'HA',C'T'          CANTEST                                      
         DC    C'SJ',C'T'          SJR                                          
*        DC    C'TM',C'T'          TMNY                                         
*        DC    C'*B',C'T'          DDSB                                         
*                                                                               
* BELOW AGENCIES FOR SPOTTT ON FQA                                              
*                                                                               
         DC    C'T1',C'Q'          TCH1                                         
         DC    C'HA',C'Q'          CANTEST                                      
*        DC    C'SJ',C'Q'          SJR                                          
         DC    C'TM',C'Q'          TMNY - 2/8/19                                
*        DC    C'*B',C'Q'          DDSB                                         
*                                                                               
* BELOW AGENCIES FOR SPOTTU ON FQA                                              
*                                                                               
         DC    C'Q1',C'Q'          TCH1BLD - 7/19/18                            
         DC    C'Q6',C'Q'          CANBLD - 2/7/18                              
*        DC    C'QA',C'Q'          SJBLD                                        
         DC    C'QT',C'Q'          TMNYBLD - 2/8/19                             
*        DC    C'QB',C'Q'          DDSBBLD                                      
*                                                                               
* BELOW AGENCY FOR ADV1 & CSC  (NOTE: CSC SHOULD ALWAYS MATCH ADV)              
*                                                                               
*** US AGENCIES                                                                 
         DC    C'SJ',C'A'          SJR (ADV) - 3/17/19                          
         DC    C'CH',C'A'          UBUAT (ADV) - 4/18/19                        
         DC    C'OO',C'A'          OMD (ADV) - 4/21/19                          
         DC    C'UB',C'A'          DAN / CARAT (ADV) - 5/19/19                  
         DC    C'FM',C'A'          HAVAS (ADV) - 6/9/19                         
         DC    C'H7',C'A'          GROUPM H7 (ADV) - 7/7/19                     
         DC    C'FR',C'A'          GTB FR (ADV) - 7/28/19                       
         DC    C'JS',C'A'          GEOMETRY GLOBAL JS (ADV) - 7/28/19           
         DC    C'G7',C'A'          GSD&M (ADV) - 9/28/19                        
         DC    C'WS',C'A'          KWG (ADV) - 9/28/19                          
         DC    C'LG',C'A'          LATINWORKS (ADV) - 9/28/19                   
         DC    C'WD',C'A'          WIEDEN & KENNEDY (ADV) - 9/28/19             
         DC    C'RP',C'A'          RPA (ADV) - 9/28/19                          
         DC    C'BI',C'A'          UWG (ADV) - 2/13/21                          
                                                                                
*** CANADIAN AGENCIES                                                           
*                                                                               
         DC    C'T1',C'A'          TCH1 (ADV)                                   
         DC    C'U#',C'A'          M2 UNIVERSAL CONVERTED 7/23/11               
         DC    C'HD',C'A'          HDTO CONVERTED 8/28/11                       
         DC    C'H0',C'A'          MINDSHARE CANADA  2011-10-08                 
         DC    C'TB',C'A'          ZENITHOPTIMEDIA    2011-10-22                
         DC    C'S4',C'A'          WASSERMAN & PARTNERS 2011-10-22              
         DC    C'NT',C'A'          MPG CANADA         2011-10-29                
         DC    C'O0',C'A'          LEO BURNETT COMPANY LTD  2011-10-29          
         DC    C'YR',C'A'          Y&R CANADA         2011-11-05                
         DC    C'WT',C'A'          INITIATIVE CANADA  2011-11-05                
         DC    C'OU',C'A'          OPTIMUM MEDIA DIRECTION 2011-11-19           
         DC    C'PT',C'A'          PADULO ADV           2011-12-10              
         DC    C'SO',C'A'          SAATCHI&SAATCHI ADV  2011-12-10              
         DC    C'EI',C'A'          ENDEAVOUR            2011-12-10              
         DC    C'TD',C'A'          TIME & SPACE         2011-12-17              
         DC    C'DN',C'A'          DDB                  2011-12-17              
         DC    C'F6',C'A'          SHARPE               2011-12-17              
         DC    C'JE',C'A'          BLAMMO               2011-12-17              
         DC    C'HY',C'A'          SHARED FIN'L SERVICES  2012-04-13            
         DC    C'E$',C'A'          MEN'S WAREHOUSE        2012-06-30            
         DC    C'FG',C'A'          LETO - TEST CANADIAN   2012-06-30            
         DC    C'H1',C'A'          DDS CANADA UNIVERSITY  2012-06-30            
         DC    C'VA',C'A'          ZENITH UAT ZOUAT       2015-08-17            
         DC    C'VH',C'A'          ZENITH UAT SOUAT       2015-08-17            
         DC    C'VI',C'A'          ZENITH UAT LBUAT       2015-08-17            
         DC    C'YE',C'A'          ZENITH UAT ZO2UAT      2015-09-14            
         DC    C'YF',C'A'          SAATCHI UAT SO2UAT     2015-09-14            
         DC    C'Y1',C'A'          STARCOM UAT LB2UAT     2015-09-14            
         DC    C'C1',C'A'          CAIRNS ONEIL MEDIA     2020-11-08            
         DC    C'AI',C'A'          ACTIVE MEDIA SERVICES  2020-11-08            
         DC    C'ON',C'A'          INNOCEAN CANADA        2020-11-08            
         DC    C'EX',C'A'          EXPEDIA CANADA CORP    2020-11-08            
         DC    C'GD',C'A'          GLASSROOM              2020-11-08            
         DC    C'ZZ',C'A'          ** END OF THIS TABLE **                      
GETTABX  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPGETBTAB 02/11/21'                                      
         END                                                                    
