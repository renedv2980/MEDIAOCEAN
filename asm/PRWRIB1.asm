*          DATA SET PRWRIB1    AT LEVEL 024 AS OF 11/05/18                      
*PHASE T405B1B,*                                                                
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-22216  11/05/18 SUPPORT NEW MEDIA TYPE "D" (DIGITAL AUDIO)*         
***********************************************************************         
         TITLE 'CHANGE LOG'                                                     
*RJS  5/7/90    DELETED 50K OF CLIENT TABLE-- USING GETMAIN AND                 
*                FREEMAIN TO GET STORAGE                                        
*        DC    C'*CLTBUF*'                                                      
*        DC    F'50000'                                                         
*        DC    50000X'00'          CLIENT BUFFER                                
*RJS  10/26/89  INSERT WEEK, MONTH, QUARTER LIST                  C02           
*RJS  10/5/89   INSERT LENGTH OF OFFLINE BUFFER                   C01           
*                                                                               
*  RJS 10/21/88  INCREASE SIZE OF CLIENT BUFFER FROM  40000 TO 50000            
*                REDUCED SIZE OF OFF LINE BUFFER FROM 50000 TO 40000            
***********************************************************************         
*************** NOTE FOR  EACH INCREASE OF TABLE SIZE, A CORRESPONDING*         
*************** CHANGE MUST BE MADE IN THE OFF LINE BUFFER            *         
***********************************************************************         
         TITLE 'T405B1 - BUFFER FOR PRINTPAK WRITER APPLICATIONS'               
GLOBALS  CSECT                                                                  
         DC    C'*GLOBAL*'                                                      
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    F'50000'            MAPS TO GLSIZE                               
         DC    50000X'00'                                                       
*                                                                               
         DC    C'*MEDBUF*'         (11*12) + 1 BYTE EOT                         
         DC    F'133'              11 MEDIA * MBFLEN (12 BYTES) + EOT           
         DC    133X'00'            MEDIA BUFFER                                 
*                                                                               
*                                                                               
*        DC    C'*PRDBUF*'         REPLACED BY GETMAIN                          
*        DC    F'16000'                                                         
*        DC    16000X'00'          PRODUCT BUFFER                               
*                                                                               
*        DC    C'*REPBUF*'                                                      
*        DC    F'60000'                                                         
*        DC    60000X'00'          REP BUFFER                                   
*                                                                               
*        DC    C'*DRDBUF*'         REPLACED BY GETMAIN                          
*        DC    F'32000'                                                         
*        DC    32000X'00'          DIVISION/REGION/DISTRICT BUFFER              
*                                                                               
         DC    C'**BUFF**'         OFF LINE BUFFER                              
         DC    F'40000'             SIZE                           C01          
         SPACE 3                                                                
         DC    40000X'00'          40K                                          
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024PRWRIB1   11/05/18'                                      
         END                                                                    
