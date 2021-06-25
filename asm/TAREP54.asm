*          DATA SET TAREP54    AT LEVEL 175 AS OF 05/01/02                      
*PHASE T70354A,*                                                                
         TITLE 'T70354 - PROGRAM TO SEE IF DAY AFTER IS HOLIDAY'                
T70354   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70354                                                         
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP             READ INVOICES AND REPORT ON THEM             
XIT      XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------          
*              CALCULATE DAYS                                                   
*---------------------------------------------------------------------          
         USING GETRETD,R3                                                       
PREP     NTR1                                                                   
         LA    R3,WORK             R3=A(GETRET BLOCK)                           
         XC    WORK,WORK           SET TO ADD 1 BUSINESS DAY TO TODAY           
         GOTO1 DATCON,DMCB,(0,TGTODAY0),(3,GRDIDY)  TODAY                       
         MVC   GRDHRS,=H'48'                        + 48 HOURS                  
         L     RF,ACOMFACS         USE GETRET TO ADD BUSINESS DAYS              
         USING COMFACSD,RF                                                      
         GOTO1 CGETRET,(R3)        CALCULATE NEXT BUSINESS DAY                  
         GOTO1 DATCON,DMCB,(3,GRDODY),(0,NEXTDAY)                               
*                                                                               
         OI    GRDFLAG,GRDFTAL     TALENT CALENDAR                              
         L     RF,ACOMFACS         USE GETRET TO ADD BUSINESS DAYS              
         GOTO1 CGETRET,(R3)        CALCULATE NEXT BUSINESS DAY                  
         GOTO1 DATCON,DMCB,(3,GRDODY),(0,NXTTALDY)                              
*                                                                               
         GOTO1 DATCON,DMCB,(0,TGTODAY0),(11,P+10)                               
         GOTO1 DATCON,DMCB,(0,NEXTDAY),(11,P+20)                                
         GOTO1 DATCON,DMCB,(0,NXTTALDY),(11,P+30)                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)             PRINT IT                             
         CLC   NEXTDAY,NXTTALDY                                                 
         BE    PREPX                                                            
         GOTO1 DATCON,DMCB,(0,NEXTDAY),(11,NOPRODD)                             
         MVC   P(NOPRODLN),NOPROD                                               
         GOTO1 SPOOL,DMCB,(R8)                               PRINT IT           
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(NOPRODLN),NOPROD)                   
*                                                                               
PREPX    XIT1                                                                   
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
         LTORG                                                                  
NOPROD   DC    C'AUTONOTE*GHOA,MZEI,JBAS,DEIS'                                  
         DC    C':DO NOT RUN TALENT PRODUCTION FOR '                            
NOPRODD  DC    C'JAN01/01'                                                      
NOPRODLN EQU   *-NOPROD                                                         
*                                                                               
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SPACE 1                                                                
         DC    H'0'                                                             
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
TODAY    DS    CL6                                                              
NEXTDAY  DS    CL6                                                              
NXTTALDY DS    CL6                                                              
DAYWEEK  DS    CL3                                                              
MYDLNQ   EQU   *-MYD                                                            
         EJECT                                                                  
       ++INCLUDE DDGETRETD                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEAD                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'175TAREP54   05/01/02'                                      
         END                                                                    
