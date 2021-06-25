*          DATA SET PRMEDTST   AT LEVEL 023 AS OF 05/01/02                      
*PHASE T41C52A                                                                  
         TITLE 'T41C52 - TEST OF NEW PRINT SYSTEM'                              
T41C52   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C52                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM+THIS PROG             
         L     R9,ASYSD                                                         
         USING PRNTSYSD,R9                                                      
         LA    R7,PRTSYSDX                                                      
         LA    R7,8(R7)                                                         
         USING PRNTBLOK,R7                                                      
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,DSPECS                                                        
         ST    R1,SPECS                                                         
         SPACE 3                                                                
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
******************************************                                      
* LOGOC                                  *                                      
*                                        *                                      
******************************************                                      
         EJECT                                                                  
* VALIDATE KEY *                                                                
         SPACE                                                                  
VK       DS    0H                                                               
         MVI   PBQINIT,0                                                        
         MVI   FTERMFLG,1                                                       
*                                                                               
         LA    R2,PRTMEDH                                                       
         GOTO1 VALIMED,DMCB                                                     
*                                                                               
         LA    R2,PRTCLTH                                                       
         GOTO1 VALICLT,DMCB                                                     
*                                                                               
         LA    R2,PRTPRDH                                                       
         GOTO1 VALIPRD,DMCB                                                     
*                                                                               
         LA    R2,PRTESTH                                                       
         GOTO1 VALIEST,DMCB                                                     
*                                                                               
*        LA    R2,PRTPUBH                                                       
*        GOTO1 VALIPUB,DMCB                                                     
*                                                                               
         LA    R2,PRTDIVH                                                       
         GOTO1 VALIDIV,DMCB                                                     
*                                                                               
         LA    R2,PRTREGH                                                       
         GOTO1 VALIREG,DMCB                                                     
*                                                                               
         LA    R2,PRTDSTH                                                       
         GOTO1 VALIDST,DMCB                                                     
*                                                                               
         LA    R2,PRTSTRTH                                                      
         GOTO1 VALISTDT,DMCB                                                    
*                                                                               
         LA    R2,PRTENDH                                                       
         GOTO1 VALIENDT,DMCB                                                    
*                                                                               
         LA    R2,PRTMEDH          RESET CURSOR                                 
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
***************************************                                         
*                                                                               
PR       DS    0H                                                               
         XC    GRSTOT(12),GRSTOT                                                
         LA    R1,IOHOOK                                                        
         ST    R1,PBHOOK                                                        
GETBUY   GOTO1 PBPRTIO,DMCB,PRNTBLOK                                            
         CLI   PBMODE,PBREQLST                                                  
         BNE   GETBUY                                                           
         BAS   RE,TOTALS                                                        
         B     EXIT                                                             
*                                                                               
IOHOOK   NTR1                                                                   
         LA    R3,P                                                             
         LA    R3,30(R3)                                                        
         USING PRTLINE,R3                                                       
*                                                                               
         CLI   PBMODE,PBPROCBU                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B2,PBACTEST),(3,PEST)      ESTIMATE                             
         MVC   PPRD,PBACTPRD           PRODUCT                                  
         MVC   PPUBNM,PBAPUBNM         PUBLICATION                              
         EDIT  (B4,PBGROSS),(10,PRTGRS)    BILLED                               
         L     R1,GRSTOT                                                        
         A     R1,PBGROSS                                                       
         ST    R1,GRSTOT                                                        
         EDIT  (B4,PBPGROSS),(10,PRTPGRS)   PAID                                
         L     R1,PBPGROSS                                                      
         A     R1,PGRSTOT                                                       
         ST    R1,PGRSTOT                                                       
         EDIT  (B4,PBBGROSS),(10,PRTBGRS)                                       
         L     R1,PBBGROSS                                                      
         A     R1,BGRSTOT                                                       
         ST    R1,BGRSTOT                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         EJECT                                                                  
********************************                                                
*                                                                               
TOTALS   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,P                                                             
         LA    R3,30(R3)                                                        
         USING PRTLINE,R3                                                       
         MVC   P+51(10),=C'**TOTALS**'                                          
         EDIT  (B4,GRSTOT),(10,PRTGRS)                                          
         EDIT  (B4,PGRSTOT),(10,PRTPGRS)                                        
         EDIT  (B4,BGRSTOT),(10,PRTBGRS)                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
* WORK AREA                                                                     
GRSTOT   DS    F                                                                
PGRSTOT  DS    F                                                                
BGRSTOT  DS    F                                                                
         SPACE 2                                                                
*                                                                               
HOOK     NTR1                                                                   
         MVC   H1+10(10),PBAMEDNM                                               
         MVC   H3+10(3),PBSELCLT                                                
         MVC   H4+10(3),PBSELPRD                                                
         LA    R2,H5+11                                                         
         EDIT  (B2,PBSELEST),(4,0(R2))                                          
*  SET UP BOXES PARAMETERS *                                                    
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         LA    R5,30(R5)                                                        
         USING PRTLINE,R5                                                       
         MVI   PEST-1,C'L'                                                      
         MVI   PPRD-1,C'C'                                                      
         MVI   PPUBNM-1,C'C'                                                    
         MVI   PRTGRS-1,C'C'                                                    
         MVI   PRTPGRS-1,C'C'                                                   
         MVI   PRTBGRS-1,C'C'                                                   
         MVI   PRTBGRS+11,C'R'                                                  
         SPACE                                                                  
         LA    R5,BOXROWS                                                       
         LA    R5,8(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,2(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
         LA    R5,H10                                                           
         LA    R5,30(R5)                                                        
         MVC   PEST,=C'EST'                                                     
         MVC   PPRD,=C'PRD'                                                     
         MVC   PPUBNM(11),=C'PUBLICATION'                                       
         MVC   PRTGRS(7),=C'ORDERED'                                            
         MVC   PRTPGRS(4),=C'PAID'                                              
         MVC   PRTBGRS(6),=C'BILLED'                                            
         DROP  R5                                                               
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
*********                                                                       
DSPECS   SSPEC H1,1,C'MEDIA'                                                    
         SSPEC H1,54,C'NEW PRINT TEST PROGRAM'                                  
         SSPEC H2,54,C'----------------------'                                  
         SSPEC H1,96,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,96,AGYADD                                                     
         SSPEC H3,1,C'CLIENT'                                                   
         SSPEC H4,1,C'PRODUCT'                                                  
         SSPEC H5,96,PAGE                                                       
         SSPEC H5,1,C'ESTIMATE'                                                 
         DC    X'00'                                                            
         SPACE                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
*                                                                               
         EJECT                                                                  
HDRTN    NTR1                                                                   
         MVC   H2+10(L'PBEFFMED),PBEFFMED                                       
         MVC   H2+15(L'PBEFMDNM),PBEFMDNM                                       
         B     EXIT                                                             
         SPACE 2                                                                
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H1,46,C'USER PRINT REPORT'                                       
         SSPEC H2,46,C'-----------------'                                       
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H3,73,REPORT                                                     
         SSPEC H4,73,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H7,6,C'CODE'                                                     
         SSPEC H8,6,C'----'                                                     
         SSPEC H7,15,C'REPORT TITLE'                                            
         SSPEC H8,15,C'------------'                                            
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PRTLINE  DSECT                                                                  
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PPUBNM   DS    CL20                                                             
         DS    CL1                                                              
PRTGRS   DS    CL10                                                             
         DS    CL2                                                              
PRTPGRS  DS    CL10                                                             
         DS    CL2                                                              
PRTBGRS  DS    CL10                                                             
         EJECT                                                                  
         SPACE                                                                  
       ++INCLUDE PRNTINCLS                                                      
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE PSFLMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRMEDSCRND                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023PRMEDTST  05/01/02'                                      
         END                                                                    
