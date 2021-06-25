*          DATA SET NEWRI3A    AT LEVEL 012 AS OF 05/01/02                      
*PHASE T3203AA                                                                  
         TITLE 'T3203A - BRMFLOWCHART'                                          
T3203A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T3203A,RR=R2                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,ANETWS2                                                       
         USING MYWORKD,R7                                                       
         LR    RE,R7                                                            
         LA    RF,WORKLENE                                                      
         XCEF                                                                   
         L     R6,ANETWS4                                                       
         ST    R6,ACLIST                                                        
         LA    R6,1000(R6)                                                      
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         ST    R2,RELO                                                          
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   *+8                                                              
         BAS   RE,EDITMOD                                                       
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBDATA,C'U'         UNIT RECORDS ONLY                            
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         PRINT GEN                                                              
         L     R4,ACLIST                                                        
         NETGO NVCLI,DMCB,SPLCLIN,(R4)                                          
         PRINT NOGEN                                                            
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         MVC   REQPRD,SPLPRO                                                    
         CLC   FLD(3),=C'ALL'                                                   
         BNE   *+8                                                              
         MVI   PRDOPT,C'A'         SEPARATE BY PRODUCT                          
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB                                                    
         CLC   SPLNET(3),=C'ALL'                                                
         BNE   *+8                                                              
         MVI   NETALL,C'Y'                                                      
*                                                                               
         LA    R2,SPLDPTH                DAYPART                                
         NETGO NVDPTALL,DMCB,SPLDPTN                                            
         OI    SPLDPTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPAKH                PACKAGE                                
         NETGO NVPAKLOK,DMCB                                                    
         OI    SPLPAKNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLSTRTH               START DATE                             
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDDH               END DATE                               
         NETGO NVENDDAT,DMCB                                                    
         SPACE                                                                  
*                                                                               
         MVI   EQVOPT,X'FF'        PRESET OPTIONS                               
         MVI   DOLLOPT,0                                                        
         MVI   ROTAOPT,0                                                        
         LA    R2,SPLOPTH                        OPTIONS                        
         CLI   5(R2),0                                                          
         BE    EDT10                                                            
         GOTO1 SCANNER,DMCB,(R2),(5,BLOCK),0                                    
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    EDT10                                                            
OPT2     CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   OPT4                                                             
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
OPT4     DS    0H                                                               
         CLC   12(3,R4),=C'LEN'    LENGTH FILTER                                
         BNE   OPT6                                                             
         TM    3(R4),X'80'         TEST NUMERIC                                 
         BZ    EDINV                                                            
         MVC   NBSELLEN,11(R4)                                                  
         B     OPTEND                                                           
OPT6     DS    0H                                                               
         CLC   12(5,R4),=C'UNITS'  UNITS OPTION (GRPS DEFAULT)                  
         BNE   OPT8                                                             
         MVI   UNTOPT,C'Y'                                                      
         B     OPTEND                                                           
OPT8     DS    0H                                                               
         CLC   12(3,R4),=C'PER'      PERIOD OPTION                              
         BNE   OPT10                                                            
         MVC   TYPOPT,22(R4)                                                    
         CLI   TYPOPT,C'M'                                                      
         BE    OPTEND                                                           
         CLI   TYPOPT,C'W'                                                      
         BE    OPTEND                                                           
         B     EDINV                                                            
OPT10    DS    0H                                                               
         CLC   12(5,R4),=C'DATES'  DATES OPTION                                 
         BNE   OPT12                                                            
         CLI   22(R4),C'Y'                                                      
         BNE   EDINV                                                            
         CLI   UNTOPT,C'Y'                                                      
         BE    EDINV                                                            
         OC    OPTIONS,OPTIONS                                                  
         BNZ   EDINV                                                            
         OI    OPTIONS,X'01'                                                    
         B     OPTEND                                                           
OPT12    DS    0H                                                               
         CLC   12(5,R4),=C'PRODS'  PRODUCTS OPTION                              
         BNE   OPT14                                                            
         CLI   22(R4),C'Y'                                                      
         BNE   EDINV                                                            
         OC    OPTIONS,OPTIONS                                                  
         BNZ   EDINV                                                            
         OI    OPTIONS,X'02'                                                    
         B     OPTEND                                                           
OPT14    DS    0H                                                               
         CLC   12(5,R4),=C'DEMOS'  DEMOS OPTION                                 
         BNE   OPT16                                                            
         MVC   DEMOPT,22(R4)                                                    
         CLI   DEMOPT,C'A'         ACTUAL DEMOS                                 
         BE    OPTEND                                                           
         B     EDINV                                                            
OPT16    CLC   12(4,R4),=C'COST'   ACTUAL COST OPTION                           
         BNE   OPT18                                                            
         MVC   COSTOPT,22(R4)                                                   
         CLI   COSTOPT,C'A'                                                     
         BE    OPTEND                                                           
         B     EDINV                                                            
OPT18    CLC   12(5,R4),=C'DOLLS'  AVERAGE AND TOTAL DOLLS                      
         BNE   OPT22                                                            
         MVC   DOLLOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE                                                                  
OPT22    CLC   12(3,R4),=C'EQV'    EQIVALENCE                                   
         BNE   OPT24                                                            
         TM    3(R4),X'80'                                                      
         BNO   EDINV                                                            
         MVC   EQVOPT,11(R4)                                                    
         B     OPTEND                                                           
OPT24    CLC   12(5,R4),=C'GRAND'  NO GRAND TOTALS                              
         BNE   OPT26                                                            
         MVC   GRAND,22(R4)                                                     
         CLI   GRAND,C'N'                                                       
         BE    OPTEND                                                           
         B     EDINV                                                            
OPT26    CLC   12(3,R4),=C'NET'  PRINT NETWORK IN UNITS COLUMN                  
         BNE   OPT28                                                            
         MVC   NETNM,22(R4)                                                     
         CLI   NETNM,C'Y'                                                       
         BE    OPTEND                                                           
         B     EDINV                                                            
OPT28    CLC   12(3,R4),=C'ROT'  USE ROTATION                                   
         BNE   EDINV                                                            
         MVI   ROTAOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE                                                                  
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
*                                                                               
EDT10    LA    R2,SPLDEMH                     DEMOS                             
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
*                                                                               
         LA    R2,SPLTITLH                    TITLE                             
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDTX                                                             
         MVC   TITLE,FLD                                                        
         MVI   QTITLE,C'Y'                                                      
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
*                                                                               
EDERR    GOTO1 ERREX                                                            
*                                                                               
         EJECT                                                                  
               LTORG                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
MYWORKD  DSECT                                                                  
QTITLE   DS    CL1                 *** PASSED TO PRINT MODULE                   
TITLE    DS    CL40                ***                                          
NETNM    DS    CL1                 ***                                          
TYPOPT   DS    CL1                 ***                                          
NETALL   DS    CL1                 ***                                          
OPTIONS  DS    CL1                 ***                                          
UNTOPT   DS    CL1                                                              
BOXOPT   DS    CL1                                                              
GRAND    DS    CL1                 ***                                          
COSTOPT  DS    CL1                 ***                                          
DOLLOPT  DS    CL1                 ***                                          
DEMOPT   DS    CL1                 ***                                          
LENOPT   DS    CL1                 ***                                          
PRDOPT   DS    CL1                 ***                                          
EQVOPT   DS    CL1                                                              
REQPRD   DS    CL3                                                              
ROTAOPT  DS    CL1                 ***                                          
ACLIST   DS    F                   ***                                          
*                                                                               
RELO     DS    F                                                                
NUMMONS  DS    F                   NUMBER OF MONTHS IN LIST                     
BINDMCB  DS    6F                                                               
DATOPT   DS    CL1                                                              
PKGBRK   DS    CL1                                                              
SKIP     DS    CL1                                                              
PREVIOUS DS    CL18                                                             
PAKNAMSV DS    CL16                                                             
MAXMONTS EQU   16                  MAXIMUM NUMBER OF MONTHS (WEEKS)             
MONLIST  DS    CL(4*MAXMONTS)      MONTH (WEEK) LIST                            
PERTYPE  DS    CL3                 PERIOD TYPE AND CONTROL BYTES                
*                                                                               
WORKLENE EQU   *-QTITLE                                                         
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIEDD                                                       
         PRINT OFF                                                              
DEMBLK   DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEWRI3A   05/01/02'                                      
         END                                                                    
