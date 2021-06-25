*          DATA SET SPREPC302  AT LEVEL 064 AS OF 04/21/99                      
*PHASE SPC302A                                                                  
*INCLUDE HEXOUT                                                                 
         SPACE 1                                                                
         TITLE 'SPC302 - SPOT BUY COS2 UPDATE'                                  
SPC302   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPC302,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPC302+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    PBUY                                                             
         CLI   MODE,STAFRST                                                     
         BE    SFST                                                             
         CLI   MODE,STALAST                                                     
         BE    SLAS                                                             
         CLI   MODE,ESTFRST                                                     
         BE    EFST                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   SC02                                                             
         XC    UPDALL,UPDALL                                                    
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
SC02     CLI   MODE,REQLAST                                                     
         BE    RLST                                                             
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
*===============================================================*               
* ESTFRST - GET THE NEW COS2 VALUE                              *               
*===============================================================*               
*                                                                               
EFST     DS    0H                                                               
         L     R6,ADEST                                                         
         USING ESTRECD,R6                                                       
*                                                                               
         MVC   COS2VAL,ECOST2      SAVE THE ORIGINAL COST                       
         DROP  R6                                                               
EFSTX    B     EXIT                                                             
*                                                                               
*===============================================================*               
* STAFRST - KEEP A COUNT OF UPDATES PER STATION                 *               
*===============================================================*               
*                                                                               
SFST     DS    0H                                                               
*                                                                               
         XC    UPDSTA,UPDSTA                                                    
*                                                                               
SFSTX    B     EXIT                                                             
*                                                                               
*===============================================================*               
* STALAST - OUT THE COUNTS OF UPDATE FOR EACH STATION           *               
*===============================================================*               
*                                                                               
SLAS     DS    0H                                                               
*                                                                               
         OC    UPDSTA,UPDSTA                                                    
         BZ    EXIT                                                             
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
*                                                                               
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   P+22(4),WORK                                                     
         MVI   P+26,C'-'                                                        
         MVC   P+27(1),WORK+4                                                   
         CLI   WORK+4,C' '                                                      
         BH    *+8                                                              
         MVI   P+(27),C'T'                                                      
*                                                                               
         MVC   P+2(19),=C'TOTAL UPDATES FOR:'                                   
*                                                                               
         EDIT  (B4,UPDSTA),(10,P+30),COMMAS=YES,ALIGN=RIGHT                     
         GOTO1 REPORT                                                           
*                                                                               
SLASX    B     EXIT                                                             
         DROP  R5,R6                                                            
*                                                                               
*===============================================================*               
* PROCBUY - MOVE IN THE NEW COST2 VALUE IF THERE IS X'73' ELEMENT               
*===============================================================*               
*                                                                               
PBUY     DS    0H                                                               
*                                                                               
         XC    COS2OLD,COS2OLD                                                  
*                                                                               
         CLI   KEY+10,0            DON'T PROCESS SPILL MARKETS                  
         BE    PBUY02                                                           
         CLI   KEY+10,X'FF'                                                     
         BNE   PBUYX                                                            
*                                                                               
PBUY02   L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         MVI   ELCODE,X'73'        GET THE COS2 ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   EXIT                DOESN'T EXIST? NEXT BUYREC                   
         CLC   COS2VAL,2(R6)                                                    
         BE    PBUYX               PROCESS ONLY IF COST2 DIFFERS                
         MVC   COS2OLD,2(R6)                                                    
         MVC   2(4,R6),COS2VAL     REPLACE WITH THE NEW COS2 VALUE              
*                                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   PBUY05                                                           
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,KEY+14,P+2,4                                     
         GOTO1 REPORT                                                           
         L     R6,ADBUY                                                         
         LA    R7,16                                                            
PBUY03   GOTO1 =V(HEXOUT),DMCB,0(R6),P+2,16                                     
         GOTO1 =V(HEXOUT),DMCB,16(R6),P+36,16                                   
         GOTO1 REPORT                                                           
         LA    R6,32(R6)                                                        
         BCT   R7,PBUY03                                                        
*                                                                               
         L     R6,ADBUY                                                         
PBUY05   GOTO1 PUTBUY                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         ICM   R3,15,UPDSTA                                                     
         LA    R3,1(R3)                                                         
         STCM  R3,15,UPDSTA                                                     
*                                                                               
         ICM   R3,15,UPDALL                                                     
         LA    R3,1(R3)                                                         
         STCM  R3,15,UPDALL                                                     
*                                                                               
         BAS   RE,PRTBUY                                                        
*                                                                               
PBUYX    B     EXIT                                                             
*===============================================================*               
* REQLAST - PRINT OUT TOTAL NUMBER OF RECORDS UPDATED           *               
*===============================================================*               
*                                                                               
RLST     DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
                                                                                
         MVC   P+2(25),=C'TOTAL RECORDS UPDATED: '                              
         EDIT  (B4,UPDALL),(10,P+30),ZERO=NOBLANK                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================*               
*        PRINT BUY RECORDS                                                      
*===============================================================*               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
PRTB2    LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PMED,QMED                                                        
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BH    *+8                                                              
         MVI   PSTA+5,C'T'                                                      
*                                                                               
         EDIT  (B1,BUYKEST),(3,PEST),FILL=0                                     
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY          NEW LINE NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         L     R6,ADBUY                                                         
         MVI   ELCODE,X'73'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B4,2(R6)),PCOS2N,6,ALIGN=RIGHT,DROP=4                           
         EDIT  (B4,COS2OLD),PCOS2O,6,ALIGN=RIGHT,DROP=4                         
         GOTO1 REPORT                                                           
PRTBX    B     EXIT                                                             
         DROP  R6,R5                                                            
***********************************************************************         
         EJECT                                                                  
*                                                                               
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
INRECS   DC    F'0',CL20'RECORDS IN'                                            
OUTRECS  DC    F'0',CL20'RECORDS OUT'                                           
*                                                                               
         SPACE 2                                                                
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
* WORKING STORAGE AREA                                                          
*                                                                               
COS2VAL  DS    XL4                                                              
COS2OLD  DS    XL4                                                              
WKIO     DS    CL2000              WORKING STORAGE IO AREA                      
*                                                                               
*                                                                               
UPDSTA   DS    CL4                 NUMBER OF RECORDS UPDATED PER STA            
UPDALL   DS    CL4                 NUMBER OF RECORDS UPDATED TOTAL              
*                                                                               
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
ESTRECD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PMED     DS    CL1                                                              
         DS    CL3                                                              
PCLT     DS    CL3                                                              
         DS    CL3                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL3                                                              
PCOS2O   DS    CL8                                                              
         DS    CL1                                                              
PCOS2N   DS    CL8                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064SPREPC302 04/21/99'                                      
         END                                                                    
