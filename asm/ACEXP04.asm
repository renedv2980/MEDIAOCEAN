*          DATA SET ACEXP04    AT LEVEL 012 AS OF 02/10/98                      
*PHASE T61504A,+0                                                               
         TITLE 'T61504 - LIST OF INVOICES'                                      
T61504   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61504,RR=R2                                                   
         L     RC,0(,R1)            RC=GENCON STORAGE AREA                      
*                                                                               
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             RA=A(TWA)                                    
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
*                                                                               
         USING T615FFD,RA                                                       
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R6,SAVXTWA          R6=LOCAL SAVED STORAGE                       
         A     R6,=AL4(SAVXDSP)    DISPLACEMENT TO 'LIST' SAVE AREA             
*                                                                               
         USING LWSD,R6                                                          
*                                                                               
         ST    R2,RELO                                                          
         GOTO1 AUTH                                                             
         EJECT ,                                                                
*              INITIAL ROUTINES                                                 
         CLI   MODE,VALKEY         VALIDATE                                     
         BNE   INVLST5                                                          
         XC    MYKEY(LWSLEN1),MYKEY START AGAIN - CLEAR SAVED STORAGE           
         LA    R4,KEY                  BUILD THE RECORD KEY                     
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SE'                                             
         SPACE 1                                                                
*              VALIDATE ACN NUMBER                                              
VALACN   LA    R2,LOGACNH                                                       
         GOTO1 ANY                                                              
         MVC   ACKEYACC+3(5),WORK                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   ACN,WORK                                                         
         SPACE 1                                                                
*              VALIDATE AGENCY IF INPUT                                         
VALAGY   LA    R2,LOGAGYH                                                       
         CLI   5(R2),0                                                          
         BE    VALPRD              NO INPUT IS OK                               
         GOTO1 ANY                                                              
         MVC   ACKEYACC+8(3),WORK                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   AGYCDE,WORK                                                      
         SPACE 1                                                                
*              VALIDATE PRODUCT IF INPUT                                        
VALPRD   LA    R2,LOGPRDH                                                       
         CLI   5(R2),0                                                          
         BE    VALMED                                                           
         GOTO1 ANY                                                              
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'3P'                                             
         MVC   ACKEYACC+3(2),WORK                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   PRD,WORK                                                         
         SPACE 1                                                                
*              VALIDATE MEDIA                                                   
VALMED   LA    R2,LOGMEDH                                                       
         CLI   5(R2),0                                                          
         BE    VALVEH                                                           
         GOTO1 ANY                                                              
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'3M'                                             
         MVC   ACKEYACC+3(2),WORK                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   MEDIA,WORK                                                       
         SPACE 1                                                                
*              VALIDATE VEHICLE                                                 
VALVEH   LA    R2,LOGVEHH                                                       
         CLI   5(R2),0                                                          
         BE    VALAPP                                                           
         CLI   LOGMEDH+5,0                                                      
         BE    FLDINV              CAN'T HAVE VEHICLE WITHOUT MEDIA             
         GOTO1 ANY                                                              
         MVC   ACKEYACC+5(8),WORK                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   VEHICLE,WORK                                                     
         SPACE 1                                                                
*              VALIDATE APPROVE FIELD                                           
VALAPP   LA    R2,LOGAPPH                                                       
         CLI   5(R2),0                                                          
         BE    VALPERD             APPROVED AND UNAPPROVED                      
         CLI   LOGAPP,C'N'                                                      
         BE    *+12                                                             
         CLI   LOGAPP,C'Y'                                                      
         BNE   FLDINV                                                           
         MVC   APP,LOGAPP                                                       
         SPACE 1                                                                
*              VALIDATE PERIOD FIELD                                            
VALPERD  XC    STDATE,STDATE                                                    
         MVC   ENDATE,=3X'FF'                                                   
         LA    R2,LOGPERH                                                       
         CLI   5(R2),0                                                          
         BE    VALAST                                                           
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),C',=,-'                              
         CLI   DMCB+4,1                                                         
         BNE   FLDINV                                                           
         GOTO1 DATVAL,DMCB,(2,BLOCK+12),WORK                                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERR13               INVALID DATE FORMAT                          
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,STDATE)                                  
         MVC   ENDATE,STDATE       IF ONLY ONE START = END                      
         CLI   BLOCK+1,0                                                        
         BE    VALAST              NO SECOND DATE                               
         GOTO1 DATVAL,DMCB,(2,BLOCK+22),WORK                                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERR13               INVALID DATE FORMAT                          
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,ENDATE)                                  
         CLC   STDATE,ENDATE       START DATE <= END DATE                       
         BH    ERR13               NO,   INVALID DATE                           
         SPACE 1                                                                
*              END OF VALIDATION SET LASTKEY                                    
VALAST   MVC   LASTKEY,SPACES                                                   
         LA    R4,LASTKEY                                                       
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SE'                                             
         MVC   ACKEYACC+3(5),ACN                                                
         MVC   ACKEYACC+8(3),AGYCDE                                             
VALX     B     XIT                                                              
         EJECT                                                                  
*              GENERATE LISTING                                                 
INVLST5  CLI   MODE,LISTRECS                                                    
         BNE   XIT                                                              
         OC    KEY,KEY                                                          
         BNZ   INVLST7                                                          
         MVC   KEY,LASTKEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                READ LEDGER RECORD TO START                  
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
INVLST7  MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
INVLST9  L     R4,AIO                                                           
         CLC   LASTKEY(8),0(R4)    IF COMP/U/L ACN                              
         BNE   INVLSTX             GET OUT                                      
         OC    AGYCDE,AGYCDE                                                    
         BZ    *+14                NO AGENCY FILTER                             
         CLC   ACKEYACC+8(3),AGYCDE                                             
         BNE   INVLST7            AGENCY DOES NOT MATCH FILTER GET NEXT         
         OC    PRD,PRD                                                          
         BZ    *+14                NO PRDUCT FILTER                             
         CLC   ACKEYCON+3(2),PRD                                                
         BNE   INVLST7          PRODUCT DOES NOT MATCH FILTER GET NEXT          
         OC    MEDIA,MEDIA                                                      
         BZ    *+14                NO MEDIA  FILTER                             
         CLC   ACKEYCON+5(2),MEDIA                                              
         BNE   INVLST7          MEDIA DOES NOT MATCH FILTER GET NEXT            
         OC    VEHICLE,VEHICLE                                                  
         BZ    *+14                NO VEHICLE  FILTER                           
         CLC   ACKEYCON+7(8),VEHICLE                                            
         BNE   INVLST7          VEHICLE DOES NOT MATCH FILTER GET NEXT          
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'44',(R4)),0                                         
         CLI   ELERR,0                                                          
         BNE   INVLST7             NOT A TRANSACTION GET NEXT                   
         L     R2,ELADDR                                                        
         SPACE 1                                                                
         USING TRANSD,R2                                                        
         TM    TRNSSTAT,X'20'                                                   
         BO    INVLST7             OFFSETTING                                   
         CLC   TRNSDATE(2),STDATE                                               
         BL    INVLST7             TRANSACTION DATE BEFORE START                
         CLC   TRNSDATE(2),ENDATE                                               
         BH    INVLST7             TRANSACTION DATE AFTER END                   
         GOTO1 GETL,DMCB,(X'60',(R4)),0                                         
         L     R2,ELADDR                                                        
         USING TRSTATD,R2                                                       
         MVI   APP2,C'N'           ASSUME NOT APPROVED                          
         OC    TRSTUPDT,TRSTUPDT                                                
         BZ    *+8                                                              
         MVI   APP2,C'Y'           ITS APPROVED                                 
         CLI   APP,0                                                            
         BE    INVLST10            APPROVED AND UNAPPROVED                      
         CLI   APP,C'Y'                                                         
         BNE   INVLST9N                                                         
         CLI   APP2,C'Y'                                                        
         BE    INVLST10            AND I WANT APPROVED                          
         B     INVLST7             ITS NOT APPROVED                             
INVLST9N CLI   APP2,C'N'                                                        
         BE    INVLST10            ITS NOT APPROVED TAKE IT                     
         B     INVLST7             ITS NOT APPROVED                             
         SPACE 1                                                                
*              BUILD THE OUTPUT LINE                                            
         USING LINED,R7                                                         
INVLST10 LA    R7,LISTAR                                                        
         MVC   LASTKEY,0(R4)          SAVE KEY AND DISPLAY                      
         MVC   LINED(LINLEN),SPACES                                             
         MVC   LINACN,ACKEYACC+3      ACN                                       
         MVC   LINAGY,ACKEYACC+8      AGENCY                                    
         MVC   LINPRD,ACKEYCON+3      PRODUCT                                   
         MVC   LINMED,ACKEYCON+5      MEDIA                                     
         MVC   LININV,ACKEYREF        INVOICE                                   
         MVC   LINAPP,APP2                                                      
         GOTO1 GETL,DMCB,(X'44',(R4)),0                                         
         L     R2,ELADDR                                                        
         SPACE 1                                                                
         USING TRANSD,R2                                                        
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(6,LINMTH)                              
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'3M'                                                  
         MVC   KEY+3(10),ACKEYCON+5  MEDIA/VEHICLE KEY                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LINVEH,WORK            VEHICLE NAME                              
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'50',(R4)),0                                         
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO GROSS 50 ELEMENT                          
         L     R2,ELADDR                                                        
         SPACE 1                                                                
         USING TRCASHD,R2                                                       
         EDIT  (P6,TRCSGRS),(10,LINAMT),2,MINUS=YES                             
         SPACE 1                                                                
         MVC   KEY,IO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                RE-READ I BROKE SEQ WITH 3M READ             
         GOTO1 LISTMON             LET CONTROLLER KEEP TRACK                    
         B     INVLST7             AND TRY NEXT                                 
         SPACE 1                                                                
INVLSTX  B     XIT                 RETURN TO CONTROLLER                         
         EJECT ,                                                                
*              EXIT ROUTINES                                                    
         SPACE 1                                                                
ERR13    MVI   ERROR,13            INVALID DATE FORMAT                          
*        B     ACMESG                                                           
         SPACE 1                                                                
ACMESG   MVI   GETMSYS,6           ACCOUNT MESSAGES  (SYST 6)                   
         OI    GENSTAT2,USMYERSY                                                
         B     THEEND                                                           
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID                                                    
*        B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT                                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT ,                                                                
*              CONSTANTS, LITERAL POOL, ETC.                                    
         SPACE 1                                                                
*        COMPLETION MESSAGES                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
*              DSECT TO COVER SCREEN LINE                                       
         SPACE 1                                                                
*                                  ACTION=LIST USE A(SCREEN LINE)               
         SPACE 1                                                                
LINED    DSECT                                                                  
LINACN   DS    CL5                 BOTTLER (ACN)                                
         DS    CL2                                                              
LINAGY   DS    CL3                 AGENCY CODE                                  
         DS    CL2                                                              
LINPRD   DS    CL2                 PRODUCT                                      
         DS    CL2                                                              
LINMED   DS    CL2                 MEDIA                                        
         DS    CL2                                                              
LINVEH   DS    CL22                VEHICLE                                      
         DS    CL2                                                              
LINMTH   DS    CL6                 MONTH                                        
         DS    CL1                                                              
LININV   DS    CL6                 INVOICE                                      
         DS    CL1                                                              
LINAMT   DS    CL10                GROSS AMOUNT                                 
         DS    CL3                                                              
LINAPP   DS    CL1                 'Y' OR 'N'                                   
LINLEN   EQU   *-LINED                                                          
         EJECT ,                                                                
*              LOCAL SAVED STORAGE                                              
         SPACE 1                                                                
LWSD     DSECT                                                                  
RELO     DS    F                                                                
         SPACE 1                                                                
MYKEY    DS    CL(L'KEY)           START CLEAR STORAGE FROM HERE                
LASTKEY  DS    CL(L'KEY)           LAST KEY FOR PAGING                          
         SPACE 1                                                                
ACN      DS    CL5                                                              
AGYCDE   DS    CL3                                                              
PRD      DS    CL2                                                              
MEDIA    DS    CL2                                                              
VEHICLE  DS    CL8                                                              
STDATE   DS    CL3                                                              
ENDATE   DS    CL3                                                              
APP      DS    CL1                                                              
APP2     DS    CL1                                                              
LWSLEN1  EQU   *-MYKEY             LENGTH OF STORAGE TO BE CLEARED              
LWSLEN   EQU   *-LWSD              LENGTH OF SAVED STORAGE                      
         EJECT ,                                                                
       ++INCLUDE ACEXPFFD                                                       
         EJECT ,                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE ACEXPFBD                                                       
         EJECT ,                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT ,                                                                
* ACEXPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACEXPWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACEXP04   02/10/98'                                      
         END                                                                    
