*          DATA SET RESFM17    AT LEVEL 067 AS OF 06/29/93                      
*PHASE T81817A,*                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T81817 - RESFM17 - A.U.R. REPORT:  COMBO/MARKET'                
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM16 (T81817) --- A.U.R. REPORT: ERRORS ON FILE       *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* JUN29/93 (BU ) --- ORIGINAL ENTRY                               *             
*                                                                 *             
*                                                                 *             
*                    **  END TOMBSTONE  **                        *             
*******************************************************************             
*                                                                               
T81817   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1817**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R2,RELO                                                          
         SPACE                                                                  
         LA    RF,SAVEREGS                                                      
         STM   R2,RC,0(RF)         SAVE REGS 2 -> C                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PRNTREPT                                                         
         B     XIT                                                              
         EJECT                                                                  
XIT      XIT1                                                                   
         SPACE 2                                                                
FOOT     ST    RE,FULL             MOVE SCREEN INPUT TO FOOT LINE               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCFUT                                                        
         LA    R5,1(RE,R5)                                                      
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         ST    R5,SVFOOTF                                                       
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
MVCFUT   MVC   0(0,R5),8(R2)       MOVE SCREEN INPUT TO FOOT LINE               
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PRNTREPT EQU   *                                                                
         SPACE                                                                  
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,FHOOK                                                         
         ST    R1,FOOTHOOK                                                      
         MVI   FOOTLNS,1                                                        
         MVC   FOOTSW,FOOTLNS                                                   
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'2C'                                                        
         MVC   KEY+AREPE(2),AGENCY     INSERT REP CODE                          
         MVC   KEY+AGRPE(7),=C'RDKILTA'     **TEST**                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     PRRE0260                                                         
*                                                                               
PRRE0240 EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
PRRE0260 EQU   *                                                                
         CLC   KEY(13),KEYSAVE     **TEST**                                     
****     CLC   KEY(6),KEYSAVE      ID/REP                                       
         BNE   PRRE0999            DONE                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         USING RAURREC,R6                                                       
         ZICM  RF,RAURLEN,2                                                     
         LA    R2,0(RF,R6)         A(END OF RECORD)                             
         BCTR  R2,0                BACK UP 1 POSITION                           
         CLI   0(R2),0             IS LAST POSITION ZERO?                       
         BE    PRRE0240            YES - GO GET NEXT RECORD                     
         MVC   P+1(25),RAURREC     LOAD KEY                                     
         OI    P+14,X'F0'          TURN ON RECORD TYPE                          
         GOTO1 DATCON,DMCB,(3,RAURKYM),(0,P+30)                                 
         GOTO1 SPOOL,PARAS,(R8)                                                 
         B     PRRE0240            GO GET NEXT RECORD                           
PRRE0999 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*  FOOT HOOK ROUTINE FOR REQUEST DETAILS                                        
FHOOK    NTR1                                                                   
         USING FHOOK,RF                                                         
         LA    R1,SAVEREGS-FHOOK    RESTORE REGISTERS                           
         AR    R1,RF                                                            
         DROP  RF                                                               
         LM    R2,RC,0(R1)         LOAD R2 -> RC                                
         MVC   P,SPACES                                                         
         CLI   FOOTSW,0            HAVE I ALREADY PRINTED EVERYTHING            
         BE    XIT                 YES                                          
         L     R5,SVFOOTF                                                       
         LA    R1,SVFOOT                                                        
         CR    R5,R1               IS THERE ANYTHING TO PRINT                   
         BE    FH40                NO                                           
         BCTR  R5,0                YES, BACK UP AND BLANK OUT COMMA             
         CLI   0(R5),C','                                                       
         BE    FH20                                                             
         CLI   0(R5),C' '          OR MAY ALREADY BE BLANKED OUT                
         BE    FH20                                                             
         DC    H'0'                SOMETHING IS WRONG                           
         SPACE 1                                                                
FH20     MVI   0(R5),C' '                                                       
         MVC   P(8),=C'DETAILS-'                                                
         MVC   P+8(101),SVFOOT                                                  
FH40     MVI   FOOTSW,0            INDICATE NO MORE TO PRINT                    
         B     XIT                                                              
         EJECT                                                                  
*  HOOK ROUTINE FOR HEADLINE DETAIL                                             
         SPACE 1                                                                
HOOK     NTR1                                                                   
         USING HOOK,RF                                                          
         LA    R1,SAVEREGS-HOOK    RESTORE REGISTERS                            
         AR    R1,RF                                                            
         DROP  RF                                                               
         LM    R2,RC,0(R1)         LOAD R2 -> RC                                
HK2      EQU   *                                                                
         MVC   H8+15(6),=C'LENGTH'                                              
         MVC   H9+15(6),DASH                                                    
         CLI   REPTYP,C'L'                                                      
         BNE   HK3                                                              
         MVC   H1+17(6),=C'LENGTH'                                              
HK3      CLI   REPTYP,C'P'                                                      
         BNE   HK5                                                              
         MVC   H1+17(6),=C'PERIOD'                                              
         CLI   MONFMT,C'A'                                                      
         BE    *+16                                                             
         MVC   H8+24(4),=C'DATE'                                                
         MVC   H9+23(6),DASH                                                    
         TM    PRNTOPT,X'20'       NO LENGTH                                    
         BZ    HK5                                                              
         MVC   H8+15(6),SPACES                                                  
         MVC   H9+15(6),SPACES                                                  
*                                                                               
HK5      MVC   H5+17(20),SVMKT                                                  
HK10     MVC   H5+9(7),AUCSTA                                                   
         SPACE 1                                                                
HK20     LA    R2,H5+84                                                         
         GOTO1 DATCON,DMCB,(3,SMONTH),(6,0(R2))   START MONTH                   
         CLC   SMONTH,EMONTH                                                    
         BE    HKXIT                                                            
         LA    R2,6(R2)                                                         
         MVI   0(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(3,EMONTH),(6,1(R2))     END MONTH                   
HKXIT    B     XIT                                                              
SAVEREGS DS    11F                                                              
         SPACE 2                                                                
DASH     DC    10C'-'                                                           
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
MYEND    MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*  MY OWN ERROR MESSAGES                                                        
         SPACE 2                                                                
GRPORSTA DC    C'EITHER GROUP OR STATION REQUIRED'                              
STAORMKT DC    C'STATION OR MKT=CODE REQUIRED'                                  
MKTONLY  DC    C'NOT MKT= - CAN NOT FILTER ON OWNER,TVB,RANK'                   
RANKE2   DC    C'RANK MUST BE FROM 1-7'                                         
TYPER    DC    C'REPORT TYPE L MUST BE WHOLE MONTH'                             
INVFMT   DC    C'INVALID FORMAT AT THIS TIME'                                   
MANYDPT  DC    C'MAXIMUM 4 DAYPARTS ALLOWED'                                    
MNYLEN   DC    C'MAXIMUM 4 LENGTHS ALLOWED'                                     
MANYLENP DC    C'FOR REPORT TYPE P, ONLY 1 LENGTH ALLOWED'                      
DATEFMT  DC    C'FORMAT IS MMM/YY'                                              
NOTQTR   DC    C'DATES MUST INCLUDE COMPLETE QUARTERS'                          
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,C'RATE ANALYSIS BY'                                         
         PSPEC H2,1,23C'-'                                                      
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REPORT                                                     
         PSPEC H2,92,REQUESTOR                                                  
         PSPEC H5,1,C'STATION'                                                  
         PSPEC H5,76,C'PERIOD'                                                  
         PSPEC H8,1,C'DAYPART'                                                  
         PSPEC H9,1,C'--------------'                                           
         PSPEC H7,33,C'- - - - UNITS - - - -'                                   
         PSPEC H8,33,C'PRIOR   CURRENT INDEX'                                   
         PSPEC H9,33,C'---------------------'                                   
         PSPEC H7,57,C'- - - - RATE - - - - '                                   
         PSPEC H8,57,C'PRIOR   CURRENT INDEX'                                   
         PSPEC H9,57,C'---------------------'                                   
         PSPEC H7,81,C'- - - - -  BILLING  - - - -'                             
         PSPEC H8,81,C'PRIOR      CURRENT    INDEX'                             
         PSPEC H9,81,C'---------------------------'                             
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*   WORK AREA                                                                   
         SPACE 2                                                                
         EJECT                                                                  
PRNTD    DSECT                                                                  
PDPT     DS    CL2                 DAYPART                                      
         DS    CL2                                                              
PDPTN    DS    CL11                DAYPART NAME                                 
         DS    CL2                                                              
PLEN     DS    CL4                 LENGTH                                       
         DS    CL2                                                              
PYR      DS    CL6                 MONTH/YEAR                                   
         DS    CL3                                                              
PURB     DS    0CL1                UNITS/RATE/BILLING                           
         SPACE 2                                                                
PUNDLD   DSECT                                                                  
PUPRI    DS    CL7                 PRIOR UNITS                                  
         DS    CL1                                                              
PUCUR    DS    CL7                 CURRENT UNITS                                
         DS    CL1                                                              
PUIND    DS    CL4                 INDEX UNITS                                  
         DS    CL4                                                              
PRPRI    DS    CL7                 PRIOR RATE                                   
         DS    CL1                                                              
PRCUR    DS    CL7                 CURRENT RATE                                 
         DS    CL1                                                              
PRIND    DS    CL4                 INDEX RATE                                   
         DS    CL4                                                              
PBPRI    DS    CL10                PRIOR BILLING                                
         DS    CL1                                                              
PBCUR    DS    CL10                CURRENT BILLING                              
         DS    CL1                                                              
PBIND    DS    CL4                 INDEX BILLING                                
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         SPACE 2                                                                
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
* RESFMFCD                                                                      
         SPACE 2                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMECD                                                       
         EJECT                                                                  
* REGENSTA                                                                      
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
* REGENATNA                                                                     
       ++INCLUDE REGENAUR                                                       
         EJECT                                                                  
* REGENSDD                                                                      
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
* REGENDPT                                                                      
       ++INCLUDE REGENDPT                                                       
         EJECT                                                                  
* REGENPGT                                                                      
       ++INCLUDE REGENPGT                                                       
         EJECT                                                                  
* RESFMWORKD                                                                    
       ++INCLUDE RESFMWORKD                                                     
         SPACE 2                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*                  ---  WORK AREA ---                                           
         DS    0F                                                               
MYWORK   DS    0CL768                                                           
UNDL     DS    0CL8                                                             
DLACCM   DS    F                   DOLLAR ACCUM                                 
UNACCM   DS    F                   UNIT ACCUM                                   
PRIORSW  DS    CL1                 Y = RECORD IS PRIOR MONTH                    
GROUP    DS    CL2                 GROUP                                        
STATN    DS    CL5                 STATION                                      
FILTER   DS    CL1                 ATHENA RECORD TYPE  (FILTER)                 
*                                  1=ALL (BLANK)                                
*                                  2=SERVICE (S)                                
*                                  3=CATEGORY (C)                               
*                                  4=ADVERTISER (A)                             
*                                                                               
TYPCODE  DS    CL9                 ADV/SERVICE/CATEGORY CODE                    
PSMONTH  DS    CL2                 PRIOR START YEAR/MONTH (BINARY)              
PEMONTH  DS    CL2                 PRIOR END YEAR/MONTH (BINARY)                
SMONTH   DS    CL2                 START MONTH                                  
EMONTH   DS    CL2                 END MONTH                                    
REPTYP   DS    CL1                 REPORT TYPE                                  
*                                   L=LENGTH                                    
*                                   P=PERIOD                                    
*                                   D=DAYPART                                   
*                                                                               
OPT$$$   DS    CL1                 DOLLAR/UNIT TYPE                             
*                                  R  =  REGULAR DOLLARS/UNITS                  
*                                  C  =  COMBO   DOLLARS/UNITS                  
*                                  B  =  BOTH COMBO + REGULAR                   
TOTPAGE  DS    CL1                 Y  =  TOTAL PAGE ONLY                        
DPT      DS    CL8                 DAYPARTS                                     
         DS    CL1                 ENDING 0                                     
DPTCNT   DS    CL1                 DAYPART COUNTER                              
LEN      DS    CL8                 LENGTHS                                      
         DS    CL2                 ENDING 0                                     
LENCNT   DS    CL1                 LENGTH COUNTER                               
PRNTOPT  DS    CL1                 X'80'  SUPPRESS 2NDARY DAYPART               
*                                  X'40'  STATION TOTALS ONLY                   
*                                  X'20'  LENGTH = TOT                          
*                                  X'10'  LIMITED ACCESS                        
*                                                                               
OWNER    DS    CL3                 OWNER                                        
OWNERN   DS    CL20                OWNER NAME                                   
TVB      DS    CL2                 TVB REGION                                   
TVBN     DS    CL18                TVB REGION NAME                              
RANK     DS    CL1                 RANK                                         
MARKET   DS    CL20                MARKET NAME                                  
SVMKT    DS    CL20                MARKET NAME (SAVED FOR PRINTING)             
GROUPN   DS    CL10                GROUP NAME                                   
         DS    CL1               FOR SPACE BTWN GROUP & SUBGROUP NAME           
SGROUPN  DS    CL10                SUB GROUP NAME                               
MONFMT   DS    CL1                 FORMAT                                       
*                                     M=MONTH                                   
*                                     Q=QUARTER                                 
*                                     A=WHOLE MONTH                             
         SPACE 1                                                                
ASAT     DS    CL2                 COMPRESSED AS AT DATE                        
CURPRG   DS    CL11                CURRENT PROGRAM NAME                         
CURDPTC  DS    CL1                 CURRENT DAYPART CODE                         
CURDPT   DS    CL5                 CURRENT DAYPART NAME                         
CURSDPT  DS    CL5                 CURRENT SECONDARY DAYPART NAME               
FOOTSW   DS    CL1                                                              
MULTSDPT DS    XL1                 X'80'  1 OR MORE SUB DAYPARTS                
*                                  X'40'  2 OR MORE SUB DAYPARTS                
         DS    0F                                                               
TOTACCM  DS    0CL16               TOTAL ACCUMULATOR                            
TOTPUN   DS    F                   PRIOR UNITS                                  
TOTUN    DS    F                   UNITS                                        
TOTPDL   DS    F                   PRIOR DOLLARS                                
TOTDL    DS    F                   DOLLARS                                      
         DS    0F                                                               
SVATNKY  DS    CL34                SAVE KEY                                     
SVKEY    DS    CL34                SAVE KEY                                     
SAVDPT   DS    CL1                 LAST DAYPART FILTER PRINTED                  
SAVSDPT  DS    CL2                 LAST SUB DAYPART/PROG TYPE PRINTED           
SAVLEN   DS    CL2                 LAST LENGTH PRINTED                          
SVADPTF  DS    A                   A(LAST DAYPART FILTER READ)                  
SVALENF  DS    A                   A(LAST LENGTH FILTER READ)                   
SVFOOTF  DS    A                   A(NEXT POSITION IN FOOTLINE)                 
SAVERE   DS    F                                                                
SAVEE    DS    F                                                                
TOTPRNT  DS    CL1                 Y=SOMETHING TO PRINT FOR LEVEL               
SVFOOT   DS    CL132                                                            
         SPACE 2                                                                
         DS    0F                                                               
BUFREC   DS    0CL56                                                            
BUFKEY   DS    0CL16                                                            
BUFTYP   DS    CL1                                                              
BUFSTA   DS    CL5                                                              
BUFDPT   DS    CL1                                                              
BUFSDPT  DS    CL2                                                              
BUFLEN   DS    CL2                                                              
BUFYR    DS    CL1                                                              
BUFMON   DS    CL1                                                              
         DS    CL3                 NOT DEFINED                                  
BUFDPTN  DS    CL15                                                             
BUFSTA2  DS    CL5                                                              
BUFTOTAL DS    CL4                                                              
         DS    0F                                                               
BUFACCM  DS    0CL16                                                            
BUFPUN   DS    F                                                                
BUFUN    DS    F                                                                
BUFPDL   DS    F                                                                
BUFDL    DS    F                                                                
*                                                                               
BUFREC2  DS    CL(*-BUFREC)                                                     
*                                                                               
BUFREC3  DS    CL(*-BUFREC)                                                     
*                                                                               
STATLIST DS    24CL7               24 GROUP/STATION ENTRIES MAX                 
STADELIM DS    XL1                 DELIMITER                                    
COMBOFLG DS    CL1                                                              
SECVIOL  DS    CL1                 SECURITY VIOLATION FLAG                      
SAVMKTCD DS    CL4                 MARKET CODE FROM X'2B' RECORD                
ASTATN   DS    A                   A(STATION IN PROGRESS)                       
STATCTR  DS    F                   STATION COUNTER                              
STATPROG DS    F                   STATION IN PROGRESS                          
ABFTABLE DS    A                   A(BUFFER PRINT AREA)                         
*                                                                               
AREPE    EQU   RAURKREP-RAURKEY    REP                                          
AGRPE    EQU   RAURKGRP-RAURKEY    GROUP                                        
ASTAE    EQU   RAURKSTA-RAURKEY    STATION                                      
ATPEE    EQU   RAURKTPE-RAURKEY    RECORD TYPE (FILTER)                         
ATCPE    EQU   RAURKTCD-RAURKEY    TYPE CODE                                    
AAGYE    EQU   RAURKAGY-RAURKEY    AGENCY                                       
ACTYPE   EQU   RAURKCTP-RAURKEY    CONTRACT TYPE                                
AOFFE    EQU   RAURKOFF-RAURKEY    OFFICE                                       
ADPTE    EQU   RAURKDPT-RAURKEY    DAYPART                                      
ASDTE    EQU   RAURKSDT-RAURKEY    SUB DAYPART                                  
APRGE    EQU   RAURKPRG-RAURKEY    PROGRAM CODE                                 
ASLNE    EQU   RAURKSLN-RAURKEY    SPOT LENGTH                                  
AYME     EQU   RAURKYM-RAURKEY     YEAR/MONTH                                   
*                                                                               
         EJECT                                                                  
T81816   CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*TVB TABLE                                                                      
       ++INCLUDE RETVBTAB                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067RESFM17   06/29/93'                                      
         END                                                                    
