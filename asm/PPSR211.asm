*          DATA SET PPSR211    AT LEVEL 012 AS OF 09/14/15                      
*PHASE T42111A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T42111 - PRINTPAK ENHANCE SPACE RESERVATION, FMT BUY'           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BPLA 09/15    NEW B2B PROFILE OPTION TO SOME REGULAR COMMENTS                 
*               SUPREGCM                                                        
*                                                                               
* KWAN 08/01/07 BUY MOVE INSERTION CHANGES                                      
*                                                                               
* KWAN 01/09/06 INSERTION RATE                                                  
*                                                                               
* KWAN 11/22/05 FIX INSERTION COST AND PREMIUM (PPBYOUT INIT PROBLEM)           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T42111   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PPSR211X-PPSR211D,T42111,RR=RE,CLEAR=YES                         
*                                                                               
         LR    R6,RC                                                            
         USING PPSR211D,R6                                                      
*                                                                               
         ST    RE,RELO11                                                        
*                                                                               
         L     RC,0(R1)                                                         
         USING POLWRKD,RC                                                       
         USING T421FFD,RA                                                       
         USING ESRWORKD,R8                                                      
         L     R9,PPFILEC                                                       
         USING POLFILED,R9                                                      
*                                                                               
         LA    R7,BUYOUTA                                                       
         USING PPBYOUTD,R7                                                      
         BRAS  RE,INITWKST                                                      
*                                                                               
         XC    WKINSCNT,WKINSCNT                                                
         LA    R5,BUYDALST                                                      
*                                                                               
F_INS20  OC    0(4,R5),0(R5)       NO MORE INSERTIONS?                          
         BZ    EXXMOD                                                           
         MVC   KEY+27(4),0(R5)     INSERTION DISK ADDRESS                       
         MVC   TMPFULL1,AREC       SAVE ORIGINAL AIO POINTER                    
         LA    RE,PBUYREC                                                       
         ST    RE,AREC                                                          
         GOTOR GETPRT                                                           
         MVC   AREC,TMPFULL1       RESTORE ORIGINAL AIO POINTER                 
         ICM   RE,15,WKINSCNT                                                   
         AHI   RE,1                                                             
         STCM  RE,15,WKINSCNT                                                   
         CHI   RE,BUYDAMXQ                                                      
         BL    *+6                                                              
         DC    H'0'                TABLE MAX ERROR ENCOUNTERED                  
*                                                                               
         BRAS  RE,INICHGTB                                                      
*                                                                               
         GOTOR GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD,0                             
*                                                                               
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    RE,PBUYREC                                                       
         STCM  RE,15,PBYOINPT                                                   
         L     RE,DATCON                                                        
         STCM  RE,15,PBYODTCN                                                   
         LA    RE,GROSS                                                         
         STCM  RE,15,PBYOVALS                                                   
         OI    PBYOCTL,X'03'       NO LINE # IN INS DATE AND NET RATES          
*                                                                               
         GOTOR APPBYOUT,DMCB,BUYOUTA                                            
*                                                                               
         BRAS  RE,RPYESRDL         REPLY ESR INSERTION DETAIL                   
         BRAS  RE,ESRTRNCM         ESR TRANSITION COMMENT                       
         BRAS  RE,BYMVTRCM         BUY MOVE TRANSITION COMMENT                  
         BRAS  RE,RPYACHRG         REPLY ADDTIONAL CHARGES                      
         BRAS  RE,CCOLRTN          CUSTOM COLUMNS                               
         BRAS  RE,REVCOMNT         REASONS FOR REVISION                         
         BRAS  RE,CONCOMNT         CONTRACT COMMENT                             
         BRAS  RE,INSREGCM         INSERTION REGULAR COMMENT                    
         BRAS  RE,POSINSCM         POSITION INSTRUCTION COMMENT                 
*                                                                               
         LA    R5,4(R5)            NEXT DISK ADDRESS                            
         B     F_INS20                                                          
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R2),0                                                          
         JNE   NXTEL                                                            
         LTR   R2,R2               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITWKST NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGE                   
*                                                                               
         LR    RE,R6                                                            
         A     RE,=A(CONR_AIO-PPSR211D)                                         
         ST    RE,A_CONREC                                                      
*                                                                               
         LR    RE,R6                                                            
         A     RE,=A(JOBR_AIO-PPSR211D)                                         
         ST    RE,A_JOBREC                                                      
                                                                                
*                                                                               
         XC    RPYERTXT,RPYERTXT                                                
         MVC   RPYERTXT(16),=C' ** !!ERROR!! **'                                
*                                                                               
         XC    SVPUBZEC,SVPUBZEC   INIT PUB, ZONE AND EDITION                   
         XC    SVPUBZNM,SVPUBZNM   PUBLICATION ZONE NAME                        
         XC    SVCSTDCM,SVCSTDCM   CONTRACT STANDARD COMMENT CODE               
         XC    SVBSTDCM,SVBSTDCM   BUY STANDARD COMMENT CODE                    
*                                                                               
         XC    SVCONKEY,SVCONKEY   INIT CONTRACT RECORD FIELDS                  
         XC    D_CONNUM,D_CONNUM   CONTRACT NUMBER                              
         XC    D_CONSIG,D_CONSIG   CONTRACT AGENCY SIGNER                       
         XC    D_CONSDT,D_CONSDT   CONTRACT START DATE                          
         XC    D_CONEDT,D_CONEDT   CONTRACT END DATE                            
         XC    D_CONLVL,D_CONLVL   CONTRACT LEVEL                               
         XC    D_CONEFD,D_CONEFD   CONTRACT EFFECTIVE DATE                      
         XC    D_CONPCT,D_CONPCT   CONTRACT PERCENT DISCOUNT                    
*                                                                               
         XC    SVJOBKEY,SVJOBKEY   INIT AD RECORD FIELDS                        
         XC    D_ADCODE,D_ADCODE   AD CODE                                      
         XC    D_ADID,D_ADID       AD ID                                        
         XC    D_CPYNUM,D_CPYNUM   AD COPY NUMBER                               
         XC    D_ADCAP1,D_ADCAP1   AD CAPTION 1                                 
         XC    D_ADCAP2,D_ADCAP2   AD CAPTION 2                                 
*                                                                               
         ZAP   PACK1BIL,=P'1000000000'                                          
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMT_NSPD NTR1  BASE=*,LABEL=*      FORMAT NEWSPAPER SPACE DESCRIPTION           
*                                                                               
         LA    R4,TMPWK1                                                        
         XC    TMPWK1,TMPWK1                                                    
         CP    PBDUNITS,=P'0'                                                   
         BE    FB4D                                                             
         MVI   0(R4),C'0'                                                       
         LA    R0,1                                                             
         ZAP   DUB,PBDUNITS                                                     
         BZ    *+8                                                              
         BRAS  RE,FBEDL                                                         
         AR    R4,R0                                                            
         MVI   0(R4),C'L'          LINES                                        
         CLI   PBDUIND,C'L'                                                     
         BE    FB4D                                                             
         MVC   0(2,R4),=C'IN'      INCHES                                       
         LA    R4,1(R4)                                                         
*                                                                               
FB4D     CLI   PBDCL,C' '          PREM                                         
         BNH   *+14                                                             
         MVC   2(1,R4),PBDCL                                                    
         MVI   3(R4),C'C'                                                       
         J     EXIT                                                             
*                                                                               
FBEDL    DS    0H                                                               
         CLI   PBDUIND,X'89'       INCHES - 2 DECIMALS                          
         JNE   FBEDL5                                                           
         EDITR (P8,DUB),(10,(R4)),2,ALIGN=LEFT                                  
         J     FBEDLX                                                           
*                                                                               
FBEDL5   EDITR (P8,DUB),(10,(R4)),ALIGN=LEFT                                    
FBEDLX   BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ESRTRNCM NTR1  BASE=*,LABEL=*      ESR TRANSITION COMMENT                       
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    EXIT                NO                                           
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         J     EXIT                                                             
*                                                                               
RPY#SRTC LR    R0,RE                                                            
         TM    ADRPYSW3,RPYTRCMQ   SUB REC REPLY CODE REPLIED?                  
         JNZ   RPY#SRTX                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#SRTCOM)              
         OI    ADRPYSW3,RPYTRCMQ                                                
RPY#SRTX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
LAST_CHR CLI   0(RF),0                                                          
         JE    *+8                                                              
         CLI   0(RF),C' '                                                       
         JNE   *+10                                                             
         BCTR  RF,0                                                             
         J     *-18                                                             
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CCOLRTN  NTR1  BASE=*,LABEL=*      CUSTOM COLUMN                                
*                                                                               
         MVC   TMPSVKEY,KEY        SAVE BUY KEY                                 
*                                                                               
         LA    R2,PBUYREC+33                                                    
         USING BYCCELD,R2          BUY CUSTOM COLUMN ELEMENT                    
         MVI   ELCODE,BYCCIDQ                                                   
*                                                                               
CCOL20   BRAS  RE,NXTEL                                                         
         BNE   CCOLX               DONE WITH CUSTOM COLUMNS                     
*                                                                               
         CLC   BYCCSQN,=X'2000'    STANDARIZED CUSTOM COLUMN?                   
         BL    CCOL20F                                                          
         XC    GKEY,GKEY                                                        
C        USING GCOLPKEY,GKEY                                                    
         MVC   C.GCOLPRID,=AL3(GCOLPRIQ)  SET GENDIR PASSIVE KEY ID             
         MVI   C.GCOLPMED,C'A'       MEDIA ALWAYS A FOR CUST COL'S              
         MVI   C.GCOLPRCD,GCOLPRCQ   RECORD CODE FOR PASSIVE KEY                
         MVC   C.GCOLPSQN,BYCCSQN    SEQUENCE NUMBER                            
         XC    C.GCOLPSQN,=X'FFFF'   COMPLEMENT SEQUENCE NUMBER                 
         MVC   GKEYSAVE,GKEY                                                    
         GOTOR VDATAMGR,DMCB,(DMINBTS,=C'DMRDHI'),=C'GENDIR',          X        
               GKEY,GKEY,(TERMNAL,0)                                            
*                                                                               
         CLC   C.GCOLPKEY,GKEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         DROP  C                                                                
*                                                                               
         GOTOR VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'GENFILE',         X        
               GKEY+36,TMPWKAIO,(TERMNAL,DMWORK)                                
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO ERRORS TOLERATED                          
*                                                                               
         LA    R5,TMPWKAIO                                                      
         LA    R5,GCOLFRST-GCOLKEY(R5)                                          
         USING PCOLELEM,R5                                                      
         B     CCOL20M                                                          
*                                                                               
CCOL20F  XC    KEY,KEY                                                          
C        USING PCOLRECD,KEY                                                     
         MVC   C.PCOLPAGY,QAGENCY  AGENCY                                       
         MVI   C.PCOLPMED,C'A'     MEDIA ALWAYS A FOR CUST COL'S                
         MVI   C.PCOLPRCD,X'D1'    RECORD CODE FOR PASSIVE KEY                  
         MVC   C.PCOLPSQN,BYCCSQN  SEQUENCE NUMBER                              
         XC    C.PCOLPSQN,=X'FFFF' COMPLEMENT SEQUENCE NUMBER                   
         DROP  C                                                                
*                                                                               
         GOTOR HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO ERRORS TOLERATED                          
*                                                                               
         LA    RE,TMPWKAIO         TEMP USE FOR CUSTOM COLUMN REC               
         ST    RE,AREC                                                          
*                                                                               
         GOTOR GETPRT                                                           
         LA    R5,TMPWKAIO+(PCOLFRST-PCOLREC)                                   
*                                                                               
CCOL20M  TM    PCOLINS,X'80'       USE IN INSERTION ORDERS?                     
         BNO   CCOL20              NO, TRY NEXT BUY CC ELEM                     
         BRAS  RE,RPYCUCOL         REPLY CUSTOM COLUMN                          
         B     CCOL20              NEXT CUSTOM COLUMN BUY ELEMENT               
*                                                                               
CCOLX    MVC   KEY,TMPSVKEY                                                     
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R2,R5                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REVCOMNT NTR1  BASE=*,LABEL=*      PROCESS REASONS FOR REVISION                 
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    EXIT                NO                                           
*                                                                               
         OC    FIDCHGTB(2),FIDCHGTB                                             
         JZ    EXIT                                                             
*                                                                               
         XC    TMPHALF1,TMPHALF1   LOOP COUNTER                                 
         LA    R2,FIDCHGTB                                                      
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#REAREV)              
*                                                                               
REVCM20  LA    R4,REVCMTAB         POINT TO REVISION COMMENT TABLE              
         LH    RE,TMPHALF1                                                      
         CHI   RE,FIDCTMXQ         PROCESSED ENTIRE CHG MAP CODE TABLE?         
         BH    REVCM50                                                          
         OC    0(2,R2),0(R2)       CHANGE MAP CODE ENTRY PRESENT?               
         BZ    REVCM50                                                          
*                                                                               
REVCM30  CLC   0(2,R2),0(R4)                                                    
         BE    REVCM40                                                          
         SR    RF,RF                                                            
         IC    RF,2(R4)                                                         
         AR    R4,RF               POINT TO NEXT ENTRY                          
         OC    0(2,R4),0(R4)                                                    
         BNZ   REVCM30                                                          
         DC    H'0'                ENTRY MUST BE IN TABLE                       
*                                                                               
REVCM40  XC    TMPWK1,TMPWK1                                                    
         SR    RF,RF                                                            
         IC    RF,2(R4)                                                         
         SHI   RF,3                ACTUAL LENGTH OF REVISION TEXT               
         CHI   RF,256                                                           
         BNH   *+6                                                              
         DC    H'0'                LENGTH IS TOO BIG                            
         CHI   RF,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TMPWK1(0),3(R4)     GET REVISION TEXT                            
         AHI   RF,1                RESTORE LENGTH FROM EX                       
         LA    RE,TMPWK1                                                        
         AR    RE,RF               POINT TO END OF REVISION TEXT                
         AHI   RF,8                LENGTH FOR " CHANGED"                        
         CHI   RF,250                                                           
         BNH   *+6                                                              
         DC    H'0'                LENGTH IS NOT SUPPORTED BY LINKIO            
         MVC   0(8,RE),=C' CHANGED'                                             
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#REAREV),    +        
               ('LD_CHARQ',TMPWK1),((RF),0)                                     
*                                                                               
         LA    R2,2(R2)            POINT TO NEXT CHG MAP CODE ENTRY             
         LH    RE,TMPHALF1                                                      
         AHI   RE,1                COUNTER INCREMENT                            
         STH   RE,TMPHALF1                                                      
         B     REVCM20                                                          
*                                                                               
REVCM50  DS    0H                                                               
*                                                                               
REVCM_X  J     EXIT                                                             
*                                                                               
* MAP CODE, LENGTH OF ENTRY (REVISION TEXT + 3), REVISION TEXT                  
*                                                                               
REVCMTAB DS    0X                                                               
         DC    AL2(D#SPECFH),AL1(L'R_TXT001+3)                                  
R_TXT001 DC    C'SPECIAL FINANCIAL HANDLING'                                    
*                                                                               
         DC    AL2(D#CLICK),AL1(L'R_TXT002+3)                                   
R_TXT002 DC    C'CLICK-THOUGH, PAGE VIEW, IMPRESSION'                           
*                                                                               
         DC    AL2(D#ACHCOD),AL1(L'R_TXT003+3)                                  
R_TXT003 DC    C'ADDITIONAL CHARGE'                                             
*                                                                               
         DC    AL2(D#LEGALW),AL1(L'R_TXT004+3)                                  
R_TXT004 DC    C'LEGAL WARNING'                                                 
*                                                                               
         DC    AL2(D#UNTRAT),AL1(L'R_TXT005+3)                                  
R_TXT005 DC    C'RATE'                                                          
*                                                                               
         DC    AL2(D#SPCDSC),AL1(L'R_TXT006+3)                                  
R_TXT006 DC    C'SPACE DESCRIPTION'                                             
*                                                                               
         DC    AL2(D#INSDAT),AL1(L'R_TXT007+3)                                  
R_TXT007 DC    C'INSERTION DATE'                                                
*                                                                               
         DC    AL2(D#PREMUM),AL1(L'R_TXT008+3)                                  
R_TXT008 DC    C'PREMIUM'                                                       
*                                                                               
         DC    AL2(D#IORCOM),AL1(L'R_TXT009+3)                                  
R_TXT009 DC    C'INSERTION ORDER COMMENT'                                       
*                                                                               
         DC    AL2(D#SPCDAT),AL1(L'R_TXT010+3)                                  
R_TXT010 DC    C'SPACE CLOSING DATE'                                            
*                                                                               
         DC    AL2(D#ONSDAT),AL1(L'R_TXT011+3)                                  
R_TXT011 DC    C'ON-SALE DATE'                                                  
*                                                                               
         DC    AL2(D#ADCODE),AL1(L'R_TXT012+3)                                  
R_TXT012 DC    C'AD CODE'                                                       
*                                                                               
         DC    AL2(D#COMPCT),AL1(L'R_TXT013+3)                                  
R_TXT013 DC    C'AGENCY COMMISSION %'                                           
*                                                                               
         DC    AL2(D#DSCPCT),AL1(L'R_TXT014+3)                                  
R_TXT014 DC    C'CASH DISCOUNT %'                                               
*                                                                               
         DC    AL2(D#INSDA2),AL1(L'R_TXT015+3)                                  
R_TXT015 DC    C'2ND INSERTION DATE'                                            
*                                                                               
         DC    AL2(D#TAXPCT),AL1(L'R_TXT016+3)                                  
R_TXT016 DC    C'TAX %'                                                         
*                                                                               
         DC    AL2(D#MCLDAT),AL1(L'R_TXT017+3)                                  
R_TXT017 DC    C'MATERIAL CLOSING DATE'                                         
*                                                                               
         DC    AL2(D#POSCOM),AL1(L'R_TXT018+3)                                  
R_TXT018 DC    C'POSITION INSTRUCTION'                                          
*                                                                               
         DC    AL2(D#REGCO1),AL1(L'R_TXT019+3)                                  
R_TXT019 DC    C'REGULAR COMMENT'                                               
*                                                                               
         DC    AL2(0)              END OF TABLE                                 
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CONCOMNT NTR1  BASE=*,LABEL=*      PROCESS CONTRACT COMMENT                     
*                                                                               
         MVC   TMPSVIO2,AREC       SAVE ORIGINAL AIO POINTER                    
         MVC   TMPSVKY2,KEY        SAVE ORIGINAL KEY                            
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         BZ    CONCM_X             NO                                           
*                                                                               
         L     RE,A_CONREC                                                      
         ST    RE,AREC                                                          
         XC    KEY,KEY                                                          
         LA    R5,CONDALST         POINT TO CONTRACT ADDRESS TABLE              
CONCM10  OC    0(4,R5),0(R5)                                                    
         BZ    CONCM_X                                                          
         MVC   KEY+27(4),0(R5)                                                  
         GOTOR GETPRT                                                           
         L     R2,A_CONREC                                                      
         LA    R2,PCONDESC-PCONKEY(R2)                                          
         LR    R4,R2                                                            
         CLI   0(R2),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID CONTRACT RECORD                      
*                                                                               
         MVI   ELCODE,X'30'        STANDARD CONTRACT COMMENT ELEM CODE          
         BRAS  RE,NXTEL                                                         
         BE    CONCM20                                                          
         LR    R2,R4                                                            
         MVI   ELCODE,X'40'        SPECIAL CONTRACT COMMENT ELEM CODE           
         BRAS  RE,NXTEL                                                         
         BNE   CONCM80                                                          
*                                                                               
CONCM20  L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#CONTCM)              
*                                                                               
         LR    R2,R4                                                            
         MVI   ELCODE,X'30'        STANDARD CONTRACT COMMENT ELEM CODE          
CONCM36  BRAS  RE,NXTEL                                                         
         BNE   CONCM50                                                          
         USING PSTDELEM,R2                                                      
         CLC   SVCSTDCM,PSTDCOM                                                 
         BE    CONCM42                                                          
         MVC   WORK(L'PSTDCOM),PSTDCOM                                          
         MVI   WORK+L'PSTDCOM,X'40'                                             
         BRAS  RE,GTCOMREC                                                      
         BE    CONCM42                                                          
         LA    R2,RPYERTXT         REPLY ERROR MSG                              
         LHI   R1,D#COMMNT         REPLY FREE FORM CONTRACT COMMENT             
         BRAS  RE,RPYCOMLN                                                      
         XC    SVCSTDCM,SVCSTDCM                                                
         B     CONCM36                                                          
CONCM42  ST    R2,TMPFULL2                                                      
         MVC   TMPBYTE2,ELCODE                                                  
         LA    R2,TMPWKAIO+33                                                   
         CLI   0(R2),X'40'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID STANDARD COMMENT RECORD              
         MVC   SVCSTDCM,PSTDCOM                                                 
         MVI   ELCODE,X'40'        COMMENT ELEM CODE                            
         LHI   R1,D#COMMNT         REPLY CONTRACT STANDARD COMMENT              
         BRAS  RE,RPYCOMLN                                                      
         BRAS  RE,NXTEL                                                         
         BE    *-12                                                             
         MVC   ELCODE,TMPBYTE2                                                  
         L     R2,TMPFULL2                                                      
         B     CONCM36                                                          
*                                                                               
CONCM50  LR    R2,R4                                                            
         MVI   ELCODE,X'40'        SPECIAL CONTRACT COMMENT ELEM CODE           
CONCM56  BRAS  RE,NXTEL                                                         
         BNE   CONCM80                                                          
         ST    R2,TMPFULL2                                                      
         LHI   R1,D#COMMNT         REPLY FREE FORM CONTRACT COMMENT             
         BRAS  RE,RPYCOMLN                                                      
         L     R2,TMPFULL2                                                      
         B     CONCM56                                                          
*                                                                               
CONCM80  LA    R5,4(R5)            POINT TO NEXT CONTRACT DISK ADDRESS          
         B     CONCM10                                                          
*                                                                               
CONCM_X  MVC   AREC,TMPSVAIO       RESTORE ORIGINAL AIO POINTER                 
         MVC   KEY,TMPSVKY2        RESTORE ORIGINAL KEY                         
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GTCOMREC NTR1  BASE=*,LABEL=*      GET COMMENT RECORD INTO TMPWKAIO             
*                                                                               
         MVC   TMPSVKEY,KEY                                                     
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PCOMKEY,RE                                                       
         MVC   PCOMKAGY,QAGENCY                                                 
         MVC   PCOMKMED,QMEDIA                                                  
         MVC   PCOMKRCD,WORK+L'PCOMKNUM                                         
         MVC   PCOMKNUM,WORK                                                    
         GOTOR HIGH                                                             
         CLC   KEY(L'PCOMKEY),KEYSAVE                                           
         JNE   SETCCNEQ                                                         
*                                                                               
         MVC   FULL,AREC           SAVE ORIGINAL AIO POINTER                    
         LA    RE,TMPWKAIO                                                      
         ST    RE,AREC                                                          
         GOTOR GETPRT                                                           
         MVC   AREC,FULL           RESTORE ORIGINAL AIO POINTER                 
         MVC   KEY,TMPSVKEY        RESTORE ORIGINAL KEY                         
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,RE                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYCOMLN NTR1  BASE=*,LABEL=*      REPLY COMMENT LINES                          
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         STH   R1,TMPHALF2         COMMENT LINE MAP CODE                        
         LHI   R4,1                FOR LOOPING                                  
*                                                                               
         CLI   1(R2),3             EMPTY COMMENT LINE?                          
         BH    R_COML64                                                         
         CLI   2(R2),C' '                                                       
         BH    R_COML64                                                         
R_COML62 LA    R2,SPACES                                                        
         LHI   R5,1                                                             
         B     R_COML80                                                         
R_COML64 CLI   2(R2),C'+'          SKIPPING LINES?                              
         BNE   R_COML66                                                         
         PACK  DUB,3(1,R2)                                                      
         TP    DUB                                                              
         BNZ   *+8                                                              
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BNZ   R_COML62                                                         
R_COML66 SR    R5,R5                                                            
         IC    R5,1(R2)                                                         
         SHI   R5,2                                                             
         LA    R2,2(R2)                                                         
         CLI   REMCHRCM,C'Y'       12A - REMOVE SPECIAL CHARS?                  
         BNE   R_COML80                                                         
         CLI   0(R2),C'*'                                                       
         BE    *+8                                                              
         CLI   0(R2),C'+'                                                       
         BE    *+8                                                              
         CLI   0(R2),C'#'                                                       
         BE    *+8                                                              
         CLI   0(R2),C'@'                                                       
         BE    *+8                                                              
         CLI   0(R2),C'&&'                                                      
         BE    *+8                                                              
         B     R_COML80                                                         
         LA    R2,1(R2)            PASS SPECIAL CHAR                            
         BCTR  R5,0                ONE LESS CHAR                                
*                                                                               
R_COML80 LH    RF,TMPHALF2                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',(RF)),        +        
               ('LD_CHARQ',0(R2)),((R5),0)                                      
         BCT   R4,R_COML80                                                      
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INICHGTB NTR1  BASE=*,LABEL=*      CHG MAP CODE TABLE FOR FLD INDICATOR         
*                                                                               
         XC    FIDCHGTB(FIDCHGLQ),FIDCHGTB                                      
         NI    WKESRFLG,X'FF'-(ESR_GENQ+ESR_ELMQ)                               
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    EXIT                NO                                           
*                                                                               
         LA    R2,PBUYREC+33                                                    
         USING PBYMELEM,R2                                                      
         MVI   ELCODE,PBYMELCQ     BUY MOVE ELEM CODE                           
INICT14  BRAS  RE,NXTEL                                                         
         BNE   INICT16                                                          
         TM    PBYMSTAT,PBYMFROQ   MOVED "FROM"?                                
         BZ    *+8                                                              
         OI    WKESRFLG,E_SRBMFQ   CONTAIN BUY MOVE "FROM" ELEM                 
         TM    PBYMSTAT,PBYMTO_Q   MOVED "TO"?                                  
         BZ    *+8                                                              
         OI    WKESRFLG,E_SRBMTQ   CONTAIN BUY MOVE "TO" ELEM                   
         B     INICT14                                                          
*                                                                               
INICT16  TM    PBUYCNTL,X'80'      DELETED?                                     
         JNZ   EXIT                IGNORE CHG ACTIVITIES FOR DELETED            
*                                                                               
         XC    TMPFULL1,TMPFULL1                                                
         XC    TMPFULL2,TMPFULL2                                                
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,PESRELCQ     ESR ELEM CODE                                
INICT20  BRAS  RE,NXTEL                                                         
         BNE   INICT30                                                          
         USING PESRELEM,R2                                                      
         MVC   TMPFULL1(L'PESRDATE),PESRDATE                                    
         TM    PESRSTAT,PESRDBMQ   DELETED BUY MOVE CHG RESV ISSUED?            
         BZ    *+8                                                              
         OI    WKESRFLG,E_SRBMDQ                                                
         TM    PESRSTAT,PESRBMOQ   BUY MOVE SPACE RESV ISSUED?                  
         BZ    *+8                                                              
         OI    WKESRFLG,E_SRBMOQ                                                
         LR    RF,R2                                                            
         B     INICT20                                                          
*                                                                               
INICT30  OC    TMPFULL1,TMPFULL1   LAST ESR DATE PRESENT?                       
         BZ    INICTX                                                           
         OI    WKESRFLG,ESR_ELMQ   ESR ELEM FOUND IN INSERTION                  
*                                                                               
         LR    R2,RF               CHG ELEMS RIGHT AFTER LAST ESR               
         MVI   ELCODE,X'24'        CHANGE ELEM CODE                             
INICT50  BRAS  RE,NXTEL                                                         
         BNE   INICT60                                                          
         USING PCHGELEM,R2                                                      
         GOTOR DATCON,DMCB,(2,PCHGDAT),(3,TMPFULL2)                             
         CLC   TMPFULL2,TMPFULL1                                                
         BL    INICT50                                                          
*                                                                               
         LA    RE,PCHG_XSS         START OF EXTENSION FOR SHORT ELEM            
         CLI   PCHGLEN,PCHGNEWL    LENGTH WITH COST INFO?                       
         BNE   *+8                                                              
         LA    RE,PCHG_XLS         START OF EXTENSION FOR LONG ELEM             
         USING PCHGEXT,RE                                                       
*                                                                               
         CLC   TMPFULL2,TMPFULL1   SAME DAY CHANGES?                            
         BNE   INICT52                                                          
         TM    WKESRFLG,ESR_ELMQ   ESR ELEM FOUND IN INSERTION?                 
         BZ    INICT52                                                          
         TM    WKESRFLG,ESR_GENQ   ESR IS GENEREATED IN SAME DAY?               
         BNZ   INICT52                                                          
         TM    PCHGIND5,X'80'      ESR GENERATED?                               
         BZ    INICT50                                                          
         OI    WKESRFLG,ESR_GENQ   ESR IS GENERATED IN SAME DAY                 
         DROP  RE                                                               
*                                                                               
INICT52  TM    PCHGIND4,X'80'      SFH STATUS CHANGED?                          
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#SPECFH)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND4,X'20'      CT, PV,IMPRESSION CHANGED?                   
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#CLICK)                                           
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND4,X'10'      ADDITIONAL CHARGES CHANGED?                  
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#ACHCOD)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND4,X'08'      LEGAL WARNINGS CHANGED?                      
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#LEGALW)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND1,X'40'      RATE CHANGE D?                               
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#UNTRAT)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND1,X'10'      DESCRIPTION CHANGED?                         
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#SPCDSC)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND1,X'08'      INSERTION DATE CHANGED?                      
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#INSDAT)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND1,X'04'      PREMIUM CHANGED?                             
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#PREMUM)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND1,X'01'      IOC CHANGE?                                  
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#IORCOM)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND2,X'80'      SPACE CLOSE DATE CHANGED?                    
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#SPCDAT)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND2,X'40'      SALE DATE CHANGED?                           
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#ONSDAT)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND2,X'08'      JOB NUMBER CHANGED?                          
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#ADCODE)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND2,X'04'      AGY COMM CHANGED?                            
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#COMPCT)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND2,X'02'      CASH DISCOUNT CHANGED?                       
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#DSCPCT)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND3,X'80'      2ND INS DATE CHANGED?                        
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#INSDA2)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND3,X'08'      TAX CHANGED?                                 
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#TAXPCT)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND3,X'02'      MATERIALS CLOSING DATE CHANGED?              
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#MCLDAT)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND3,X'01'      POSITION INSTRUCTIONS CHANGED?               
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#POSCOM)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         TM    PCHGIND1,X'02'      REGULAR COMMENT CHANGED?                     
         BZ    *+14                                                             
         MVC   TMPHALF1,=AL2(D#REGCO1)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
         B     INICT50                                                          
*                                                                               
INICT60  TM    WKESRFLG,E_SRBMOQ   BUY MOVE SPACE RESERV ISSUED?                
         BNZ   INICT70                                                          
         TM    WKESRFLG,E_SRBMFQ   BUY MOVE "FROM" ELEM PRESENT?                
         BZ    INICT70                                                          
         TM    PBUYCNTL,X'80'      DELETED?                                     
         BZ    INICT70                                                          
         MVC   TMPHALF1,=AL2(D#FROINS)                                          
         BRAS  RE,INICTPUT                                                      
*                                                                               
INICT70  DS    0H                                                               
*                                                                               
INICTX   J     EXIT                                                             
*                                                                               
INICTPUT LR   R0,RE                                                             
         SR   RF,RF                                                             
         LA   RE,FIDCHGTB                                                       
INICTP10 CHI  RF,FIDCTMXQ          MAX ENTRIES REACHED?                         
         BL   *+6                                                               
         DC   H'0'                                                              
         OC   0(2,RE),0(RE)        BLANK ENTRY?                                 
         BZ   INICTP40                                                          
         CLC  0(2,RE),TMPHALF1     ALREADY IN TABLE?                            
         BE   INICTPX                                                           
         AHI  RF,1                 COUNTER INCREMENT                            
         LA   RE,2(RE)             POINT TO NEXT ENTRY                          
         B    INICTP10                                                          
INICTP40 MVC  0(2,RE),TMPHALF1     PUT MAP CODE IN TABLE                        
INICTPX  LR   RE,R0                                                             
         BR   RE                                                                
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INSREGCM NTR1  BASE=*,LABEL=*      PROCESS INSERTION REGULAR COMMENT            
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    EXIT                NO                                           
*                                                                               
         TM    PBUYCNTL,X'80'      DELETED?                                     
         BZ    REGCM10                                                          
         CLI   DELBUYCM,C'R'       SHOW COMMENT FOR DELETED BUYS?               
         JNE   EXIT                                                             
*                                                                               
REGCM10  L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         ZIC   R4,SUPREGCM       COUNT OF REG. COMMENTS TO IGNORE               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'66'                                                     
REGCM16  BRAS  RE,NXTEL                                                         
         BNE   REGCM_X                                                          
*                                                                               
         CH    R4,=H'0'                                                         
         BE    REGCM20     FINISHED SKIPPING OR NOT SKIPPING AT ALL             
         BL    REGCM20     SHOULD NEVER REALLY HAPPEN                           
         SH    R4,=H'1'    SKIP THIS REGULAR                                    
         B     REGCM16     KEEP LOOKING                                         
*                                                                               
REGCM20  TM    ADRPYSW2,RPYRGCMQ                                                
         BNZ   REGCM30                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#INSRCM)              
         OI    ADRPYSW2,RPYRGCMQ                                                
         LHI   RF,D#REGCO1                                                      
         STH   RF,TMPHALF1         MAP CODE FOR PI COMMENT                      
         BRAS  RE,RPYFLDID         TMPHALF1 IS SET                              
*                                                                               
REGCM30  LHI   R1,D#IORCOM         REPLY INSERTION REGULAR COMMENT              
         BRAS  RE,RPYCOMLN                                                      
         B     REGCM16                                                          
*                                                                               
REGCM_X  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
POSINSCM NTR1  BASE=*,LABEL=*      PROCESS POSITION INSTRUCTION COMMENT         
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    EXIT                NO                                           
*                                                                               
         CLI   POSITINS,C'Y'       PRINT POSITION INSTRUCTION?                  
         JNE   EXIT                                                             
*                                                                               
         TM    PBUYCNTL,X'80'      DELETED?                                     
         BZ    PICOM10                                                          
         CLI   DELBUYCM,C'R'       SHOW COMMENT FOR DELETED BUYS?               
         JNE   EXIT                                                             
*                                                                               
PICOM10  L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         LA    R2,PBUYREC+33                                                    
*                                                                               
         MVI   ELCODE,X'68'        POSITION INSTRUCTION COMM ELEM CODE          
PICOM16  BRAS  RE,NXTEL                                                         
         JNE   EXIT                                                             
         CLI   1(R2),2                                                          
         BNH   PICOM16                                                          
         CLC   2(4,R2),=C'COM='    EMBEDDED STANDARD COMMENTS?                  
         BE    PICOM30                                                          
         BRAS  RE,PICOM_HD         REPLY PI SUB-RECORD HEADER                   
         LHI   R1,D#POSCOM         REPLY POSITION INSTRUCTION COMMENT           
         BRAS  RE,RPYCOMLN                                                      
         B     PICOM16                                                          
*                                                                               
PICOM30  SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         SHI   RF,2+4                                                           
         LTR   RF,RF               COMMENT CODE PRESENT?                        
         BZ    PICOM16                                                          
         MVC   WORK(10),SPACES                                                  
         LA    RE,WORK+6                                                        
         SR    RE,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),4+2(R2)     GET COMMENT CODE                             
         MVI   WORK+L'PCOMKNUM,X'40'                                            
         ST    R2,TMPFULL2         SAVE BUY RECORD POINTER                      
         MVC   TMPBYTE2,ELCODE     SAVE BUY RECORD ELEMENT CODE                 
         CLC   SVBSTDCM,WORK                                                    
         BE    PICOM36                                                          
PICOM30M BRAS  RE,GTCOMREC                                                      
         BE    PICOM36                                                          
         LA    R2,RPYERTXT         REPLY ERROR MSG                              
         BRAS  RE,PICOM_HD         REPLY PI SUB-RECORD HEADER                   
         LHI   R1,D#POSCOM         REPLY POSITION INSTRUCTION COMMENT           
         BRAS  RE,RPYCOMLN                                                      
         XC    SVBSTDCM,SVBSTDCM                                                
         B     PICOM16                                                          
*                                                                               
PICOM36  LA    R2,TMPWKAIO+33                                                   
         CLI   0(R2),X'40'                                                      
         BNE   PICOM30M                                                         
         MVC   SVBSTDCM,WORK                                                    
         MVI   ELCODE,X'40'        COMMENT ELEM CODE                            
*                                                                               
         CLI   0(R2),X'40'                                                      
         BE    *+12                                                             
PICOM40  BRAS  RE,NXTEL                                                         
         BNE   PICOM46                                                          
*                                                                               
         CLC   2(5,R2),=C'SHIP='                                                
         BE    PICOM40                                                          
         CLC   2(6,R2),=C'LABEL='                                               
         BE    PICOM40                                                          
         CLC   2(4,R2),=C'MAT='                                                 
         BE    PICOM40                                                          
         CLC   2(3,R2),=C'RC='                                                  
         BE    PICOM40                                                          
*                                                                               
         BRAS  RE,PICOM_HD         REPLY PI SUB-RECORD HEADER                   
         LHI   R1,D#POSCOM         REPLY POSITION INSTRUCTION COMMENT           
         BRAS  RE,RPYCOMLN                                                      
         B     PICOM40                                                          
*                                                                               
PICOM46  MVC   ELCODE,TMPBYTE2                                                  
         L     R2,TMPFULL2                                                      
         B     PICOM16                                                          
*                                                                               
PICOM_X  J     EXIT                                                             
*                                                                               
PICOM_HD ST    RE,TMPSVRE                                                       
         TM    ADRPYSW2,RPYPICMQ   IO COMMENT SUB REC MAP CODE REPLIED?         
         JNZ   PICOM_HX                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#PICOMN)              
         OI    ADRPYSW2,RPYPICMQ                                                
         LHI   RF,D#POSCOM                                                      
         STH   RF,TMPHALF1         MAP CODE FOR PI COMMENT                      
         BRAS  RE,RPYFLDID         TMPHALF1 IS SET                              
PICOM_HX L     RE,TMPSVRE                                                       
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYESRDL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    EXIT                NO                                           
*                                                                               
         BRAS  RE,SETESRDT         SET ESR DETAIL VALUES                        
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#SR2DET)              
         OI    ADRPYREC,RPYDETLQ   DETAIL SECTION REPLIED                       
*                                                                               
         L     RF,AESRDSST         POINT TO ESR STORAGE BLOCK                   
         USING ESRDSD,RF                                                        
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESRLKY),    +        
               ('LD_CHARQ',H_SRLKEY),('H_SRLKYL',0)                             
         DROP  RF                                                               
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'99'                                                     
         BRAS  RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                INSERTION SHOULD HAVE SERIAL#                
         USING PSERELEM,R2                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#BYSER),     +        
               ('LD_SPAKQ',PSERNUM),(L'PSERNUM,0)                               
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INSDAT),    +        
               ('LD_BDATQ',PBUYKDAT),(L'PBUYKDAT,0)                             
         MVC   TMPHALF1,=AL2(D#INSDAT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
         CLI   PBDFREQ,C'M'        MONTHLY?                                     
         BNE   *+12                                                             
         MVI   TMPWK1,C'M'                                                      
         B     ESRD024D                                                         
         MVC   TMPWK1(1),PBDBFD                                                 
         CLI   TMPWK1,C'W'         WEEK OF?                                     
         BE    ESRD024D                                                         
         CLI   TMPWK1,C'B'         BEST FOOD DAY?                               
         BE    ESRD024D                                                         
         MVI   TMPWK1,C'D'         DEFAULT TO DAILY                             
ESRD024D GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INSFRQ),    +        
               ('LD_CHARQ',TMPWK1),(1,0)                                        
*                                                                               
ESRD026  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INSBYL),    +        
               ('LD_CHARQ',D_INSDLN),(L'D_INSDLN,0)                             
                                                                                
         BRAS  RE,RPYFLDID                                                      
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTNUM),    +        
               ('LD_UBINQ',PBUYKEST),(L'PBUYKEST,0)                             
*                                                                               
         MVC   TMPSVKEY,KEY                                                     
         MVC   TMPSVAIO,AREC                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY     AGY/MED/RECODE/CLT/PRD                       
         MVI   KEY+3,X'07'         ESTIMATE RECORD CODE                         
         MVC   KEY+10(2),PBUYKEST  BINARY ESTIMATE NUMBER                       
         GOTOR HIGH                                                             
         LA    RE,TMPWKAIO                                                      
         ST    RE,AREC                                                          
         GOTOR GETPRT                                                           
         MVC   KEY,TMPSVKEY        RESTORE ORIGINAL SETTINGS                    
         MVC   AREC,TMPSVAIO                                                    
         LA    RE,TMPWKAIO                                                      
         LA    RE,PESTELEM-PESTKEY(RE)                                          
         CLI   0(RE),X'07'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID ESTIMATE RECORD                      
         MVC   TMPWK1(L'PESTNAME),PESTNAME-PESTELEM(RE)                         
         MVC   TMPWK2(L'PESTNAM2),PESTNAM2-PESTELEM(RE)                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTNAM),    +        
               ('LD_CHARQ',TMPWK1),(L'PESTNAME,0)                               
         OC    TMPWK2(L'PESTNAM2),SPACES                                        
         CLC   TMPWK2(L'PESTNAM2),SPACES                                        
         BE    ESRD030                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTNM2),    +        
               ('LD_CHARQ',TMPWK2),(L'PESTNAM2,0)                               
*                                                                               
ESRD030  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INSTYP),    +        
               ('LD_CHARQ',D_MODCOD),(L'D_MODCOD,0)                             
*                                                                               
         OC    BPUB+4(2),BPUB+4    ALREADY SENT IN HEADER SECTION?              
         BNZ   ESRD036                                                          
         OC    PBUYKZON,PBUYKZON   PUB ZONE PRESENT?                            
         BZ    ESRD034D                                                         
         XC    TMPWK1,TMPWK1                                                    
         GOTOR APUBEDIT,DMCB,(X'08',PBUYKPUB),(C'S',TMPWK1)                     
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBZON),    +        
               ('LD_CHARQ',TMPWK1+8+1),(2,0)                                    
         BRAS  RE,RPYPZNAM         REPLY PUBLICATION ZONE NAME                  
*                                                                               
ESRD034D OC    PBUYKEDT,PBUYKEDT                                                
         BZ    ESRD036                                                          
         OC    PBUYKZON,PBUYKZON   PUB ZONE PRESENT?                            
         BNZ   ESRD034H                                                         
         XC    TMPWK1,TMPWK1                                                    
         GOTOR APUBEDIT,DMCB,(X'08',PBUYKPUB),(C'S',TMPWK1)                     
         LA    RF,TMPWK1+8+1                                                    
         B     *+8                                                              
ESRD034H LA    RF,TMPWK1+8+1+2+1                                                
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBEDT),    +        
               ('LD_CHARQ',0(RF)),(3,0)                                         
         GOTOR APUBEDIT,DMCB,(X'08',PBUYKPUB),(C'E',TMPWK1)                     
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#EDTEXP),    +        
               ('LD_CHARQ',TMPWK1),(11,0)                                       
*                                                                               
ESRD036  BRAS  RE,RPYCONTR         REPLY CONTRACT INFORMATION                   
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#IOTYPE),    +        
               ('LD_CHARQ',D_ESRTYP),(L'D_ESRTYP,0)                             
*                                                                               
         LA    RF,L'PBYOSPC1                                                    
         MVC   TMPWK1(L'PBYOSPC1),PBYOSPC1                                      
         OC    TMPWK1(L'PBYOSPC1),SPACES                                        
         CLC   TMPWK1(L'PBYOSPC1),SPACES                                        
         BNE   *+16                                                             
         CLI   QMEDIA,C'N'         NEWSPAPER?                                   
         BNE   *+12                                                             
         BRAS  RE,FMT_NSPD                                                      
         BRAS  RE,ESRD_SPD         REPLY SPACE DESCRIPTION                      
         LA    RF,L'PBYOSPC2                                                    
         MVC   TMPWK1(L'PBYOSPC2),PBYOSPC2                                      
         OC    TMPWK1(L'PBYOSPC2),SPACES                                        
         CLC   TMPWK1(L'PBYOSPC2),SPACES                                        
         BE    *+8                                                              
         BRAS  RE,ESRD_SPD         REPLY SPACE DESCRIPTION                      
*                                                                               
         BRAS  RE,RPYADINF         REPLY AD INFORMATION                         
*                                                                               
         OC    PBDMDATE,PBDMDATE                                                
         BZ    ESRD066                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#MCLDAT),    +        
               ('LD_BDATQ',PBDMDATE),(L'PBDMDATE,0)                             
         MVC   TMPHALF1,=AL2(D#MCLDAT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD066  CLI   SUPPREAC,C'Y'       SUPPRESS AGENCY COMMISSION?                  
         BE    ESRD070                                                          
         EDITR PBDACP,(6,TMPWK1),3,ALIGN=LEFT,IZERO=Y                           
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#COMPCT),    +        
               ('LD_CHARQ',TMPWK1),(6,0)                                        
         MVC   TMPHALF1,=AL2(D#COMPCT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#COMAMT),    +        
               ('LD_CBINQ',AGYCOM),(L'AGYCOM,0)                                 
         MVC   TMPHALF1,=AL2(D#COMPCT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD070  CLI   SUPPRECD,C'Y'       SUPPRESS CASH DISCOUNT?                      
         BE    ESRD074                                                          
         EDITR PBDCD,(4,TMPWK1),1,ALIGN=LEFT,IZERO=Y                            
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#DSCPCT),    +        
               ('LD_CHARQ',TMPWK1),(4,0)                                        
         MVC   TMPHALF1,=AL2(D#DSCPCT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#DSCAMT),    +        
               ('LD_CBINQ',CSHDSC),(L'CSHDSC,0)                                 
         MVC   TMPHALF1,=AL2(D#DSCPCT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD074  CLI   SHWTAXAM,C'B'       SHOW BOTH TAX % AND $?                       
         BE    *+12                                                             
         CLI   SHWTAXAM,C'%'       SHOW TAX %?                                  
         BNE   ESRD076                                                          
         EDITR PBDTAX,(9,TMPWK1),4,ALIGN=LEFT,IZERO=Y                           
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TAXPCT),    +        
               ('LD_CHARQ',TMPWK1),(9,0)                                        
         MVC   TMPHALF1,=AL2(D#TAXPCT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD076  CLI   SHWTAXAM,C'B'       SHOW BOTH TAX % AND $?                       
         BE    *+12                                                             
         CLI   SHWTAXAM,C'$'       SHOW TAX $?                                  
         BNE   ESRD082                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#TAXAMT),    +        
               ('LD_CBINQ',TAX),(L'TAX,0)                                       
         MVC   TMPHALF1,=AL2(D#TAXPCT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD082  OC    PBDSDATE,PBDSDATE                                                
         BZ    ESRD092                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ONSDAT),    +        
               ('LD_BDATQ',PBDSDATE),(L'PBDSDATE,0)                             
         MVC   TMPHALF1,=AL2(D#ONSDAT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD092  OC    PBDCDATE,PBDCDATE                                                
         BZ    ESRD096                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SPCDAT),    +        
               ('LD_BDATQ',PBDCDATE),(L'PBDCDATE,0)                             
         MVC   TMPHALF1,=AL2(D#SPCDAT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD096  CLI   SHWINSCO,C'N'                                                    
         BE    ESRD098                                                          
         BRAS  RE,FMT_COST                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#COSIND),    +        
               ('LD_CHARQ',D_COSIND),(L'D_COSIND,0)                             
         MVC   TMPHALF1,=AL2(D#UNTRAT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
         LHI   RF,D#UNTRAT                                                      
         CLC   LIOBPCV1,=AL1(03,03,00,19)                                       
         BL    *+8                                                              
         LHI   RF,D#ICOST1                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',(RF)),        +        
               ('LD_CHARQ',D_INSCOS),(L'D_INSCOS,0)                             
         MVC   TMPHALF1,=AL2(D#UNTRAT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
         CLI   SHWINSCO,C'B'       BOTH GROSS AND NET?                          
         BNE   ESRD096H                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#COSIN2),    +        
               ('LD_CHARQ',D_COSIN2),(L'D_COSIN2,0)                             
         MVC   TMPHALF1,=AL2(D#UNTRAT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ICOST2),    +        
               ('LD_CHARQ',D_INSCO2),(L'D_INSCO2,0)                             
         MVC   TMPHALF1,=AL2(D#UNTRAT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD096H CLC   LIOBPCV1,=AL1(03,03,00,19)                                       
         BL    ESRD098                                                          
         CLI   QMEDIA,C'N'                                                      
         BNE   ESRD098                                                          
         CLI   SUPNEWSR,C'Y'       SUPPRESS NEWSPAPER RATE?                     
         BE    ESRD098                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#RATIND),    +        
               ('LD_CHARQ',D_RATIND),(L'D_RATIND,0)                             
         MVC   TMPHALF1,=AL2(D#UNTRAT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#UNTRAT),    +        
               ('LD_CHARQ',D_INSRAT),(L'D_INSRAT,0)                             
         MVC   TMPHALF1,=AL2(D#UNTRAT)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD098  OC    PBYOPRM,SPACES                                                   
         CLC   PBYOPRM,SPACES      NOTHING IN PREMIUM?                          
         BE    ESRD100                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PREMUM),    +        
               ('LD_CHARQ',PBYOPRM),(L'PBYOPRM,0)                               
         MVC   TMPHALF1,=AL2(D#PREMUM)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD100  BRAS  RE,R_ESRD_2         REPLY ESR DETAIL PART 2                      
*                                                                               
         J     EXIT                                                             
*                                                                               
ESRD_SPD ST    RE,TMPSVRE                                                       
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SPCDSC),    +        
               ('LD_CHARQ',TMPWK1),((RF),0)                                     
         MVC   TMPHALF1,=AL2(D#SPCDSC)                                          
         BRAS  RE,RPYFLDID                                                      
         L     RE,TMPSVRE                                                       
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R2,R3                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
R_ESRD_2 NTR1  BASE=*,LABEL=*      PART 2 OF ESR DETAIL DATA STREAM             
*                                                                               
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'88'        CLICK-THROUGH ELEM CODE                      
         BRAS  RE,NXTEL                                                         
         BNE   ESRD104                                                          
         USING PCLCKTEL,R2                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CLICK),     +        
               ('LD_SPAKQ',PCLCKTS),(L'PCLCKTS,0)                               
         MVC   TMPHALF1,=AL2(D#CLICK)                                           
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD104  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'87'        PAGE VIEWS ELEM CODE                         
         BRAS  RE,NXTEL                                                         
         BNE   ESRD108                                                          
         USING PPAGEVEL,R2                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#VIEWS),     +        
               ('LD_SPAKQ',PPAGEVS),(L'PPAGEVS,0)                               
         MVC   TMPHALF1,=AL2(D#CLICK)                                           
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD108  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'92'        ESTIMATED IMPRESSIONS ELEM CODE              
         BRAS  RE,NXTEL                                                         
         BNE   ESRD110                                                          
         USING PIMPRSEL,R2                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTIMP),    +        
               ('LD_SPAKQ',PIMPRS),(L'PIMPRS,0)                                 
         MVC   TMPHALF1,=AL2(D#CLICK)                                           
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD110  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'93'        ACTUAL IMPRESSIONS ELEM CODE                 
         BRAS  RE,NXTEL                                                         
         BNE   ESRD112                                                          
         USING PAIMSPEL,R2                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACTIMP),    +        
               ('LD_SPAKQ',PAIMPRS),(L'PAIMPRS,0)                               
         MVC   TMPHALF1,=AL2(D#CLICK)                                           
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD112  CLI   INSDAYOW,C'Y'       SHOW INSERTION DAY OF WEEK?                  
         BNE   ESRD118                                                          
         GOTOR DATCON,DMCB,(3,PBUYKDAT),(0,WORK)                                
         GOTOR VGETDAY,DMCB,WORK,WORK+6                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INSDWK),    +        
               ('LD_CHARQ',WORK+6),(3,0)                                        
*                                                                               
ESRD118  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'96'        MATERIAL EXTENSION DATE ELEM CODE            
         BRAS  RE,NXTEL                                                         
         BNE   ESRD120                                                          
         USING PEXDATEL,R2                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#MCLXDT),    +        
               ('LD_BDATQ',PEXDATE),(L'PEXDATE,0)                               
*                                                                               
ESRD120  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'89'        MATERIAL EXTENSION DAYS ELEM CODE            
         BRAS  RE,NXTEL                                                         
         BNE   ESRD124                                                          
         USING PEXDAYEL,R2                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#MCLXDY),    +        
               ('LD_SPAKQ',PEXDAYS),(L'PEXDAYS,0)                               
*                                                                               
ESRD124  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'82'        FREE STANDING INSERTS ELEM CODE              
         BRAS  RE,NXTEL                                                         
         BNE   ESRD128                                                          
         USING PBFSIEL,R2                                                       
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#FSINS),     +        
               ('LD_SPAKQ',PBFSI),(L'PBFSI,0)                                   
*                                                                               
ESRD128  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'98'        SITE LOCATION ELEM CODE                      
         BRAS  RE,NXTEL                                                         
         BNE   ESRD132                                                          
         USING PISITEEL,R2                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SITELO),    +        
               ('LD_CHARQ',PISITE),(L'PISITE,0)                                 
*                                                                               
ESRD132  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'83'        REFERENCE NUMBER ELEM CODE                   
         BRAS  RE,NXTEL                                                         
         BNE   ESRD136                                                          
         USING PBREFEL,R2                                                       
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#REFNUM),    +        
               ('LD_CHARQ',PBREFNO),(L'PBREFNO,0)                               
*                                                                               
ESRD136  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,PISNMELQ     ISSUE NAME ELEM CODE                         
         BRAS  RE,NXTEL                                                         
         BNE   ESRD140                                                          
         USING PISNMELD,R2                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ISSNM),     +        
               ('LD_CHARQ',PISNAME),(L'PISNAME,0)                               
*                                                                               
ESRD140  OC    PBDIDAT2,PBDIDAT2                                                
         BZ    ESRD144                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INSDA2),    +        
               ('LD_BDATQ',PBDIDAT2),(L'PBDIDAT2,0)                             
         MVC   TMPHALF1,=AL2(D#INSDA2)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
ESRD144  OC    D_NCONLE,D_NCONLE                                                
         BZ    ESRD148                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CONLIE),    +        
               ('LD_CHARQ',D_NCONLE),(L'D_NCONLE,0)                             
*                                                                               
ESRD148  CLI   PBDSPACE,X'FF'      OUTDOOR?                                     
         BNE   ESRD164                                                          
         MVC   TMPWK1(5),SPACES                                                 
         CP    PBDSHOW,=P'0'                                                    
         BE    ESRD148K                                                         
         CP    PBDSHOW,=P'99999'   SPECIAL VALUE FOR SPC (SPECIALS)             
         BNE   *+14                                                             
         MVC   TMPWK1(3),=C'SPC'                                                
         B     ESRD148H                                                         
         EDITR PBDSHOW,(5,TMPWK1),ALIGN=LEFT                                    
*                                                                               
ESRD148H GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SHOWGS),    +        
               ('LD_CHARQ',TMPWK1),(5,0)                                        
ESRD148K CP    PBDREG,=P'0'                                                     
         BE    ESRD148M                                                         
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#REGDSP),    +        
               ('LD_SPAKQ',PBDREG),(L'PBDREG,0)                                 
ESRD148M CP    PBDILLUM,=P'0'                                                   
         BE    ESRD164                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ILLPAN),    +        
               ('LD_SPAKQ',PBDILLUM),(L'PBDILLUM,0)                             
*                                                                               
ESRD164  DS    0H                                                               
*                                                                               
         BRAS  RE,CKSER#TB         CK AND UPDATE SERIAL# TABLE                  
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R2,R3                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETESRDT NTR1  BASE=*,LABEL=*      SET ESR DETAIL VALUES                        
*                                                                               
         NI    ADRPYREC,X'FF'-RPYDETLQ                                          
         MVI   ADRPYSW2,0          RESET DETAIL SUB REC REPLY CODES             
*                                                                               
         LH    RE,NUMPRCIN         # OF INSERTION PROCESSED                     
         AHI   RE,1                                                             
         STH   RE,NUMPRCIN                                                      
*                                                                               
         LA    R0,D_SVSTRT                                                      
         LHI   R1,D_SVDLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR REPLY INFORMATION BLOCK                
*                                                                               
         MVC   WKBFDAY,PBDBFD                                                   
         MVC   WKINSDT,PBUYKDAT                                                 
         MVC   WKFREQU,PBDFREQ                                                  
         MVC   WKBUYLN,PBUYKLIN                                                 
         GOTOR FMTBUYL#,2          GET INSERTION DATE WITH REF#                 
         MVC   D_INSDLN,TMPCL20                                                 
*                                                                               
         BRAS  RE,SETMODCD         DETERMINE MODIFICATION CODE                  
*                                                                               
         MVI   D_ESRTYP,D_ESRTYQ   DEFAULT TO ESR                               
*                                                                               
         CLI   PBUYKMED,C'N'       NEWSPAPER?                                   
         BNE   *+16                                                             
         BRAS  RE,FMTNRT           FORMAT RATE FOR NEWSPAPER                    
         BRAS  RE,FMTNCLE          FORMAT CLE FOR NEWSPAPER                     
         B     *+8                                                              
         BRAS  RE,FMTORF           FORMAT RATE FOR OTHER MEDIAS                 
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYPZNAM NTR1  BASE=*,LABEL=*      REPLY PUBLICATION ZONE NAME                  
*                                                                               
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         LA    R4,1(R9)                                                         
         LA    R4,4095(R4)         EXTEND POLFILED COVERAGE                     
         USING POLFILED,R9,R4                                                   
*                                                                               
         MVC   TMPSVKEY,KEY                                                     
         MVC   TMPSVAIO,AREC                                                    
*                                                                               
         CLC   PBUYKPUB(L'SVPUBZEC),PUBKPUB                                     
         BE    RPYZN30                                                          
         CLC   PBUYKPUB(L'SVPUBZEC),SVPUBZEC                                    
         BE    RPYZN40                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PUBKMED),PBUYKMED                                          
         MVC   KEY+L'PUBKMED(L'SVPUBZEC),PBUYKPUB                               
         MVC   KEY+L'PUBKMED+L'SVPUBZEC(L'PBUYKAGY),PBUYKAGY                    
         MVI   KEY+L'PUBKMED+L'SVPUBZEC+L'PBUYKAGY,X'81'                        
         GOTOR HIGHPB                                                           
         LA    RE,TMPWKAIO                                                      
         ST    RE,AREC                                                          
         GOTOR GETPUB                                                           
         LA    RE,TMPWKAIO+(PUBNAMEL-PUBKEY)                                    
         CLI   0(RE),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID PUB RECORD                           
*                                                                               
         MVC   TMPWK2(L'PUBZNAME),(PUBZNAME-PUBNAMEL)(RE)                       
         MVC   SVPUBZEC,KEY+L'PUBKMED                                           
         MVC   SVPUBZNM,(PUBZNAME-PUBNAMEL)(RE)                                 
         B     RPYZN60                                                          
*                                                                               
RPYZN30  MVC   TMPWK2(L'PUBZNAME),PUBZNAME                                      
         B     RPYZN60                                                          
*                                                                               
RPYZN40  MVC   TMPWK2(L'PUBZNAME),SVPUBZNM                                      
         B     RPYZN60                                                          
*                                                                               
RPYZN60  OC    TMPWK2(L'PUBZNAME),SPACES                                        
         CLC   TMPWK2(L'PUBZNAME),SPACES                                        
         BE    RPYZN_X                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PZNAME),    +        
               ('LD_CHARQ',TMPWK2),(L'PUBZNAME,0)                               
*                                                                               
RPYZN_X  MVC   KEY,TMPSVKEY        RESTORE ORIGINAL SETTINGS                    
         MVC   AREC,TMPSVAIO                                                    
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R3,R4                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYCONTR NTR1  BASE=*,LABEL=*      REPLY CONTRACT INFORMATION                   
*                                                                               
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         MVC   TMPSVKEY,KEY                                                     
         MVC   TMPSVAIO,AREC                                                    
*                                                                               
         OC    SVCONKEY,SVCONKEY   HAVE CONTRACT KEY?                           
         BZ    RPYCN20                                                          
         CLC   PBUYKDAT,D_CONSDT   LESS THAN CONTRACT START DATE?               
         BL    RPYCN20                                                          
         CLC   PBUYKDAT,D_CONEDT   GREATER THAN CONTRACT END DATE?              
         BH    RPYCN20                                                          
*                                                                               
         B     RPYCN60             USE CONTRACT INFO GOTTEN PREVIOUSLY          
*                                                                               
RPYCN20  XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING PCONKEY,RF                                                       
         MVC   PCONKAGY,QAGENCY                                                 
         MVC   PCONKMED,QMEDIA                                                  
         MVI   PCONKRCD,X'10'      CONTRACT RECORD CODE                         
         MVC   PCONKCLT,PBUYKCLT                                                
         CLI   PCLTPROF+5,C'2'     SUB CLIENT?                                  
         BNE   *+10                                                             
         MVC   PCONKCLT,PCLTPROF+6                                              
         MVC   PCONKPUB,PBUYKPUB                                                
         MVC   PCONKZON,PBUYKZON                                                
         MVC   PCONKEDT,PBUYKEDT                                                
         DROP  RF                                                               
         L     RE,A_CONREC                                                      
         ST    RE,AREC                                                          
         GOTOR HIGH                                                             
         B     RPYCN26                                                          
RPYCN24  GOTOR SEQ                                                              
*                                                                               
RPYCN26  CLC   KEY(PCONKEDT-PCONKEY),KEYSAVE                                    
         BNE   RPYCN_X                                                          
         GOTOR GETPRT                                                           
*                                                                               
         L     R2,A_CONREC                                                      
         LA    R2,PCONDESC-PCONKEY(R2)                                          
         CLI   0(R2),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID CONTRACT RECORD                      
         USING PCONDESC,R2                                                      
         CLC   PBUYKDAT,PCONSDT    LESS THAN CONTRACT START DATE?               
         BL    RPYCN24                                                          
         CLC   PBUYKDAT,PCONEDT    GREATER THAN CONTRACT END DATE?              
         BH    RPYCN24                                                          
*                                                                               
         SR    RF,RF                                                            
         LA    RE,CONDALST         POINT TO CONTRACT ADDRESS TABLE              
         LHI   RF,1                COUNTER                                      
RPYCN32  CHI   RF,CONDAMXQ                                                      
         BL    *+6                                                              
         DC    H'0'                TABLE MAX'D                                  
         CLC   KEY+27(4),0(RE)     ALREADY IN TABLE?                            
         BE    RPYCN40                                                          
         OC    0(4,RE),0(RE)                                                    
         BZ    RPYCN34                                                          
         LA    RE,4(RE)                                                         
         AHI   RF,1                                                             
         B     RPYCN32                                                          
RPYCN34  MVC   0(4,RE),KEY+27                                                   
*                                                                               
RPYCN40  LA    RF,KEY                                                           
         USING PCONKEY,RF                                                       
         MVC   D_CONNUM,PCONNUM    SAVE CONTRACT NUMBER                         
         MVC   SVCONKEY,KEY        SAVE CONTRACT KEY                            
         DROP  RF                                                               
*                                                                               
         MVC   D_CONSDT,PCONSDT    SAVE CONTRACT START DATE                     
         MVC   D_CONEDT,PCONEDT    SAVE CONTRACT END DATE                       
         MVC   D_CONSIG,PCONREQ    SAVE AGENCY SIGNER                           
         OC    D_CONSIG,SPACES                                                  
*                                                                               
         MVI   ELCODE,X'20'        CURRENT LEVEL ELEM CODE                      
         BRAS  RE,NXTEL                                                         
         BNE   RPYCN60                                                          
         USING PRBELEM,R2                                                       
         MVC   D_CONEFD,PRBDATE    SAVE CONTRACT EFFECTIVE DATE                 
         ZAP   D_CONLVL,PRBLEVEL   SAVE CONTRACT LEVEL                          
         ZAP   D_CONPCT,PRBPCT     SAVE CONTRACT PCT DISCOUNT (2 DECS)          
*                                                                               
RPYCN60  CLI   CON#REQD,C'Y'       SHOW CONTRACT NUMBER?                        
         BNE   RPYCN62                                                          
         OC    D_CONNUM,D_CONNUM   CONTRACT NUMBER PRESENT?                     
         BZ    RPYCN62                                                          
         EDITR (B2,D_CONNUM),(05,TMPSWRK),0,ALIGN=LEFT                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CONTR#),    +        
               ('LD_CHARQ',TMPSWRK),(05,0)                                      
*                                                                               
RPYCN62  CLC   D_CONSIG,SPACES     CONTRACT AGENCY SIGNER PRESENT?              
         BE    RPYCN64                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CONAUT),    +        
               ('LD_CHARQ',D_CONSIG),(L'D_CONSIG,0)                             
*                                                                               
RPYCN64  OC    D_CONSDT,D_CONSDT   CONTRACT START DATE PRESENT?                 
         BZ    RPYCN68                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CONSDT),    +        
               ('LD_BDATQ',D_CONSDT),(L'D_CONSDT,0)                             
*                                                                               
RPYCN68  OC    D_CONEDT,D_CONEDT   CONTRACT END DATE PRESENT?                   
         BZ    RPYCN72                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CONEDT),    +        
               ('LD_BDATQ',D_CONEDT),(L'D_CONEDT,0)                             
*                                                                               
RPYCN72  CLI   SUPPRELV,C'B'       SUPPRESS CONTRACT LEVEL AND PCT?             
         BE    RPYCN76                                                          
         CLI   SUPPRELV,C'L'       SUPPRESS CONTRACT LEVEL?                     
         BE    RPYCN76                                                          
         OC    D_CONLVL,D_CONLVL   CONTRACT LEVEL PRESENT?                      
         BZ    RPYCN76                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CONLVL),    +        
               ('LD_SPAKQ',D_CONLVL),(L'D_CONLVL,0)                             
*                                                                               
RPYCN76  CLI   SUPPREED,C'Y'       SUPPRESS CONTRACT EFFECTIVE DATE?            
         BE    RPYCN80                                                          
         OC    D_CONEFD,D_CONEFD   CONTRACT EFFECTIVE DATE PRESENT?             
         BZ    RPYCN80                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#EFFDAT),    +        
               ('LD_BDATQ',D_CONEFD),(L'D_CONEFD,0)                             
*                                                                               
RPYCN80  CLI   SUPPRELV,C'B'       SUPPRESS CONTRACT LEVEL AND PCT?             
         BE    RPYCN84                                                          
         CLI   SUPPRELV,C'P'       SUPPRESS CONTRACT PCT?                       
         BE    RPYCN84                                                          
         OC    D_CONPCT,D_CONPCT   CONTRACT PCT DISCOUNT PRESENT?               
         BZ    RPYCN84                                                          
         EDITR D_CONPCT,(10,TMPSWRK),2,ALIGN=LEFT                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PCTDIS),    +        
               ('LD_CHARQ',TMPSWRK),(10,0)                                      
*                                                                               
RPYCN84  DS    0H                                                               
*                                                                               
RPYCN_X  MVC   KEY,TMPSVKEY                                                     
         MVC   AREC,TMPSVAIO                                                    
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R2                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYADINF NTR1  BASE=*,LABEL=*      REPLY AD INFORMATION                         
*                                                                               
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         MVC   TMPSVKEY,KEY                                                     
         MVC   TMPSVAIO,AREC                                                    
*                                                                               
         OC    SVJOBKEY,SVJOBKEY   HAVE JOB KEY?                                
         BZ    RPYAD20                                                          
         CLC   PBDJOB,D_ADCODE     SAME AD CODE?                                
         BE    RPYAD60                                                          
*                                                                               
RPYAD20  XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING PJOBKEY,RF                                                       
         MVC   PJOBKAGY,QAGENCY                                                 
         MVC   PJOBKMED,QMEDIA                                                  
         MVI   PJOBKRCD,X'15'      JOB RECORD CODE                              
         MVC   PJOBKCLT,PBUYKCLT                                                
         MVC   PJOBKPRD,PBUYKPRD                                                
         MVC   PJOBKJOB,PBDJOB                                                  
         L     RE,A_JOBREC                                                      
         ST    RE,AREC                                                          
         GOTOR HIGH                                                             
         CLC   KEY(L'PJOBKEY),KEYSAVE                                           
         BNE   RPYAD_X                                                          
         GOTOR GETPRT                                                           
*                                                                               
         SR    RF,RF                                                            
         LA    RE,JOBDALST         POINT TO JOB ADDRESS TABLE                   
         LHI   RF,1                COUNTER                                      
RPYAD32  CHI   RF,JOBDAMXQ                                                      
         BL    *+6                                                              
         DC    H'0'                TABLE MAX'D                                  
         CLC   KEY+27(4),0(RE)     ALREADY IN TABLE?                            
         BE    RPYAD40                                                          
         OC    0(4,RE),0(RE)                                                    
         BZ    RPYAD34                                                          
         LA    RE,4(RE)                                                         
         AHI   RF,1                                                             
         B     RPYAD32                                                          
RPYAD34  MVC   0(4,RE),KEY+27                                                   
*                                                                               
RPYAD40  L     R2,A_JOBREC                                                      
         LA    R2,PJOBELEM-PJOBKEY(R2)                                          
         CLI   0(R2),X'15'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID JOB RECORD                           
         USING PJOBELEM,R2                                                      
*                                                                               
         MVC   D_ADCODE,PBDJOB     AD CODE                                      
         OC    D_ADCODE,SPACES                                                  
         MVC   D_ADID,PJOBADID     AD ID                                        
         OC    D_ADID,SPACES                                                    
         MVC   D_CPYNUM,PJOBCPY    COPY NUMBER                                  
         OC    D_CPYNUM,SPACES                                                  
         MVC   D_ADCAP1,PJOBCAP1   AD CAPTION 1                                 
         OC    D_ADCAP1,SPACES                                                  
         MVC   D_ADCAP2,PJOBCAP2   AD CAPTION 2                                 
         OC    D_ADCAP2,SPACES                                                  
*                                                                               
RPYAD60  CLI   SHWADCOD,C'Y'       PROFILE SET TO SHOW AD CODE?                 
         BNE   RPYAD64                                                          
         CLC   D_ADCODE,SPACES     AD CODE PRESENT?                             
         BE    RPYAD64                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADCODE),    +        
               ('LD_CHARQ',D_ADCODE),(L'D_ADCODE,0)                             
         MVC   TMPHALF1,=AL2(D#ADCODE)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
RPYAD64  CLC   D_ADID,SPACES       AD ID PRESENT?                               
         BE    RPYAD66                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#AD_ID),     +        
               ('LD_CHARQ',D_ADID),(L'D_ADID,0)                                 
         MVC   TMPHALF1,=AL2(D#ADCODE)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
RPYAD66  CLI   SHWCPYNO,C'Y'       PROFILE SET TO SHOW COPY#?                   
         BNE   RPYAD68                                                          
         CLC   D_CPYNUM,SPACES     COPY NUMBER PRESENT?                         
         BE    RPYAD68                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CPYNUM),    +        
               ('LD_CHARQ',D_CPYNUM),(L'D_CPYNUM,0)                             
*                                                                               
RPYAD68  CLI   SHWCAPTN,C'Y'       PROFILE SET TO SHOW CAPTION?                 
         BNE   RPYAD76                                                          
         CLC   D_ADCAP1,SPACES     AD CAPTION 1 PRESENT?                        
         BE    RPYAD72                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADCAP),     +        
               ('LD_CHARQ',D_ADCAP1),(L'D_ADCAP1,0)                             
*                                                                               
RPYAD72  CLC   D_ADCAP2,SPACES     AD CAPTION 2 PRESENT?                        
         BE    RPYAD76                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADCAP2),    +        
               ('LD_CHARQ',D_ADCAP2),(L'D_ADCAP2,0)                             
*                                                                               
RPYAD76  DS    0H                                                               
*                                                                               
RPYAD_X  MVC   KEY,TMPSVKEY                                                     
         MVC   AREC,TMPSVAIO                                                    
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R2,RF                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYFLDID NTR1  BASE=*,LABEL=*      REPLY FIELD INDICATOR                        
*                                                                               
         OC    FIDCHGTB(2),FIDCHGTB                                             
         JZ    EXIT                                                             
*                                                                               
         LA    RE,FIDCHGTB                                                      
         SR    RF,RF                                                            
RPYFI20  CHI   RF,FIDCTMXQ         NOT IN TABLE?                                
         BH    RPYFI_X                                                          
         CLC   TMPHALF1,0(RE)      MAP CODE MATCHED ENTRY IN TABLE?             
         BE    RPYFI30                                                          
         LA    RE,2(RE)            POINT TO NEXT ENTRY                          
         AHI   RF,1                COUNTER INCREMENT                            
         B     RPYFI20                                                          
*                                                                               
RPYFI30  L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         MVI   TMPBYTE3,C'C'                                                    
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#FLDIND),    +        
               ('LD_CHARQ',TMPBYTE3),(L'TMPBYTE3,0)                             
*                                                                               
RPYFI_X  J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R3                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* R1 = 1 INSERTION DATE W/ REF# AS THAT RECALLED FROM BUY                       
* R1 = 2 INSERTION DATE W/ REF#, HAS YEAR DIGITS (YY), NO FREQUENCY             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTBUYL# NTR1  BASE=*,LABEL=*      FORMAT INSERTION DATE W/ LINE #              
*                                                                               
         LR    R3,R1               SAVE FORMATTING FLAG                         
*                                                                               
         MVC   TMPCL20,SPACES      CLR RETURN VALUE                             
         LA    R5,TMPCL20                                                       
         CHI   R3,2                NO NEED FOR LEADING CHAR?                    
         BE    FMTBL#02                                                         
         CLI   WKBFDAY,0                                                        
         BE    FMTBL#02                                                         
         MVC   0(1,R5),WKBFDAY                                                  
         AHI   R5,1                                                             
*                                                                               
FMTBL#02 GOTOR DATCON,DMCB,(3,WKINSDT),(7,0(R5))                                
         AHI   R5,5                SET NEXT OUTPUT ADDRESS                      
         CLI   WKFREQU,C'M'        MONTHLY?                                     
         BNE   *+14                                                             
         SHI   R5,2                                                             
         MVC   0(2,R5),SPACES                                                   
*                                                                               
         CHI   R3,2                NEED YEAR DIGITS?                            
         BNE   FMTBL#03                                                         
         GOTOR DATCON,DMCB,(3,WKINSDT),(6,DUB)                                  
         MVI   0(R5),C'/'                                                       
         MVC   1(2,R5),DUB+4       SET YY                                       
         AHI   R5,1+2                                                           
*                                                                               
FMTBL#03 CLI   WKBUYLN,1           TEST SUBLINE TO DISPLAY                      
         BE    FMTBL#X                                                          
         MVI   0(R5),C'-'                                                       
         SR    R0,R0                                                            
         IC    R0,WKBUYLN                                                       
         CVD   R0,DUB                                                           
         CHI   R0,100                                                           
         BL    FMTBL#04                                                         
*                                                                               
         DP    DUB,=P'10'          DISPLAY 100 - 239 AS A0 - N9                 
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  2(1,R5),DUB+7                                                    
         ZAP   DUB,DUB(6)                                                       
         CVB   RF,DUB                                                           
         SHI   RF,10                                                            
         LA    RF,LINETAB(RF)                                                   
         MVC   1(1,R5),0(RF)                                                    
         B     FMTBL#X                                                          
*                                                                               
FMTBL#04 OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(2,R5),DUB                                                      
         CLI   1(R5),C'0'                                                       
         BNE   FMTBL#X                                                          
         MVC   1(2,R5),2(R5)                                                    
*                                                                               
FMTBL#X  J     EXIT                                                             
*                                                                               
LINETAB  DC    C'ABCDEFGHIJKLMNOP'                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETMODCD NTR1  BASE=*,LABEL=*      DETERMINE MODIFICATION CODE                  
*                                                                               
         MVI   D_MODCOD,C'N'       NEW                                          
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,PESRELCQ     EIO ELEM CODE                                
SETMD20  BRAS  RE,NXTEL                                                         
         BNE   SETMD60                                                          
         USING PESRELEM,R2                                                      
         OC    PESRDATE,PESRDATE                                                
         BZ    SETMD20                                                          
*                                                                               
         MVI   D_MODCOD,C'C'       CHANGED                                      
         TM    PBUYCNTL,X'80'      DELETED?                                     
         BZ    SETMD46                                                          
         TM    WKESRFLG,E_SRBMTQ   BUY MOVE "TO"?                               
         BNZ   *+8                 CHANGE RESV FOR BUY MOVE "TO"                
         MVI   D_MODCOD,C'D'       DELETED                                      
         B     SETMD_X                                                          
*                                                                               
SETMD46  OC    FIDCHGTB(2),FIDCHGTB                                             
         BNZ   SETMD_X                                                          
         MVI   D_MODCOD,C'U'       UNCHANGED                                    
         B     SETMD_X                                                          
*                                                                               
SETMD60  TM    WKESRFLG,E_SRBM_Q   BUY MOVE ELEM PRESENT?                       
         BZ    SETMD_X                                                          
         LA    R2,PBUYREC+33                                                    
         USING PBYMELEM,R2                                                      
         MVI   ELCODE,PBYMELCQ     BUY MOVE ELEM CODE                           
         XC    TMPWK1,TMPWK1                                                    
         LA    R4,TMPWK1                                                        
         SR    R5,R5                                                            
SETMD62  BRAS  RE,NXTEL                                                         
         BNE   SETMD62H                                                         
         ZAP   0(L'PBYMSER#,R4),PBYMSER#                                        
         LA    R4,L'PBYMSER#(R4)                                                
         AHI   R5,1                                                             
         B     SETMD62                                                          
SETMD62H LA    R4,TMPWK1                                                        
SETMD62M OC    0(L'PBYMSER#,R4),0(R4)                                           
         BZ    SETMD_X                                                          
         ZAP   WKBYSER#,0(L'PBYMSER#,R4)                                        
         BRAS  RE,FND_B_SR         SEARCH FOR BUY ESR ELEMS                     
         BE    SETMD68                                                          
         LA    R2,TMPWKAIO+33                                                   
         MVI   ELCODE,PBYMELCQ     BUY MOVE ELEM CODE                           
SETMD62P BRAS  RE,NXTEL                                                         
         BNE   SETMD62U                                                         
         LA    RE,TMPWK1                                                        
SETMD62S CHI   R5,20               10 LEVELS OF BUY MOVE ENCOUNTERED?           
         BNH   *+6                                                              
         DC    H'0'                                                             
         OC    0(L'PBYMSER#,RE),0(RE)                                           
         BNZ   *+18                                                             
         AHI   R5,1                                                             
         ZAP   0(L'PBYMSER#,RE),PBYMSER#                                        
         B     SETMD62U                                                         
         CP    PBYMSER#,0(L'PBYMSER#,RE)                                        
         BE    SETMD62P                                                         
         LA    RE,L'PBYMSER#(RE)                                                
         B     SETMD62S                                                         
SETMD62U LA    R4,L'PBYMSER#(R4)   PROCESS NEXT BUY MOVE SERIAL#                
         B     SETMD62M                                                         
*                                                                               
SETMD68  MVI   D_MODCOD,C'C'       CHANGED                                      
*                                                                               
SETMD_X  J     EXIT                                                             
*                                                                               
FND_B_SR LR    R3,RE               FIND BUY ESR ELEMS                           
         BRAS  RE,GBUYR_S#         GET BUY RECORD FROM SERIAL#                  
         JNE   F_B_SR55                                                         
         LA    R2,TMPWKAIO+33                                                   
         BRAS  RE,FND_BESR         LASTEST ESR ELEM FOUND?                      
         JE    F_B_SR_X                                                         
F_B_SR55 LR    RE,R3                                                            
         J     RE_CCNEQ                                                         
F_B_SR_X LR    RE,R3                                                            
         J     RE_CCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSER#TB NTR1  BASE=*,LABEL=*      CK AND UPDATE SERIAL# TABLE                  
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'99'                                                     
         BRAS  RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                INSERTION SHOULD HAVE SERIAL#                
         USING PSERELEM,R2                                                      
         L     R4,ASER#TAB         POINT TO TABLE OF SERIAL#S                   
         USING SER#TABD,R4                                                      
         SR    RE,RE                                                            
         ICM   RE,3,NUMSER#S                                                    
CKS#20   CP    PSERNUM,S#SERIAL    FOUND IN TABLE?                              
         BE    CKS#30                                                           
         LA    R4,SER#TBLQ(R4)                                                  
         BCT   RE,CKS#20                                                        
*                                                                               
         MVI   S#STATUS,S#ADDEDQ   SERIAL# IS ADDED IN PROCESS                  
         ZAP   S#SERIAL,PSERNUM                                                 
         ICM   RE,3,NUMSER#S                                                    
         AHI   RE,1                NEW ENTRY IS ADDED                           
         CHI   RE,MAXSER#Q                                                      
         BNH   *+6                                                              
         DC    H'0'                TABLE MAX REACHED                            
         STCM  RE,3,NUMSER#S                                                    
*                                                                               
CKS#30   CLI   S#STATUS,S#NOTU_Q   NOT USED?                                    
         BNE   *+6                                                              
         DC    H'0'                NOT USED INSERTION CANNOT GET HERE!          
         MVC   S#MODCOD,D_MODCOD   SAVE MODIFICATION CODE                       
         MVC   S#PRCCNT,NUMPRCIN   SAVE ORDER OF PROCESSING COUNTER             
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R2,R4                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTNRT   NTR1  BASE=*,LABEL=*      FORMAT NEWSPAPER RATES                       
*                                                                               
         MVI   TMPBYTE1,0          COUNTER FOR # OF CHARS IN COST IND           
*                                                                               
         MVC   D_RATIND,SPACES                                                  
         MVC   D_INSRAT,SPACES                                                  
         MVC   TMPSWRK,SPACES                                                   
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         CLI   PBDCTYP,C'N'        NET INPUT SO DISPLAY AS NET                  
         BNE   FMTNRT02                                                         
*                                                                               
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
FMTNRT02 CLI   PBDCOSTY,C'U'       TEST UNIT RATE                               
         BE    FMTNRT06                                                         
         C     R1,=F'99999999'     SEE IF TOTAL RATE OVER 999,999.99            
         BNH   FMTNRT04                                                         
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'          HAVE ENTERED PENNIES WHEN BUYING             
         LTR   R1,R1               (NO ROOM)                                    
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         EDITR (R1),(9,TMPSWRK+5),0,FLOAT=-,ALIGN=LEFT                          
         B     FMTNRT08                                                         
*                                                                               
FMTNRT04 EDITR (R1),(9,TMPSWRK+5),2,FLOAT=-,ALIGN=LEFT                          
         B     FMTNRT08                                                         
*                                                                               
FMTNRT06 EDITR (R1),(11,TMPSWRK+5),5,FLOAT=-,ALIGN=LEFT                         
*                                                                               
         LA    R1,TMPSWRK+5-3      START OF OUTPUT                              
         AR    R1,R0               + LENGTH                                     
         CLC   =C'000',0(R1)                                                    
         BNE   FMTNRT08                                                         
         MVC   0(3,R1),SPACES      MOVE SOME BLANKS                             
*                                                                               
FMTNRT08 LA    R1,TMPSWRK+5                                                     
*                                                                               
* IF COST TYPE NOT 'U' DISPLAY IT, ELSE DISPLAY COST IND IF NOT C' '            
*                                                                               
         CLI   PBDCOSTY,C'U'       TEST UNIT RATE                               
         BE    FMTNRT10            YES - CHECK PBDCOSIN                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSTY                                                 
         SR    RF,RF                                                            
         IC    RF,TMPBYTE1                                                      
         AHI   RF,1                                                             
         STC   RF,TMPBYTE1                                                      
*                                                                               
FMTNRT10 CLI   PBDCOSIN,C' '       TEST DEFAULT COST TYPE                       
         BE    FMTNRT12                                                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSIN                                                 
         SR    RF,RF                                                            
         IC    RF,TMPBYTE1                                                      
         AHI   RF,1                                                             
         STC   RF,TMPBYTE1                                                      
*                                                                               
FMTNRT12 CLI   PBDCTYP,C'N'        NET INPUT                                    
         BNE   FMTNRT16                                                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCTYP                                                  
         SR    RF,RF                                                            
         IC    RF,TMPBYTE1                                                      
         AHI   RF,1                                                             
         STC   RF,TMPBYTE1                                                      
*                                                                               
FMTNRT16 TM    PBDRLIND,X'08'      TEST FROZEN RATE                             
         BZ    FMTNRT20                                                         
         BCTR  R1,0                                                             
         MVI   0(R1),C'*'                                                       
         SR    RF,RF                                                            
         IC    RF,TMPBYTE1                                                      
         AHI   RF,1                                                             
         STC   RF,TMPBYTE1                                                      
*                                                                               
FMTNRT20 SR    RF,RF                                                            
         IC    RF,TMPBYTE1                                                      
         LR    RE,R1               R1 POINTING TO FORMATTED RATE                
         AR    RE,RF                                                            
         MVC   D_INSRAT,0(RE)      FORMATTED RATE                               
         MVI   D_RATIND,C'G'       DEFAULT TO GROSS                             
         BCTR  RF,0                FOR EX                                       
         CHI   RF,0                                                             
         BL    FMTNRT28                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D_RATIND(0),0(R1)   GET COST INDS                                
FMTNRT28 CP    PBDCOS,=P'0'                                                     
         BNE   FMTNRT30                                                         
         MVC   D_INSRAT,SPACES                                                  
         MVC   D_INSRAT(04),=C'FREE'                                            
         MVC   D_RATIND,SPACES                                                  
* * * *  MVI   D_RATIND,C'F'                                                    
         B     FMTNRTX                                                          
*                                                                               
FMTNRT30 CLI   PBDCOSTY,C'U'       UNIT RATE?                                   
         BNE   FMTNRTX                                                          
         LA    RF,D_INSRAT                                                      
         CLI   0(RF),C' '                                                       
         BNH   *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         MVI   0(RF),C'/'                                                       
         MVC   1(2,RF),=C'LI'      TO INDICATE LINES                            
         CLI   PBDUIND,C'L'                                                     
         BE    *+10                                                             
         MVC   1(2,RF),=C'IN'      TO INDICATE INCHES                           
*                                                                               
FMTNRTX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTORF   NTR1  BASE=*,LABEL=*      FORMAT NON-NEWSPAPER RATE FORMAT             
*                                                                               
         MVI   TMPBYTE1,0          COUNTER FOR # OF CHARS IN COST IND           
*                                                                               
         MVC   D_RATIND,SPACES                                                  
         MVC   D_INSRAT,SPACES                                                  
         MVC   TMPSWRK,SPACES                                                   
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         CLI   PBDCTYP,C'N'        NET INPUT SO DISPLAY AS NET                  
         BNE   FMTORF02                                                         
*                                                                               
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               =NET PCT                                     
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
FMTORF02 EDITR (R1),(10,TMPSWRK+2),2,ALIGN=LEFT,FLOAT=-                         
         LA    R1,TMPSWRK+2                                                     
         CLI   PBDCOSIN,C' '       TEST DEFAULT COST TYPE                       
         BE    FMTORF04                                                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSIN                                                 
         SR    RF,RF                                                            
         IC    RF,TMPBYTE1                                                      
         AHI   RF,1                                                             
         STC   RF,TMPBYTE1                                                      
*                                                                               
FMTORF04 CLI   PBDCTYP,C'N'        DISPLAY AS NET                               
         BNE   FMTORF18                                                         
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCTYP                                                  
         SR    RF,RF                                                            
         IC    RF,TMPBYTE1                                                      
         AHI   RF,1                                                             
         STC   RF,TMPBYTE1                                                      
*                                                                               
FMTORF18 TM    PBDRLIND,X'08'      TEST FROZEN                                  
         BZ    FMTORF20                                                         
         BCTR  R1,0                                                             
         MVI   0(R1),C'*'                                                       
         SR    RF,RF                                                            
         IC    RF,TMPBYTE1                                                      
         AHI   RF,1                                                             
         STC   RF,TMPBYTE1                                                      
*                                                                               
FMTORF20 SR    RF,RF                                                            
         IC    RF,TMPBYTE1                                                      
         LR    RE,R1               R1 POINTING TO FORMATTED RATE                
         AR    RE,RF                                                            
         MVC   D_INSRAT,0(RE)      FORMATTED RATE                               
         MVI   D_RATIND,C'G'       DEFAULT TO GROSS                             
         BCTR  RF,0                FOR EX                                       
         CHI   RF,0                                                             
         BL    FMTORF28                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D_RATIND(0),0(R1)   GET COST INDS                                
FMTORF28 CP    PBDCOS,=P'0'                                                     
         JNE   FMTORFX                                                          
         MVC   D_INSRAT,SPACES                                                  
         MVC   D_INSRAT(04),=C'FREE'                                            
         MVC   D_RATIND,SPACES                                                  
         MVI   D_RATIND,C'F'                                                    
*                                                                               
FMTORFX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FORMAT NEWSPAPER CONTRACT LINEAGE EQUIVALENCY                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTNCLE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =X'7B00',PBDSPACE                                                
         BE    FMTNCLX                                                          
         CLC   =C'# ',PBDSPACE                                                  
         BE    FMTNCLX                                                          
         CLC   =C'* ',PBDSPACE     SEE IF SPACE BUY                             
         BNL   FMTNCLX             IF NOT - NO CLE                              
                                                                                
         MVC   D_NCONLE,SPACES                                                  
         LA    R5,D_NCONLE         SET CONTRACT LINEAGE EQUIVALENCY             
         CLI   PBDUIND,C'I'-X'40'  SEE IF LOWER CASE I                          
         BNE   FMTNCL02                                                         
         EDITR PBDUNITS,(6,0(R5)),2,ALIGN=LEFT                                  
         AR    R5,R0                                                            
         MVI   0(R5),C'I'                                                       
         B     FMTNCLX                                                          
                                                                                
FMTNCL02 EDITR PBDUNITS,(5,0(R5)),ALIGN=LEFT                                    
         AR    R5,R0                                                            
         CLI   PBDUIND,C'I'                                                     
         BNE   *+12                                                             
         MVI   0(R5),C'I'                                                       
         AHI   R5,1                                                             
         CP    PBDCLMS,=P'0'                                                    
         BE    FMTNCLX                                                          
         MVI   0(R5),C'/'                                                       
         AHI   R5,1                                                             
         EDITR PBDCLMS,(5,0(R5)),ALIGN=LEFT                                     
                                                                                
FMTNCLX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BYMVTRCM NTR1  BASE=*,LABEL=*      BUY MOVE TRANSITION COMMENT                  
*                                                                               
         TM    WKESRFLG,E_SRBM_Q   BUY MOVE ELEM PRESENT?                       
         JZ    EXIT                                                             
         TM    WKESRFLG,E_SRBM#Q   BUY MOVE ORDER HAS BEEN ISSUED?              
         JNZ   EXIT                                                             
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         CLC   LIOBPCV1,=AL1(03,04,00,05)                                       
         JL    EXIT                                                             
*                                                                               
         MVI   WKBYMCSW,0          INIT BUY MOVE COMMENT SWITCH                 
         NI    ADRPYSW3,X'FF'-RPYTRCMQ                                          
*                                                                               
         TM    WKESRFLG,E_SRBMFQ   BUY MOVE "FROM" ELEM PRESENT?                
         BZ    BYMVT_50                                                         
         OI    WKBYMCSW,B_MVFROQ                                                
         NI    WKBYMCSW,X'FF'-B_MVTO_Q                                          
         BRAS  RE,BYMCM_TF         DO "FROM" BUY MOVE COMMENT                   
*                                                                               
BYMVT_50 TM    WKESRFLG,E_SRBMTQ   BUY MOVE "TO" ELEM PRESENT?                  
         BZ    BYMVT_X                                                          
         OI    WKBYMCSW,B_MVTO_Q                                                
         NI    WKBYMCSW,X'FF'-B_MVFROQ                                          
         BRAS  RE,BYMCM_TF         DO "TO" BUY MOVE COMMENT                     
*                                                                               
BYMVT_X  J     EXIT                                                             
*                                                                               
BYMCM_TF ST    RE,TMPSVRE          "TO" & "FROM" BUY MOVE COMMENT               
         USING PBYMELEM,R2                                                      
         LA    R2,PBUYREC+33                                                    
BYMCM20  MVI   ELCODE,PBYMELCQ     BUY MOVE ELEM CODE                           
BYMCM24  BRAS  RE,NXTEL                                                         
         BNE   BYMCM36                                                          
*                                                                               
         TM    WKBYMCSW,B_MVFROQ   PROCESS BUY MOVE "FROM" COMMENT?             
         BZ    *+12                                                             
         TM    PBYMSTAT,PBYMFROQ   BUY MOVED "FROM" ELEM?                       
         BZ    BYMCM24                                                          
*                                                                               
         TM    WKBYMCSW,B_MVTO_Q   PROCESS BUY MOVE "TO" COMMENT?               
         BZ    *+12                                                             
         TM    PBYMSTAT,PBYMTO_Q   BUY MOVED "TO" ELEM?                         
         BZ    BYMCM24                                                          
*                                                                               
         ZAP   WKBYSER#,PBYMSER#   SERIAL# OF "FROM" INSERTION                  
         BRAS  RE,GBUYR_S#         GET BUY RECORD FROM SERIAL#                  
         BNE   BYMCM_X                                                          
         LA    R2,TMPWKAIO+33                                                   
         BRAS  RE,FND_BESR         LASTEST EIO ELEM FOUND?                      
         BNE   BYMCM32                                                          
*                                                                               
         USING PESRELEM,R2                                                      
BYMCM26  XC    TMPWK1,TMPWK1       PREPARE TO REPLY TRANSITION COMMENT          
         BRAS  RE,RPY#SRTC         REPLY TRANSITION COMMENT REC CODE            
         GOTOR DATCON,DMCB,(3,PESRDATE),(21,WKSRCDAT)                           
         TM    WKBYMCSW,B_MVFROQ   PROCESS BUY MOVE "FROM" COMMENT?             
         BZ    BYMCM26M                                                         
         MVC   TMPWK1(L'BYMCMTX3),BYMCMTX3                                      
         MVC   WKINSDT,TMPWKAIO+(PBUYKDAT-PBUYKEY)                              
         MVC   WKFREQU,TMPWKAIO+33+(PBDFREQ-PBDELEM)                            
         MVC   WKBUYLN,TMPWKAIO+(PBUYKLIN-PBUYKEY)                              
         GOTOR FMTBUYL#,2          GET INSERTION DATE WITH REF#                 
         MVC   TMPWK1+L'BYMCMTX3(L'D_INSDLN),TMPCL20                            
         LA    RF,TMPWK1+L'BYMCMTX3+L'D_INSDLN                                  
         BRAS  RE,LAST_CHR                                                      
         MVC   02(10,RF),BYMCMTX3                                               
         MVC   12(03,RF),=C'FOR'                                                
         MVC   16(03,RF),TMPWKAIO+(PBUYKCLT-PBUYKEY)                            
         MVI   19(RF),C'/'                                                      
         MVC   20(03,RF),TMPWKAIO+(PBUYKPRD-PBUYKEY)                            
         BRAS  RE,RPY#TRCM         REPLY TRANSITION COMMENT                     
         XC    TMPWK1,TMPWK1                                                    
         MVC   TMPWK1(L'BYMCMTX4),BYMCMTX4                                      
         BRAS  RE,FMT_BSR#         FORMAT BUY SPACE RESERVATION #               
         MVC   TMPWK1+L'BYMCMTX4(25),TMPWK2                                     
         LA    RF,TMPWK1+L'BYMCMTX4+25+1                                        
         BRAS  RE,LAST_CHR                                                      
         MVC   02(10,RF),=C'CREATED ON'                                         
         MVC   13(10,RF),WKSRCDAT                                               
         BRAS  RE,RPY#TRCM         REPLY TRANSITION COMMENT                     
BYMCM26M TM    WKBYMCSW,B_MVTO_Q   PROCESS BUY MOVE "TO" COMMENT?               
         BZ    BYMCM26X                                                         
         MVC   TMPWK1(L'BYMCMTX2),BYMCMTX2                                      
         BRAS  RE,FMT_BSR#         FORMAT BUY SPACE RESERVATION #               
         MVC   TMPWK1+L'BYMCMTX2(25),TMPWK2                                     
         LA    RF,TMPWK1+L'BYMCMTX2+25+1                                        
         BRAS  RE,LAST_CHR                                                      
         MVC   02(03,RF),=C'FOR'                                                
         MVC   06(03,RF),TMPWKAIO+(PBUYKCLT-PBUYKEY)                            
         MVI   09(RF),C'/'                                                      
         MVC   10(03,RF),TMPWKAIO+(PBUYKPRD-PBUYKEY)                            
         BRAS  RE,RPY#TRCM         REPLY TRANSITION COMMENT                     
         XC    TMPWK1,TMPWK1                                                    
         MVC   TMPWK1+00(10),=C'CREATED ON'                                     
         MVC   TMPWK1+11(10),WKSRCDAT                                           
         BRAS  RE,RPY#TRCM         REPLY TRANSITION COMMENT                     
BYMCM26X B     BYMCM_X                                                          
*                                                                               
BYMCM32  LA    R2,TMPWKAIO+33      LOOK FOR BUY MOVE "FROM" ELEM AGAIN          
         B     BYMCM20                                                          
*                                                                               
BYMCM36  TM    WKBYMCSW,B_MVRTFQ   ALREADY REPLIED?                             
         BNZ   BYMCM_X                                                          
         TM    WKESRFLG,E_SRBMTQ   HAS BUY MOVE "TO" INSERTION?                 
         BZ    BYMCM_X                                                          
         XC    TMPWK1,TMPWK1                                                    
         MVC   TMPWK1(L'BYMCMTX1),BYMCMTX1                                      
         BRAS  RE,RPY#SRTC         REPLY TRANSITION COMMENT REC CODE            
         BRAS  RE,RPY#TRCM         REPLY TRANSITION COMMENT                     
         OI    WKBYMCSW,B_MVRTFQ                                                
BYMCM_X  L     RE,TMPSVRE                                                       
         BR    RE                                                               
*                                                                               
GBUYR_S# LR    R0,RE               GET BUY RECORD FROM SERIAL#                  
         MVC   TMPSVKEY,KEY                                                     
         MVC   TMPSVAIO,AREC                                                    
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PSERKEY,RE                                                       
         MVC   PSERKAGY,PBUYKAGY                                                
         MVC   PSERKMED,PBUYKMED                                                
         MVI   PSERKRCD,PSERKIDQ                                                
         MVC   PSERKCLT,PBUYKCLT                                                
         ZAP   DUB,PACK1BIL                                                     
         SP    DUB,WKBYSER#                                                     
         MVC   PSERKNUM,DUB+3                                                   
         MVC   WKDMINBT,DMINBTS    SAVE DATA MANAGER BITS                       
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTOR HIGH                                                             
         CLC   KEY(25),KEYSAVE     RECORD FOUND?                                
         JE    GB_S#50                                                          
         MVC   KEY,TMPSVKEY        RESTORE ORIGINAL SETTINGS                    
         MVC   AREC,TMPSVAIO                                                    
         MVC   DMINBTS,WKDMINBT                                                 
         LR    RE,R0                                                            
         J     RE_CCNEQ                                                         
*                                                                               
GB_S#50  LA    RE,TMPWKAIO                                                      
         ST    RE,AREC                                                          
         GOTOR GETPRT                                                           
         MVC   KEY,TMPSVKEY        RESTORE ORIGINAL SETTINGS                    
         MVC   AREC,TMPSVAIO                                                    
         MVC   DMINBTS,WKDMINBT                                                 
         LR    RE,R0                                                            
         J     RE_CCEQ                                                          
*                                                                               
FND_BESR LR    R1,RE               FIND BUY ESR ELEM                            
         USING PESRELEM,R2                                                      
         MVI   ELCODE,PESRELCQ                                                  
         LR    RF,R2                                                            
         BRAS  RE,NXTEL                                                         
         JE    *-6                                                              
         LR    R2,RF                                                            
         LR    RE,R1                                                            
         CLI   PESRELEM,PESRELCQ   LATEST ESR ELEM FOUND?                       
         JNE   RE_CCNEQ                                                         
         OC    PESRDATE,PESRDATE   ACTUAL ESR ELEM?                             
         JZ    RE_CCNEQ                                                         
         J     RE_CCEQ                                                          
*                                                                               
RE_CCEQ  CR    RE,RE               CC EQUAL                                     
         J     *+6                                                              
RE_CCNEQ LTR   RE,RE               CC NOT EQUAL                                 
         BR    RE                                                               
*                                                                               
FMT_BSR# LR    R5,RE               FORMAT BUY SPACE RESERVATION #               
         XC    TMPWK2,TMPWK2                                                    
         USING PESRELEM,R2                                                      
         USING ESRLKEYD,R4                                                      
         LA    R4,TMPWK2                                                        
         MVI   ESRLGKEY,C'-'                                                    
         MVC   ESRLGKEY+1(ESRLGKYL-1),ESRLGKEY                                  
         MVC   ESRLGSRT,=C'SR'                                                  
         MVC   ESRLGMED,TMPWKAIO+(PBUYKMED-PBUYKEY)                             
         MVC   WORK(L'PESRDATE),PESRDATE                                        
         MVC   WORK(L'PESR#YER),PESR#YER                                        
         GOTOR DATCON,DMCB,(3,WORK),(10,DUB)                                    
         MVC   ESRLGYER,DUB+6                                                   
         MVC   ESRLGCLT,TMPWKAIO+(PBUYKCLT-PBUYKEY)                             
         CLI   ESRLGCLT+2,C' '                                                  
         JNE   *+8                                                              
         MVI   ESRLGCLT+2,C'-'                                                  
         EDITR PESR#SQ#,ESRLGRNO,0,FILL=0                                       
         MVI   ESRLGRNO+L'ESRLGRNO,C' '                                         
         MVC   ESRLGRET,SPACES                                                  
         MVC   ESRLGRE#,SPACES                                                  
         CLI   PESR#REV,0                                                       
         JE    FMT_BSRX            NO REVISION #                                
         MVI   ESRLGRNO+L'ESRLGRNO,C'-'                                         
         MVC   ESRLGRET,=C'REV'                                                 
         EDITR PESR#REV,ESRLGRE#,0,FILL=0                                       
         SR    RE,RE                                                            
         ICM   RE,7,PESR#SQ#                                                    
         CHI   RE,9999                                                          
         JH    FMT_BSRX                                                         
         MVC   ESRLGKEY+DISPER#Q(LNAERF#Q),ESRLGKEY+DISP_R#Q                    
         MVI   ESRLGKEY+(ESRLGKYL-L'ESRLGR#E),C' '                              
FMT_BSRX LR    RE,R5                                                            
         BR    RE                                                               
*                                                                               
RPY#TRCM LR    R0,RE               REPLY TRANSITION COMMENT                     
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#COMMNT),    +        
               ('LD_CHARQ',TMPWK1),(L'ESRCOMLN,0)                               
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
BYMCMTX1 DC    C'REPLACEMENT SPACE RESERVATION TO FOLLOW'                       
BYMCMTX2 DC    C'INSERTION HAS BEEN MOVED TO SR# '                              
BYMCMTX3 DC    C'INSERTION REPLACES '                                           
BYMCMTX4 DC    C'PREVIOUSLY ON SPACE RESERVATION '                              
*                                                                               
         LTORG                                                                  
         DROP  RB,RE,R2,R3,R4                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYACHRG NTR1  BASE=*,LABEL=*      REPLY ADDITIONAL CHARGES                     
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    EXIT                NO                                           
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'44'        ADDITIONAL CHARGE                            
RPYAC20  BRAS  RE,NXTEL                                                         
         BNE   RPYAC60                                                          
         USING PACELEM,R2                                                       
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#ACR)                 
         MVC   TMPHALF1,=AL2(D#ACHCOD)                                          
         BRAS  RE,RPYFLDID                                                      
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACHCOD),    +        
               ('LD_CHARQ',PACCODE),(L'PACCODE,0)                               
*                                                                               
         OC    PACAMT,PACAMT                                                    
         BNZ   *+10                                                             
         ZAP   PACAMT,=P'0'                                                     
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACHGRS),    +        
               ('LD_SPAKQ',PACAMT),(L'PACAMT,0)                                 
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACHSAC),    +        
               ('LD_CHARQ',PACAC),(L'PACAC,0)                                   
*                                                                               
         MVC   TMPWK1(L'SPACES),SPACES                                          
         OC    PACACOM,PACACOM                                                  
         BNZ   RPYAC52                                                          
         MVC   TMPWK1(2),=C'-1'    INDICATING N/A                               
         LHI   RF,2                                                             
         B     RPYAC54                                                          
RPYAC52  EDIT  PACACOM,(8,TMPWK1),3,COMMAS=NO,ALIGN=LEFT                        
         LR    RF,R0                                                            
RPYAC54  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACHCPT),    +        
               ('LD_CHARQ',TMPWK1),((RF),0)                                     
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ACHCDA),    +        
               ('LD_CHARQ',PACCD),(L'PACCD,0)                                   
*                                                                               
         B     RPYAC20             CK FOR MORE                                  
*                                                                               
RPYAC60  DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R2,R3                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPYCUCOL NTR1  BASE=*,LABEL=*      REPLY CUSTOM COLUMNS                         
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR UPLOAD?                                  
         JZ    EXIT                NO                                           
*                                                                               
         USING BYCCELD,R2                                                       
         CLI   BYCCELM,BYCCIDQ                                                  
         BE    *+6                                                              
         DC    H'0'                NO LONGER POINTING TO CC ELEM                
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#BCC)                 
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CCSEQN),    +        
               ('LD_UBINQ',BYCCSQN),(L'BYCCSQN,0)                               
*                                                                               
         LA    RE,TMPWKAIO                                                      
         USING PCOLRECD,RE                                                      
         CLI   PCOLKRCD,X'61'                                                   
         BE    *+6                                                              
         DC    H'0'                NO LONGER POINTING TO CUSTOM COL REC         
*                                                                               
         XC    TMPWK1,TMPWK1       PREPARE TO REPLY CUSTOM COL DATA FLD         
*                                                                               
         CLI   PCOLTYP,C'D'        DATE?                                        
         BE    RPYCC40                                                          
         CLI   PCOLTYP,C'P'        PERIOD?                                      
         BE    RPYCC44                                                          
         CLI   PCOLTYP,C'T'        TEXT?                                        
         BE    RPYCC48                                                          
         CLI   PCOLTYP,C'N'        NUMERIC?                                     
         BE    RPYCC52                                                          
         CLI   PCOLTYP,C'$'        DOLLAR?                                      
         BE    RPYCC52                                                          
         CLI   PCOLTYP,C'%'        PERCENT?                                     
         BE    RPYCC52                                                          
         DC    H'0'                UNKNOWN TYPE                                 
*                                                                               
RPYCC40  MVC   DUB(3),BYCCDATA                                                  
         BRAS  RE,GETBCCDT                                                      
         MVC   TMPWK1(10),WORK                                                  
         LA    RF,10               LENGTH OF DATE FORMAT MM/DD/YYYY             
         B     RPYCC70                                                          
*                                                                               
RPYCC44  MVC   DUB(3),BYCCDATA                                                  
         BRAS  RE,GETBCCDT                                                      
         MVC   TMPWK1(10),WORK                                                  
         MVI   TMPWK1+10,C'-'                                                   
         MVC   DUB(3),BYCCDATA+3                                                
         BRAS  RE,GETBCCDT                                                      
         MVC   TMPWK1+11(10),WORK                                               
         LA    RF,10+1+10          DATE RANGE (MM/DD/YYYY-MM/DD/YYYY)           
         B     RPYCC70                                                          
*                                                                               
RPYCC48  SR    RE,RE                                                            
         IC    RE,BYCCLEN                                                       
         SHI   RE,BYCCHDRL         MINUS OVERHEAD                               
         LR    RF,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TMPWK1(0),BYCCDATA                                               
         B     RPYCC70                                                          
*                                                                               
RPYCC52  XC    DUB,DUB             DUMMY CURRENCY DEFINITION FLD                
         MVC   DUB+3(1),PCOLDECS                                                
         LA    RF,DUB                                                           
         CURED (P8,BYCCDATA),(20,TMPWK1),(RF),ALIGN=LEFT,FLOAT=-,      +        
               COMMAS=YES                                                       
         LR    RF,R0               # OF SIGNIFICANT CHARS                       
*                                                                               
RPYCC70  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CCFDAT),    +        
               ('LD_CHARQ',TMPWK1),((RF),0)                                     
*                                                                               
         J     EXIT                                                             
*                                                                               
GETBCCDT LR    R0,RE                                                            
         GOTOR DATCON,DMCB,(3,DUB),(23,TMPWK2)                                  
         MVC   WORK+0(2),TMPWK2+5                                               
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),TMPWK2+8                                               
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(4),TMPWK2+0                                               
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,RE,R2,R3                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMT_COST NTR1  BASE=*,LABEL=*      FORMAT INSERTION COST                        
*                                                                               
         MVC   D_INSCOS,SPACES                                                  
         MVC   D_INSCO2,SPACES                                                  
         MVI   D_COSIND,C'G'                                                    
         L     R0,GROSS                                                         
*                                                                               
         CLI   SHWINSCO,C'B'       BOTH GROSS AND NET?                          
         BNE   F_COS16                                                          
         EDIT  (R0),D_INSCOS,2,ALIGN=LEFT,MINUS=YES,COMMAS=YES                  
         L     R0,GROSS                                                         
         S     R0,AGYCOM                                                        
         MVI   D_COSIN2,C'N'                                                    
         EDIT  (R0),D_INSCO2,2,ALIGN=LEFT,MINUS=YES,COMMAS=YES                  
         J     EXIT                                                             
*                                                                               
F_COS16  CLI   SHWINSCO,C'G'       GROSS?                                       
         BE    F_COS30                                                          
         CLI   SHWINSCO,C'T'       NET?                                         
         BE    F_COS20                                                          
         CLI   PBDCTYP,C'N'        BUY IS ENETERED IN NET?                      
         BE    F_COS20                                                          
         CLI   PBDCOSIN,C'S'       SHOW NET FOR "S" RATE BUYS?                  
         BNE   F_COS30                                                          
F_COS20  S     R0,AGYCOM                                                        
         MVI   D_COSIND,C'N'                                                    
*                                                                               
F_COS30  EDIT  (R0),D_INSCOS,2,ALIGN=LEFT,MINUS=YES,COMMAS=YES                  
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP                                                                   
*                                                                               
PPSR211D DSECT                                                                  
*                                                                               
RELO11   DS    F                                                                
*                                                                               
A_CONREC DS    A                   ADDRESS OF CONTRACT RECORD                   
A_JOBREC DS    A                   ADDRESS OF JOB RECORD                        
*                                                                               
PACK1BIL DS    PL6                 PACK 1,000,000,000                           
*                                                                               
BUYOUTA  DS    XL(BUYOUTLQ)                                                     
BUYOUTLQ EQU   600                                                              
*                                                                               
SVBUYKEY DS    XL(L'KEY)           SAVE BUY KEY                                 
SVCONKEY DS    XL(L'KEY)           SAVE CONTRACT KEY                            
SVJOBKEY DS    XL(L'KEY)           SAVE JOB KEY                                 
*                                                                               
GKEY     DS    XL48                                                             
GKEYSAVE DS    XL48                                                             
*                                                                               
TMPSVKEY DS    XL(L'KEY)           TEMP SAVE KEY STORAGE                        
TMPSVAIO DS    XL(L'AREC)                                                       
*                                                                               
TMPSVKY2 DS    XL(L'KEY)                                                        
TMPSVIO2 DS    XL(L'AREC)                                                       
*                                                                               
SVPUBZEC DS    XL(L'PUBKPUB+L'PUBKZON+L'PUBKED)                                 
SVPUBZNM DS    XL(L'PUBZNAME)                                                   
SVCSTDCM DS    CL(L'PSTDCOM)                                                    
SVBSTDCM DS    CL(L'PSTDCOM)                                                    
*                                                                               
RPYERTXT DS    CL60                REPLY ERROR TXT                              
*                                                                               
WKINSCNT DS    XL4                                                              
WKBFDAY  DS    XL(L'PBDBFD)                                                     
WKINSDT  DS    XL(L'PBUYKDAT)                                                   
WKFREQU  DS    XL(L'PBDFREQ)                                                    
WKBUYLN  DS    XL(L'PBUYKLIN)                                                   
WKDMINBT DS    XL(L'DMINBTS)                                                    
WKBYSER# DS    PL(L'PSERNUM)                                                    
WKSRCDAT DS    CL10                SPACE RESV CREATED DATE - MMMDD/YYYY         
*                                                                               
WKESRFLG DS    X                   ESR WORK FLAG                                
ESR_GENQ EQU   X'80'               ESR GENERATED IN SAME DAY                    
ESR_ELMQ EQU   X'40'               ESR ELEM EXIST IN INSERTION                  
E_SRBMFQ EQU   X'20'               CONTAIN BUY MOVE "FROM" INSERTION            
E_SRBMTQ EQU   X'10'               CONTAIN BUY MOVE "TO" INSERTION              
E_SRBMDQ EQU   X'08'               DELETED BUY MOVE CHANGE RESV ISSUED          
E_SRBMOQ EQU   X'04'               BUY MOVE SPACE RESERVATION ISSUED            
E_SRBM_Q EQU   E_SRBMFQ+E_SRBMTQ   BOTH BUY MOVE "FROM" AND "TO"                
E_SRBM#Q EQU   E_SRBMDQ+E_SRBMOQ   BUY MOVE RESV HAS BEEN ISSUED                
*                                                                               
WKBYMCSW DS    X                   BUY MOVE COMMENT SWITCH                      
B_MVTO_Q EQU   X'80'               PROCESS "TO" COMMENT                         
B_MVFROQ EQU   X'40'               PROCESS "FROM" COMMENT                       
B_MVRTFQ EQU   X'20'               REPLACEMENT IO TO FOLLOW MSG REPLIED         
*                                                                               
TMPSVRE  DS    F                   FOR SAVING RETURN ADDRESSES                  
TMPFULL1 DS    F                   TEMP WORKING STORAGES                        
TMPFULL2 DS    F                                                                
TMPHALF1 DS    H                                                                
TMPHALF2 DS    H                                                                
TMPBYTE1 DS    X                                                                
TMPBYTE2 DS    X                                                                
TMPBYTE3 DS    X                                                                
TMPPL4   DS    PL4                                                              
TMPCL20  DS    CL20                                                             
TMPSWRK  DS    CL80                                                             
TMPWK1   DS    XL256                                                            
TMPWK2   DS    XL256                                                            
*                                                                               
FIDCHGTB DS    (FIDCTMXQ)XL2       TABLE OF "CHANGED" MAP CODES                 
FIDCTMXQ EQU   100                 MAXIMUM ENTRIES IN TABLE                     
FIDCHGLQ EQU   *-FIDCHGTB                                                       
*                                                                               
* SAVED DATA FOR DETAIL SECTION OF DATA STREAM                                  
*                                                                               
D_SVSTRT DS    0X                                                               
D_INSDAT DS    XL(L'PBUYKDAT)      INSERTION DATE                               
D_INSDLN DS    CL15                INSERTION DATE WITH LINE NUMBER              
D_MODCOD DS    C                   MODIFICATION CODE (CHG/DEL/CAN ETC.)         
D_PUBZON DS    CL(L'QZONE)         PUBLICATION ZONE                             
D_PUBEDT DS    CL(L'QEDITION)      PUBLICATION EDITION                          
*                                                                               
D_CONNUM DS    XL(L'PCONNUM)       CONTRACT NUMBER                              
D_CONSIG DS    CL(L'PCONREQ)       CONTRACT AGENCY SIGNER                       
D_CONSDT DS    XL(L'PCONSDT)       CONTRACT START DATE                          
D_CONEDT DS    XL(L'PCONEDT)       CONTRACT END DATE                            
D_CONLVL DS    XL(L'PRBLEVEL)      CONTRACT LEVEL                               
D_CONEFD DS    XL(L'PRBDATE)       CONTRACT EFFECTIVE DATE                      
D_CONPCT DS    PL(L'PRBPCT)        CONTRACT PERCENT DISCOUNT                    
*                                                                               
D_ADCODE DS    CL(L'PJOBKJOB)      AD CODE                                      
D_ADID   DS    CL(L'PJOBADID)      AD ID                                        
D_CPYNUM DS    CL(L'PJOBCPY)       COPY NUMBER                                  
D_ADCAP1 DS    CL(L'PJOBCAP1)      AD CAPTION 1                                 
D_ADCAP2 DS    CL(L'PJOBCAP2)      AD CAPTION 2                                 
*                                                                               
D_ESRTYP DS    C                   INSERION ORDER TYPE                          
D_ESRTYQ EQU   C'S'                ENHANCED SPACE RESERVATION (DEFAULT)         
*                                                                               
D_RATIND DS    CL3                 INSERTION RATE INDICATOR                     
D_INSRAT DS    CL14                INSERTION RATE                               
D_COSIND DS    CL1                 INSERTION COST INDICATOR                     
D_INSCOS DS    CL14                INSERTION COST                               
D_COSIN2 DS    CL1                 INSERTION COST INDICATOR 2                   
D_INSCO2 DS    CL14                INSERTION COST 2                             
D_NCONLE DS    CL12                CONTRACT LINEAGE EQUIVALENCY                 
D_SVDLNQ EQU  *-D_SVSTRT                                                        
*                                                                               
TMPWKAIO DS    XL4096              TEMP WORKING AIO                             
CONR_AIO DS    XL4096              AIO FOR CONTRACT RECORD                      
JOBR_AIO DS    XL4096              AIO FOR JOB RECORD                           
*                                                                               
PPSR211X EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPSR2WRK1                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSR2WRK2                                                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012PPSR211   09/14/15'                                      
         END                                                                    
*                                                                               
