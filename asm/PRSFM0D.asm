*          DATA SET PRSFM0D    AT LEVEL 049 AS OF 05/01/02                      
*PHASE T41C0DA,*                                                                
*INCLUDE SRCHCALL                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE NUMED                                                                  
         TITLE 'T41C0D PRINTPAK PUB ISSUE DATE RECORDS'                         
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
*   BOBY 8/95   ACCEPT TRANSFER FROM ANY SCREEN                                 
*                                                                               
*   BPLA 4/95   DISALLOW PUB=ALL UNLESS LISTING/REPORTING                       
*                                                                               
*                                                                               
*   BPLA 9/94   DISPLAY MEDIA IN HEADHOOK                                       
*   BPLA 9/94   TRY AND FIX PUB NAME DISPLAY                                    
*               CLEAR PUBNM AND PUBZNM IN MYPUBVAL                              
*                                                                               
*   BPLA 6/94   WHEN LISTING/REPORTING USE THE ENTERED YEAR AS                  
*               A FILTER                                                        
*                                                                               
*   BPLA 9/93   ADD CODE TO FORMAT RECORD - USING OPTIONS                       
*                                                                               
T41C0D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C0D,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
*                                                                               
         MVI   FMTSW,0             CLEAR FORMATTING INFO                        
         XC    FMTSTD,FMTSTD                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        RE-DISPLAY RECORD                            
         BE    DR                                                               
         CLI   MODE,XRECPUT        RE-DISPLAY RECORD                            
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DR                                                               
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    DR                                                               
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
*                                                                               
         CLI   TRANSSW,C'Y'        IF A TRANSFER INTO PROGRAM                   
         BNE   VKTRANSX                                                         
*                                                                               
         GOTO1 =A(GETKEYS),RR=RELO     GET KEY FIELDS                           
*                                                                               
VKTRANSX DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VKL                                                              
*                                                                               
         LA    R2,FSIMEDH         MEDIA                                         
         GOTO1 VALIMED                                                          
*                                                                               
*                                                                               
VK5      LA    R2,FSIPUBH                                                       
*                                                                               
         XC    FSIPUBN,FSIPUBN      CLEAR PUB NAME + ZONE + FREQ                
         XC    FSIPUBZ,FSIPUBZ                                                  
         XC    FSIFREQ,FSIFREQ                                                  
         FOUT  FSIPUBNH                                                         
         FOUT  FSIPUBZH                                                         
         FOUT  FSIFREQH                                                         
*                                                                               
         GOTO1 =A(MYPUBVAL),RR=RELO                                             
         MVC   FSIPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    FSIPUBNH+6,X'80'                                                 
         MVC   FSIPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    FSIPUBZH+6,X'80'                                                 
         MVC   FSIFREQ,MYPUBFQ      FREQUENCY                                   
         OI    FSIFREQH+6,X'80'                                                 
*                                                                               
         LA    R2,FSIYRH           YEAR                                         
         XC    QYR,QYR                                                          
         CLI   5(R2),0                                                          
         BE    VK15                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK15                                                             
         BAS   RE,MYYRVAL                                                       
         B     VK20                                                             
*                                                                               
VK15     CLI   ACTNUM,ACTREP       SEE IF REPORTING                             
         BE    VK20                                                             
         MVI   ERROR,MISSING      IF NOT YEAR IS REQUIRED                       
         B     ERRX                                                             
*                                                                               
VK20     DS    0H                                                               
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PISSREC,R4                                                       
         MVC   PISSKAGY,AGENCY                                                  
         MVC   PISSKMED,QMED                                                    
         MVI   PISSKTYP,X'29'                                                   
         MVC   PISSKPUB,BPUB                                                    
         MVC   PISSKYR,QYR                                                      
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
         B     VKXIT                                                            
*                                                                               
*        VALIDATE LIST KEY                                                      
*                                                                               
VKL      LA    R2,FSLMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
VKL5     LA    R2,FSIPUBH          PUB                                          
         XC    BPUB,BPUB                                                        
         XC    FSIPUBN,FSIPUBN                                                  
         XC    FSIPUBZ,FSIPUBZ                                                  
         XC    FSIFREQ,FSIFREQ                                                  
         FOUT  FSIPUBNH                                                         
         FOUT  FSIPUBZH                                                         
         FOUT  FSIFREQH                                                         
*                                                                               
         CLI   5(R2),0             ANY PUB INPUT?                               
         BE    VKL10                                                            
*                                                                               
         CLC   =C'ALL',8(R2)       TREAT SAME AS NO PUB                         
         BE    VKL10                                                            
*                                                                               
         GOTO1 =A(MYPUBVAL),RR=RELO                                             
         MVC   FSIPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    FSIPUBNH+6,X'80'                                                 
         MVC   FSIPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    FSIPUBZH+6,X'80'                                                 
         MVC   FSIFREQ,MYPUBFQ      FREQUENCY                                   
         OI    FSIFREQH+6,X'80'                                                 
*                                                                               
*                                                                               
*                                                                               
VKL10    XC    QYR,QYR             FOR LISTING CLEAR QYR                        
         LA    R2,FSLYRH           YEAR                                         
         CLI   5(R2),0                                                          
         BE    VKXIT                                                            
         CLC   =C'ALL',8(R2)                                                    
         BE    VKXIT                                                            
         BAS   RE,MYYRVAL                                                       
*                                                                               
VKXIT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE YEAR FIELD                                                    
*                                                                               
MYYRVAL  NTR1                                                                   
         LR    R4,R2                                                            
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BNE   VKYR5                                                            
         CLI   ACTNUM,ACTREP       SEE IF REPORTING                             
         BE    VKYRX                                                            
         B     ERRX                IF NOT THE MUST HAVE YEAR                    
*                                                                               
VKYR5    DS    0H                                                               
         CLI   5(R2),2             ACCEPT LAST 2 DIGITS OF YEAR                 
         BNE   VKYR7                                                            
         CLI   8(R2),C'0'                                                       
         BL    INVERR                                                           
         CLI   8(R2),C'9'                                                       
         BH    INVERR                                                           
         CLI   9(R2),C'0'                                                       
         BL    INVERR                                                           
         CLI   9(R2),C'9'                                                       
         BH    INVERR                                                           
*                                                                               
*                          NOTE***  CHECK BELOW WORK THROUGH 2070               
*                                                                               
         CLC   8(2,R2),=C'70'       SEE IF LOWER THAT 70                        
         BL    VKYR6                                                            
         MVC   QYR(2),=C'19'        IF HIGHER ASSUME 19                         
         MVC   QYR+2(2),8(R2)                                                   
         B     VKYRX                                                            
*                                                                               
VKYR6    MVC   QYR(2),=C'20'  IF YEAR IS LOWER THAN 70 - ASSUME 20              
         MVC   QYR+2(2),8(R2)                                                   
         B     VKYRX                                                            
*                                                                               
VKYR7    CLI   5(R2),4          ACCEPT FULL YEAR                                
         BNE   INVERR                                                           
         LA    R4,4                                                             
         LA    R5,8(R2)                                                         
VKYR9    CLI   0(R5),C'0'                                                       
         BL    INVERR                                                           
         CLI   0(R5),C'9'                                                       
         BH    INVERR                                                           
         LA    R5,1(R5)                                                         
         BCT   R4,VKYR9                                                         
         CLC   8(2,R2),=C'19'                                                   
         BL    INVERR                                                           
         CLC   8(2,R2),=C'20'    THIS CHECK IS GOOD THROUGH 2099                
         BH    INVERR                                                           
         MVC   QYR(4),8(R2)                                                     
*                                                                               
VKYRX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PISSREC,R6                                                       
         MVC   FSIMED,PISSKMED     MEDIA                                        
         OI    FSIMEDH+6,X'80'     TRANSMIT                                     
*                                                                               
DK12     GOTO1 =V(PUBEDIT),DMCB,(C'0',PISSKPUB),(0,FSIPUB),RR=RELO              
         OI    FSIPUBH+6,X'80'                                                  
*                                                                               
         MVC   MYPUB,PISSKPUB                                                   
         BAS   RE,MYVPUB                                                        
         MVC   FSIPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    FSIPUBNH+6,X'80'                                                 
         MVC   FSIPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    FSIPUBZH+6,X'80'                                                 
         MVC   FSIFREQ,MYPUBFQ      FREQUENCY                                   
         OI    FSIFREQH+6,X'80'                                                 
*                                                                               
*                                                                               
*                                                                               
         MVC   FSIYR,PISSKYR                                                    
         OI    FSIYRH+6,X'80'        TRANSMIT                                   
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         MVI   FMTSW,0                                                          
         XC    FMTSTD,FMTSTD                                                    
*                                                                               
         LA    R2,FSIOPTH                                                       
         CLI   5(R2),0             CHK FOR INPUT                                
         BNE   EDTOPT              GO TO EDIT FORMAT OPTIONS                    
*                                                                               
         LA    R2,FSIDT1H          SPACE                                        
         XR    R5,R5               COUNTER                                      
         LA    RE,DATETAB                                                       
         LH    RF,=H'450'          150 X 3                                      
         XCEF                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'29'                                                     
         GOTO1 REMELEM             REMOVE ALL OLD ELEMENTS                      
*                                                                               
VR10     LA    R3,ELEMENT                                                       
         USING PISSEL29,R3                                                      
         XC    ELEMENT,ELEMENT                                                  
*                                                                               
VR15     CLI   5(R2),0             IF NO INPUT IN DATE FIELD                    
         BNE   VR30                                                             
         B     VR50                                                             
*                                                                               
VR30     CLC   =C'DEL',8(R2)    DELETE THIS ENTRY                               
         BNE   VR40                                                             
         B     VR50                                                             
*                                                                               
VR40     DS    0H                                                               
         CLI   5(R2),3                                                          
         BL    INVERR                                                           
         CLI   5(R2),5                                                          
         BH    INVERR                                                           
         MVI   0(R3),X'29'         ELEMENT CODE                                 
         MVI   1(R3),PISSELLN      ELEMENT LENGTH                               
         XC    WORK(10),WORK                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   WORK(0),8(R2)       EXECUTED                                     
*                                                                               
         LA    R4,WORK                                                          
         AH    R1,=H'1'                                                         
         AR    R4,R1                                                            
         MVI   0(R4),C'/'                                                       
         MVC   1(2,R4),QYR+2                                                    
*                                                                               
         SR    R4,R4          VALIDATE MMMDD/YY                                 
         CLI   WORK+1,C'/'     CATER TO N/N/YY - EDIT FOR MMMDD/YY              
         BE    VR43                                                             
         CLI   5(R2),3            SEE IF MONTH INPUT                            
         BNE   *+8                                                              
         LA    R4,2           VALIDATE MMM/YR                                   
*                                                                               
VR43     DS    0H                                                               
         GOTO1 DATVAL,DMCB,((R4),WORK),WORK+10                                  
         OC    DMCB(4),DMCB                                                     
         BZ    INVERR                                                           
*                                                                               
         GOTO1 DATCON,DMCB,WORK+10,(3,PISSDAT)                                  
*                                                                               
VR45     BAS   RE,DUPDAT        MAKE SURE NO DUPLICATE DATE ENTRIES             
         BE    DUPERR                                                           
*                                                                               
VR49     CH    R5,=H'150'                                                       
         BE    NOMORE                                                           
         GOTO1 ADDELEM                                                          
VR49X    LA    R5,1(R5)            COUNTER                                      
*                                                                               
VR50     BAS   RE,BUMP             BUMP TO NEXT LINE                            
         LA    R1,FSILAST                                                       
         CR    R2,R1               SEE IF AT END OF SCREEN                      
         BL    VR10                                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* EDIT OPTIONS FOR FORMATING                                                    
*                                                                               
EDTOPT   DS    0H                                                               
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(12,(R2)),(5,SCANBLK)                               
         LA    R4,SCANBLK                                                       
EDTO5    CLI   0(R4),0                                                          
         BE    EDTOX                                                            
         CLC   =C'FREQ',12(R4)                                                  
         BNE   EDTO20                                                           
         CLI   FMTSW,0          SEE IF ALREADY GIVEN                            
         BNE   INVERR                                                           
         CLI   1(R4),0                                                          
         BE    INVERR                                                           
         L     R5,=A(FREQTAB)  TABLE OF FORMATABLE FREQUENCIES                  
         A     R5,RELO             RELOCATE ADDRESS                             
EDTO8    CLI   0(R5),X'FF'     END OF TABLE                                     
         BE    INVERR                                                           
         CLC   0(2,R5),22(R4)                                                   
         BE    EDTO10                                                           
         LA    R5,3(R5)                                                         
         B     EDTO8                                                            
*                                                                               
EDTO10   MVC   FMTSW,2(R5)                                                      
         B     EDTNXTO                                                          
*                                                                               
EDTO20   CLC   =C'START',12(R4)                                                 
         BNE   INVERR                                                           
         CLI   1(R4),0                                                          
         BE    INVERR                                                           
         OC    FMTSTD,FMTSTD      SEE IF I ALREADY HAVE ONE                     
         BNZ   INVERR                                                           
         XC    WORK(20),WORK                                                    
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   WORK(0),22(R4)      EXECUTED                                     
*                                                                               
         LA    R5,WORK                                                          
         AH    R1,=H'1'                                                         
         AR    R5,R1                                                            
         MVI   0(R5),C'/'                                                       
         MVC   1(2,R5),QYR+2                                                    
*                                                                               
         SR    R5,R5          VALIDATE MMMDD/YY                                 
         CLI   WORK+1,C'/'     CATER TO N/N/YY EDIT FOR MMDDYY                  
         BE    EDTO25                                                           
         CLI   1(R4),3        SEE IF INPUT LENGTH IS 3 - EDIT MMM/YY            
         BNE   *+8                                                              
         LA    R5,2           VALIDATE MMM/YR                                   
*                                                                               
EDTO25   DS    0H                                                               
         GOTO1 DATVAL,DMCB,((R5),WORK),WORK+10                                  
         OC    DMCB(4),DMCB                                                     
         BZ    INVERR                                                           
*                                                                               
         GOTO1 DATCON,DMCB,WORK+10,(3,FMTSTD)                                   
*                                                                               
EDTNXTO  DS    0H                                                               
         LA    R4,34(R4)         BUMP TO NEXT OPTION                            
         B     EDTO5                                                            
*                                                                               
EDTOX    DS    0H                                                               
         CLI   FMTSW,0          SEE IF FREQ ENTERED                             
         BNE   EDTOX8           YES THEN USE IT                                 
*                               ELSE TRY AND USE FREQ FROM PUB                  
         CLC   MYPUBFQ,SPACES                                                   
         BE    INVERR                                                           
         OC    MYPUBFQ,SPACES                                                   
         L     R5,=A(FREQTAB)      TABLE OF FORMATABLE FREQUENCIES              
         A     R5,RELO             RELOCATE ADDRESS                             
EDTOX3   CLI   0(R5),X'FF'     END OF TABLE                                     
         BE    INVERR                                                           
         CLC   0(2,R5),MYPUBFQ                                                  
         BE    EDTOX4                                                           
         LA    R5,3(R5)                                                         
         B     EDTOX3                                                           
*                                                                               
EDTOX4   MVC   FMTSW,2(R5)                                                      
*                                                                               
EDTOX8   OC    FMTSTD,FMTSTD    SEE IF START GIVEN                              
         BNZ   EDTOX10                                                          
         CLI   FMTSW,2                                                          
         BH    MISSERR          FOR WEEKLIES START MUST BE GIVEN                
*                               FOR MONTHLIES SET MONTH TO JAN                  
         MVC   FMTSTD(2),=X'9301'     SET FOR JAN                               
*                                                                               
EDTOX10  DS    0H                                                               
         CLI   FMTSW,2                                                          
         BNH   FILLSCR                                                          
         CLI   FMTSTD+2,0       FOR WEEKLIES THEY MUST GIVE ME A DAY            
         BNE   FILLSCR                                                          
         MVI   ERROR,INVDATE                                                    
         B     ERRX                                                             
*                                                                               
FILLSCR  DS    0H                                                               
         TWAXC FSIDT1H          CLEAR SCREEN                                    
*                                                                               
         CLI   FMTSW,2          SEE IF WEEKLY                                   
         BH    FILLS50                                                          
*                                                                               
         LA    R2,FSIDT1H                                                       
         ZIC   R5,FMTSTD+1                                                      
         MH    R5,=H'3'                                                         
         LA    R4,MONTAB-3(R5)                                                  
FILLS5   CLI   0(R4),X'FF'      END OF TABLE                                    
         BE    FILLSX           THEN DONE                                       
*                                                                               
         MVC   8(3,R2),0(R4)                                                    
         CLI   FMTSTD+2,0         SEE IF DAY GIVEN                              
         BE    FILLS10                                                          
         EDIT  (B1,FMTSTD+2),(2,11(R2)),0,ALIGN=LEFT                            
*                                                                               
FILLS10  FOUT  (R2)                                                             
         OI    6(R2),X'01'     CHANGE TO MODIFIED FOR NEXT INPUT                
         BAS   RE,BUMP                                                          
         LA    R4,3(R4)                                                         
         CLI   FMTSW,1                                                          
         BE    *+8                                                              
         LA    R4,3(R4)     FOR BI-MONTHLY BUMP 2 MONTHS                        
         B     FILLS5                                                           
*                                                                               
         DS    4C                                                               
MONTAB   DS    0C                                                               
         DC    C'JAN'                                                           
         DC    C'FEB'                                                           
         DC    C'MAR'                                                           
         DC    C'APR'                                                           
         DC    C'MAY'                                                           
         DC    C'JUN'                                                           
         DC    C'JUL'                                                           
         DC    C'AUG'                                                           
         DC    C'SEP'                                                           
         DC    C'OCT'                                                           
         DC    C'NOV'                                                           
         DC    C'DEC'                                                           
         DC    X'FFFFFFFFFFFF'                                                  
*                                                                               
         EJECT                                                                  
FILLS50  DS    0H            HERE FOR WEEKLIES                                  
         LA    R2,FSIDT1H                                                       
         GOTO1 DATCON,DMCB,(3,FMTSTD),(0,WORK)                                  
         LH    R5,=H'7'                                                         
         CLI   FMTSW,3                                                          
         BE    FILLS55                                                          
         LH    R5,=H'14'                                                        
*                                                                               
FILLS55  DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(4,8(R2))                                   
         OI    6(R2),X'01'     CHANGE TO MODIFIED FOR NEXT INPUT                
         BAS   RE,BUMP                                                          
*                                                                               
FILLS60  DS    0H                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+10,(R5)                                     
         CLC   WORK(2),WORK+10      SEE IF YEAR CHANGED                         
         BNE   FILLSX               YES - THEN DONE                             
         MVC   WORK(6),WORK+10      NO - THEN KEEP GOING                        
         B     FILLS55                                                          
*                                                                               
FILLSX   DS    0H                                                               
         XC    FSIOPT,FSIOPT       CLEAR OPTIONS AND EXIT                       
         FOUT  FSIOPTH                                                          
*                                                                               
         MVI   FMTSW,0             CLEAR AFTER FORMAT                           
         XC    FMTSTD,FMTSTD                                                    
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
       MVC   CONHEAD(42),=C'ISSUE DATES DISPLAYED. HIT ENTER TO ACCEPT'         
         NI    CONACTH+4,X'DF'     UNVALIDATE ACTION                            
         LA    R2,FSIDT1H         CURSOR TO FIRST DATE                          
         GOTO1 ERREX2                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         TWAXC FSIDT1H                                                          
         LA    R2,FSIDT1H                                                       
         LA    R4,150              SET FOR LOOP                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'29'                                                     
         USING PISSEL29,R6                                                      
         BAS   RE,GETEL                                                         
         B     DR20                                                             
*                                                                               
DR10     BAS   RE,NEXTEL                                                        
*                                                                               
DR20     BNE   DRX                 NO ELEMENTS LEFT                             
         OC    PISSDAT,PISSDAT                                                  
         BZ    DR25                                                             
*                                                                               
DR24     GOTO1 DATCON,DMCB,(3,PISSDAT),(7,WORK)                                 
         CLC   WORK+3(2),=C'00'                                                 
         BNE   DR24D                                                            
         XC    WORK+3(2),WORK+3                                                 
         FOUT  (R2),WORK,3                                                      
         B     DR25                                                             
*                                                                               
DR24D    FOUT  (R2),WORK,5                                                      
*                                                                               
DR25     DS    0H                                                               
*                                                                               
DR30     BAS   RE,BUMP                                                          
         BCT   R4,DR10                                                          
*                                                                               
DRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         OI    GLSTSTAT,RETEXTRA                                                
         LA    R6,KEY                                                           
         USING PISSREC,R6                                                       
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         MVC   PISSKAGY,AGENCY     CREATE KEY - AGENCY                          
         MVC   PISSKMED,QMED                    MEDIA CODE                      
         MVI   PISSKTYP,X'29'                  TYPE                             
         CLI   FSLPUBH+5,0                                                      
         BE    *+10                                                             
         MVC   PISSKPUB,BPUB                    PUB                             
         CLI   FSLYRH+5,0                   SEE IF YEAR GIVEN                   
         BE    *+10                                                             
         MVC   PISSKYR,QYR                                                      
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    DS    0H                                                               
         CLC   KEY(4),KEYSAVE      TEST FOR ALL DONE                            
         BE    LR035                                                            
         XC    KEY,KEY                                                          
         B     LRX                                                              
*                                                                               
LR035    DS    0H                                                               
         OC    BPUB,BPUB          SEE IF PUB GIVEN                              
         BZ    LR037                                                            
         CLC   PISSKPUB,BPUB                                                    
         BE    LR037                                                            
         XC    KEY,KEY                                                          
         B     LRX                                                              
*                                                                               
LR037    DS    0H                                                               
         OC    QYR,QYR           SEE IF YEAR GIVEN                              
         BZ    LR039                                                            
         CLC   PISSKYR,QYR                                                      
         BE    LR039                                                            
         B     LR020              SKIP TO NEXT RECORD                           
*                                                                               
LR039    DS    0H                                                               
         GOTO1 GETREC              GET THE COST RECORD                          
         MVC   MYDSKADD,DMDSKADD   SAVE D/A FOR LIST                            
*                                                                               
         L     R6,AIO                                                           
         USING LISTD,R5                                                         
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    *+8                                                              
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
*                                                                               
LR39D    GOTO1 =V(PUBEDIT),DMCB,(C'0',PISSKPUB),(0,LPUB),RR=RELO                
*                                                                               
         MVC   SMYPUB,MYPUB         SAVE REAL MYPUB, MYPUBFQ                    
         MVC   SMYPUBFQ,MYPUBFQ     AND NAMES                                   
         MVC   SPUBNM,PUBNM                                                     
         MVC   SPUBZNM,PUBZNM                                                   
*                                                                               
         MVC   MYPUB,PISSKPUB                                                   
         BAS   RE,MYVPUB                                                        
         MVC   LPUBN,PUBNM         DISPLAY PUB NAME                             
         CLI   MODE,PRINTREP              IF DOING REPORT                       
         BNE   LR39F                      PUT ZONE ON LINE 2                    
         MVI   ZONESW,C'N'                                                      
         CLC   PUBZNM,SPACES         SEE IF PUB HAS A ZONE                      
         BE    LR39F                                                            
         MVI   ZONESW,C'Y'                                                      
         MVC   LPUBN+132(L'PUBZNM),PUBZNM                                       
*                                                                               
LR39F    MVC   LYR,PISSKYR                                                      
*                                                                               
LR39X    DS    0H                                                               
         MVC   MYPUB,SMYPUB        RESTORE MYPUB                                
         MVC   MYPUBFQ,SMYPUBFQ    RESTORE MYPUBFQ                              
         MVC   PUBNM,SPUBNM        RESTORE PUB NAME                             
         MVC   PUBZNM,SPUBZNM      RESTORE ZONE NAME                            
*                                                                               
         DS    0H                                                               
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BNE   LR085               NO THEN GO TO NEXT RECORD                    
*                                                                               
         USING PISSEL29,R6                                                      
         MVI   ELCODE,X'29'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR080                                                            
         LA    R3,LDATE                                                         
         LA    R4,12              12 DATES PER LINE                             
         B     LR050                                                            
*                                                                               
LR040    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   LR080                                                            
*                                                                               
LR050    GOTO1 DATCON,DMCB,(3,PISSDAT),(7,0(R3))                                
         CLC   3(2,R3),=C'00'                                                   
         BNE   LR070                                                            
         MVC   3(2,R3),=C'  '                                                   
*                                                                               
LR070    DS    0H                                                               
         LA    R3,7(R3)                                                         
         BCT   R4,LR040            GO DO ANOTHER ELEM                           
         B     LR090                                                            
*                                                                               
LR080    DS    0H                  END OF RECORD                                
*                                                                               
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BNE   LR085                                                            
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         GOTO1 SPOOL,DMCB,SPOOLD   SKIP A LINE                                  
         LA    R6,KEY              MUST RESET R6 TO KEY                         
         B     LR020               GO TO SEQ READ                               
*                                                                               
LR085    MVC   DMDSKADD,MYDSKADD   RESTORE D/A                                  
         GOTO1 LISTMON             CALL LISTMON                                 
         LA    R6,KEY              MUST RESET R6 TO KEY                         
         B     LR020                                                            
*                                                                               
LR090    DS    0H                                                               
         CLI   ZONESW,C'Y'         SEE IF PRINTING ZONE ON LINE 2               
         BNE   LR095                                                            
         LA    R3,LDATE+132        IF SO - DON'T PRINT NOW                      
         LA    R4,12                                                            
         MVI   ZONESW,C'N'                                                      
         B     LR040                                                            
*                                                                               
LR095    GOTO1 SPOOL,DMCB,SPOOLD                                                
         LA    R3,LDATE            RESET R3                                     
         LA    R4,12               RESET FOR NEW LINE                           
         B     LR040               GO BACK AND DO NEXT ELEM                     
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* SET UP THE PUB NUMBER & GET NAME                                              
*                                                                               
MYVPUB   NTR1                                                                   
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   PUBNM,SPACES                                                     
         MVC   PUBZNM,SPACES                                                    
         MVC   MYPUBFQ,SPACES      FREQ                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),MYPUB    MOVE PUB/ZONE/EDTN                           
         CLC   MYPUB+4(2),=X'FFFF'    ALL ZONES/EDTS                            
         BNE   *+10                                                             
         XC    PUBKPUB+4(2),PUBKPUB+4   READ "BASE" PUB                         
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VPNO                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUBNAMEL,R6                                                      
         MVC   PUBNM,PUBNAME                                                    
         MVC   PUBZNM(3),=C'ALL'                                                
         CLC   MYPUB+4(2),=X'FFFF'   ALL ZONES/EDTS                             
         BE    *+10                                                             
         MVC   PUBZNM,PUBZNAME                                                  
         OC    PUBZNM,SPACES       JUST IN CASE                                 
         DROP  R6                                                               
*                                                                               
         CLI   FSIMED,C'N'         SEE IF NEWSPAPERS                            
         BE    VP10                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'       GET PRODUCTION ELEM                           
         BAS   RE,GETEL                                                         
         BNE   VP10                                                             
         USING PUBGEND,R6                                                       
         MVC   MYPUBFQ,PUBMFREQ                                                 
*                                                                               
VP10     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         B     XIT                                                              
*                                                                               
VPNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
* PRINT REPORT                                                                  
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     LR                  USE LIST REC LOGIC                           
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
         MVC   H2(5),=C'MEDIA'                                                  
         MVC   H2+10(1),QMED                                                    
         MVC   H2+15(10),MEDNM                                                  
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CHECK IF DATE ALREADY INPUT                                            
*        R3 - ELEMENT                                                           
*                                                                               
DUPDAT   NTR1                                                                   
         USING PISSEL29,R3                                                      
         LA    R2,DATETAB          KEEP TABLE OF DATES INPUT                    
         LA    R1,150              MAXIMUM OF 150 DATES                         
*                                                                               
DUP10    OC    0(3,R2),0(R2)      END OF TABLE                                  
         BE    DUP20                                                            
         CLC   PISSDAT,0(R2)                                                    
         BE    YES                                                              
         LA    R2,3(R2)           BUMP TABLE                                    
         BCT   R1,DUP10                                                         
*                                                                               
DUP20    MVC   0(3,R2),PISSDAT                                                  
         B     NO                                                               
         DROP  R3                                                               
         SPACE 3                                                                
*                                                                               
*        BUMP TO NEXT FIELD                                                     
*                                                                               
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     ERRX                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
DUPERR   MVI   ERROR,DUPDATER                                                   
         B     ERRX                                                             
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
*                                                                               
NOMORE   MVI   ERROR,NOMORERM                                                   
         LA    R2,FSIDT1H                                                       
         B     ERRX                                                             
*                                                                               
ERRX     GOTO1 ERREX                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,58,C'ISSUE DATE REPORT'                                       
         SSPEC H2,58,C'-----------------'                                       
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'PUB CODE        PUB NAME               YEAR'              
         SSPEC H7,47,C'DATES'                                                   
         SSPEC H8,1,C'--------        --------               ----'              
         SSPEC H8,47,C'-----'                                                   
         DC    X'00'                                                            
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
SMYPUB   DS    XL6                                                              
SMYPUBFQ DS    CL2                                                              
SPUBNM   DS    CL20                                                             
SPUBZNM  DS    CL20                                                             
*                                                                               
FREQTAB  DS    0C                                                               
         DC    C'M ',AL1(1)      MONTHLY                                        
         DC    C'BM',AL1(2)      BI-MONTHLY                                     
         DC    C'W ',AL1(3)      WEEKLY                                         
         DC    C'BW',AL1(4)      BI-WEEKLY                                      
         DC    X'FF'                                                            
*                                                                               
         TITLE 'GET KEY FIELDS FROM GLOBBER AREA - GETKEYS'                     
***********************************************************************         
*                                                                     *         
*               GET KEY FIELDS FROM GLOBBER AREA - GETKEYS            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETKEYS  NTR1  BASE=*                                                           
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',FSLMEDH,,GLVPRMD GET MEDIA                
         GOTO1 VGLOBBER,DMCB,=C'DELE'    DELETE MEDIA                           
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',FSLPUBH,,GLVPRPUB GET PUB                 
         GOTO1 VGLOBBER,DMCB,=C'DELE'    DELETE PUB                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',FSLYRH,,GLVPRPER  GET YEAR                
         GOTO1 VGLOBBER,DMCB,=C'DELE'    DELETE YEAR                            
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*        VALIDATE PUB FIELD                                                     
*                                                                               
MYPUBVAL NTR1  BASE=*                                                           
         LR    R4,R2                                                            
         XC    BPUB(6),BPUB                                                     
         MVI   ALLZE,C'N'          INITIALIZE                                   
         MVC   MYPUBFQ,SPACES                                                   
         MVC   PUBNM,SPACES                                                     
         MVC   PUBZNM,SPACES                                                    
*                                                                               
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BNE   VKPUB5                                                           
VKPUB2   CLI   ACTNUM,ACTREP       SEE IF REPORTING                             
         BE    VKPX                                                             
         B     VKPERR              IF NOT THE MUST HAVE PUB                     
*                                                                               
VKPUB5   DS    0H                                                               
         CLI   5(R2),3                                                          
         BNE   VKPUB7                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VKPUB2                                                           
*                                                                               
VKPUB7   CLI   8(R2),C'='          PUB NAME SEARCH                              
         BNE   VKPUB10                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,QMED                                                    
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         B     VKPUB30                                                          
         DROP  R3                                                               
*                                                                               
VKPUB10  DS    0H                                                               
         LR    R2,R4                                                            
         MVI   ERROR,INVALID                                                    
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,SCANBLK)                                    
         CLI   DMCB+4,0                                                         
         BE    VKPERR                                                           
         LA    R1,SCANBLK                                                       
         CLC   =C'ALL',44(R1)                                                   
         BNE   VKPUB30                                                          
         MVI   ALLZE,C'Y'          WANT ONLY ACROSS ALL ZONES AND ED            
         ZIC   R3,0(R1)                                                         
         STC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R1)                                                   
*                                                                               
VKPUB30  LR    R2,R4                                                            
         GOTO1 VALIPUB                                                          
         CLI   ALLZE,C'Y'                                                       
         BNE   VKPUB40                                                          
         MVC   BPUB+4(2),=X'FFFF'  ALL ZONES/EDTNS                              
         ZIC   R1,5(R2)                                                         
         AH    R1,=H'4'            PUT BACK THE ORIGINAL LEN                    
         STC   R1,5(R2)                                                         
*                                                                               
*                                                                               
VKPUB40  DS    0H                                                               
         CLI   FSIMED,C'N'         SEE IF NEWSPAPERS                            
         BE    VKPUB45                                                          
*                                                                               
         L     R6,AIO                                                           
         AH    R6,DATADISP         POINT TO FIRST ELEMENT                       
         MVI   ELCODE,X'20'       GET PRODUCTION ELEM                           
         SR    RF,RF                                                            
*                                                                               
         CLI   0(R6),0             CHECK FOR END OF RECORD                      
         BE    VKPUB45                                                          
         CLC   ELCODE,0(R6)        LOOK FOR ELEMENT                             
         BE    *+16                                                             
         IC    RF,1(R6)            ELEMENT LENGTH                               
         LA    R6,0(RF,R6)                                                      
         B     *-26                                                             
*                                                                               
         USING PUBGEND,R6                                                       
         MVC   MYPUBFQ,PUBMFREQ                                                 
*                                                                               
VKPUB45  DS    0H                                                               
*                                                                               
VKPX     XIT1                                                                   
*                                                                               
VKPERR   DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMEDD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMFDD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                                                                
ORIGKEY  DS    XL(L'KEY)                                                        
MYKEY    DS    XL(L'KEY)                                                        
ALLZE    DS    CL1                                                              
*                                                                               
PUBZNM   DS    CL20                                                             
MYPUBFQ  DS    CL2               NON-NEWS FREQ CODE                             
MYPUB    DS    XL6                                                              
QYR      DS    CL4                                                              
MYDSKADD DS    XL4                                                              
FMTSW    DS    CL1                                                              
FMTSTD   DS    XL3                                                              
ZONESW   DS    CL1                                                              
SCANBLK  DS    CL250                                                            
DATETAB  DS    XL(150*3)    450 BYTES                                           
         EJECT                                                                  
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
LPUB     DS    CL15                PUBLICATION NUMBER ZONE/EDT                  
         DS    CL1                                                              
LPUBN    DS    CL20                PUB NAME                                     
         DS    CL3                                                              
LYR      DS    CL4                 YEAR                                         
         DS    CL3                                                              
LDATE    DS    CL5                                                              
         DS    CL2                                                              
         EJECT                                                                  
       ++INCLUDE PISSREC                                                        
         EJECT                                                                  
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
PUBGEND  DSECT                                                                  
       ++INCLUDE PUBGENEL                                                       
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*PPSRCHPARM                                                                     
*DDGLOBEQUS                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE PPSRCHPARM                                                     
       ++INCLUDE DDGLOBEQUS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049PRSFM0D   05/01/02'                                      
         END                                                                    
