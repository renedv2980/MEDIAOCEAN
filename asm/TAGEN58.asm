*          DATA SET TAGEN58    AT LEVEL 011 AS OF 05/29/15                      
*PHASE T70258E,*                                                                
         TITLE 'T70258 - VCAST LIST'                                            
T70258   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70258                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=A(TWAHOLE)                                
         USING WORKD,R7                                                         
                                                                                
*              MODE CONTROLLED ROUTINES                                         
                                                                                
         BRAS  RE,INIT                                                          
                                                                                
         CLI   MODE,VALKEY                                                      
         JNE   *+12                                                             
         BRAS  RE,VK                                                            
         BRAS  RE,VR                                                            
                                                                                
         CLI   MODE,LISTRECS                                                    
         JNE   *+8                                                              
         BRAS  RE,LR                                                            
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE PROGRAM                                *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 INITIAL,DMCB,PFTAB                                               
                                                                                
         CLI   TGCTSTTY,TASTTYPP                                                
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYP2                                                
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYP3                                                
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYP4                                                
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYP5                                                
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYP6                                                
         JE    XIT                                                              
         GOTO1 FLDVAL,DMCB,(X'0A',VCLAVHDH),(8,VCLDVERH)                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINES                     *         
***********************************************************************         
                                                                                
INHEXFFS DC    20X'FF'                                                          
                                                                                
PFTAB    DS    0H                                                               
         DC    AL1(PF20X-*,20,PFTINT+PFTCPROG,(PF20X-PF20)/KEYLNQ)              
         DC    AL1(PFTVCAST)                                                    
         DC    CL3'S  ',CL8'        ',CL8'CHANGE'                               
PF20     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYCUR,L'LISPID-1),AL2(0)                                  
         DC    AL1(KEYTYCUR,L'LISCAT-1),AL2(LISCAT-LISPID)                      
         DC    AL1(KEYTYCUR,L'LISSORT-1),AL2(LISSORT-LISPID)                    
PF20X    DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(X'40',VCLAGYH),(X'80',VCLVERH)                      
         JE    XIT                                                              
                                                                                
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
         MVC   TIUSERID,TWAORIG    INITIALIZE SYSIO VARIABLES                   
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLCACDQ                                                   
         OI    TIQFLAG2,TIQFNLIM                                                
                                                                                
         LA    R2,VCLAGYH                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',VCLAGYH)                              
         MVC   TIFAGY,TGAGY                                                     
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
         GOTO1 RECVAL,DMCB,TLCOICDQ,(8,VCLCIDH),VCLCIDNH                        
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ      COMMERCIAL MUST HAVE AT                      
         BRAS  RE,GETEL            LEAST ONE VERSION                            
         JNE   VKCOINV                                                          
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
         MVC   TGCOM,TLCOICOM      SAVE INTERNAL COMMERCIAL NUMBER              
         MVC   TIFCOM,TLCOICOM     AND SET SYSIO FILTER                         
         DROP  R3                                                               
                                                                                
         MVI   TGVER,0                                                          
         CLI   VCLVERH+5,0         IF VERSION FILTER IS PROVIDED                
         JE    VK10                                                             
         LA    R2,VCLVERH          ENSURE IT IS NUMERIC                         
         GOTO1 VALINUM                                                          
                                                                                
         MVC   TGVER,ACTUAL                                                     
         CLI   TGVER,1                                                          
         JE    VK10                                                             
                                                                                
         USING TLVRD,R3                                                         
         XC    TLVRKEY,TLVRKEY     AND ENSURE IT EXISTS AS A VERSION            
         MVI   TLVRCD,TLVRCDQ      ON THE COMMERCIAL                            
         MVC   TLVRCOM,TIFCOM                                                   
         MVC   TLVRVER,ACTUAL                                                   
         GOTO1 HIGH                                                             
         CLC   TLVRKEY,KEYSAVE                                                  
         JNE   VKINV                                                            
         DROP  R3                                                               
                                                                                
VK10     BAS   RE,BLDCOTBL         BUILD COMMERCIAL VERSION TABLE               
                                                                                
         GOTO1 FLDVAL,DMCB,(X'20',VCLAGYH),(X'80',VCLVERH)                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO BUILD VERSION TABLE FOR COMMERCIAL                *         
***********************************************************************         
                                                                                
BLDCOTBL NTR1                                                                   
         MVI   COMVTBL,1         INITIALIZE COMMERCIAL VERSION TABLE            
         LA    R2,COMVTBL+1                                                     
                                                                                
         USING TLVRD,R3                                                         
         LA    R3,KEY                                                           
         XC    TLVRKEY,TLVRKEY   READ ALL VERSION KEYS ATTACHED TO              
         MVI   TLVRCD,TLVRCDQ    THE COMMERCIAL                                 
         MVC   TLVRCOM,TIFCOM                                                   
         GOTO1 HIGH                                                             
         J     BCOT20                                                           
BCOT10   GOTO1 SEQ                                                              
BCOT20   CLC   KEY(TLVRVER-TLVRD),KEYSAVE                                       
         JNE   BCOT30                                                           
         MVC   0(1,R2),TLVRVER   SAVE VERSION NUMBER IN TABLE                   
         LA    R2,1(R2)                                                         
         J     BCOT10                                                           
         DROP  R3                                                               
                                                                                
BCOT30   MVI   0(R2),X'FF'       MARK END OF TABLE                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR MESSAGES FOR VALKEY ROUTINE                            *         
***********************************************************************         
                                                                                
VKCOINV  LA    R2,VCLCIDH                                                       
         J     VKINV                                                            
                                                                                
VKINV    MVI   ERROR,INVALID                                                    
         J     VKEND                                                            
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     VKEND                                                            
                                                                                
VKEND    GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VKEY ROUTINES                     *         
***********************************************************************         
                                                                                
VKHEXFFS DC    20X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE SCREEN                                   *         
***********************************************************************         
                                                                                
VR       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(X'80',VCLSEL1H),(X'80',VCLLSTH)                     
         JNE   VR10                IF SELECT FIELDS ARE EMPTY                   
         XC    FRSTKEY,FRSTKEY     SET TO CONTINUE LISTING                      
                                                                                
VR10     GOTO1 FLDVAL,DMCB,(X'80',VCLAVERH),(X'80',VCLDVERH)                    
         JNE   VR40                                                             
         LA    R2,VCLSEL1H         IF ADD/DELETE FIELDS DO NOT CONTAIN          
         LA    R3,VCLLSTH          INPUT, ENSURE NO SELECT FIELDS               
VR20     CR    R2,R3               CONTAIN "C"                                  
         JE    XIT                                                              
         TM    1(R2),X'20'                                                      
         JO    VR30                                                             
         CLI   8(R2),C'C'                                                       
         JE    VRINV                                                            
VR30     ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         J     VR20                                                             
                                                                                
VR40     CLI   VCLAVERH+5,0        ENSURE THAT ADD AND DELETE FIELDS            
         JE    VR50                ARE NOT BOTH POPULATED                       
         CLI   VCLDVERH+5,0                                                     
         JNE   VRDINV                                                           
                                                                                
VR50     LA    R2,VCLAVERH         R2=A(POPULATED FIELD)                        
         CLI   VCLAVERH+5,0                                                     
         JNE   *+8                                                              
         LA    R2,VCLDVERH                                                      
         BAS   RE,BLDINTBL         BUILD INPUT TABLE                            
                                                                                
         LA    R2,VCLSEL1H         IF AT LEAST ONE CAST MEMBER                  
         LA    R5,VCLLSTH          HAS BEEN SELECTED ...                        
         GOTO1 FLDVAL,DMCB,(X'80',(R2)),(X'80',(R5))                            
         JE    XIT                                                              
                                                                                
         BAS   RE,VALWID           VALIDATE WEB APPLICATION IDS                 
                                                                                
         LA    R3,KEY              R3=A(KEY)                                    
         XC    SVCASORT,SVCASORT                                                
                                                                                
VR60     CR    R2,R5                                                            
         JH    VR80                                                             
         TM    1(R2),X'20'         BUMP THROUGH SELECT FIELDS                   
         JO    VR70                LOOKING FOR ONES WITH INPUT                  
         CLI   5(R2),0                                                          
         JE    VR70                                                             
         CLI   8(R2),C'C'          C IS ONLY ACCEPTABLE INPUT                   
         JNE   VRINV                                                            
         ST    R2,ASELFLD                                                       
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BAS   RE,PROCAST          PROCESS CAST RECORDS                         
VR70     ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         J     VR60                                                             
                                                                                
VR80     OC    SVCASORT,SVCASORT   IF ANY CAST RECORDS UPDATED                  
         JZ    XIT                 CLEAR INPUT FIELDS ...                       
         GOTO1 FLDVAL,DMCB,(1,VCLSEL1H),(X'80',VCLLSTH)                         
                                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'30',0)                                   
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO              ... AND UNVERIFY COMMERCIAL                  
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACOVDTE,TACOVDTE                                                
         JZ    XIT                                                              
         XC    TACOVDTE,TACOVDTE                                                
         XC    TACOVTIM,TACOVTIM                                                
         XC    TACOVSTU,TACOVSTU                                                
         MVI   TACOUVST,TACOUVNM+TACOUVER                                       
         TM    SVCASORT,X'80'                                                   
         JZ    *+8                                                              
         MVI   TACOUVST,TACOUVMU+TACOUVER                                       
         GOTO1 PUTREC                                                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO BUILD RAW AND SORTED INPUT TABLES                 *         
*        ON ENTRY ... R2=A(VERSION INPUT FIELD)                       *         
***********************************************************************         
                                                                                
BLDINTBL NTR1                                                                   
         BAS   RE,ALLVER         IF USER IS INPUTTING ALL VERSIONS              
         JE    XIT               BUILD TABLE AND EXIT                           
                                                                                
         MVI   INPVTBL,X'FF'     INITIALIZE INPUT TABLE                         
         MVI   ELEMENT,X'FF'     AND WORK AREA                                  
                                                                                
         USING SCAND,R5                                                         
         LA    R5,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R5),0                                         
         CLI   4(R1),0                                                          
         JE    VRINV                                                            
         ZIC   R0,4(R1)                                                         
                                                                                
BIT10    CLC   SCDATA2,SPACES    ENTRY SHOULD NEVER INCLUDE AN                  
         JNE   VRINV             EQUALS SIGN                                    
                                                                                
         XC    RANGEH(L'RANGEH+L'RANGE),RANGEH                                  
                                                                                
         SR    R1,R1             USE R1 TO COUNT NUMBER OF DASHES               
         LA    RE,SCDATA1        IN ENTRY                                       
         LA    RF,L'SCDATA1                                                     
BIT20    CLI   0(RE),C'-'                                                       
         JNE   BIT30                                                            
         MVI   0(RE),C'='        REPLACE DASHES WITH EQUAL SIGNS                
         AHI   R1,1                                                             
BIT30    LA    RE,1(RE)                                                         
         BCT   RF,BIT20                                                         
                                                                                
         CHI   R1,0              NO DASHES MEANS ENTRY IS NOT A RANGE           
         JE    BIT50                                                            
         CHI   R1,1              MAXIMUM OF ONE DASH PERMITTED                  
         JNE   VRINV                                                            
                                                                                
         MVI   RANGEH,L'RANGEH+L'RANGE     IF ENTRY IS A RANGE                  
         MVC   RANGEH+5(1),SCLEN1          BUILD MOCK FIELD TO SCAN             
         MVC   RANGE,SCDATA1                                                    
         ST    R5,SVENTRY        AND SAVE A(CURR ENTRY IN SCAN BLOCK)           
         ST    R0,SVENCNT                                                       
                                                                                
         LA    R5,WORK                                                          
         GOTO1 SCANNER,DMCB,RANGEH,(R5),0                                       
         CLI   4(R1),0                                                          
         JE    VRINV                                                            
         ZIC   R0,4(R1)                                                         
                                                                                
         TM    SCVAL1,X'80'      FIRST ENTRY IN RANGE MUST BE NUMERIC           
         BZ    VRINV                                                            
         CLC   SCBIN1,=F'1'      BETWEEN 1                                      
         BL    VRINV                                                            
         CLC   SCBIN1,=F'249'    AND 249                                        
         BH    VRINV                                                            
                                                                                
         TM    SCVAL2,X'80'      SECOND ENTRY IN RANGE MUST BE NUMERIC          
         JZ    VRINV                                                            
         CLC   SCBIN2,=F'2'      BETWEEN 2                                      
         JL    VRINV                                                            
         CLC   SCBIN2,=F'250'    AND 250                                        
         JH    VRINV                                                            
                                                                                
         CLC   SCBIN1,SCBIN2     FIRST ENTRY MUST BE LESS THAN SECOND           
         JNL   VRINV                                                            
                                                                                
BIT40    CLC   SCBIN1,SCBIN2     PROCESS ALL NUMBERS IN RANGE UNTIL             
         JNH   BIT50             FIRST ENTRY EXCEEDS SECOND                     
         L     R5,SVENTRY                                                       
         L     R0,SVENCNT                                                       
         XC    RANGE,RANGE                                                      
         J     BIT120                                                           
                                                                                
BIT50    LA    R3,COMVTBL        R3=A(COMMERCIAL VERSION TABLE)                 
                                                                                
         CLI   SCLEN1,3          IF FIELD CONTAINS A VERSION CODE               
         JH    VRINV             IT MUST BE 3 CHARACTERS OR LESS                
         TM    SCVAL1,X'80'      AND VALID NUMERIC                              
         JZ    VRINV                                                            
         MVC   ACTUAL,SCBIN1+3   SAVE CODE IN ACTUAL                            
BIT60    CLI   0(R3),X'FF'                                                      
         JE    BIT70                                                            
         CLC   ACTUAL,0(R3)                                                     
         JE    BIT80                                                            
         LA    R3,1(R3)                                                         
         J     BIT60                                                            
                                                                                
BIT70    OC    RANGE,RANGE       CODE MUST EXIST ON COMMERCIAL                  
         JZ    VRINV             UNLESS IT IS PART OF A RANGE                   
         J     BIT110                                                           
                                                                                
BIT80    LA    RE,ELEMENT                                                       
BIT90    CLI   0(RE),X'FF'       SAVE ACTUAL INTO WORK AREA'S FIRST             
         JE    BIT100            EMPTY SLOT                                     
         CLC   ACTUAL,0(RE)      DO NOT ALLOW VERSION TO BE DUPLICATED          
         JE    VRINV                                                            
         LA    RE,1(RE)                                                         
         J     BIT90                                                            
BIT100   MVC   0(1,RE),ACTUAL                                                   
         MVI   1(RE),X'FF'                                                      
                                                                                
         OC    RANGE,RANGE       IF VERSION IS PART OF RANGE                    
         JZ    BIT120                                                           
BIT110   ZIC   RE,SCBIN1+3       BUMP TO NEXT NUMBER IN RANGE                   
         AHI   RE,1              AND GO PROCESS                                 
         STC   RE,SCBIN1+3                                                      
         J     BIT40                                                            
                                                                                
BIT120   LA    R5,SCANNEXT       ELSE, BUMP TO NEXT ENTRY IN SCANNER            
         BCT   R0,BIT10          BLOCK                                          
         DROP  R5                                                               
                                                                                
         LA    R1,INPVTBL                                                       
                                                                                
BIT130   LA    RE,ELEMENT                                                       
BIT140   CLI   0(RE),X'FF'       FIND FIRST MEANINGFUL VALUE IN                 
         JE    BIT180            WORK AREA                                      
         CLI   0(RE),0                                                          
         JNE   BIT150                                                           
         LA    RE,1(RE)                                                         
         J     BIT140                                                           
                                                                                
BIT150   LR    RF,RE                                                            
BIT160   LA    RF,1(RF)          COMPARE IT TO NEXT MEANINGFUL VALUE            
         CLI   0(RF),X'FF'       IN WORK AREA                                   
         JE    BIT170                                                           
         CLI   0(RF),0                                                          
         JE    BIT160                                                           
         CLC   0(1,RE),0(RF)                                                    
         JL    BIT160                                                           
         LR    RE,RF                                                            
         J     BIT160                                                           
                                                                                
BIT170   AHI   R0,1              ADD VERSIONS TO INPUT TABLE FROM               
         MVC   0(1,R1),0(RE)     SMALLEST TO LARGEST                            
         LA    R1,1(R1)                                                         
         MVI   0(RE),0                                                          
         J     BIT130                                                           
                                                                                
BIT180   MVI   0(R1),X'FF'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE CHECKS IF ALL VERSIONS ARE INPUTTED                  *         
*        ON ENTRY ... R2=A(VERSION INPUT FIELD)                       *         
***********************************************************************         
                                                                                
ALLVER   NTR1                                                                   
         CLI   5(R2),1           IF INPUT IS ONE CHARACTER LONG                 
         JNE   AVER10                                                           
         CLI   8(R2),C'*'        AND IS A C'*'                                  
         JE    AVER20                                                           
         J     NO                USER IS INPUTTING ALL VERSIONS                 
                                                                                
AVER10   CLI   5(R2),3           IF INPUT IS THREE CHARACTERS LONG              
         JNE   NO                                                               
         CLC   =C'ALL',8(R2)     AND C'ALL'                                     
         JNE   NO                                                               
AVER20   MVI   INPVTBL,251       USER IS INPUTTING ALL VERSIONS                 
         MVI   INPVTBL+1,X'FF'                                                  
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT VERSIONS 1 AND 2 ARE NOT ADDED TO       *         
*        WEB APPLICATION LOCKED CAST MEMBERS                          *         
*        ON ENTRY ... R2 = A(FIRST PERFORMER SELECT FIELD)            *         
*                     R5 = A(LAST PERFORMER SELECT FIELD)             *         
***********************************************************************         
                                                                                
VALWID   NTR1                                                                   
         LR    R1,R2               R1 = A(CURRENT SELECT FIELD)                 
                                                                                
         USING LISTD,R1                                                         
VWID10   ZIC   RE,0(R1)                                                         
         AR    R1,RE              BUMP R1 TO PID HEADER                         
         AHI   R1,8               THEN PID DATA FIELD                           
                                                                                
         CLI   8(R2),C'C'         IF SELECTED PERFORMER HAS VITA                
         JNE   VWID30             COMPLETION ID STAMP, RETURN ERROR             
         CLC   =C'VC',LISWID                                                    
         JE    VRWEBERR                                                         
         CLC   =C'TC',LISWID                                                    
         JE    VRWEBERR                                                         
         CLC   =C'RC',LISWID                                                    
         JE    VRWEBERR                                                         
                                                                                
         CLC   =C'VS',LISWID      IF SELECTED PERFORMER HAS VITA                
         JE    VWID20             SESSION ID STAMP                              
         CLC   =C'TS',LISWID                                                    
         JE    VWID20                                                           
         CLC   =C'RS',LISWID                                                    
         JNE   VWID30                                                           
VWID20   CLI   INPVTBL,2          ... AND ADDING/DELETING VERSION 1 OR          
         JNH   VRWEBERR           2, RETURN ERROR                               
         CLI   INPVTBL+1,2                                                      
         JE    VRWEBERR                                                         
         CLI   INPVTBL,251                                                      
         JNE   VWID30                                                           
         CLI   VCLAVERH+5,0       ... AND DELETING ALL VERSIONS,                
         JE    VRWEBERR           RETURN ERROR                                  
         CLI   LISV12,C'Y'        ... AND ADDING ALL VERSIONS WHEN              
         JNE   VRWEBERR           NOT ALREADY ON V1 & V2, RETURN ERROR          
                                                                                
VWID30   LA    R1,LIS1LNQ+LIS2LNQ(R1)                                           
         LR    R2,R1                                                            
         CR    R2,R5              BUMP R1 AND R2 TO THE NEXT SELECT             
         JNH   VWID10             FIELD                                         
         J     XIT                                                              
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        ROUTINE PROCESSES SELECTED CAST RECORD                       *         
*        ON ENTRY ... R2=A(SELECT FIELD)                              *         
*                     R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
PROCAST  NTR1                                                                   
         USING LISTD,R2                                                         
         AHI   R2,8                R2=A(LIST LINE)                              
                                                                                
         OC    LISSORT,LISSORT                                                  
         JZ    XIT                                                              
                                                                                
         USING TLCAD,R3                                                         
         XC    TLCAKEY,TLCAKEY                                                  
         MVI   TLCACD,TLCACDQ      READ CAST RECORD                             
         MVC   TLCACOM,TIFCOM                                                   
         GOTO1 HEXIN,DMCB,LISSORT+1,TLCASORT,L'LISSORT-1                        
         GOTO1 HIGH                                                             
         CLC   KEY(TLCASSN-TLCAD),KEYSAVE                                       
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R2                                                               
                                                                                
         MVC   SVCASORT,TLCASORT SAVE SORT KEY                                  
         DROP  R3                                                               
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ    CHANGE IS NOT ALLOWED IF                       
         BRAS  RE,GETEL          PERFORMER HAS CAST ASSOCIATIONS                
         JE    VRERM492                                                         
                                                                                
         LA    R2,BLOCK          R2=A(ORIGINAL VERSIONS)                        
         MVI   BLOCK,X'FF'                                                      
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         JNE   PCAST10                                                          
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM         COPY CAST'S ORIGINAL VERSIONS                  
         ZIC   RE,TAFNLEN        INTO WORK AREA                                 
         SHI   RE,TAFNLNQ+1                                                     
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   BLOCK(0),TAFNNAME                                                
         LA    RF,BLOCK+1                                                       
         AR    RF,RE                                                            
         MVI   0(RF),X'FF'                                                      
                                                                                
         MVI   0(R4),X'FF'         AND DELETE EXISTING ELEMENT                  
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
PCAST10  LA    R4,ELEMENT          INITIALIZE NEW ELEMENT                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNTYPE,TAFNTVER                                                
         LHI   R0,TAFNLNQ          R0=ELEMENT LENGTH ACCUMULATOR                
         LA    R1,TAFNNAME         R1=A(START POSITION IN NEW ELEMENT)          
         BAS   RE,ADDVERS          ADD NEW VERSIONS                             
         BAS   RE,DELVERS          OR DELETE EXISTING VERSIONS                  
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC              UPDATE CAST RECORD                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ADDS VERSION ELEMENT TO CAST RECORD                  *         
*        ON ENTRY ... R0=INITIALIZED ELEMENT LENGTH ACCUMULATOR       *         
*                     R1=A(START POSITION IN NEW ELEMENT)             *         
*                     R2=A(ORIGINAL VERSIONS)                         *         
*                     R4=A(NEW ELEMENT)                               *         
***********************************************************************         
                                                                                
         USING TAFND,R4                                                         
ADDVERS  NTR1                                                                   
         CLI   VCLAVERH+5,0      EXIT IF NOT ADDING VERSIONS                    
         JE    XIT                                                              
                                                                                
         CLI   INPVTBL,251       IF ADDING ALL VERSIONS                         
         JE    AVERS10                                                          
         CLI   0(R2),251         OR ALREADY ON ALL VERSIONS                     
         JNE   AVERS20                                                          
AVERS10  MVI   TAFNNAME,251      INSERT ALL VERSION INTO ELEMENT                
         AHI   R0,1                                                             
         J     AVERS60           AND SKIP AHEAD                                 
                                                                                
AVERS20  LA    R3,INPVTBL        R3=A(VERSIONS TO ADD)                          
                                                                                
AVERS30  CLI   0(R2),X'FF'       BUILD NEW ELEMENT                              
         BE    AVERS40                                                          
         CLC   0(1,R3),0(R2)                                                    
         BL    AVERS40                                                          
         MVC   0(1,R1),0(R2)                                                    
         LA    R2,1(R2)                                                         
         CLC   0(1,R1),0(R3)                                                    
         BNE   AVERS50                                                          
         LA    R3,1(R3)                                                         
         B     AVERS50                                                          
                                                                                
AVERS40  CLI   0(R3),X'FF'                                                      
         BE    AVERS60                                                          
         MVC   0(1,R1),0(R3)                                                    
         LA    R3,1(R3)                                                         
AVERS50  LA    R1,1(R1)                                                         
         AHI   R0,1                                                             
         B     AVERS30                                                          
                                                                                
AVERS60 STC    R0,TAFNLEN          STORE ELEMENT LENGTH                         
         GOTO1 ADDELEM             AND ADD ELEMENT                              
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE DELETES VERSIONS FROM CAST RECORD                    *         
*        ON ENTRY ... R0=INITIALIZED ELEMENT LENGTH ACCUMULATOR       *         
*                     R1=A(START POSITION IN NEW ELEMENT)             *         
*                     R2=A(ORIGINAL VERSIONS)                         *         
*                     R4=A(NEW ELEMENT)                               *         
***********************************************************************         
                                                                                
         USING TAFND,R4                                                         
DELVERS  NTR1                                                                   
         CLI   VCLDVERH+5,0      EXIT IF NOT DELETING VERSIONS                  
         JE    XIT                                                              
         CLI   INPVTBL,251       OR DELETING ALL VERSIONS                       
         JE    XIT                                                              
         CLI   ELEMENT,0         OR CAST IS NOT ON ANY VERSIONS                 
         JE    XIT                                                              
                                                                                
DVER10   CLI   0(R2),X'FF'       BUILD NEW ELEMENT                              
         BE    DVER50                                                           
         LA    RF,INPVTBL                                                       
DVER20   CLI   0(RF),X'FF'                                                      
         BE    DVER30                                                           
         CLC   0(1,RF),0(R2)                                                    
         BE    DVER40                                                           
         LA    RF,1(RF)                                                         
         B     DVER20                                                           
DVER30   MVC   0(1,R1),0(R2)                                                    
         LA    R1,1(R1)                                                         
         AHI   R0,1                                                             
DVER40   LA    R2,1(R2)                                                         
         B     DVER10                                                           
                                                                                
DVER50   CHI   R0,TAFNLNQ        IF CAST IS STILL ON ANY VERSIONS               
         JE    XIT                                                              
         STC   R0,TAFNLEN        STORE ELEMENT LENGTH                           
         GOTO1 ADDELEM           AND ADD ELEMENT                                
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR MESSAGES FOR VALREC ROUTINE                            *         
***********************************************************************         
                                                                                
VRSELINV L     R2,ASELFLD                                                       
         J     VRINV                                                            
                                                                                
VRERM492 L     R2,ASELFLD                                                       
         MVC   MYMSGNO,=Y(ERMUS492)                                             
         J     VREXTEND                                                         
                                                                                
VRDINV   LA    R2,VCLDVERH                                                      
         J     VRINV                                                            
                                                                                
VRINV    MVI   ERROR,INVALID                                                    
         J     VREND                                                            
                                                                                
VRWEBERR MVC   MYMSGNO,=Y(ERUSEWEB) RECORD MUST BE UPDATED FROM                 
         J     VREXTEND             WEB APPLICATION                             
                                                                                
VREXTEND MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         J     VREND                                                            
                                                                                
VREND    GOTO1 EXIT,DMCB,0                                                      
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VALREC ROUTINES                   *         
***********************************************************************         
                                                                                
VRHEXFFS DC    20X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO LIST RECORDS                                      *         
***********************************************************************         
                                                                                
LR       NTR1  BASE=*,LABEL=*                                                   
         GOTO1 FLDVAL,DMCB,(1,VCLCST1H),(X'40',VCLLSTH)                         
         OI    GLSTSTAT,RETEXTRA                                                
                                                                                
         LA    R2,LISTAR           SETUP SCREEN INFORMATION                     
         MVI   NLISTS,8                                                         
                                                                                
         XC    NAMEH(L'NAMEH+L'NAME),NAMEH                                      
         MVI   NAMEH,L'NAMEH+L'NAME                                             
                                                                                
         OC    TIKEY,TIKEY                                                      
         JZ    LR10                                                             
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
         MVC   TIQSKEY,KEY                                                      
                                                                                
LR10     OC    FRSTKEY,FRSTKEY                                                  
         JZ    LR20                                                             
         MVC   TIQSKEY,FRSTKEY                                                  
                                                                                
LR20     LA    R0,LRHOOK           SETUP SYSIO AND CALL IT                      
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
                                                                                
         NI    VCLAGYH+4,X'DF'                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO PROCESS SYSIO RECORDS                             *         
*        ON ENTRY ... R2=A(LIST LINE)                                 *         
***********************************************************************         
                                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC                                                   
         JNE   XIT                                                              
                                                                                
         MVC   AIO,TIAREC                                                       
                                                                                
         BRAS  RE,TRNTATR                                                       
                                                                                
         BAS   RE,FILTREC                                                       
         JNE   LH110                                                            
                                                                                
         XC    CSTVARS(CVLNQ),CSTVARS                                           
                                                                                
         USING LISTD,R2                                                         
         MVC   LISTAR,SPACES                                                    
         GOTO1 SSNPACK,DMCB,TISSN,LISPID                                        
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         JNE   LH70                                                             
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         CLI   TAFNNAME,251                                                     
         JNE   LH10                                                             
         MVI   LISVER,C'*'                                                      
         J     LH70                                                             
                                                                                
LH10     ZIC   R3,TAFNLEN                                                       
         SHI   R3,TAFNLNQ                                                       
         LA    R4,TAFNNAME                                                      
         DROP  R4                                                               
                                                                                
         LA    R5,LISVER                                                        
         LA    R6,L'LISVER-8(R5)                                                
         J     LH50                                                             
                                                                                
LH20     CR    R5,R6             IF NO MORE ROOM TO DISPLAY NEXT                
         JL    LH30              VERSION, INDICATE THERE ARE MORE               
         MVI   0(R5),C'+'        VERSIONS                                       
         J     LH70                                                             
                                                                                
LH30     CLC   RANGEX,0(R4)                                                     
         JE    LH60                                                             
                                                                                
         CLC   RANGEC,LASTDISP                                                  
         JE    LH40                                                             
                                                                                
         MVI   0(R5),C'-'                                                       
         AHI   R5,1                                                             
         EDIT  RANGEC,(3,0(R5)),ALIGN=LEFT                                      
         AR    R5,R0                                                            
                                                                                
LH40     MVI   0(R5),C','                                                       
         AHI   R5,1                                                             
                                                                                
LH50     EDIT  (1,0(R4)),(3,0(R5)),ALIGN=LEFT                                   
         MVC   LASTDISP,0(R4)                                                   
         AR    R5,R0                                                            
                                                                                
LH60     MVC   RANGEC,0(R4)                                                     
         ZIC   RE,0(R4)                                                         
         AHI   RE,1                                                             
         STC   RE,RANGEX                                                        
                                                                                
         LA    R4,1(R4)                                                         
         BCT   R3,LH20                                                          
                                                                                
         CLC   LASTDISP,RANGEC                                                  
         JE    LH70                                                             
         MVI   0(R5),C'-'                                                       
         EDIT  RANGEC,(3,1(R5)),ALIGN=LEFT                                      
                                                                                
LH70     GOTO1 LISTMON                                                          
         DROP  R2                                                               
                                                                                
         USING LISLINE2,R2                                                      
         L     R2,ATHISLST                                                      
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         CLI   TAFNNAME,251                                                     
         JNE   LH80                                                             
         MVI   LISV12,C'Y'                                                      
                                                                                
LH80     CLI   TAFNLEN,5                                                        
         JL    LH90                                                             
         CLI   TAFNNAME+1,2                                                     
         JNE   LH90                                                             
         MVI   LISV12,C'Y'                                                      
         DROP  R4                                                               
                                                                                
         USING TLCAD,R4                                                         
LH90     L     R4,TIAREC                                                        
         GOTO1 HEXOUT,DMCB,TLCASORT,LISSORT+1,6,0                               
         MVC   LISCAT,TICAT                                                     
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         ST    R4,AIO                                                           
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   LH100                                                            
         L     R4,TGELEM                                                        
         MVC   LISWID,TAFNNAME                                                  
         DROP  R4                                                               
                                                                                
LH100    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',TISSN),NAMEH                          
         MVC   LISNAME,NAME                                                     
                                                                                
         LA    R2,LIS2LNQ(R2)                                                   
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ST    R2,ATHISLST                                                      
         DROP  R2                                                               
                                                                                
         CLI   LISTNUM,1                                                        
         JNE   LH110                                                            
         MVC   FRSTKEY,TIKEY                                                    
                                                                                
LH110    MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO FILTER SYSIO RECORDS                              *         
*        ON ENTRY ... AIO=A(CAST RECORD)                              *         
***********************************************************************         
                                                                                
FILTREC  NTR1                                                                   
         CLI   TGVER,0             IF FILTERING BY VERSION                      
         JE    YES                                                              
         MVI   ELCODE,TAFNELQ      GET CAST'S VERSION ELEMENT                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         JNE   NO                                                               
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         ZIC   R0,TAFNLEN                                                       
         SHI   R0,TAFNLNQ                                                       
         LA    R1,TAFNNAME                                                      
FREC10   CLC   TGVER,0(R1)         ENSURE THAT CAST PASSES THE                  
         JE    YES                 FILTER                                       
         CLI   0(R1),251                                                        
         JE    YES                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,FREC10                                                        
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR LISTREC ROUTINES                  *         
***********************************************************************         
                                                                                
LRHEXFFS DC    20X'FF'                                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TATRNTR                                                        
***********************************************************************         
*        DSECT FOR SCREEN AND PRINT LINE                              *         
***********************************************************************         
                                                                                
LISTD    DSECT                                                                  
LISPID   DS    CL6                                                              
         DS    CL8                                                              
LISVER   DS    CL61                                                             
LIS1LNQ  EQU   *-LISTD                                                          
LISLINE2 DS    CL8                                                              
LISNAME  DS    CL16                                                             
         DS    CL8                                                              
LISCAT   DS    CL3                                                              
         DS    CL8                                                              
LISSORT  DS    XL14                                                             
         DS    CL8                                                              
LISWID   DS    CL2                                                              
LISV12   DS    CL1                                                              
LIS2LNQ  EQU   *-LISLINE2                                                       
                                                                                
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR58D                                                       
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                              *         
***********************************************************************         
                                                                                
WORKD    DSECT                                                                  
FRSTKEY  DS    XL(L'TIKEY)       KEY OF FIRST DISPLAYED RECORD                  
                                                                                
NAMEH    DS    XL8               FAUX CAST NAME FIELD                           
NAME     DS    XL33                                                             
                                                                                
SVCASORT DS    XL(L'TLCASORT)    SAVED SORT KEY OF CHANGED CAST                 
                                                                                
CSTVARS  DS    0X                                                               
LASTDISP DS    X                 LAST DISPLAYED VERSION                         
RANGEC   DS    X                 RANGE CURRENT                                  
RANGEX   DS    X                 RANGE END                                      
CVLNQ    EQU   *-CSTVARS                                                        
                                                                                
SVENTRY  DS    F                 SAVED ADDRESS OF SCANNER BLOCK ENTRY           
SVENCNT  DS    F                 SAVED NUMBER OF ENTRIES IN SCAN BLOCK          
RANGEH   DS    XL8               FIELD FOR FAKING OUT SCANNER                   
RANGE    DS    XL7               INTO UNDERSTANDING RANGES                      
                                                                                
COMVTBL  DS    XL251             COMMERCIAL VERSION TABLE                       
INPVTBL  DS    XL251             INPUT CAST VERSION TABLE                       
INIVTTBL DS    XL251             INITIAL CAST VERSIONS TABLE                    
                                                                                
ASELFLD  DS    A                 A(SELECT FIELD)                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
                                                                                
* TASYSDSECT                                                                    
* DDPERVALD                                                                     
* FATIOB                                                                        
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011TAGEN58   05/29/15'                                      
         END                                                                    
