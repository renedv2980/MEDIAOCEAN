*          DATA SET SRGAP03    AT LEVEL 010 AS OF 11/03/20                      
*PHASE T16F03A                                                                  
T16F03   TITLE 'SRGAP03 ($GAP) - US SPOT OM'                                    
T16F03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$GP3**,RA,RR=RE                                    
         USING WORKD,RC            RC = A(WORKING STORAGE)                      
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         L     RE,0(R1)                                                         
         MVC   SRPARS,0(RE)        SAVE S/R PARAMETER LIST                      
SRPARMSD USING SRPARMD,SRPARS                                                   
*                                                                               
         L     R9,SRPARMSD.SRQASYSF                                             
         USING SYSFACD,R9          R9 = A(SYSTEM FACILITIES)                    
         L     RF,VSYSFAC2         GET THE SPOT SYSFAC                          
         USING SPSYSFAC,RF                                                      
         MVC   VRECUP,SRECUP                                                    
         DROP  RF                                                               
*                                                                               
         MVC   AUTL,SRPARMSD.SRQAUTL                                            
         L     RF,AUTL                                                          
         MVC   ATBUFF,TBUFF-UTLD(RF)  GETTING OUR MSG FROM TSAR BUFFER          
         L     RF,ATBUFF                                                        
         SHI   RF,2                                                             
         XR    RE,RE                                                            
         ICM   RE,3,0(RF)          GET THE LENGTH OF THE MESSAGE                
         STCM  RE,3,MQMSGLEN                                                    
*                                                                               
MAIN10   BAS   RE,INITLIZE         INITIALIZE COMMON VARIABLES                  
*                                                                               
         BAS   RE,WORKITIN         PROCESS WHAT WAS PASSED IN TBUFF             
         BNE   NO                                                               
*                                                                               
YES      SR    RC,RC               SET CC TO EQ                                 
NO       LTR   RC,RC               SET CC TO NEQ                                
XIT      XIT1                      RETURN TO CALLER                             
*                                                                               
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZES COMMON VARIABLES                                                  
***********************************************************************         
INITLIZE NTR1                                                                   
         MVI   BITFLAG1,0                                                       
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,IOA1-WORKD                                                    
         ST    R7,AIO1                                                          
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,IOA2-WORKD                                                    
         ST    R7,AIO2                                                          
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,SPULAREA-WORKD                                                
         ST    R7,ASPLAREA                                                      
*                                                                               
         LR    R7,RC               FOR SAVING EDICT RECORDS                     
         AHI   R7,WRKRIOA-WORKD                                                 
         ST    R7,AWRKRIOA                                                      
         LR    RE,R7               NOTHING IN IT YET EXCEPT L(LENGTH)           
         LH    RF,=Y(WRECQLNQ)                                                  
         XCEFL                                                                  
         L     RE,AWRKRIOA                                                      
         MVI   1(RE),2                                                          
*                                                                               
         MVC   AWRKRBUF,SRPARMSD.SRQATIA                                        
*                                                                               
         L     R8,VSSB             R8 = A(SSB)                                  
         USING SSBD,R8                                                          
         NI    SSBJFLAG,255-SSBJFWKR STOP ABEND LOOPS                           
         MVC   SYSNAME,SSBSYSNA                                                 
         MVC   SYSN1,SSBSYSN1                                                   
         MVC   RECLEN,SSBTWAL      SAVE TEMPSTR TWA RECORD LENGTH               
         DROP  R8                                                               
*                                                                               
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         OI    TPRGIND,X'14'       SET CONVERTED MAX IOS                        
         MVC   TERMNUM,TNUM        SAVE TERMINAL NUMBER                         
         MVC   USERNUM,TUSER       SAVE USER ID NUMBER                          
         XC    TUSER,TUSER         DUMMY TERMINALS SHOULD NOT HAVE ID #         
         DROP  R1                                                               
***************                                                                 
* COMFACS STUFF                                                                 
***************                                                                 
         L     R1,SRPARMSD.SRQACOMF    R1 = A(COMFACS)                          
         USING COMFACSD,R1                                                      
         MVC   VADDAY,CADDAY                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VLOCKET,CLOCKET                                                  
         MVC   VSWITCH,CSWITCH                                                  
         DROP  R1                                                               
         L     R1,RELO                                                          
         L     R0,=A(GOMSPACK)                                                  
         AR    R0,R1                                                            
         ST    R0,VMSPACK                                                       
         L     R0,=A(GOMSUNPK)                                                  
         AR    R0,R1                                                            
         ST    R0,VMSUNPK                                                       
***************                                                                 
* CORERES STUFF                                                                 
***************                                                                 
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4,=X'D9000A0C'    SPOOL                                     
         GOTO1 VCALLOV,DMCB                                                     
         MVC   ASPOOL,DMCB                                                      
*                                                                               
         MVI   DMCB+7,X'14'           CLPACK                                    
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VCLPACK,DMCB                                                     
*                                                                               
         MVI   DMCB+7,X'15'           CLUNPK                                    
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VCLUNPK,DMCB                                                     
*                                                                               
         MVI   DMCB+7,X'75'           PARSNIP                                   
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VPARSNIP,DMCB                                                    
*                                                                               
         MVI   DMCB+7,X'7A'           STAPACK                                   
         GOTO1 VCALLOV,DMCB                                                     
         MVC   ASTAPACK,DMCB                                                    
***************                                                                 
* GET TODAY'S DATE                                                              
***************                                                                 
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKTIME                                                      
         ST    R1,FULL                                                          
         AP    PACKTIME,FULL                                                    
*                                                                               
         CP    PACKTIME,=P'240000'    PAST MIDNIGHT?                            
         BL    INIT10                                                           
         SP    PACKTIME,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,WORK,WORK,F'1'                                       
*                                                                               
INIT10   GOTO1 VDATCON,DMCB,(0,WORK),(3,BTODAY)                                 
         GOTO1 VDATCON,DMCB,(3,BTODAY),(15,JDTTODAY)                            
*                                                                               
INITX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DETERMINES WHICH TYPE OF GAP MESSAGE WE HAVE AND THEN ROUTES IT TO            
* THE CORRECT PROCESSING ROUTINE                                                
***********************************************************************         
WORKITIN NTR1                                                                   
*****                                                                           
* PUT 255 NULLS AFTER THE MESSAGE SO WE DON'T PROCESS AS IF THERE WAS           
* RELEVANT DATA THERE                                                           
*****                                                                           
         L     R7,ATBUFF                                                        
         SHI   R7,2                                                             
         XR    RE,RE                                                            
         ICM   RE,3,0(R7)          LENGTH OF THE MSG IN BUFFER                  
         LA    RE,2(R7,RE)         POINT AFTER THE MESSAGE                      
         XC    0(255,RE),0(RE)                                                  
*****                                                                           
         L     R7,ATBUFF                                                        
         USING EVNTDSCT,R7         DSECT AT OEXPRESS                            
*                                                                               
         LA    R2,EVNTOBJS                                                      
         USING EVNTOBJD,R2                                                      
*                                                                               
WITIN10  CLI   EOBJTID,0           MATCHED ON TRANSMISSION ID?                  
         BE    WITINNO             NO, CAN'T PROCESS THIS EVENT                 
*                                                                               
         CLC   EVTYPE,EOBJTID      MATCH ON THIS EVENT TYPE?                    
         BE    WITIN15                                                          
         LA    R2,EOBJNXT          NO, CHECK NEXT TRANSMISSION ID               
         B     WITIN10                                                          
*                                                                               
WITIN15  BRAS  RE,SWTCHCTL                                                      
         BRAS  RE,GETORDER         ORDER WILL BE IN AIO1                        
*                                                                               
         XC    ELEM,ELEM           INITIALIZE                                   
         XC    CMTELEM,CMTELEM                                                  
         XC    ACTELEM,ACTELEM                                                  
         XC    ECNELEM,ECNELEM                                                  
*                                                                               
WITIN20  LA    R4,ELEM             WE'LL ALSO HAVE DOXMT TO DEAL WITH           
         USING DOSTELD,R4                                                       
         MVI   DOSTEL,DOSTELQ      X'12'                                        
         MVI   DOSTLEN,DOSTLNQ     FOR MOST OEXPRESS STATI                      
         ZAP   DUB,JDTTODAY        0CYYDDDF                                     
         SRP   DUB,1,0             SHIFT LEFT A NIBBLE 00000000CYYDDD0F         
         MVC   DOSTDATE,DUB+4      SAVE PWOS DATE                               
*                                                                               
         ZAP   DUB,PACKTIME        0HHMMSSF                                     
         SRP   DUB,64-1,0          RID UNITS OF SEC    0000000000HHMMSF         
         MVC   DOSTTIME,DUB+5      X'0800' = 8AM, X'1530' = 330PM               
*                                                                               
WITIN25  ICM   RF,15,EOBJRTN       A(ROUTINE)                                   
         A     RF,RELO                                                          
         BASR  RE,RF               EXECUTE SPECIFIC ROUTINE                     
         DROP  R2,R4                 CMTELEM WOULD BE SET IF ANY                
*                                                                               
         CLI   ELEM,0              ANY STATUS ELEMENT TO ADD?                   
         BE    WITINNO             NONE                                         
*                                                                               
********                                                                        
* UPDATING US SPOT OM ORDER RECORD                                              
********                                                                        
WITIN100 BRAS  RE,PTXMTVER         LOCATE THE X'11' ELEM TO BE CHANGED          
*                                                                               
         USING DOXMTELD,R6                                                      
*  COPY DATE/TIME/STATUS  FROM THE X'12' ELEMENT WE BUILT                       
         MVC   DOXMTSTD(DOSTIDNM-DOSTDATE),DOSTDATE-DOSTELD+ELEM                
*                                                                               
         BRAS  RE,PTSTAVER         LOCATE WHERE TO PUT NEW X'12' ELEM           
         BNE   WITINNO             PROBLEM WITH ORDER, NOT WORTH DUMP           
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO),ELEM,(R6)                                 
*                                                                               
         BRAS  RE,BLDCOLOR         WILL BUILD COLOR ELEM AND PUT                
*                                                                               
         CLI   CMTELEM,0           ANY COMMENT ELEMENT?                         
         BE    WITINYES            NONE                                         
*                                                                               
         NI    BITFLAG1,X'FF'-BF1NWCOM                                          
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   KEY+DOKCMT-DOKEY,2  LOOK FOR REP COMMENT RECORD                  
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE  REP COMMENT RECORD DOESN'T EXIST?          
         BE    WITIN120                                                         
         OI    BITFLAG1,BF1NWCOM     NO, NEED A NEW REP COMMENT RECORD          
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEFL                                                                  
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   0(L'DOKEY,R6),KEYSAVE                                            
         MVC   DORAGY,AGENCY                                                    
         DROP  R6                                                               
         B     WITIN140                                                         
*                                                                               
WITIN120 MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
WITIN140 L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   DORLEN,=Y(DORFRST-DOKEY)  REMOVES ALL COMMENT ELEMENTS           
         LA    R6,DORFRST                                                       
         XC    0(2,R6),0(R6)                                                    
         DROP  R6                                                               
*                                                                               
WITIN160 GOTO1 VRECUP,DMCB,(C'S',AIO1),CMTELEM,(C'R',(R6))                      
* OVERFLOW CAN'T HAPPEN WITH 1 COMMENT ELEM TO AN EMPTY RECORD                  
         CLI   8(R1),C'R'          RECORD OVERFLOW?                             
         BNE   WITIN180            YES, CAN'T FIT ANYMORE, CUT REST OFF         
*                                                                               
         LLC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         BRAS  RE,CREAUCOM         CREATE US COMMENT ELEMENT                    
         BE    WITIN160              WE HAVE YET ANOTHER                        
*                                                                               
WITIN180 TM    BITFLAG1,BF1NWCOM   NEED A NEW COMMENT RECORD?                   
         BNZ   WITIN190                                                         
         GOTO1 PUT                                                              
         B     WITINYES                                                         
*                                                                               
WITIN190 GOTO1 ADD                                                              
*                                                                               
WITINYES B     YES                                                              
*                                                                               
WITINNO  B     NO                                                               
*                                                                               
EVNTOBJS DC    C'R',AL4(PROCRSPN)  RESPONSE                                     
         DC    C'A',AL4(PROCFACC)  ACCESS                                       
         DC    C'C',AL4(PROCCONC)  CONCLUSION                                   
*****    DC    C'D',AL4(PROCRDEA)  DEACTIVATE   N/A AS PER EBUBE                
*****    DC    C'N',AL4(PROCNRSP)  NO RESPONSE  N/A AS PER EBUBE                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* LOCATE WHERE TO PUT STATUS ELEM IN ORDER RECORD BASED ON VERSION #            
*                                                                               
* ON ENTRY:    AIO1                A(ORDER RECORD)                              
*              (R4)                BUILT STATUS ELEMENT (X'12')                 
*                                                                               
* ON EXIT:     (R6)                A(TO INSERT ELEM)                            
***********************************************************************         
PTSTAVER NTR1                                                                   
         SR    RF,RF                                                            
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         XR    R0,R0                                                            
*                                                                               
PTSTVLP  CLI   0(R6),0             WE SHOULD NEVER HIT END OF RECORD            
         BE    PTSTVNO                                                          
         CLI   0(R6),DOSPELQ       X'03' - ORDER'S VERSION# IS HERE             
         BE    PTSTV010                                                         
         CLI   0(R6),DOSTELQ       X'12' - ORDER STATUS ELEMENTS                
         BE    PTSTV020                                                         
PTSTVBMP IC    R0,1(R6)            BUMP TO THE NEXT ELEMENT                     
         AR    R6,R0                                                            
         B     PTSTVLP                                                          
***********************************                                             
* X'03' ELEMENT                                                                 
***********************************                                             
         USING DOSPELD,R6                                                       
PTSTV010 XR    RE,RE               RE = ORDER'S SEND VERSION                    
         ICM   RE,3,DOSPVER#                                                    
*                                                                               
         CLM   RE,3,BINSNDCT       ORDER VER# ALREADY SAME AS MSG VER#?         
         BNE   PTSTVBMP                                                         
PTSTVD   USING DOSTELD,ELEM                                                     
         CLI   PTSTVD.DOSTSTAT,QCFMD  MESSAGE IS A CONFIRM?                     
         BNE   PTSTVBMP                                                         
         OI    PTSTVD.DOSTTYPE,DCNFMVER  SO HWON KNOWS NEXT SEND IS A           
         TM    DOSPFLG3,DOSPFVDA         WAS IT LAST SENT TO VIDEA?             
         BZ    PTSTVBMP                                                         
         L     R1,AIO1                                                          
         OI    DORSTAT-DOKEY(R1),DORSVDAQ VIDEA CONFIRMED ORDER                 
         B     PTSTVBMP                   BUMP IN THE REVISION #                
         DROP  PTSTVD                                                           
*                                                                               
PTSTV020 CLM   RE,3,BINSNDCT       ORDER'S VERSION = MESSAGE'S VERSION?         
         BNH   PTSTV030                                                         
***********************************                                             
* X'12' ELEMENT                                                                 
***********************************                                             
         USING DOSTELD,R6                                                       
         CLI   DOSTSTAT,DSENT      ARE WE A SENT STATUS?                        
         BNE   PTSTVBMP            NO, BUMP UNTIL VERSIONS MATCH                
         LTR   RF,RF               TRYING TO FIND THE SEND TYPE?                
         BNZ   PTSTV030            YES, LETS EXIT                               
         CLI   DOSTLEN,DOSTLNQ7                                                 
         BNE   PTSTV025                                                         
         TM    DOSTTYP2,DOSTXRSD   OE RESEND?                                   
         BO    PTSTVBMP             YES, DON'T INCREMENT                        
PTSTV025 BCTR  RE,0                YES, DECREMENT SEND VERSION #                
         B     PTSTVBMP            RE, SHOULD NEVER GO BELOW 0                  
*                                                                               
PTSTV030 LTR   RF,RF               DID WE FIND THE SEND TYPE ALREADY?           
         BNZ   PTSTV040            YES                                          
         LR    RF,R6               NO, LETS GO FIND THE SEND TYPE               
         CLI   DOSTSTAT,DSENT        UNLESS ALREADY ON SEND STATUS              
         BE    PTSTV040                                                         
         AHI   RE,1                  DON'T KNOW SEND TYPE, BUMP RE TO           
         B     PTSTVBMP                 FORCE CHECK AT PTSTV020 TO FAIL         
*                                                                               
PTSTV040 NI    MISCFLG1,X'FF'-MF1SNDCN                                          
         CLI   DOSTLEN,DOSTLNQ7                                                 
         BNE   PTSTV050                                                         
         TM    DOSTTYP2,DOSTCNCL                                                
         BZ    *+8                                                              
         OI    MISCFLG1,MF1SNDCN   SEND WAS A CANCEL SEND                       
         TM    DOSTTYP2,DOSTXRSD                                                
         BZ    *+8                                                              
         OI    MISCFLG1,MF1RESND   SEND WAS A RESEND                            
PTSTV050 LR    R6,RF               RESTORE R6                                   
*                                                                               
PTSTVYES SR    RC,RC               SET CC TO EQ                                 
PTSTVNO  LTR   RC,RC               SET CC TO NEQ                                
         XIT1  REGS=(R6)                                                        
         LTORG                                                                  
         EJECT                                                                  
         DROP  R6                                                               
***********************************************************************         
* LOCATE WHERE TO PUT TRANSMISSION ELEMENT INTO ORDER RECORD BASED ON           
* THE VERSION NUMBER                                                            
*                                                                               
* ON ENTRY:    AIO1                A(ORDER RECORD)                              
*              DOSTELEM            BUILT STATUS ELEMENT (X'12')                 
*                                                                               
* ON EXIT:     (R6)                A(TRANSMISSION ELEM TO BE CHANGED)           
***********************************************************************         
PTXMTVER NTR1                                                                   
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         XR    R0,R0                                                            
*                                                                               
PTXMVLP  CLI   0(R6),0             WE SHOULD NEVER HIT END OF RECORD            
         BE    PTXMVNO                                                          
         CLI   0(R6),DOSPELQ       WE GET THE ORDER'S VERSION FROM THIS         
         BE    PTXMV010                                                         
         CLI   0(R6),DOXMTELQ                                                   
         BE    PTXMV020                                                         
PTXMVBMP IC    R0,1(R6)            BUMP TO THE NEXT ELEMENT                     
         AR    R6,R0                                                            
         B     PTXMVLP                                                          
*                                                                               
         USING DOSPELD,R6                                                       
PTXMV010 XR    RE,RE               RE = ORDER'S SEND VERSION                    
         ICM   RE,3,DOSPVER#                                                    
         B     PTXMVBMP                                                         
*                                                                               
PTXMV020 CLM   RE,3,BINSNDCT       ORDER'S SEND CT <= MSG SEND COUNT?           
         BNH   PTXMVYES                                                         
         BCTR  RE,0                NO, DECREMENT SEND VERSION #                 
         B     PTXMVBMP                                                         
*                                                                               
PTXMVYES SR    RC,RC                                                            
PTXMVNO  LTR   RC,RC                                                            
         XIT1  REGS=(R6)                                                        
         EJECT                                                                  
***********************************************************************         
* READ THE ORDER RECORD/DELETE CURRENT COLOR PTR/ADD NEW COLOR PTR              
* SVSTAT HAS CURRENT STATU                                                      
* ELEM CONTAINS STATUS ELEMENT                                                  
***********************************************************************         
BLDCOLOR NTR1                                                                   
         MVI   BYTE,0                                                           
         MVC   SVSTAT,DOSTSTAT-DOSTELD+ELEM                                     
*                                                                               
         BRAS  RE,ORDSTADR         R1 = A(STATUS TABLE)                         
*                                                                               
         USING ORDCD,R1                                                         
BLDCLR10 CLC   SVSTAT,ORDCSTAT                                                  
         BE    BLDCLR20                                                         
         LA    R1,ORDCDLNQ(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   BLDCLR10                                                         
         DC    H'0'                WE SHOULD ALWAYS FIND A MATCH                
*                                                                               
BLDCLR20 MVC   BYTE,1(R1)          SAVE CORRESPONDING COLOR                     
*                                                                               
         L     R6,AIO1             FIND COLOR ELEMENT IN ORDER REC              
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         DROP  R6                                                               
*                                                                               
         L     R6,AIO1             FIND COLOR ELEMENT IN ORDER REC              
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,COLELQ                                                    
         MVI   ELCDHI,COLELQ                                                    
         BRAS  RE,BNEXTEL                                                       
         BNE   BLDCLR80                                                         
*                                                                               
K        USING DSCKTYPE,KEY                                                     
         USING COLOREL,R6                                                       
*                                                                               
         XC    KEY,KEY             READ AND DELETE OLD COLOR POINTER            
         MVI   K.DSCKTYPE,DSCKTYPQ X'0DB8'                                      
         MVI   K.DSCKSTYP,DSCKSTYQ                                              
         MVC   K.DSCKAGMD,BAGYMD                                                
         MVC   K.DSCKBYR,QBUYER                                                 
         OI    K.DSCKBYR+2,C' '                                                 
         MVC   K.DSCKSTAT,COLCOL                                                
         MVC   K.DSCKDATE,COLDATE                                               
         MVC   K.DSCKORDR,BINORDER                                              
         MVC   KEY+14(4),SVORDDA                                                
         DROP  K                                                                
         DROP  R6                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLDCLR80                  CONVERSION NOT YET RUN !               
         MVI   DMINBTS,X'80'       READ UPDATE NOW                              
         BRAS  RE,HIGH                                                          
         OI    KEY+13,X'80'        DELETE OLD                                   
         BRAS  RE,WRITE                                                         
*                                                                               
Y        USING COLOREL,ELEM+128                                                 
BLDCLR80 XC    ELEM+128(128),ELEM+128    BUILD NEW COLOR ELEMENT                
         MVI   Y.COLEL,COLELQ                                                   
         MVI   Y.COLELLEN,COLLENQ                                               
         MVC   Y.COLCOL,BYTE                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,Y.COLDATE)                                 
         XC    Y.COLDATE,=X'FFFF'       COMPLEMENT DATE                         
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,COLELQ                                                    
         MVI   ELCDHI,COLELQ                                                    
         BRAS  RE,BNEXTEL                                                       
         BNE   BLDCLR85                                                         
*                                                                               
BLDCLR82 GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6)  DELETE OLD COLOR ELEM              
         BRAS  RE,BNEXTEL2                   IN CASE ANY MORE AROUND            
         BE    BLDCLR82                                                         
*                                                                               
BLDCLR85 GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM+128,(R6)   ADD NEW                  
*                                                                               
         GOTO1 PUT                 WRITE THE ORDER RECORD                       
*                                                                               
K        USING DSCKTYPE,KEY                                                     
BLDCLR87 XC    KEY,KEY             CREATE NEW COLOR POINTER                     
         MVI   K.DSCKTYPE,DSCKTYPQ X'0DB8'                                      
         MVI   K.DSCKSTYP,DSCKSTYQ                                              
         MVC   K.DSCKAGMD,BAGYMD                                                
         MVC   K.DSCKBYR,QBUYER                                                 
         OI    K.DSCKBYR+2,C' '                                                 
         MVC   K.DSCKSTAT,Y.COLCOL                                              
         MVC   K.DSCKDATE,Y.COLDATE                                             
         MVC   K.DSCKORDR,BINORDER                                              
         MVC   KEY+14(4),SVORDDA   MOVE SAVED DISK ADDRESS                      
         DROP  K,Y                                                              
*                                                                               
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLDCLR90                                                         
         MVI   DMINBTS,X'88'       UPDATING NOW                                 
         BRAS  RE,HIGH                                                          
         NI    KEY+13,X'7F'        UNDELETE                                     
         BRAS  RE,WRITE                                                         
         B     BLDCLRX                                                          
*                                                                               
BLDCLR90 MVC   KEY,KEYSAVE         ADD                                          
         BRAS  RE,ADDDIR                                                        
*                                                                               
BLDCLRX  J     YES                                                              
*                                                                               
ORDSTADR BASR  R1,RE               POINT R1 TO STATUS TABLE                     
         DC    AL1(DSENT),C'G'                                                  
         DC    AL1(QRJCT),C'G'                                                  
         DC    AL1(QEMPTY),C'G'                                                 
         DC    AL1(QERRORED),C'G'                                               
*                                                                               
         DC    AL1(QAPP),C'R'                                                   
         DC    AL1(DDLVRD),C'R'                                                 
*                                                                               
         DC    AL1(QCFMD),C'K'                                                  
         DC    AL1(QNODARE),C'K'                                                
         DC    AL1(QUNDARE),C'K'                                                
         DC    X'FF'                                                            
         EJECT                                                                  
ORDCD    DSECT                                                                  
ORDCSTAT DS    XL1                 ORDER OR MG STATUS                           
ORDCCLR  DS    XL1                 COLOR CODE                                   
ORDCDLNQ EQU   *-ORDCD                                                          
*                                                                               
T16F03   CSECT                                                                  
***********************************************************************         
* PROCESSES THE RESPONSE EVENT                                                  
*                                                                               
* ON ENTRY:    R7                  A(GAP MESSAGE)                               
*              R4                  A(STATUS ELEMENT BEING BUILT)                
*              R6                  A(WHERE TO INSERT STATUS ELEM)               
***********************************************************************         
         USING DOSTELD,R4                                                       
PROCRSPN ST    RE,SAVERE                                                        
*                                                                               
         OC    EVRRSPNS,=CL32' '     UPPERCASE THE REPSONSE                     
*                                                                               
         CLC   =C'ACCEPT',EVRRSPNS   PLAIN ACCEPT?                              
         BE    PRSPN20                                                          
         CLC   =C'CONFIRM',EVRRSPNS       OR CONFIRM?                           
         BE    PRSPN20                                                          
*                                                                               
         CLC   =C'CANCEL CONFIRMED',EVRRSPNS  OR CANCEL CONFIRMED?              
         BE    PRSPN10                                                          
         CLC   =C'CANCEL ACCEPTED',EVRRSPNS   OR CANCEL ACCEPTED?               
         BNE   PRSPN100                                                         
PRSPN10  OI    DOSTTYPE,DCNFMCAN     SET CANCEL CONFIRMED BIT                   
*                                                                               
PRSPN20  MVI   DOSTSTAT,QCFMD        CONFIRMED BY SELLER                        
         MVI   DOSTLEN,DOSTLNQ3      CONFIRM HAVE EXT LENGTH                    
         OI    DOSTTYPE,DCNFMFUL                                                
         B     PRSPN200                                                         
*                                                                               
PRSPN100 CLC   =C'DECLINE',EVRRSPNS  DECLINE?                                   
         BE    PRSPN120                                                         
         CLC   =C'REJECT',EVRRSPNS   OR REJECT?                                 
         BE    PRSPN120                                                         
*                                                                               
         CLC   =C'CANCEL DECLINED',EVRRSPNS  CANCEL DECLINED?                   
         BE    PRSPN110                                                         
         CLC   =C'CANCEL REJECTED',EVRRSPNS  OR CANCEL REJECTED?                
         BNE   PRSPN150                                                         
PRSPN110 MVI   DOSTLEN,DOSTLNQ3      CANCELLED REJECT HAS EXT LENGTH            
         OI    DOSTTYPE,DRJTCAN      SET CANCELLED REJECT BIT                   
*                                                                               
PRSPN120 MVI   DOSTSTAT,QRJCT        REJECTED BY SELLER                         
         B     PRSPN200                                                         
*                                                                               
PRSPN150 XC    ELEM,ELEM           DOESN'T MATCH ANY KNOWN RESPONSE             
         B     PRSPNX                                                           
***************                                                                 
* CHECK IF ANY COMMENT TO SAVE                                                  
***************                                                                 
PRSPN200 LA    R1,EVRCMMNT                                                      
         ST    R1,AMORCMTS                                                      
*                                                                               
         BRAS  RE,CREAUCOM         EQ OR NEQ DOESN'T MATTER HERE                
*                                                                               
PRSPNX   L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R4                                                               
***********************************************************************         
* PROCESSES THE FIRST ACCESS EVENT                                              
*                                                                               
* ON ENTRY:    R7                  A(GAP MESSAGE)                               
*              R4                  A(STATUS ELEMENT BEING BUILT)                
*              R6                  A(WHERE TO INSERT STATUS ELEM)               
***********************************************************************         
PROCFACC ST    RE,SAVERE                                                        
*                                                                               
         USING DOSTELD,R4                                                       
PFACC50  MVI   DOSTSTAT,QAPP       C'A' - APPROVED/OPENED                       
*                                                                               
PFACCX   L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R4                                                               
***********************************************************************         
* PROCESSES THE CONCLUSION EVENT                                                
*                                                                               
* ON ENTRY:    R7                  A(GAP MESSAGE)                               
*              R4                  A(STATUS ELEMENT BEING BUILT)                
*              R6                  A(WHERE TO INSERT STATUS ELEM)               
***********************************************************************         
PROCCONC ST    RE,SAVERE                                                        
*                                                                               
PCONC10  XC    ELEM,ELEM           NO STATUS ELEM FOR THIS                      
*                                                                               
PCONCX   L     RE,SAVERE                                                        
         BR    RE                                                               
***********************************************************************         
* SWITCHES CONTROL TO THE CORRECT SPOT SYSTEM                                   
*                                                                               
* ON ENTRY:    R7                  A(GAP MESSAGE)                               
*                                                                               
* ON EXIT:     SPTSENUM            AGENCY'S SPOT SENUM                          
***********************************************************************         
SWTCHCTL NTR1                                                                   
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   EXITPRG             EXIT PROGRAM, SHOULD WAIT FOR IT             
*                                                                               
         LA    R4,EVDCBZID                                                      
         USING BIZNZIDD,R4                                                      
         MVC   AGENCY,BZIAALPH     AGENCY ALPHA AND MEDIA CODE ARE IN           
         MVC   QMED,BZIMEDIA         THE BUSINESS ID                            
*                                                                               
SWSPT10  GOTO1 CALCORDR,DMCB,BZUORDNM                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         PACK  DUB,BZUORDSC                                                     
         CVB   R1,DUB                                                           
         STCM  R1,3,BINSNDCT                                                    
*                                                                               
         PACK  DUB,BZUORDGS                                                     
         CVB   R1,DUB                                                           
         STCM  R1,3,BINGAPSQ                                                    
*                                                                               
SWSPT20  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5REC,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,HIGHCT                                                        
*                                                                               
         L     R6,AIO1                                                          
         USING CT5REC,R6                                                        
*                                                                               
         CLC   CT5KEY,KEY          ERR IF WE CAN'T FIND ACCESS REC              
         BE    *+6                                                              
         DC    H'0'                    AGY ALPHA NOT VALID                      
*                                                                               
         LA    R6,CT5DATA                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,FIRSTEL                                                       
         BE    *+6                 ERROR IF WE CAN'T FIND SPOT SYS ELEM         
         DC    H'0'                                                             
*                                                                               
         USING CTSYSD,R6                                                        
SWSPT30  CLI   CTSYSNUM,2            HAS TO BE SPOT                             
         BE    SWSPT35                                                          
         BAS   RE,NEXTEL                                                        
         BE    SWSPT30                                                          
         DC    H'0'                                                             
*                                                                               
SWSPT35  MVC   SPTSENUM,CTSYSSE    GET SENUM FOR AGENCY'S SPOT FILE             
         DROP  R6                                                               
*                                                                               
         MVC   DMCB(1),SPTSENUM    SWITCH TO SPOT SYSTEM                        
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BE    SWSPT40                                                          
         CLI   DMCB+4,2            SWITCHED, BUT SYSTEM NOT OPENED?             
         BE    EXITPRG             YES, SHOULD WAIT UNTIL OPENED                
         DC    H'0'                USER NOT AUTHORIZED                          
*                                                                               
SWSPT40  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'AGYKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO1                                                          
         USING AGYKEY,R6                                                        
         MVC   SVAPRF07,AGYPROF+07   SAVE AGY PROFILE BYTE #8 (OFF 7)           
*                                                                               
         LA    R6,AGYEL                                                         
         MVI   ELCODE,X'02'                                                     
SWSPT50  BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING AGYMEDEL,R6                                                      
         CLC   AGYMEDCD,QMED       MATCH MEDIA IN MEDIA CODE ELEM?              
         BNE   SWSPT50                                                          
         MVC   BAGYMD,AGYMEDBT                                                  
         DROP  R6                                                               
*                                                                               
SWSPTYES J     YES                                                              
*                                                                               
SWSPTNO  J     NO                                                               
*                                                                               
***********************************************************************         
* SWITCHES SYSTEM WHETHER THE PROGRAM HAS BEEN AUTHORIZED TO SWITCH             
* SYSTEMS OR NOT                                                                
*                                                                               
* ON ENTRY:    DMCB    BYTE  0     SYSTEM SENUM TO SWITCH TO                    
***********************************************************************         
SWTCHSYS NTR1                                                                   
         MVC   DMCB+1(3),=3X'FF'   DON'T CARE IF PROGRAM IS AUTHORIZED          
         GOTO1 VSWITCH,DMCB,,0                                                  
         CLI   DMCB+4,0            SUCCESSFUL?                                  
         BNE   SWSYSNO             NO                                           
*                                                                               
SWSYSYES J     YES                                                              
*                                                                               
SWSYSNO  J     NO                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERTS THE 8 BYTE NUMERIC ORDER NUMBER TO A 4 BYTE BINARY CODE              
*                                                                               
* ON ENTRY:    PARAM 1             A(ORDER NUMBER IN EBCDIC FORM)               
*                                                                               
* ON EXIT:     BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
***********************************************************************         
CALCORDR NTR1                                                                   
         L     R2,DMCB                                                          
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         LR    RE,R2               MAKE SURE ORDER NUMBER IS VALID              
         LA    RF,L'BZUORDNM                                                    
CORDR10  CLI   0(RE),C'0'                                                       
         BL    CORDRNO                                                          
         CLI   0(RE),C'9'                                                       
         BH    CORDRNO                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,CORDR10                                                       
*                                                                               
         CLI   1(R2),C'3'          NEW STYLE ORDER NUMBER?                      
         BNH   CORDR20             NO                                           
         PACK  DUB,0(8,R2)         NEW STYLE ORDER NUMBER                       
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,BINORDER                                                   
         OI    BINORDER,X'80'                                                   
         XC    BINORDER,=4X'FF'                                                 
         B     CORDRYES                                                         
*                                                                               
CORDR20  GOTO1 VHEXIN,DMCB,(R2),BINORDER,L'BZUORDNM                             
         MVC   PACKOF4B,BINORDER                                                
         OI    PACKOF4B+3,X'0F'    CONVERT IT TO PACK                           
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),BINORDER  STICK IN DAYS IN YEAR                    
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=4X'FF'                                                 
*                                                                               
         PACK  DUB,4(4,R2)         SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=4X'FF'                                                 
*                                                                               
CORDRYES J     YES                                                              
*                                                                               
CORDRNO  J     NO                                                               
         EJECT                                                                  
***********************************************************************         
* CREATES AN AMERICAN ORDER COMMENT ELEMENT FOR DARE ORDER RECORD               
***********************************************************************         
CREAUCOM NTR1                                                                   
         L     R4,AMORCMTS                                                      
         CLC   0(70,R4),ALLSPCES   WE HAVE 70 BYTES OF NOTHING?                 
         BNH   CUCOMNO             THEN NO MORE COMMENTS                        
*                                                                               
         LA    R1,CMTELEM          SAVE ANY RESPONSE COMMENT                    
         USING DOCOMELD,R1                                                      
         MVI   DOCOMEL,DOCOMELQ    X'30' - DARE COMMENT ELEMENT                 
         LLC   R0,DOCOMLIN         BUMP COMMENT LINE # NO MATTER WHAT           
         AHI   R0,1                                                             
         STC   R0,DOCOMLIN                                                      
         MVC   DOCOMTXT(70),0(R4)     ONLY SAVE 70 CHAR PER LINE                
         LA    R4,70(R4)                                                        
         ST    R4,AMORCMTS                                                      
*                                                                               
         LA    RF,DOCOMTXT         WHAT IS THE COMMENT LENGTH?                  
         LA    RE,DOCOMTXT+70-1                                                 
CUCOM10  CLI   0(RE),C' '          REMOVE TRAILING SPACES                       
         BH    CUCOM20                                                          
         BCTR  RE,0                                                             
         CR    RE,RF                                                            
         BNL   CUCOM10                                                          
         B     CUCOMNO             PROTECT EVEN THO THIS CAN'T HAPPEN           
*                                                                               
CUCOM20  LA    RE,1(RE)                                                         
         LA    RF,CMTELEM                                                       
         SR    RE,RF                                                            
         STC   RE,DOCOMLEN                                                      
*                                                                               
CUCOMYES J     YES                                                              
*                                                                               
CUCOMNO  XC    CMTELEM,CMTELEM                                                  
         J     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY ORDER RECORD FOR UPDATE AND PUTS IT IN AIO1                   
*                                                                               
* ON ENTRY:    BAGYMD              BINARY AGENCY/MEDIA                          
*              BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETORDER NTR1                                                                   
*                                                                               
GTORD50  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DOKTYPE,DOKTYPQ     X'0D34'                                      
         MVI   DOKSUBTY,DOKSTYPQ                                                
*                                                                               
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER                                                
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
* READ ORDER RECORD FOR UPDATE                                                  
         MVC   SVORDDA,KEY+14      SAVE DISK ADDRESS FOR PASSIVES               
         MVI   DMINBTS,X'80'       UPDATIVE                                     
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
GORDRYES J     YES                                                              
*                                                                               
GORDRNO  J     NO                                                               
         EJECT                                                                  
EXITPRG  L     RD,SAVERD           EXIT THE PROGRAM                             
         J     XIT                                                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
BNEXTEL  CLI   0(R6),0                                                          
         JE    BNEXTELX                                                         
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
BNEXTEL2 CLI   0(R6),0                                                          
         JE    BNEXTELX                                                         
         CLC   ELCDLO,0(R6)                                                     
         JH    BNEXTEL                                                          
         CLC   ELCDHI,0(R6)                                                     
         JL    BNEXTEL                                                          
         CR    RB,RB                                                            
         J     *+6                                                              
BNEXTELX LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
ENQZMSG  DC    CL40'+ENQDEQ+ MEDZ LONG UPDATE     (FACPAK)'                     
ENQXMSG  DC    CL40'+ENQDEQ+ MEDZ UPDATE ENDED    (FACPAK)'                     
ALLSPCES DC    132C' '                                                          
         SPACE 1                                                                
DMRDHI   DC    C'DMRDHI '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMADD    DC    C'DMADD  '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMUNLK   DC    C'DMUNLK '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC'                                                        
*                                                                               
*                                                                               
GFILE    DC    CL8'GFILE'                                                       
PRTQUE   DC    CL8'PRTQUE'                                                      
*                                                                               
SPTFILE  DC    C'SPTFILE'                                                       
SPTDIR   DC    C'SPTDIR'                                                        
CTFILE   DC    C'CTFILE'                                                        
STATION  DC    C'STATION'                                                       
XSPDIR   DC    CL8'XSPDIR'                                                      
XSPFILE  DC    CL8'XSPFIL'                                                      
         EJECT                                                                  
***********************************************************************         
* PUTS OUT A MESSAGE TO THE CONSOLE AND HARDCOPY                                
*                                                                               
* ON ENTRY:    PARAM  1            OFFSET FROM BEGINNING OF PROGRAM             
***********************************************************************         
WTOMSG   NTR1  BASE=*,LABEL=*                                                   
         XC    MSG2,MSG2                                                        
         MVC   MSG2(4),DMCB        COPY OFFSET FROM BEG OF PROGRAM              
         GOTO1 VHEXOUT,DMCB,MSG2,MSG1DISP,4                                     
*                                                                               
         XC    MSG2,MSG2                                                        
         MVC   MSG2,0(R7)                                                       
         XR    R0,R0                                                            
         WTO   TEXT=((MSGSTARL,D),(MSGPROBL,D),(MSGSTARL,D),(MSG1L,D), X        
               (MSG2L,D),(0,E)),DESC=2                                          
*                                                                               
         DC    H'0'                SO WE GET AN IMAGE                           
         LTORG                                                                  
MSGSTARL DC    H'80'                                                            
         DC    80C'*'                                                           
MSGPROBL DC    H'80'                                                            
         DC    CL49'**$GAP PROBLEM**  PLEASE CONTACT  WHOA AT EXT5324'          
         DC    CL31' IF YOU SEE THIS MESSAGE!!!!!!'                             
MSG1L    DC    H'80'                                                            
MSG1     DC    CL80' '                                                          
         ORG   MSG1                                                             
         DC    CL04'EDCT'                                                       
MSG1SYS  DC    CL01'?'                                                          
         DC    CL07', ADDR='                                                    
MSG1ADDR DC    CL08'????????'                                                   
         DC    CL07', DISP='                                                    
MSG1DISP DC    CL08'????????'                                                   
         ORG   MSG1+L'MSG1                                                      
MSG2L    DC    H'80'                                                            
MSG2     DC    CL80' '                                                          
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* PROCESSES THE RECIPIENT DEACTIVATION EVENT                                    
*                                                                               
* ON ENTRY:    R7                  A(GAP MESSAGE)                               
*              R4                  A(STATUS ELEMENT BEING BUILT)                
*              R6                  A(WHERE TO INSERT STATUS ELEM)               
***********************************************************************         
PROCRDEA ST    RE,SAVERE                                                        
*&&DO                                                                           
         GOTO1 VDATCON,DMCB,(0,EVDDSTMP+2),(3,CORSTEXD)                         
*                                                                               
         PACK  DUB,EVDTSTMP(4)     STORE TIME (HHMM) AS BINARY VALUE            
         CVB   R1,DUB                                                           
         STCM  R1,3,CORSTEXT                                                    
*&&                                                                             
         GOTO1 VDATCON,DMCB,(6,JDTTODAY),(3,CORSTEXD)                           
*                                                                               
         ZAP   DUB,PACKTIME                                                     
         SRP   DUB,64-1,0          GET RID OF SECONDS                           
         CVB   R1,DUB                                                           
         STCM  R1,3,CORSTEXT                                                    
*                                                                               
PRDEAYS  L     RE,SAVERE                                                        
         BR    RE                                                               
***********************************************************************         
* PROCESSES THE NO RESPONSE EVENT                                               
*                                                                               
* ON ENTRY:    R7                  A(GAP MESSAGE)                               
*              R4                  A(STATUS ELEMENT BEING BUILT)                
*              R6                  A(WHERE TO INSERT STATUS ELEM)               
***********************************************************************         
PROCNRSP ST    RE,SAVERE                                                        
*&&DO                                                                           
         GOTO1 VDATCON,DMCB,(0,EVNDSTMP+2),(3,CORSTEXD)                         
*                                                                               
         PACK  DUB,EVNTSTMP(4)     STORE TIME (HHMM) AS BINARY VALUE            
         CVB   R1,DUB                                                           
         STCM  R1,3,CORSTEXT                                                    
*&&                                                                             
         GOTO1 VDATCON,DMCB,(6,JDTTODAY),(3,CORSTEXD)                           
*                                                                               
         ZAP   DUB,PACKTIME                                                     
         SRP   DUB,64-1,0          GET RID OF SECONDS                           
         CVB   R1,DUB                                                           
         STCM  R1,3,CORSTEXT                                                    
*                                                                               
PNRSPX   L     RE,SAVERE                                                        
         BR    RE                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
***********************************************************************         
GOMSPACK NTR1  BASE=*,WORK=(R4,8)                                               
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPRF07                                                
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,SRPARMSD.SRQACOMF                                       
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 ASTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    GOMSP10                                                          
         LHI   R1,*-T16F03         GOT A STAPACK ERROR                          
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
GOMSP10  L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
         XIT1                                                                   
         DROP  R4                                                               
***********************************************************************         
* PROVIDE MSUNPK ENTRY POINT FOR LINKAGE TO STAPACK                             
***********************************************************************         
GOMSUNPK NTR1  BASE=*,WORK=(R4,8)                                               
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPRF07                                                
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,SRPARMSD.SRQACOMF                                       
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 ASTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
*****    B     *+6    <--- MHER 1/17/95  IGNORE ERRORS                          
*****    DC    H'0'                                                             
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*========================================================                       
* SPTDIR COMMANDS                                                               
*========================================================                       
         SPACE 1                                                                
ADDDIR   DS    0H                                                               
         BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(SPTDIR)                                                      
*                                                                               
ADDXKEY  DS    0H                                                               
         BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(XSPDIR)                                                      
*                                                                               
HIGH     MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
HIGHXSP  MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(XSPDIR)                                                      
*                                                                               
SEQ      BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
SEQXSP   BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(XSPDIR)                                                      
*                                                                               
WRITE    BRAS  R1,GODIR                                                         
         DC    AL4(DMWRT)                                                       
         DC    AL4(SPTDIR)                                                      
*                                                                               
HIGHCT   MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(CTFILE)                                                      
*                                                                               
WRTCT    BRAS  R1,GODIR                                                         
         DC    AL4(DMWRT)                                                       
         DC    AL4(CTFILE)                                                      
*                                                                               
ADDCT    BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(CTFILE)                                                      
*                                                                               
GODIR    NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,0(R1)         COMMAND                                      
         A     RE,RELO                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
*                                                                               
         ICM   RE,15,4(R1)         FILE NAME                                    
         A     RE,RELO                                                          
         ST    RE,DMCB+4                                                        
         CLC   =C'CTFILE',0(RE)    ARE WE LOOKING AT THE CONTROL FILE           
         BNE   GODIR10                                                          
         LA    RE,KEY                                                           
         ST    RE,DMCB+8                                                        
         L     RE,AIO                                                           
         ST    RE,DMCB+12                                                       
         B     GODIR20                                                          
*                                                                               
GODIR10  LA    RE,KEYSAVE                                                       
         ST    RE,DMCB+8                                                        
         LA    RE,KEY                                                           
         ST    RE,DMCB+12                                                       
*                                                                               
GODIR20  GOTO1 VDATAMGR,DMCB                                                    
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         LTORG                                                                  
         SPACE 1                                                                
*========================================================                       
* FILE COMMANDS                                                                 
*========================================================                       
         SPACE 1                                                                
GET      BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'               DSPL TO DISK ADDRESS                         
         SPACE 1                                                                
GETXSP   BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(XSPFILE)                                                     
         DC    H'36'               DSPL TO DISK ADDRESS                         
*                                                                               
PUT      BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
         SPACE 1                                                                
PUTXSP   BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(XSPFILE)                                                     
         DC    H'36'               DSPL TO DISK ADDRESS                         
*                                                                               
ADD      BRAS  R1,GOFILE                                                        
         DC    AL4(ADDREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
*                                                                               
GOFILE   NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,0(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB             SET COMMAND ADDRESS                          
         MVC   DMCB(1),DMINBTS                                                  
*                                                                               
         ICM   RE,15,4(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB+4           SET FILENAME ADDRESS                         
*                                                                               
         LA    RE,KEY                                                           
         AH    RE,8(R1)            GET DSPL OF DISK ADDRESS IN KEY              
         ST    RE,DMCB+8                                                        
*                                                                               
         MVC   DMCB+12(4),AIO                                                   
*                                                                               
         LA    RE,DMWORK                                                        
         ST    RE,DMCB+16                                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB                                                    
         TM    8(R1),X'02'         DELETED RECORD?                              
         BNZ   GOFILX              YEAH, WE DON'T WANNA DIE                     
         TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOFILX   MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FACIDTAB                                                       
       ++INCLUDE FACIDTABD                                                      
         SPACE                                                                  
EVNTOBJD DSECT                                                                  
EOBJTID  DS    CL1                 TRANSMISSION ID                              
EOBJRTN  DS    AL4                 A(ROUTINE)                                   
EOBJNXT  DS    0C                                                               
*                                                                               
BIZNZIDD DSECT              *** 32 BYTES -  AAMCCCTCAMP#ORDER#                  
BZIAALPH DS    CL2                 AGENCY ALPHA                                 
BZIMEDIA DS    C                   MEDIA                                        
********                                                                        
* FOR CA SPOT OM                                                                
********                                                                        
BZICLTCD DS    CL3                 CLIENT CODE                                  
BZIACTYP DS    C                   ACTION TYPE (BOOKING/CANCELLATION)           
BZICMPNM DS    CL6                 CAMPAIGN NUMBER WITH LEADING ZEROS           
BZIORDNM DS    CL7                 ORDER NUMBER WITH LEADING ZEROS              
BZIREVNM DS    CL2                 REVISION NUMBER WITH LEADING 0'S             
         DS    CL10                SPARE                                        
********                                                                        
* FOR US ORDER EXPRESS                                                          
********                                                                        
         ORG   BZICLTCD                                                         
BZUORDNM DS    CL8                 ORDER NUMBER WITH LEADING ZEROS              
BZUORDSC DS    CL5                 ORDER SEND COUNT WITH LEADING ZEROS          
BZUORDGS DS    CL5                 ORDER GAP SEQ # WITH LEADING ZEROS           
         DS    CL11                SPARE                                        
         EJECT                                                                  
EVNTDSCT DSECT              *** WE DO NOT GET THE FP=AGY=SJSYS=SPT              
EVSRVHDR DS    CL8                 'CANSPTOM'                                   
EVTYPE   DS    CL1                 EVENT TYPE                                   
EVDCBZID DS    CL32                DOCUMENT BUSINESS ID                         
EVEVENTS DS    0C                                                               
*******                                                                         
* EVTYPE = C'R' - RESPONSE EVENT                                                
*******                                                                         
EVRRBZID DS    CL128               RECIPIENT BUSINESS ID                        
EVRRSPNS DS    CL32                RESPONSE                                     
EVRDSTMP DS    CL8                 DATE STAMP  YYYYMMDD                         
         DS    C                                                                
EVRTSTMP DS    CL6                 TIME STAMP  HHMMSS                           
EVRIP    DS    CL15                XXX.XXX.XXX.XXX                              
EVRCMMNT DS    CL1024              COMMENT                                      
EVRACTED DS    CL64                ACTED ON BY                                  
EVRADDI1 DS    CL60                ADDITIONAL INFO LINE 1                       
EVRADDI2 DS    CL60                ADDITIONAL INFO LINE 2                       
EVRADDI3 DS    CL60                ADDITIONAL INFO LINE 3                       
EVREOL   DS    0C                  END-OF-LINE                                  
*******                                                                         
* EVTYPE = C'A' - ACCESS EVENT                                                  
*******                                                                         
         ORG   EVEVENTS                                                         
EVARBZID DS    CL128               RECIPIENT BUSINESS ID                        
EVADSTMP DS    CL8                 DATE STAMP  YYYYMMDD                         
         DS    C                                                                
EVATSTMP DS    CL6                 TIME STAMP  HHMMSS                           
EVAIP    DS    CL15                XXX.XXX.XXX.XXX                              
*******                                                                         
* EVTYPE = C'C' - CONCLUSION EVENT                                              
*******                                                                         
         ORG   EVEVENTS                                                         
EVCDSTMP DS    CL8                 DATE STAMP  YYYYMMDD                         
         DS    C                                                                
EVCTSTMP DS    CL6                 TIME STAMP  HHMMSS                           
EVCCAUSE DS    CL11                CAUSE                                        
*******                                                                         
* EVTYPE = C'D' - DECACTIVATION EVENT                                           
*******                                                                         
         ORG   EVEVENTS                                                         
EVDRBZID DS    CL128               RECIPIENT BUSINESS ID                        
EVDDSTMP DS    CL8                 DATE STAMP  YYYYMMDD                         
         DS    C                                                                
EVDTSTMP DS    CL6                 TIME STAMP  HHMMSS                           
*******                                                                         
* EVTYPE = C'N' - NO REPSONSE EVENT                                             
*******                                                                         
         ORG   EVEVENTS                                                         
EVNDSTMP DS    CL8                 DATE STAMP  YYYYMMDD                         
         DS    C                                                                
EVNTSTMP DS    CL6                 TIME STAMP  HHMMSS                           
EVNRBZID DS    CL128               RECIPIENT BUSINESS ID                        
                                                                                
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
*                                                                               
RELO     DS    A                                                                
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
AUTL     DS    A                                                                
ATBUFF   DS    A                                                                
AIO      DS    A                                                                
AIO1     DS    A                   A(IOAREA #1)                                 
AIO2     DS    A                   A(IOAREA #2)                                 
ASPLAREA DS    A                   A(SPOOL AREA)                                
AWRKRIOA DS    A                   A(IO AREA USED BY EDICT)                     
AWRKRBUF DS    A                   A(WORKER BUFFER AREA)                        
AMORCMTS DS    A                   A(MORE CMT ELEMS MIGHT BE NEEDED)            
*                                                                               
VADDAY   DS    V                                                                
VDATCON  DS    V                                                                
VGETFACT DS    V                                                                
VHELLO   DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VLOCKET  DS    V                                                                
VRECUP   DS    V                                                                
VCLUNPK  DS    V                                                                
VMSPACK  DS    V                                                                
VMSUNPK  DS    V                                                                
VPARSNIP DS    V                                                                
VSWITCH  DS    V                                                                
VCLPACK  DS    V                                                                
*                                                                               
ASPOOL   DS    A                                                                
ASTAPACK DS    A                                                                
AREPFACS DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
FULL     DS    F                                                                
SVORDDA  DS    A                   SAVED DISK ADDRESS OF ORDER                  
*                                                                               
DMINBTS  DS    X                                                                
RECLEN   DS    H                                                                
TERMNUM  DS    H                   TERMINAL NUMBER                              
USERNUM  DS    H                   USER ID NUMBER                               
TRNNUM   DS    H                   TRANSACTION COUNT                            
HALF     DS    H                                                                
DATADISP DS    H                                                                
MQMSGLEN DS    H                                                                
*                                                                               
AGENCY   DS    XL2                 AGENCY POWER CODE                            
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
USERID   DS    CL10                CONTROL USER ID                              
BAGYMD   DS    XL1                 BINARY AGENCY/MEDIA                          
BCLT     DS    XL2                 BINARY CLIENT CODE                           
BEST     DS    XL1                 BINARY ESTIMATE                              
BMKTSTA  DS    XL5                 BINARY MARKET STATION                        
BPRD     DS    XL1                                                              
BINCAMPN DS    XL4                 CAMPAIGN NUMBER (FF COMPLEMENT)              
*                                                                               
BINORDER DS    0XL4                ORDER NUMBER (FF COMPLEMENT)                 
BINORDDT DS    XL2                   (YEAR-90)*1000 + JULIAN DAY                
BINORDSQ DS    XL2                   SEQUENCE NUMBER (0-9999)                   
*                                                                               
BINREVSN DS    XL2                 REVISION # (FF COMPLEMENT)                   
BINSNDCT DS    XL2                 SEND COUNT                                   
BINGAPSQ DS    XL2                 GAP SEQ #                                    
SVAPRF07 DS    XL1                 SAVED COPY OF APROF+07                       
SVCPRF00 DS    XL1                 SAVED COPY OF CPROF+00                       
SVCOFFC  DS    XL1                 CLIENT OFFICE                                
SVSTAT   DS    XL1                 SAVED ORDER STATUS                           
         DS    0F                                                               
PACKOF4B DS    PL4                 PACKED NUMBER OF 4 BYTES                     
JDTTODAY DS    PL4                 JULIAN DATE OF TODAY                         
PACKTIME DS    PL4                 PACKED TIME (IE: 0125959F)                   
*                                                                               
QMED     DS    CL1                 EBCDIC MEDIA                                 
QBUYER   DS    CL12                       BUYER CODE                            
QCLT     DS    CL3                        CLIENT                                
QPRD1    DS    CL3                        PRODUCT 1                             
QPRD2    DS    CL3                        PRODUCT 2                             
QEST1    DS    CL3                        ESTIMATE 1                            
QSTA     DS    CL8                        STATION                               
QACTTYPE DS    CL1                        ACTION TYPE (B OR C)                  
*                                                                               
ESTPW    DS    CL3                 PW PERCENTAGE                                
ESTCOST2 DS    XL4                 COST2 PERCENTAGE                             
ESTLOKYM DS    XL2                 BYTE 0 - YEAR, BYTE 1 MONTH                  
*                                                 X'80' - PREVIOUS              
*                                                 X'40' - SUBSEQUENT            
*                                                                               
THESYSID DS    XL1                 SAVED SYSTEM ID                              
*                                                                               
SPTSENUM DS    XL1                 SPOT SYSTEM SENUM                            
BYTE     DS    C                                                                
*                                                                               
PIGPRD   DS    XL1                 PIGGYBACK PRODUCT BINARY CODE                
PIGEST   DS    XL1                 PIGGYBACK ESTIMATE                           
*                                                                               
MISCFLG1 DS    XL1                 VARIOUS BIT FLAGS FOR X'11' ELEM             
MF1XMTUP EQU   X'80'                -  XMT HAS BEEN UPDATED MUST PUTREC         
MF1NOXMT EQU   X'40'                -  NO TRANSMISSION ELEMENT FOUND            
MF1SNDCN EQU   X'20'                -  SEND WAS A SEND CANCEL                   
MF1RESND EQU   X'10'                -  SEND WAS A RESEND                        
*                                                                               
BITFLAG1 DS    XL1                 VARIOUS BIT FLAGS                            
BF1NWCOM EQU   X'80'                - NEED TO ADD REP COMMENT RECORD            
*                                                                               
EOBJNUMB DS    XL1                 DARE OBJECT NUMBER                           
EOBJDLNQ EQU   1                   DARE DELIVERY NOTIFICATION                   
EOBJOAPQ EQU   2                   DARE ORDER APPROVAL                          
EOBJORJQ EQU   3                   DARE ORDER REJECTION                         
EOBJOCMQ EQU   4                   DARE ORDER REJECTION COMMENT                 
EOBJOTRQ EQU   5                   DARE ORDER TRAILER                           
EOBJOCFQ EQU   6                   DARE ORDER CONFIRMATION                      
EOBJOLNQ EQU   7                   DARE ORDER LINE NUMBER EQUIVALENTS           
EOBJERRQ EQU   8                   DARE ERROR NOTIFICATION                      
EOBJARCQ EQU   9                   DARE AGENCY RECALL                           
EOBJORAQ EQU   10                  DARE ORDER RECALL ACKNOWLEDGEMENT            
EOBJMOKQ EQU   11                  MAKEGOOD CONFIRMATION                        
EOBJMCNQ EQU   12                  MAKEGOOD CANCELLATION                        
EOBJDFXQ EQU   13                  DARE FAX DELIVERY NOTIFICATION               
EOBJCFXQ EQU   14                  DARE FAX CANCELLATION                        
EOBJMKGQ EQU   15                  MAKEGOOD HEADER                              
EOBJEFXQ EQU   16                  DARE FAX ERROR                               
EOBJSALE EQU   17                  SALESPERSON REASSIGNMENT                     
EOBJURL  EQU   18                  URL CONFIRMATION                             
*                                                                               
BTODAY   DS    XL3                 TODAY'S DATE IN BINARY (YMD)                 
PRIORDAT DS    XL3                 YESTERDAY'S OR SOME PRIOR DAY'S DATE         
SYSNAME  DS    CL3                 3 CHR FACPAK NAME                            
SYSN1    DS    CL1                 1 CHR FACPAK NAME                            
WRKFILNO DS    XL2                 WORK FILE NUMBER                             
WRKRECNO DS    F                   WORKER FILE RECORD NUMBER                    
*                                                                               
WORK     DS    XL64                                                             
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
DKEY     DS    XL64                KEY OF LAST DA RECORD ADDED                  
HEADER   DS    XL32                                                             
DMWORK   DS    12D                                                              
*                                                                               
FILENAME DS    CL7                 DMGR FILENAME                                
LASTFILE DS    CL1                 PREVIOUS FILE NUMBER                         
DALINK   DS    CL4                 DISK ADDRESS OF LAST ADDREC                  
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
ELCODE   DS    XL1                                                              
REVISION DS    XL1                 REVISION NUMBER                              
ORDAUDTR DS    X                   ORDER AUDIT TRAIL ACTION                     
REPORDTP DS    XL1                 REP ORDER TYPE                               
COPYQ    EQU   X'01'                                                            
CHANGEQ  EQU   X'02'                                                            
ADDQ     EQU   X'03'                                                            
NUMSPOTS DS    XL1                                                              
*                                                                               
TRDEMTHD DS    CL1                 TRADE METHOD                                 
TRDEDATA DS    CL8                 DATA TO DETERMINE TRADE                      
*                                                                               
RECCOUNT DS    F                   RECORD COUNT                                 
*                                                                               
DEMSAVE  DS    XL6                 DEMO EXIT SAVE AREA FOR DEMPROS              
*                                                                               
SAVEBUFF DS    0XL12                                                            
SBVPQTAB DS    A                   A(PQTAB)                                     
SBVPQFIL DS    A                   A(PQFILE DTF)                                
SBSAVE   DS    A                   DISPLACEMENT TO START OF SAVE AREA           
*                                                                               
SAVESIN  DS    F                                                                
NEXTSIN  DS    F                                                                
QSIN     DS    CL6                                                              
PRTQID   DS    CL8                                                              
*                                                                               
PROFDAR  DS    CL16                DAR PROFILE                                  
PDARONHD EQU   PROFDAR+0            - ON-HOLD IF MKGD OKAYED BY REP?            
*                                                                               
PROFOM   DS    CL16                OM PROFILE                                   
POMUSEOM EQU   PROFOM               - USES ORDER MANAGER?                       
POMAUCFM EQU   PROFOM+2             - PARITAL CONFIRM WORKFLOW                  
POMMGREP EQU   PROFOM+15            - CREATE MKGD TRANSACTION REPORT?           
*                                                                               
REMUSER  DS    CL3                                                              
BIGSPLKY DS    CL144                                                            
*                                                                               
ACURELEM DS    F                   ADDRESS OF CURRENT ELEMENT                   
ELEM     DS    XL256                                                            
CMTELEM  DS    XL256                                                            
ACTELEM  DS    XL66                ACTED ON BY ELEM, MAX OF 64 CHARS            
ECNELEM  DS    XL62                MAX LENGTH FOR ECONTRACT TEXT IS 60          
*                                                                               
IOA1     DS    6000C               I/O AREA                                     
IOA2     DS    6000C               I/O AREA                                     
*                                                                               
SPULAREA DS    XL3200                                                           
*                                                                               
WRKRIOA  DS    14336C              BYTES 0-1   = LENGTH OF RECORD               
*                                  BYTE  2     = L(DATA)+1                      
*                                  BYTES 3-??  = DATA                           
WRECQLNQ EQU   *-WRKRIOA                                                        
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
* DMWRKRD                                                                       
* DMWRKRK                                                                       
* FADSECTS                                                                      
* FAPQPL                                                                        
* CTGENFILE                                                                     
* CTGENDARE                                                                     
* CTGENRAD                                                                      
* DMGREQUS                                                                      
* DMPRTQD                                                                       
* DMPRTQS                                                                       
* DMPRTQK   <--- PREFIXED WITH 'SR'                                             
* DDCOMFACS                                                                     
* DMFILTABD                                                                     
* TASYSWORKD                                                                    
* DMREQHDRA                                                                     
* DDEDICTFIL                                                                    
* FAFACTS                                                                       
* SPTRPAT                                                                       
* SPTRSHIP                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE FAPQPL                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENDARE                                                      
       ++INCLUDE CTGENRAD                                                       
       ++INCLUDE DMGREQUS                                                       
       ++INCLUDE DMPRTQD                                                        
       ++INCLUDE DMPRTQS                                                        
*PREFIX=SR                                                                      
       ++INCLUDE DMPRTQK                                                        
*PREFIX=                                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMFILTABD                                                      
       ++INCLUDE TASYSWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
       ++INCLUDE DMWRKFL                                                        
*********INCLUDE DMWRKFK                                                        
       ++INCLUDE DDEDICTFIL                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPTRPAT                                                        
       ++INCLUDE SPTRSHIP                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SRGAPFFD                                                       
         EJECT                                                                  
       ++INCLUDE GEGENSPSAL                                                     
         EJECT                                                                  
       ++INCLUDE SPGENCDORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPSYSFAC                                                       
         EJECT                                                                  
       ++INCLUDE DDDARETABD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SRGAP03   11/03/20'                                      
         END                                                                    
