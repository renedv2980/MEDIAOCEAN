*          DATA SET SRDXM00    AT LEVEL 016 AS OF 03/12/19                      
*PHASE T16300C                                                                  
***********************************************************************         
* NOTE: MOST OF THIS CODE HAS BEEN COPIED FROM SRDAR00 WITH SOME                
*       CHANGES AS WOULD NOT REQUIRE THE EDICT FILE.  ALL THE MESSAGES          
*       ARE COMING IN VIA MQ TO THE TSAR BUFFER.  THIS PROGRAM IS               
*       STARTED BY SRMQP00:INREDIXM ROUTINE                                     
***********************************************************************         
T16300   TITLE 'SRDXM00 ($DXML) - DARE XML UPDATE FACILITY'                     
T16300   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$DXM**,RA,CLEAR=YES,RR=RE                          
         USING WORKD,RC            RC = A(WORKING STORAGE)                      
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         MVC   SRPARMS(8*4),0(R1)  SAVE SERVICE REQUEST PARAMETER LIST          
SRPARMSD USING SRPARMD,SRPARMS                                                  
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
         CHI   RE,4064             IF L(MESSAGE) > 4064                         
         BL    MAIN10                                                           
**** SWITCH INTO XA MODE (31-BIT ADDRESSING)                                    
         BRAS  RE,ON31                                                          
**** GRAB ADDRESS FROM TCBRBUFF                                                 
         L     R8,VSSB             R8 = A(SSB)                                  
         USING SSBD,R8                                                          
         L     RF,SSBTKADR                                                      
         MVC   ATBUFF,TCBRBUFF-TCBD(RF)  SAVE "REAL" ADDRESS OF TBUFF           
         DROP  R8                                                               
**** SWITCH OUT OF XA MODE (24-BIT ADDRESSING)                                  
         BRAS  RE,OFF31                                                         
*                                                                               
MAIN10   BAS   RE,INITLIZE         INITIALIZE COMMON VARIABLES                  
*                                                                               
         BAS   RE,WORKITIN         PROCESS WHAT WAS PASSED IN TBUFF             
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
         LR    R7,RC               FOR SAVING MY MESSAGES                       
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
         LR    R7,RC               FOR EDICT                                    
         AHI   R7,HUGEBLCK-WORKD                                                
         ST    R7,AHUGEBLK                                                      
*                                                                               
         L     R8,VSSB             R8 = A(SSB)                                  
         USING SSBD,R8                                                          
         NI    SSBJFLAG,255-SSBJFWKR STOP ABEND LOOPS                           
         MVC   AFACIDT,SSBAFID                                                  
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
         MVI   DMCB+7,X'15'           CLUNPK                                    
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VCLUNPK,DMCB                                                     
*                                                                               
         MVI   DMCB+7,X'7A'           STAPACK                                   
         GOTO1 VCALLOV,DMCB                                                     
         MVC   ASTAPACK,DMCB                                                    
*                                                                               
         MVI   DMCB+7,X'2B'           SPGETBUY                                  
         GOTO1 VCALLOV,DMCB                                                     
         MVC   AGETBUY,DMCB                                                     
***************                                                                 
* GET TODAY'S DATE                                                              
***************                                                                 
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,BTODAY)                                 
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    INIT10                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,WORK,WORK,F'1'                                       
*                                                                               
INIT10   GOTO1 VDATCON,DMCB,(3,BTODAY),(15,JDTTODAY)                            
*                                                                               
INITX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PUTS THE MESSAGE POINTED BY R7 WHOSE LENGTH IS IN R8 INTO AIO1                
*                                                                               
* ON ENTRY:    (R7)                A(MESSAGE)                                   
*              (R8)                L(MESSAGE) SAY N                             
*                                                                               
* ON EXIT:     CLOBBERING AIO1                                                  
*              BYTE  0             L(MESSAGE + 1) = N+1                         
*              BYTES 1-(N)         MESSAGE                                      
***********************************************************************         
PUTINIOA NTR1                                                                   
         L     R6,AIO1                                                          
         LR    R1,R8               L'MSG                                        
         LA    R1,1(R1)            +1 TO INCL LENGTH                            
         STC   R1,0(R6)                                                         
         SHI   R1,2                L'MSG-1                                      
         EX    R1,*+8                                                           
         J     YES                                                              
         MVC   1(0,R6),0(R7)                                                    
         SPACE 2                                                                
***********************************************************************         
* PUTS THE RECORD IN AIO1 INTO THE WORKER IO AREA                               
*                                                                               
* NOTE: FORMAT OF THE RECORD IN AIO1 IS:                                        
*              BYTE  0             LENGTH OF THE RECORD (SAY N)                 
*              BYTES 1-(N-1)       ACTUAL RECORD                                
***********************************************************************         
PUTINWKR NTR1                                                                   
         L     R6,AWRKRIOA         R6 = A(WHERE TO PUT THE RECORD)              
         OC    0(2,R6),0(R6)       IF WE HAVE AN ERROR                          
         BZ    PIOAX               THEN DON'T ADD TO IOA                        
*                                                                               
         ZICM  RE,0(R6),2                                                       
         AR    R6,RE                                                            
*                                                                               
         L     RE,AIO1             COPY RECORD OVER TO WORKER RECORD            
         ZICM  R1,0(RE),1                                                       
         BZ    PIOAX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RE)                                                    
*                                                                               
         LA    RE,1(R1,R6)         RE = A(WHERE TO PUT NEXT RECORD)             
         L     R6,AWRKRIOA                                                      
         SR    RE,R6                                                            
         STCM  RE,3,0(R6)                                                       
         CH    RE,=Y(WRECQLNQ)     MAKE SURE WE DON'T EXCEED THE LENGTH         
         BNH   PIOAX                                                            
         LHI   R1,*-T16300         CAN'T FIT INTO AWRKRIOA                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
PIOAX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DETERMINES WHICH TYPE OF DARE RECORD WE HAVE AND THEN ROUTES IT TO            
* THE CORRECT PROCESSING ROUTINE                                                
***********************************************************************         
WORKITIN NTR1                                                                   
         CLC   MQMSGLEN,=AL2(4064) NOT IN TBUFF OF UTL?                         
         BNH   WITIN10                                                          
**** SET 31-BIT ADDRESSING                                                      
         BRAS  RE,ON31                                                          
*****                                                                           
* PUT 100 SPACES AFTER THE MESSAGE SO WE DON'T PROCESS AS IF THERE WAS          
* RELEVANT DATA THERE (IE: DLNNOT FOR A MG WHEN IT IS A REGULAR DLNNOT)         
*****                                                                           
WITIN10  L     R7,ATBUFF                                                        
         SHI   R7,2                                                             
         XR    RE,RE                                                            
         ICM   RE,3,0(R7)          LENGTH OF THE MSG IN BUFFER                  
         LA    RE,2(R7,RE)         POINT AFTER THE MESSAGE                      
         XC    0(255,RE),0(RE)                                                  
*****                                                                           
         L     R7,ATBUFF                                                        
         AHI   R7,8                BUMP PAST THE "REDIXML "                     
*                                                                               
         MVI   THISISMG,C'N'       DEFAULT TO NOT MG ACTION                     
         MVI   SVSTAT,0            CLEAR SAVED STATUS                           
*                                                                               
         LR    R6,R7               R6 = A(DARE REC W/O REDIXML INFO)            
         LA    R1,DAREOBJS                                                      
         USING DAREOBJD,R1                                                      
*                                                                               
WITIN30  CLI   DOBJTID,0           MATCHED ON TRANSMISSION ID?                  
         BE    WITINNO             NO                                           
*                                                                               
         CLC   DOBJTID,0(R6)       MATCH ON THIS TRANSMISSION ID?               
         BE    WITIN50                                                          
         LA    R1,DOBJNXT          NO, CHECK NEXT TRANSMISSION ID               
         B     WITIN30                                                          
*                                                                               
WITIN50  MVC   DOBJNUMB,DOBJRIDN   YES, SAVE THE RETURN ID NUMBER               
         DROP  R1                                                               
*                                                                               
         LR    R7,R6               TO FREE R6 FOR GENERAL IO STUFF              
         ZIC   RF,DOBJNUMB                                                      
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     WITINTBL(RF)                                                     
*                                                                               
WITINTBL B     PROCDLNT                                                         
         B     PROCOAPP                                                         
         B     PROCOREJ                                                         
         B     PROCOCFM                                                         
         B     PROCERNT                                                         
         B     PROCORAK                                                         
         B     PROCDFAX                                                         
         B     PROCCNFX                                                         
         B     PROCERFX                                                         
*                                                                               
WITINNO  J     NO                                                               
*                                                                               
DAREOBJS DC    CL6'DLNNOT',AL1(DOBJDLNQ)                                        
         DC    CL6'ORDAPP',AL1(DOBJOAPQ)                                        
         DC    CL6'CANAPP',AL1(DOBJOAPQ)                                        
         DC    CL6'ORDREJ',AL1(DOBJORJQ)                                        
         DC    CL6'CANREJ',AL1(DOBJORJQ)                                        
         DC    CL6'ORDCFM',AL1(DOBJOCFQ)                                        
         DC    CL6'CANCFM',AL1(DOBJOCFQ)                                        
         DC    CL6'ORDCAN',AL1(DOBJOCFQ)                                        
         DC    CL6'ERRNOT',AL1(DOBJERRQ)                                        
         DC    CL6'ORDRCL',AL1(DOBJORAQ)                                        
         DC    CL6'DLNFAX',AL1(DOBJDFXQ)                                        
         DC    CL6'CANFAX',AL1(DOBJCFXQ)                                        
         DC    CL6'ERRFAX',AL1(DOBJEFXQ)                                        
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE DELIVERY NOTIFICATION FOR AGENCY                                
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCDLNT DS    0H                                                               
         USING RDLNNOTD,R7                                                      
PDLNTD   USING RTN2SNDR,RDNTRTRN                                                
         MVC   QREPCON,RDNTRPCN    COPY THESE VALUES                            
         MVC   QRETURN,RDNTRTRN                                                 
         GOTO1 VHEXIN,DMCB,PDLNTD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,PDLNTD.RTNPWRCD                                           
         NI    MISCFLG1,X'FF'-MF1XMTUP  NO UPDATES TO XMT ELEM                  
*                                                                               
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         MVI   QMED,C'T'                                                        
         CLI   PDLNTD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+12                                                             
         OI    MISCFLG3,MF3RADIO                                                
         MVI   QMED,C'R'                                                        
         DROP  PDLNTD                                                           
*                                                                               
PDLNT100 GOTO1 CALCORDR,DMCB,RDNTORDR  CONVERT ORDER NUMBER TO BINARY           
         BNE   PDLNTNO                                                          
*                                                                               
         MVC   USERID,RDNTTOID                                                  
         BRAS  RE,SWTCHSPT         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   PDLNTNO                                                          
*                                                                               
         MVC   QMGGROUP,MDNTOFRI-MDLNNOTD(R7)   COULD BE DELNOT FOR MKG         
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   PDLNTNO                                                          
         CLC   QMGGROUP,=C'  '     DELNOT FOR MKGD?                             
         BH    PDLNT300                                                         
***************                                                                 
* DELIVERY NOTIFICATION FOR AN ORDER, NOT FOR A MAKEGOOD RESPONSE               
***************                                                                 
         NI    BITFLAG2,X'FF'-BF2SPNDG                                          
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
PDLNT110 CLI   0(R6),0                                                          
         BNE   PDLNT115                                                         
         LHI   R1,REFORDNT         NEVER TRANSMITTED                            
         BRAS  RE,SNDERROR                                                      
         B     PDLNTNO                                                          
*                                                                               
PDLNT115 CLI   0(R6),DOXMTELQ                                                   
         BE    PDLNT120                                                         
PDLNT117 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PDLNT110                                                         
*                                                                               
         USING DOXMTELD,R6                                                      
PDLNT120 CLI   DOXMTSTA,QSNTPNDG   IMMEDIATE UNLOCKED?                          
         BNE   PDLNT122                                                         
         OI    BITFLAG2,BF2SPNDG                                                
         B     PDLNT117            YES, PUT DELNOT IN NEXT XMT ELEM             
*                                                                               
PDLNT122 GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDNTTIME,DUB+L'DOXMTDND,L'RDNTTIME                   
*                                                                               
         OC    DOXMTDND,DOXMTDND   IF NO DELIVERY DATE OR TIME ALREADY          
         BZ    PDLNT130                                                         
         OC    DOXMTDNT,DOXMTDNT                                                
         BZ    PDLNT130            THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTDND,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    PDLNTNO                    YES, IGNORE THIS RECORD               
         BL    PDLNT130                   NO, OLDER                             
         CLC   DOXMTDNT,DUB+L'DOXMTDND    IS ELEM'S TIME MORE RECENT?           
         BNL   PDLNTNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
PDLNT130 MVC   DOXMTDND,DUB                                                     
         MVC   DOXMTDNT,DUB+L'DOXMTDND                                          
         MVI   SVSTAT,MNSTDELV     SINCE ORDER DSECT DOESN'T HAVE THIS          
*                                                                               
         CLI   QMED,C'R'           CURRENTLY ONLY KATZ RADIO                    
         BNE   *+14                                                             
         MVC   DOXMTDID,=X'FFFB'   AS THEY MIGHT NOT HAVE IDS ANYMORE           
         B     *+10                                                             
         MVC   DOXMTDID,DLNFRID    SAVE WHERE DELNOT CAME FROM                  
*                                                                               
         XC    ELEM,ELEM           COPY TRANSMISSION ELEMENT                    
         MVC   ELEM(DOXMTLNQ),DOXMTEL                                           
         DROP  R6                                                               
*                                                                               
         OI    MISCFLG1,MF1XMTUP   XMT HAS BEEN UPDATED                         
***************                                                                 
* TIME TO PROCESS THE STATUS HISTORY ELEMENTS                                   
***************                                                                 
PDLNT200 DS    0H                                                               
         NI    BITFLAG3,X'FF'-BF3SPNDG                                          
         XR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
PDLNT210 CLI   0(R6),0                                                          
         BNE   PDLNT215                                                         
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    PDLNTNO                    YES, THEN DON'T SHOW ERROR            
         LHI   R1,REFORDNT         NEVER TRANSMITTED                            
         BRAS  RE,SNDERROR                                                      
         B     PDLNTNO                                                          
*                                                                               
PDLNT215 CLI   0(R6),DOSPELQ                                                    
         BNE   PDLNT220                                                         
         MVC   REVISION,DOSPREVN-DOSPELD(R6)      SAVE REVISION                 
PDLNT220 CLI   0(R6),DOSTELQ                                                    
         BE    PDLNT240                                                         
PDLNT230 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PDLNT210                                                         
*                                                                               
         USING DOSTELD,R6                                                       
PDLNT240 CLI   DOSTSTAT,QSNTPNDG   IMMEDIATE UNLOCKED?                          
         BNE   PDLNT250                                                         
         OI    BITFLAG3,BF3SPNDG                                                
         B     PDLNT230            YES, SKIP THIS STATUS ELEM                   
*                                                                               
PDLNT250 CLI   DOSTSTAT,QAPP       APPROVED?                                    
         BE    PDLNT230            YES, SKIP THIS STATUS ELEM                   
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDNTTIME,DUB+L'DOSTDATE,L'RDNTTIME                   
*                                                                               
         CLI   DOSTSTAT,DDLVRD     DO WE HAVE A DELIVERY STATUS?                
         BNE   PDLNT270                                                         
*                                                                               
         CLC   DOSTDATE,DUB        IS DATE/TIME MORE RECENT?                    
         BH    PDLNTNO                                                          
         BL    PDLNT260            NO, OLDER                                    
         CLC   DOSTTIME,DUB+L'DOSTDATE                                          
         BNL   PDLNTNO             YES OR SAME, IGNORE THIS RECORD              
*                                                                               
PDLNT260 MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         CLI   QMED,C'R'           KATZ RADIO MIGHT NOT HAVE IDS                
         BE    PDLNT290            SO LEAVE WHATEVER WE HAD ALONE               
         MVC   DOSTIDNM,DLNFRID    SAVE WHERE DELNOT CAME FROM                  
         B     PDLNT290                                                         
*                                                                               
PDLNT270 TM    MISCFLG1,MF1NOXMT                                                
         BZ    PDLNT280                                                         
         XC    ELEM,ELEM                                                        
         MVC   ELEM(DOSTLNQ2),DOSTEL                                            
         DROP  R6                                                               
*                                                                               
PDLNT280 LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ2                                                 
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,DDLVRD                                                  
         MVI   SVSTAT,DDLVRD                                                    
*                                                                               
         CLI   QMED,C'R'           CURRENTLY ONLY KATZ RADIO                    
         BE    *+14                AS THEY MIGHT NOT HAVE IDS ANYMORE           
         MVC   DOSTIDNM,DLNFRID    SAVE WHERE DELNOT CAME FROM                  
         B     PDLNT289                                                         
*                                                                               
         MVI   DOSTLEN,DOSTLNQ6    ONGER LENGTH FOR REP PREFIX/OFFICE           
         LR    RE,R6                                                            
         XR    R0,R0                                                            
PDLNT282 CLI   0(RE),0             LOOK FOR DOWIG ELEM                          
         BE    PDLNT289            DON'T HAVE IT SO LEAVE BLANK FOR NOW         
         CLI   0(RE),DOWIGELQ      X'50'                                        
         BE    PDLNT288                                                         
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     PDLNT282                                                         
*                                                                               
         USING DOWIGELD,RE                                                      
PDLNT288 MVC   DOSTDRPP,DOWIGRPP                                                
         MVC   DOSTDRPO,DOWIGRPO                                                
         DROP  R2,RE                                                            
*                                                                               
PDLNT289 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
PDLNT290 MVI   SVSTAT,DDLVRD       SINCE ORDER DSECT DOESN'T HAVE THIS          
         B     PDLNT350                                                         
***************                                                                 
* DELNOT'S FOR AGY REPSONSE TO REP MAKEGOODS                                    
***************                                                                 
PDLNT300 DS    0H                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         DROP  R6                                                               
*                                                                               
         BRAS  RE,GETNOTCE         READ THE MAKEGOOD NOTICE                     
         BNE   PDLNTNO                                                          
*                                                                               
         XC    ELEM,ELEM           CREATE STATUS ELEMENT                        
         LA    R6,ELEM                                                          
         USING MNSTELD,R6                                                       
         MVI   MNSTEL,MNSTELQ                                                   
         MVI   MNSTLEN,MNSTLENQ                                                 
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,MNSTDATE)                          
         GOTO1 VHEXIN,DMCB,RDNTTIME,MNSTTIME,L'RDNTTIME                         
         MVI   MNSTSTAT,MNSTDELV   DELIVERED STATUS                             
         MVI   SVSTAT,MNSTDELV     SAVE IT FOR LATER                            
         MVI   THISISMG,C'Y'                                                    
*                                                                               
         L     R6,AIO1             LATEST FIRST                                 
         LA    R6,MNRFRST-MNKEY(R6)                                             
*                             FIRST ELEMENT SHOULD BE STATUS ELEM!!!            
PDLNT320 CLI   MNSTSTAT,MNSTCANM   IS IT A CANMORE STATUS?                      
         BE    PDLNT330                                                         
         CLI   MNSTSTAT,MNSTCAN    OR A CANCELLED STATUS?                       
         BE    PDLNT340                                                         
         CLI   MNSTSTAT,MNSTSAPP   OR SELF-APPLIED?                             
         BNE   PDLNT340                                                         
         DROP  R6                                                               
*                                  YES, THEN ADD DELNOT AFTER                   
PDLNT330 ZIC   R1,1(R6)                                                         
         AR    R6,R1               BUMP IT                                      
         B     PDLNT320            AND CHECK AGAIN                              
*                                                                               
PDLNT340 GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
         XC    ELEM,ELEM                                                        
*                                                                               
PDLNT350 XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)  MOVE REC TO KEY FOR CLARITY                  
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUT                                                           
*                                                                               
         CLC   QMGGROUP,=C'  '     DELNOT FOR MKGD?                             
         BH    PDLNTYS5                                                         
*                                                                               
         LA    R1,ELEM+DOXMTSTA-DOXMTEL       R1=A(STATUS FIELD)                
         TM    MISCFLG1,MF1NOXMT                                                
         BZ    PDLNTYES                                                         
         LA    R1,ELEM+DOSTSTAT-DOSTEL                                          
PDLNTYES CLI   0(R1),QRCLDELN      RECALLED, REP DELIVERED?                     
         BNE   PDLNTYS1                                                         
         CLI   REVISION,0          AND REVISION?                                
         BNE   PDLNTYS5            YES, SKIP COLOR AND EXTRA                    
*                                                                               
PDLNTYS1 CLI   0(R1),QNODARE       NOT DARE?                                    
         BE    PDLNTYS5            YES, DON'T CARE ABOUT CLR                    
         TM    BITFLAG2,BF2SPNDG   WAS X'11' SNTPNDING?                         
         BO    PDLNTYS5            YES, DON'T CARE ABOUT CLR                    
         TM    BITFLAG3,BF3SPNDG   WAS X'12' SNTPNDING?                         
         BO    PDLNTYS5            YES, DON'T CARE ABOUT CLR                    
         BRAS  RE,BLDCOLOR                                                      
*                                                                               
PDLNTYS5 BAS   RE,DAREMAIL                                                      
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGE KEY                   
         J     YES                                                              
*                                                                               
PDLNTNO  TM    MISCFLG1,MF1XMTUP   HAVE I MADE CHANGES TO XMT ELEM?             
         BO    PDLNT350            YES                                          
         J     NO                                                               
*                                                                               
PDLNTTBL DC    C'DLNNOT',AL1(MDLNNOTL)                                          
         DC    C'AGYXM1',AL1(PAGYXM1L)                                          
         DC    C'AGYXM2',AL1(PAGYXM2L)                                          
         DC    C'ORDTLR',AL1(RORDTLRL)                                          
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER APPROVAL                                                  
*                                                                               
* ON ENTRY:    R7                  A(ORDAPP MESSAGE IN TSAR BUFFER)             
***********************************************************************         
PROCOAPP DS    0H                                                               
         USING RORDAPPD,R7                                                      
POAPPD   USING RTN2SNDR,ROAPRTRN                                                
         GOTO1 VHEXIN,DMCB,POAPPD.RTNAGYMD,BAGYMD,2                             
*                                                                               
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         MVI   QMED,C'T'                                                        
         CLI   POAPPD.RTNAGYMD+1,C'2' AM I RADIO?                               
         BNE   POAPP80                NO, PROCESS THE APPROVAL NOW              
         OI    MISCFLG3,MF3RADIO      YES, PROCESS IT LATER                     
         MVI   QMED,C'R'                                                        
*                                                                               
POAPP10  LA    RF,POAPPTBL         T(RECORDS) PROCESSED BY APPROVAL             
         USING MSGTBLD,RF                                                       
POAPP15  CLI   MSGTBLTX,X'FF'      EOT?                                         
         BNE   POAPP20                                                          
         LHI   R1,REFTLRWR         THIS MESSAGE TYPE IS NOT KNOWN               
         BRAS  RE,SNDERROR                                                      
         B     POAPPNO                                                          
*                                                                               
POAPP20  CLC   MSGTBLTX,0(R7)                                                   
         BE    *+12                                                             
         LA    RF,MSGTBLLQ(RF)                                                  
         B     POAPP15                                                          
*                                                                               
         XR    R8,R8                                                            
         IC    R8,MSGTBLLN                                                      
         BAS   RE,PUTINIOA                                                      
         BAS   RE,PUTINWKR                                                      
*                                                                               
         CLC   =C'ORDTLR',0(R7)    WE PUT THE TRAILER OUT?                      
         BE    POAPP80             YES, PROCESS THE APPROVAL THEN               
         LA    R7,2(R7,R8)         BUMP PAST MESSAGE AND CRLF                   
         B     POAPP10                                                          
*                                                                               
POAPP80  CLC   MQMSGLEN,=AL2(4064) DID WE SWITCH TO 31-BIT?                     
         BNH   POAPP85             NO                                           
**** SWITCH OUT OF XA MODE (24-BIT ADDRESSING)                                  
         BRAS  RE,OFF31                                                         
POAPP85  GOTO1 =A(APPROVAL),DMCB,(RC),RR=RELO                                   
POAPPYES JE    YES                                                              
POAPPNO  J     NO                                                               
*                                                                               
POAPPTBL DC    C'ORDAPP',AL1(RORDAPPL)                                          
         DC    C'CANAPP',AL1(RORDAPPL)                                          
*****    DC    C'ORDSAL',AL1(RORDSALL)                                          
         DC    C'AGYXM1',AL1(PAGYXM1L)                                          
         DC    C'AGYXM2',AL1(PAGYXM2L)                                          
         DC    C'AGYXM3',AL1(PAGYXM3L)                                          
         DC    C'ORDCOM',AL1(RORDCOML)                                          
         DC    C'ORDTLR',AL1(RORDTLRL)                                          
         DC    X'FF'                                                            
*                                                                               
MSGTBLD  DSECT                                                                  
MSGTBLTX DS    CL6                 MESSAGE TYPE                                 
MSGTBLLN DS    X                   L(MESSAGE)                                   
MSGTBLLQ EQU   *-MSGTBLD                                                        
*                                                                               
T16300   CSECT                                                                  
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER REJECTION                                                 
*                                                                               
* ON ENTRY:    R7                  A(ORDREJ MESSAGE IN TSAR BUFFER)             
***********************************************************************         
PROCOREJ DS    0H                                                               
         USING RORDREJD,R7                                                      
POREJ10  LA    RF,POREJTBL         T(RECORDS) PROCESSED BY REJECTION            
         USING MSGTBLD,RF                                                       
POREJ15  CLI   MSGTBLTX,X'FF'      EOT?                                         
         BNE   POREJ20                                                          
         LHI   R1,REFTLRWR         THIS MESSAGE TYPE IS NOT KNOWN               
         BRAS  RE,SNDERROR                                                      
         B     POREJNO                                                          
*                                                                               
POREJ20  CLC   MSGTBLTX,0(R7)                                                   
         BE    *+12                                                             
         LA    RF,MSGTBLLQ(RF)                                                  
         B     POREJ15                                                          
*                                                                               
         XR    R8,R8                                                            
         IC    R8,MSGTBLLN                                                      
         BAS   RE,PUTINIOA                                                      
         BAS   RE,PUTINWKR                                                      
*                                                                               
         CLC   =C'ORDTLR',0(R7)    WE PUT THE TRAILER OUT?                      
         BE    POREJ80             YES, PROCESS THE REJECTION THEN              
         LA    R7,2(R7,R8)         BUMP PAST MESSAGE AND CRLF                   
         B     POREJ10                                                          
*                                                                               
POREJ80  GOTO1 =A(REJECT),DMCB,(RC),RR=RELO                                     
         B     POREJYES                                                         
*                                                                               
POREJYES J     YES                                                              
*                                                                               
POREJNO  J     NO                                                               
*                                                                               
POREJTBL DC    C'ORDREJ',AL1(RORDREJL)                                          
         DC    C'CANREJ',AL1(RORDREJL)                                          
*****    DC    C'ORDSAL',AL1(RORDSALL)                                          
         DC    C'AGYXM1',AL1(PAGYXM1L)                                          
         DC    C'AGYXM2',AL1(PAGYXM2L)                                          
         DC    C'AGYXM3',AL1(PAGYXM3L)                                          
         DC    C'ORDCOM',AL1(RORDCOML)                                          
         DC    C'ORDTLR',AL1(RORDTLRL)                                          
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER CONFIRMATION                                              
*                                                                               
* ON ENTRY:    R7                  A(ORDCFM MESSAGE IN TSAR BUFFER)             
***********************************************************************         
PROCOCFM DS    0H                                                               
         USING RORDCFMD,R7                                                      
         MVC   QREPCON,ROCFRPCN                                                 
         MVC   QRETURN,ROCFRTRN                                                 
*                                                                               
POCFM10  CLC   =C'ORDLIN',0(R7)    SPECIAL CODE FOR ORDLIN                      
         BE    POCFM50                                                          
*                                                                               
         LA    RF,POCFMTBL         T(RECORDS) PROCESSED BY CONFIRMATION         
         USING MSGTBLD,RF                                                       
POCFM15  CLI   MSGTBLTX,X'FF'      EOT?                                         
         BNE   POCFM20                                                          
         L     R6,AIO1                                                          
         MVC   0(132,R6),0(R7)                                                  
         LHI   R1,REFTLRWR         THIS MESSAGE TYPE IS NOT KNOWN               
         BRAS  RE,SNDERROR                                                      
         B     POCFMNO                                                          
*                                                                               
POCFM20  CLC   MSGTBLTX,0(R7)                                                   
         BE    *+12                                                             
         LA    RF,MSGTBLLQ(RF)                                                  
         B     POCFM15                                                          
*                                                                               
         XR    R8,R8                                                            
         IC    R8,MSGTBLLN                                                      
         BAS   RE,PUTINIOA                                                      
POCFM30  BAS   RE,PUTINWKR                                                      
*                                                                               
         CLC   =C'ORDTLR',0(R7)    WE PUT THE TRAILER OUT?                      
         BE    POCFM80             YES, PROCESS THE CONFIRMATION THEN           
         LA    R7,2(R7,R8)         BUMP PAST MESSAGE AND CRLF                   
         B     POCFM10                                                          
***********************************                                             
* ORDLIN RECORD HAS SPECIAL VALIDATION                                          
* PLUS MOST OF IT IS WHITESPACE SO WE'RE SHRINKING IT SO WE CAN HAVE            
*   A NUMBER OF THEM BEFORE FILLING UP WRKRIOA                                  
***********************************                                             
         USING RORDLIND,R7                                                      
POCFM50  LA    R8,RORDLINL         R8 IS STILL NEEDED FOR BUMPING               
         LA    RF,L'ROLNBLIN       MAKE SURE AGENCY BUYLINE NUMERIC             
         LA    RE,ROLNBLIN                                                      
POLIN10  CLI   0(RE),C'0'                                                       
         BL    POLIN20                                                          
         CLI   0(RE),C'9'                                                       
         BH    POLIN20                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,POLIN10                                                       
         B     POLIN30                                                          
*                                                                               
POLIN20  L     R1,AWRKRIOA                                                      
         LR    R0,R7                                                            
         LA    R7,3(R1)                                                         
         LHI   R1,REFBDABL         BAD AGENCY BUYLINE                           
         BRAS  RE,SNDERROR                                                      
         B     POCFMNO                                                          
*                                                                               
POLIN30  PACK  DUB,ROLNBLIN        AGENCY BUYLINE IS FROM 1-255                 
         CVB   R1,DUB                                                           
         LTR   R1,R1               CANNOT HAVE 0                                
         BZ    POLIN20                                                          
         CHI   R1,499              CANNOT HAVE MORE THAN 499                    
         BH    POLIN20                                                          
*                                                                               
         L     R6,AIO1             SAVE THIS FIRST                              
         MVC   1(ROLNRLIN+L'ROLNRLIN-RORDLIND,R6),0(R7)                         
         LA    R6,ROLNRLIN+L'ROLNRLIN-RORDLIND(R6)   R6 IS A(LAST CHAR)         
POLIN35  CLI   0(R6),C' '          TO COMPRESS TO AVOID OVERFLOW                
         BH    *+8                                                              
         BCT   R6,POLIN35          SAFE BECAUSE OF THE AGY BUYLINE              
*                                                                               
         LA    R1,1(R6)            BECAUSE OF THE LAST CHARACTER                
         L     R6,AIO1                                                          
         SR    R1,R6                                                            
         STC   R1,0(R6)                                                         
         B     POCFM30                                                          
***********************************                                             
* END OF ORDLIN SPECIAL VALIDATION                                              
***********************************                                             
POCFM80  CLC   MQMSGLEN,=AL2(4064) DID WE SWITCH TO 31-BIT?                     
         BNH   POCFM85             NO                                           
**** SWITCH OUT OF XA MODE (24-BIT ADDRESSING)                                  
         BRAS  RE,OFF31                                                         
POCFM85  GOTO1 =A(CONFIRM),DMCB,(RC),RR=RELO                                    
POCFMYES J     YES                                                              
POCFMNO  J     NO                                                               
*                                                                               
POCFMTBL DC    C'ORDCFM',AL1(RORDCFML)                                          
         DC    C'CANCFM',AL1(RORDCFML)                                          
         DC    C'ORDCAN',AL1(RORDCFML)                                          
*****    DC    C'ORDSAL',AL1(RORDSALL)                                          
         DC    C'AGYXM1',AL1(PAGYXM1L)                                          
         DC    C'AGYXM2',AL1(PAGYXM2L)                                          
         DC    C'AGYXM3',AL1(PAGYXM3L)                                          
         DC    C'ORDCOM',AL1(RORDCOML)                                          
****     DC    C'ORDLIN',AL1(RORDLINL)  SPECIAL CODE NEEDED                     
         DC    C'ORDURL',AL1(RORDURLL)                                          
         DC    C'ORDTLR',AL1(RORDTLRL)                                          
         DC    X'FF'                                                            
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ERROR NOTIFICATION FOR AGENCY                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCERNT DS    0H                                                               
         USING RDLNNOTD,R7                                                      
PERNTD   USING RTN2SNDR,RDNTRTRN                                                
         MVC   QREPCON,RDNTRPCN                                                 
         MVC   QRETURN,RDNTRTRN                                                 
*                                                                               
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         MVI   QMED,C'T'                                                        
         CLI   PERNTD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+12                                                             
         OI    MISCFLG3,MF3RADIO                                                
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,PERNTD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,PERNTD.RTNPWRCD                                           
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
         DROP  PERNTD                                                           
*                                                                               
         GOTO1 CALCORDR,DMCB,RDNTORDR    CONVERT ORDER NUMBER TO BINARY         
         BNE   PERNTNO                                                          
*                                                                               
         MVC   USERID,RDNTTOID                                                  
         BAS   RE,SWTCHSPT         SWTICH CONTROL TO SPOT SYSTEM                
         BNE   PERNTNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   PERNTNO                                                          
*                                                                               
         CLI   RDNTEFLG+L'RDNTEFLG,C' '   IS THIS FOR A MAKEGOOD GROUP?         
         BNH   PERNT10                    NO                                    
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         MVC   BCLT,DOIDCLT                                                     
         GOTO1 VCLUNPK,DMCB,DOIDCLT,QCLT                                        
         MVC   QPRD1,DOIDPRD                                                    
         MVC   QPRD2,DOIDPRD2                                                   
         MVC   BEST,DOIDEST                                                     
         EDIT  (B1,DOIDEST),(3,QEST1),FILL=0                                    
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         DROP  R6                                                               
         MVC   QMGGROUP,MDNTOFRI-MDLNNOTD(R7)   MAKEGOOD GROUP CODE             
*                                                                               
         BRAS  RE,GETNOTCE         READ THE MAKEGOOD NOTICE                     
         BNE   PERNTNO                                                          
*                                                                               
         XC    ELEM,ELEM           WRITE A ERROR STATUS ELEM TO REC             
         LA    R6,ELEM                                                          
         USING MNSTELD,R6                                                       
         MVI   MNSTEL,MNSTELQ                                                   
         MVI   MNSTLEN,MNSTELNQ                                                 
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,MNSTDATE)                          
         GOTO1 VHEXIN,DMCB,RDNTTIME,MNSTTIME,L'RDNTTIME                         
         MVI   MNSTSTAT,MNSTERR    ERROR STATUS                                 
         MVI   SVSTAT,MNSTERR      SAVE IT FOR LATER                            
         MVI   THISISMG,C'Y'                                                    
*****                                                                           
         CLC   =C'ACK',RDNTEFLG    ACKNOWLEDGEMENT TYPE?                        
         BE    PERNTNO             NOTHING TO MARK UNFORTUNATELY                
*****                                                                           
         PACK  DUB,RDNTEFLG                                                     
         CVB   R1,DUB                                                           
         STCM  R1,3,MNSTERRN       SAVE ERROR NUMBER                            
         DROP  R6                                                               
*                                                                               
         L     R6,AIO1             LOOK FOR LAST STATUS                         
         LA    R6,MNRFRST-MNKEY(R6)                                             
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
         B     PERNTWRT            AND WRITE THE RECORD BACK OUT                
*                                                                               
PERNT10  L     R6,AIO1             REMOVE ANY OLD ERROR COMMENTS                
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
PERNT13  CLI   0(R6),0                                                          
         BE    PERNT20                                                          
         CLI   0(R6),DOCOMELQ                                                   
         BE    PERNT16                                                          
         CLI   1(R6),1                                                          
         BH    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PERNT13                                                          
PERNT16  GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6)                                     
         B     PERNT10                                                          
*                                                                               
PERNT20  LA    R2,ELEM             ADD THE ERROR COMMENT TO RECORD              
         USING DOCOMELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCOMEL,DOCOMELQ                                                 
         MVI   DOCOMLEN,DOCOMOVH+L'RDNTEFLG                                     
         MVC   DOCOMTXT(3),RDNTEFLG      COPY THE NUMBER FOR NOW                
         CLC   RDNTEFLG,=C'   '    DO WE HAVE AN ERROR CODE?                    
         BNE   PERNT29                                                          
         LA    RE,RDNTEFLG+3                                                    
         LR    R0,RE                                                            
         LA    RF,150              MAXIMUM OF 150 BYTES                         
PERNT22  CLC   =X'0D25',0(RE)      HIT CRLF?                                    
         BE    PERNT24                                                          
         CLI   0(RE),0              OR NULL?                                    
         BE    PERNT24             THEN SAVE THE MESSAGE                        
         LA    RE,1(RE)                                                         
         BCT   RF,PERNT22                                                       
PERNT24  SR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DOCOMTXT+3(0),RDNTEFLG+3                                         
         AHI   RE,1+3+DOCOMOVH     1 FOR BCTR, 3 CODE, AND OVERHEAD             
         STC   RE,DOCOMLEN                                                      
*                                                                               
PERNT29  GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
PERNT30  CLI   0(R6),0                                                          
         BE    PERNTNO                                                          
*                                                                               
PERNT35  CLI   0(R6),DOXMTELQ                                                   
         BE    PERNT40                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PERNT30                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
PERNT40  CLI   DOXMTSTA,QCFMD      ERROR ON A CONFIRMED ORDER?                  
         BE    PERNTNO             NOT GOING TO HAVE ERRNOT ON A ERRNOT         
         CLI   DOXMTSTA,QRJCT      ERROR ON AN REJECTED ORDER?                  
         BE    PERNTNO                                                          
         CLI   DOXMTSTA,QUNDARE    ERROR ON AN UNDARED ORDER?                   
         BE    PERNTNO                                                          
         CLI   DOXMTSTA,QNODARE    ERROR ON AN NOTDARED ORDER?                  
         BE    PERNTNO                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,DOXMTSTD)                          
         GOTO1 VHEXIN,DMCB,RDNTTIME,DOXMTSTT,L'RDNTTIME                         
         MVI   DOXMTSTA,QERRORED      ORDER IS IN ERROR                         
         MVI   SVSTAT,QERRORED                                                  
         DROP  R6                                                               
*                                                                               
PERNT99  OI    MISCFLG1,MF1XMTUP                                                
***************                                                                 
* TIME TO PROCESS STATUS HISTORY ELEMENT                                        
***************                                                                 
PERNT100 SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
PERNT110 CLI   0(R6),0                                                          
         BE    PERNTNO                    YES, THEN DON'T GIVE ERROR            
PERNT115 CLI   0(R6),DOSTELQ                                                    
         BE    PERNT120                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PERNT110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
PERNT120 CLI   DOSTSTAT,QCFMD      ERROR ON A CONFIRMED ORDER?                  
         BE    PERNTNO             NOT GOING TO HAVE ERRNOT ON A ERRNOT         
         CLI   DOSTSTAT,QRJCT      ERROR ON A REJECTED ORDER?                   
         BE    PERNTNO                                                          
         CLI   DOSTSTAT,QUNDARE    ERROR ON A UNDARED ORDER?                    
         BE    PERNTNO                                                          
         CLI   DOSTSTAT,QNODARE    ERROR ON A NOTDARED ORDER?                   
         BE    PERNTNO                                                          
         DROP  R6                                                               
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDNTTIME,DUB+L'DOSTDATE,L'RDNTTIME                   
*                                                                               
         LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QERRORED                                                
         MVI   SVSTAT,QERRORED                                                  
         DROP  R2                                                               
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
PERNT130 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    PERNT140         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    PERNT140                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   PERNT140                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PERNT130                                                         
*                                                                               
PERNT140 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
PERNTWRT XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
PERNTYES BRAS  RE,BLDCOLOR                                                      
         BAS   RE,DAREMAIL                                                      
         J     YES                                                              
*                                                                               
PERNTNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   PERNTWRT                                                         
         J     NO                                                               
***********************************************************************         
* PROCESSES THE ORDER RECALL ACKNOWLEDGEMENT                                    
*                                                                               
* ON ENTRY:    R7                  A(ORDRCL MESSAGE IN TSAR BUFFER)             
***********************************************************************         
PROCORAK DS    0H                                                               
         USING RORDRCLD,R7                                                      
RECLLD   USING RTN2SNDR,RORCRTRN                                                
         GOTO1 VHEXIN,DMCB,RECLLD.RTNAGYMD,BAGYMD,2                             
*                                                                               
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         MVI   QMED,C'T'                                                        
         CLI   RECLLD.RTNAGYMD+1,C'2' AM I RADIO?                               
         BNE   RECLL80                NO, PROCESS THE RECALL ACK                
         OI    MISCFLG3,MF3RADIO      YES, PROCESS IT LATER                     
         MVI   QMED,C'R'                                                        
*                                                                               
RECLL10  LA    RF,RECLLTBL         T(RECORDS) PROCESSED BY RECALL ACK           
         USING MSGTBLD,RF                                                       
RECLL15  CLI   MSGTBLTX,X'FF'      EOT?                                         
         BNE   RECLL20                                                          
         LHI   R1,REFTLRWR         THIS MESSAGE TYPE IS NOT KNOWN               
         BRAS  RE,SNDERROR                                                      
         B     RECLLNO                                                          
*                                                                               
RECLL20  CLC   MSGTBLTX,0(R7)                                                   
         BE    *+12                                                             
         LA    RF,MSGTBLLQ(RF)                                                  
         B     RECLL15                                                          
*                                                                               
         XR    R8,R8                                                            
         IC    R8,MSGTBLLN                                                      
         BAS   RE,PUTINIOA                                                      
         BAS   RE,PUTINWKR                                                      
*                                                                               
         CLC   =C'ORDTLR',0(R7)    WE PUT THE TRAILER OUT?                      
         BE    RECLL80             YES, PROCESS THE APPROVAL THEN               
         LA    R7,2(R7,R8)         BUMP PAST MESSAGE AND CRLF                   
         B     RECLL10                                                          
*                                                                               
RECLL80  CLC   MQMSGLEN,=AL2(4064) DID WE SWITCH TO 31-BIT?                     
         BNH   RECLL85             NO                                           
**** SWITCH OUT OF XA MODE (24-BIT ADDRESSING)                                  
         BRAS  RE,OFF31                                                         
RECLL85  BRAS  RE,RCLAKNWL                                                      
         DROP  R7                                                               
RECLLNO  JNE   NO                                                               
RECLLYES J     YES                                                              
*                                                                               
RECLLTBL DC    C'ORDRCL',AL1(RORCAPPL)                                          
*****    DC    C'ORDSAL',AL1(RORDSALL)                                          
         DC    C'AGYXM1',AL1(PAGYXM1L)                                          
         DC    C'AGYXM2',AL1(PAGYXM2L)                                          
         DC    C'AGYXM3',AL1(PAGYXM3L)                                          
         DC    C'ORDTLR',AL1(RORDTLRL)                                          
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE DARE FAX DELIVERY NOTIFICATION                                  
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCDFAX DS    0H                                                               
         BRAS  RE,DAREDLFX                                                      
         JNE   NO                                                               
         J     YES                                                              
***********************************************************************         
* PROCESSES THE DARE FAX CANCELLATION                                           
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCCNFX DS    0H                                                               
         BRAS  RE,DARECNFX                                                      
         JNE   NO                                                               
         J     YES                                                              
***********************************************************************         
* PROCESSES THE MAKEGOOD HEADER                                                 
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCMKGX DS    0H                                                               
         BRAS  RE,DAREMKGX                                                      
         JNE   NO                                                               
         J     YES                                                              
***********************************************************************         
* PROCESSES THE DARE FAX ERROR                                                  
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCERFX DS    0H                                                               
         BRAS  RE,DAREERFX                                                      
         JNE   NO                                                               
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SWITCHES CONTROL TO THE CORRECT SPOT SYSTEM                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
*              USERID              SET TO ID OF SPOT AGENCY                     
*                                                                               
* ON EXIT:     SPTSENUM            AGENCY'S SPOT SENUM                          
*              DLNFRID             ID OF WHERE DLNNOT CAME FROM (REP)           
*              ROUTNGCD            ROUTING CODE                                 
***********************************************************************         
SWTCHSPT NTR1                                                                   
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         JNE   EXITPRG             EXIT PROGRAM, SHOULD WAIT FOR IT             
*                                                                               
         XC    KEY,KEY             LOOK FOR THE USER ID RECORD                  
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,USERID                                                    
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,HIGHCT                                                        
*                                                                               
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
*                                                                               
         CLC   CTIKEY,KEY          ERR IF WE CAN'T FIND THE ID REC              
         BE    SWSPT00                                                          
SWSPTERR CLC   =C'ERRNOT',0(R7)        ERROR ON ERRNOT?                         
         BE    SWSPTNO                                                          
         LHI   R1,REFAIDNV             AGY ID NOT VALID                         
         BRAS  RE,SNDERROR                                                      
         B     SWSPTNO                                                          
*                                                                               
SWSPT00  SR    R0,R0                                                            
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,2            DESCRIPTION ELEM FOR ID NUM                  
         BRAS  RE,FIRSTEL                                                       
         BNE   SWSPTERR                                                         
         MVC   SIGNON2H,2(R6)      SAVE ID NUM OF AGENCY ID                     
*                                                                               
         NI    BITFLAG1,X'FF'-BF1PSSWD   PASSWORD NOT REQUIRED YET              
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,7                                                         
         BRAS  RE,FIRSTEL                                                       
         BNE   SWSPT05             NOT REQUIRED IF NO X'07' ELEM                
         TM    2(R6),X'80'         PASSWORD REQUIRED?                           
         BZ    *+8                                                              
         OI    BITFLAG1,BF1PSSWD                                                
*                                                                               
SWSPT05  L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,FIRSTEL                                                       
         BNE   SWSPTERR            ERROR IF WE CAN'T FIND SPOT SYS ELEM         
         USING CTSYSD,R6                                                        
SWSPT10  CLI   CTSYSNUM,2                                                       
         BE    SWSPT15                                                          
         BRAS  RE,NEXTEL                                                        
         BE    SWSPT10                                                          
         B     SWSPTERR                                                         
*                                                                               
SWSPT15  MVC   SPTSENUM,CTSYSSE                                                 
         DROP  R6                                                               
***************                                                                 
* GET THE ROUTING CODE                                                          
***************                                                                 
         XC    ROUTNGCD,ROUTNGCD                                                
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'33'        US AGENCY EXTRA INFO ELEMENT                 
         BRAS  RE,FIRSTEL                                                       
         BNE   SWSPTERR            ERROR IF WE CAN'T FIND ELEM                  
         USING CTUSAD,R6                                                        
         MVC   ROUTNGCD,CTUSADRC                                                
****     CLC   =C'SJ',ROUTNGCD                                                  
****     BNE   *+8                                                              
****     MVI   ROUTNGCD+2,C'R'     FROM 'SJ NY'  TO 'SJRNY'                     
         DROP  R6                                                               
***************                                                                 
* SPECIAL CODE FOR MAKEGOOD REP OK                                              
***************                                                                 
SWSPT20  CLC   =C'MKGROK',0(R7)    PROCESSING FOR A MAKEGOOD REP OK?            
         BNE   SWSPT30                                                          
*                                                                               
         TM    BITFLAG1,BF1PSSWD   YES, PASSWORD IS REQUIRED                    
         BNZ   SWSPT60                  YES, WE KNOW ALREADY                    
         L     R6,AIO1                  NO, FIND OUT IF IT IS ELSEWHERE         
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,7            SEE SRCON00 LABEL VALSYS4 (IDOPTS)           
         BRAS  RE,FIRSTEL                                                       
         BNE   SWSPT60                                                          
         TM    2(R6),X'80'         USER ID REQUIRES PASSWORD?                   
         BZ    *+12                                                             
         OI    BITFLAG1,BF1PSSWD   YES, PASSWORD IS REQUIRED                    
         B     SWSPT60                                                          
         DROP  R6                                                               
***************                                                                 
* SPECIAL CODE FOR DELIVERY NOTIFICATIONS                                       
***************                                                                 
SWSPT30  CLC   =C'DLNNOT',0(R7)    PROCESSING FOR A DELIVERY NOTICE?            
         BE    *+14                                                             
         CLC   =C'DLNFAX',0(R7)                     OR DELIVERY FAX?            
         BNE   SWSPT60             NEITHER, WHO CARES ABOUT ID OF SNDR          
         USING RDLNNOTD,R7                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,RDNTFRID                                                  
         DROP  R4,R7                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,HIGHCT                                                        
*                                                                               
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         CLC   CTIKEY,KEY                                                       
         BE    SWSPT45                                                          
         CLC   =C'DLNNOT',0(R7)    ON DELIVERY NOTIFICATION?                    
         BNE   SWSPT40                                                          
         MVC   DLNFRID,=X'FFFB'    YES, KATZ RADIO MIGHT NOT HAVE IDS           
         B     SWSPT60                                                          
*                                                                               
SWSPT40  CLC   =C'DLNFAX',0(R7)    ON DELIVERY FAX?                             
         BNE   SWSPTNO                                                          
         MVC   DLNFRID,=X'FFFF'    LEAVE DESTID AS FAX                          
         B     SWSPT60                                                          
*                                                                               
SWSPT45  SR    R0,R0                                                            
         LA    R6,CTIDATA                                                       
SWSPT50  CLI   0(R6),0                                                          
         BNE   SWSPT55                                                          
         LHI   R1,REFRIDNV           SENDER ID NOT VALID                        
         BRAS  RE,SNDERROR                                                      
         B     SWSPTNO                                                          
*                                                                               
SWSPT55  CLI   0(R6),2             DESCRIPTION ELEM FOR ID NUM                  
         BE    SWSPT59                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SWSPT50                                                          
*                                                                               
SWSPT59  MVC   DLNFRID,2(R6)       SAVE ID NUM OF DESTINATION ID                
         DROP  R6                                                               
*                                                                               
SWSPT60  MVC   DMCB(1),SPTSENUM    SWITCH TO SPOT SYSTEM                        
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BE    SWSPT70                                                          
         CLI   DMCB+4,2            SWITCHED, BUT SYSTEM NOT OPENED?             
         JE    EXITPRG             YES, SHOULD WAIT UNTIL OPENED                
         B     SWSPTERR            USER NOT AUTHORIZED                          
*                                                                               
SWSPT70  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'AGYKEY),KEYSAVE                                            
         BNE   SWSPTYES                                                         
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO1                                                          
         USING AGYKEY,R6                                                        
         MVC   SVAPRF07,AGYPROF+07   SAVE AGY PROFILE BYTE #8 (OFF 7)           
         DROP  R6                                                               
*                                                                               
SWSPTYES J     YES                                                              
*                                                                               
SWSPTNO  J     NO                                                               
         EJECT                                                                  
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
* PUTS AN ENTRY INTO THE DARE MAIL STORAGE AREA IN THE SSB                      
***********************************************************************         
DAREMAIL NTR1                                                                   
         L     RE,VSSB                                                          
         L     RE,SSBDARTB-SSBD(RE)                                             
*                                                                               
         LA    R1,DAREXAGY                                                      
DMAIL00  CLI   0(R1),0             ANY AGENCIES LEFT IN TABLE?                  
         BE    DMAIL10             NONE                                         
         CLC   AGENCY,0(R1)                                                     
         BE    DMAILX              AGENCY DOESN'T WANT DARE MAIL                
         LA    R1,L'DAREXAGY(R1)                                                
         B     DMAIL00                                                          
*                                                                               
DMAIL10  CLC   0(4,RE),=X'FFFFFFFF'                                             
         BE    DMAILX              NO ROOM LEFT                                 
         CLC   SIGNON2H,0(RE)      USER ID MATCH?                               
         BE    DMAIL20             YES                                          
         OC    0(4,RE),0(RE)       EMPTY SLOT?                                  
         BZ    DMAIL20             YES                                          
         LA    RE,4(RE)                                                         
         B     DMAIL10                                                          
*                                                                               
DMAIL20  MVC   0(2,RE),SIGNON2H                                                 
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B         ADDJUST FOR DDS TIME                         
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
         MVO   FULL,PACKOF4B       PWOS HHMM??                                  
         MVC   2(2,RE),FULL                                                     
***      BAS   RE,SETDAR           SETS THE UTL'S AND STUFF                     
*                                                                               
DMAILX   J     XIT                                                              
*                                                                               
DAREXAGY DS    0CL2                AGENCIES THAT DON'T WANT DAREMAIL            
         DC    C'WI'                                                            
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* THIS GETS THE CLIENT SO WE CAN LOOK UP THE PRODUCT CODES                      
***********************************************************************         
GETCLTRC NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLTHDRD,R4                                                       
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BNE   GCRECNO                                                          
* READ CLIENT RECORD                                                            
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
GCRECYES J     YES                                                              
*                                                                               
GCRECNO  J     NO                                                               
         SPACE 2                                                                
***********************************************************************         
* THIS GETS THE EBCDIC PRODUCT CODE FOR THE BINARY PRODUCT CODE                 
*                                                                               
* ON ENTRY:    PARAM 1  BYTE 0     BINARY PRODUCT CODE                          
*                       BYTES 1-3  A(EBCDIC PRODUCT CODE)                       
***********************************************************************         
GETQPRD  NTR1                                                                   
         CLI   0(R1),0             NO BINARY CODE?                              
         BNE   GQPRD00                                                          
*                                                                               
         L     RF,0(R1)                                                         
         LA    RF,0(RF)                                                         
         XC    0(3,RF),0(RF)       THEN CLEAR OUT THIS FIELD                    
         B     GQPRDX                                                           
*                                                                               
GQPRD00  L     R6,AIO1             ELSE SEARCH THROUGH CLIENT PRD LIST          
         USING CLTHDRD,R6                                                       
         LA    RE,CLIST                                                         
GQPRD10  OC    0(3,RE),0(RE)                                                    
         BNZ   GQPRD15                                                          
         LHI   R1,*-T16300         NO SUCH PRODUCT!!                            
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
GQPRD15  CLC   3(1,RE),0(R1)                                                    
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     GQPRD10                                                          
*                                                                               
         L     RF,0(R1)                                                         
         LA    RF,0(RF)                                                         
         MVC   0(3,RF),0(RE)                                                    
*                                                                               
GQPRDX   J     XIT                                                              
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
         LA    RF,L'RDNTORDR                                                    
CORDR10  CLI   0(RE),C'0'                                                       
         BL    CORDR20                                                          
         CLI   0(RE),C'9'                                                       
         BH    CORDR20                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,CORDR10                                                       
         B     CORDR30                                                          
*********                                                                       
* EMERGENCY CODE PUT IN AS SBS DID NOT HANDLE DDS 2010 ORDERS THAT              
*    USE LEADING ZEROS                                                          
*********                                                                       
CORDR20  CLI   0(RE),C' '          WE HAVE TRAILING SPACES?                     
         BE    *+12                      OR NULLS?                              
         CLI   0(RE),0                                                          
         BNE   CORDRNO                                                          
*                                                                               
         LA    R0,L'RDNTORDR       R0 = L'ORDER # AS IT CAME IN                 
         SR    R0,RF                                                            
         L     R1,SRPARMSD.SRQACOMF                                             
         L     RF,CCASHVAL-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,(X'80',(R2)),(R0)                                      
         CLI   DMCB,0              VALID NUMBER?                                
         BNE   CORDRNO                                                          
*                                                                               
         ZAP   DUB,DMCB+4(8)      RESULT IS IN PENNIES                          
         SRP   DUB,64-2,0         ORDER # IS NOT IN PENNIES, SO /100            
         OI    DUB+7,X'0F'                                                      
         UNPK  0(8,R2),DUB        REFORMAT TO 8 DIGITS W/ LEADING 0'S           
*                                                                               
*********                                                                       
CORDR30  CLI   1(R2),C'3'         IS 2ND DIGIT HIGHER THAN A 3?                 
         BNH   CORDR50                                                          
         PACK  DUB,0(8,R2)         NEW STYLE ORDER NUMBER                       
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,BINORDER                                                   
         OI    BINORDER,X'80'                                                   
         XC    BINORDER,=4X'FF'                                                 
         B     CORDRYES                                                         
*                                                                               
CORDR50  GOTO1 VHEXIN,DMCB,(R2),BINORDER,L'RDNTORDR                             
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
         XC    BINORDDT,=X'FFFF'                                                
*                                                                               
         PACK  DUB,4(4,R2)         SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=X'FFFF'                                                
*                                                                               
CORDRYES J     YES                                                              
*                                                                               
CORDRNO  CLC   =C'ERRNOT',0(R7)                                                 
         JE    NO                                                               
         LHI   R1,REFBDORD     BAD ORDER NUMBER                                 
         BRAS  RE,SNDERROR                                                      
         J     NO                                                               
         EJECT                                                                  
***********************************************************************         
* COMMON EXIT POINTS                                                            
***********************************************************************         
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
REPFILE  DC    C'REPFILE'                                                       
SPTDIR   DC    C'SPTDIR'                                                        
REPDIR   DC    C'REPDIR'                                                        
CTFILE   DC    C'CTFILE'                                                        
STATION  DC    C'STATION'                                                       
XSPDIR   DC    CL8'XSPDIR'                                                      
         EJECT                                                                  
***********************************************************************         
* OPENS A NEW WORKER FILE FOR CREATE                                            
***********************************************************************         
WRKRCREA NTR1                                                                   
         L     R4,AIO2                                                          
         XC    0(256,R4),0(R4)     CLEAR SFH                                    
         USING WLHDRD,R4                                                        
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,SIGNON2H                                                 
         MVC   WLSYSPRG,=C'DAR'                                                 
         MVC   WLSUBPRG,SYSN1       *** FACPAK ID                               
         MVI   WLDAY,0                                                          
*                                                                               
         MVI   WLCLASS,C'T'        CLASS 'T'                                    
         MVI   WLTYPE,C'A'         TYPE A FOR IMMEDIATE EXECUTION               
         MVI   WLATTB,WLATOBJ      SET OBJECT CODED DATA FLAG                   
         MVC   WLDESC,=CL16' '     FILL IN DESC '$MAD LUIDLUID   '              
         MVC   WLDESC(4),=C'$DAR'                                               
         L     RF,AUTL                                                          
         USING UTLD,RF                                                          
         MVC   WLDESC+5(8),TSYM                                                 
         DROP  RF                                                               
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         MVC   WRKFILNO,WLREPRNO   EXTRACT FILE NUMBER                          
*                                                                               
WCREAX   J     YES                                                              
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* SEND DIRECTLY TO WORKER FILE                                                  
*                                                                               
* ON ENTRY:    PARAM 1             A(RECORD TO BE SENT)                         
***********************************************************************         
WRKRSEND NTR1                                                                   
         L     R4,0(R1)                                                         
*                                                                               
         LA    R3,ELEM             FILL INDEX KEY WITH USER ID                  
         USING UKRECD,R3                                                        
         XC    ELEM,ELEM                                                        
         MVC   UKUSRID,SIGNON2H    OF THE AGENCY                                
         DROP  R3                                                               
*                                  CALL DATAMGR TO CREATE FILE                  
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',=C'WRKFILE',(R3),(R4),AWRKRBUF         
         CLI   8(R1),0                                                          
         JE    YES                                                              
         LHI   R1,*-T16300         WRKR FILE FULL???? MOST LIKELY               
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
         SPACE 2                                                                
***********************************************************************         
* CLOSE THE WORKER FILE                                                         
***********************************************************************         
WRKRCLOS NTR1                                                                   
         L     R4,AIO2                                                          
         USING WLHDRD,R4                                                        
         XC    0(WLSOFEND-WLHDRD,R4),0(R4)                                      
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         J     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD XPOT KEY FOR CHECK FOR CHANGE SUPPORT                                     
* ON ENTRY : AIO1      DARE ORDER RECORD                                        
******       AGYMD     AGENCY/MEDIA                                             
******       BUYER     BUYER CODE                                               
******       BINORDER  BINARY ORDER#                                            
*            QMGGROUP  GROUP CODE                                               
*            SVORDDA   ORDER RECORD D/A                                         
*            SVMKNDA   NOTICE RECORD D/A                                        
*                                                                               
*  ON EXIT : XSPOT KEYS ADDED                                                   
***********************************************************************         
CHKFRCHG NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
CFCD     USING MNKEY,KEY                                                        
         MVI   CFCD.MNDKTYPE,MNDKTYPQ       X'0D'                               
         MVI   CFCD.MNDKSTYP,MNDKSTYQ       X'BC'                               
*                                                                               
         MVC   CFCD.MNDKAGMD,BAGYMD         AGENCY MEDIA                        
         MVC   CFCD.MNDKORDR,BINORDER       ORDER NUMBER                        
         MVC   CFCD.MNDKBYR,QBUYER          BUYER    (FROM GETORDER)            
         MVC   CFCD.MNDKCLT,BCLT            CLIENT                              
         MVC   CFCD.MNDKPRD,BPRD            PRODUCT                             
         MVC   CFCD.MNDKEST,BEST            ESTIMATE                            
         MVC   CFCD.MNDKMKT,BMKTSTA         MARKET   (FROM GETORDER)            
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(2,CFCD.MNDKDATE)                             
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    CFC010                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTOR VDATCON,DMCB,(5,0),(0,DUB)   THE TIME                            
         GOTOR VADDAY,DMCB,DUB,DUB,F'1'                                         
         GOTOR VDATCON,DMCB,(0,DUB),(2,CFCD.MNDKDATE)                           
*                                                                               
CFC010   TIME  TU                                                               
         STCM  R0,15,CFCD.MNDKTIME                                              
         XC    CFCD.MNDKTIME,=X'FFFFFFFF'                                       
*                                                                               
         MVC   KEY+36(4),SVORDDA   D/A OF ORDER RECORD (FROM GETORDER)          
         BRAS  RE,ADDXKEY                                                       
*                                                                               
         CLC   QMGGROUP,=C'   '    NO MAKEGOOD, SO WE'RE DONE                   
         JNH   XIT                                                              
         MVC   CFCD.MNDKGPCD,QMGGROUP       GROUP CODE                          
         MVC   KEY+36(4),SVMKNDA   D/A OF MKGD NOTICE REC (GETNOTCE)            
         BRAS  RE,ADDXKEY                                                       
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PUTS OUT A MESSAGE TO THE CONSOLE AND HARDCOPY                                
*                                                                               
* ON ENTRY:    WHCHEDCT            ON WHAT EDICT FILE PROBLEM OCCURED           
*              EDCTFDSK            ADDRESS OF THE EDICT RECORD                  
*              PARAM  1            OFFSET FROM BEGINNING OF PROGRAM             
***********************************************************************         
WTOMSG   NTR1  BASE=*,LABEL=*                                                   
         XC    MSG2,MSG2                                                        
         MVC   MSG2(4),DMCB        COPY OFFSET FROM BEG OF PROGRAM              
         MVC   MSG2+4(4),EDCTFDSK                                               
         MVC   MSG2+7(1),RECNUM                                                 
         MVC   MSG1SYS,WHCHEDCT                                                 
         GOTO1 VHEXOUT,DMCB,MSG2+4,MSG1ADDR,4                                   
         GOTO1 VHEXOUT,DMCB,MSG2,MSG1DISP,4                                     
*                                                                               
         XC    MSG2,MSG2                                                        
         MVC   MSG2,0(R7)                                                       
         XR    R0,R0                                                            
         WTO   TEXT=((MSGSTARL,D),(MSGPROBL,D),(MSGSTARL,D),(MSG1L,D), X        
               (MSG2L,D),(0,E)),DESC=2                                          
***********************                                                         
***********************                                                         
         ZIC   R1,RECNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,RECNUM                                                        
*                                                                               
         CH    R1,EDCTRPBQ         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   WTO30               YES, NO PROBLEM                              
*                                                                               
         ZIC   R2,EDCTFDSK+2       NO                                           
         LA    R2,1(R2)                                                         
         MVI   RECSKIP,0                                                        
         OI    BITFLAG1,BF1SKPTB   SKIP TRACK/BLOCK READ                        
*                                                                               
         CH    R2,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   WTO20               YES                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,EDCTFDSK                                                    
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST       ANY MORE TRACKS?                             
         BH    WTO30               NO MORE, UPDATE AS WELL                      
         STCM  R2,3,EDCTFDSK       TRACK NUMBER                                 
*                                                                               
WTO10    LA    R2,1                                                             
WTO20    STC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         MVI   EDCTFDSK+3,0                                                     
*                                                                               
WTO30    DS    0H                                                               
         DC    H'0'                SO WE GET AN IMAGE                           
         LTORG                                                                  
MSGSTARL DC    H'80'                                                            
         DC    80C'*'                                                           
MSGPROBL DC    H'80'                                                            
         DC    CL49'**$DAR PROBLEM**  PLEASE CONTACT  WHOA AT EXT5324'          
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
         LHI   R1,*-T16300         GOT A STAPACK ERROR                          
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
***********************************************************************         
* SENDS A DXX NOTIFICATION TO UNDO SELF APPLY                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD)                              
*                                                                               
***********************************************************************         
SENDDXX  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,ASPLAREA                                                      
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         L     R8,ASPLAREA                                                      
         USING SPOOLD,R8                                                        
         MVC   SPOOLID,=C'DAR'                                                  
         MVI   USERLANG,0          ENGLISH                                      
         MVC   SPOOLDM,VDATAMGR                                                 
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,SRPARMSD.SRQACOMF                                       
         MVC   SPOOLBUF,SRPARMSD.SRQATIA                                        
*                                                                               
         LA    R2,SPOOLKEY                                                      
         USING PQPLD,R2                                                         
         MVI   PLCLASS,C'Z'        ERROR REPORT STAYS HERE !                    
         MVC   PLSUBID,=C'DXX'                                                  
         MVC   PLUSER,=X'0011'     SET TO SJR                                   
         MVC   PLDESC(11),=CL11'*MKGD ERR*'                                     
*                                                                               
         XC    P1,P1               OPEN PRINTQ ENTRY                            
         BAS   RE,SNDDPRNT                                                      
         B     SNDD10                                                           
*                                                                               
SNDDPRNT LR    R0,RE                                                            
         GOTO1 ASPOOL,DMCB,(R8)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SNDD10   MVC   SPOOLRPN,PLREPNO                                                 
         MVI   PLCC,0                                                           
*                                                                               
         MVC   P1+26(27),=C'** MAKEGOOD ERROR REPORT **'                        
         BAS   RE,SNDDPRNT                                                      
         MVI   P1+26,C'-'                                                       
         MVC   P1+27(26),P1+26                                                  
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         XC    P,P                                                              
         BAS   RE,SNDDPRNT         SKIP A LINE                                  
         MVC   P(8),RDNTTOID-RDLNNOTD(R7)  AGENCY ID                            
         MVC   P+9(40),=C'SELF APPLIED MAKEGOOD HAS BEEN CANCELLED'             
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P+4(5),=C'MEDIA'                                                 
         MVC   P+14(L'QMED),QMED                                                
         MVC   P+46(5),=C'BUYER'                                                
         MVC   P+54(L'QBUYER),QBUYER                                            
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P+4(6),=C'CLIENT'                                                
         MVC   P+14(L'QCLT),QCLT                                                
         MVC   P+46(7),=C'PRODUCT'                                              
         MVC   P+54(L'QPRD1),QPRD1                                              
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P+4(8),=C'ESTIMATE'                                              
         MVC   P+14(L'QEST1),QEST1                                              
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P+4(7),=C'STATION'                                               
         MVC   P+14(L'QSTA),QSTA                                                
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P+4(4),=C'MGA='                                                  
         MVC   P+8(L'QMNUMCD),QMNUMCD                                           
         MVC   P+46(5),=C'ORDER'                                                
         MVC   P+52(8),6(R7)                                                    
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P(26),=CL26'*** END OF DDS MESSAGE ***'                          
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         BAS   RE,SNDDPRNT                                                      
         J     XIT                                                              
         DROP  R2,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SENDS AN ERROR NOTIFICATION OUT                                               
*                                                                               
* ON ENTRY:    R7                  A(DARE MESSAGE), BUT ATBUFF ALSO             
*              R1 HAS ERROR NUMBER (X'80'=DON'T REVERSE SNDR/RCVR)              
*                                                                               
* **NOTE** WE WILL NOT SEND ERRNOT FOR DLNNOT                                   
***********************************************************************         
SNDERROR NTR1  BASE=*,LABEL=*                                                   
         ST    R1,FULL             SAVE ERROR NUMBER                            
*                                                                               
         L     RE,ASPLAREA         PUT MSG TO BUFFER FOR MQPUT                  
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
         L     R3,ASPLAREA         USE ASPLAREA AS ERRNOT IS SMALL              
         MVC   0(16,R3),=CL16'RADIOEDIXML*****'                                 
         LA    R3,16(R3)                                                        
*                                                                               
         L     R7,ATBUFF           USE TBUFF FOR ERROR CREATION                 
         CLC   MQMSGLEN,=AL2(4064) IN 31-BIT MODE (MSG TOO LONG)?               
         BNH   SNDE05                                                           
**** SET 31-BIT ADDRESSING                                                      
         BRAS  RE,ON31             YES, WE ARE                                  
*****                                                                           
SNDE05   AHI   R7,8                BUMP PAST THE "REDIXML "                     
*****                                                                           
         CLC   =C'DLNNOT',0(R7)                                                 
         BE    SNDEX               SKIP ERRORS FOR DLNNOTS                      
*****                                                                           
*                                                                               
         USING RDLNNOTD,R3                                                      
         MVC   RDNTTID,=C'ERRNOT'  ERROR NOTIFICATION                           
         MVC   RDNTORDR,6(R7)      ORDER # IS ALWAYS 6 BYTES FROM BEG.          
         MVC   RDNTFRID,RDNTTOID-RDLNNOTD(R7)   SWAP TO/FROM                    
         MVC   RDNTTOID,RDNTFRID-RDLNNOTD(R7)                                   
*                                                                               
         TM    FULL,X'80'                       DON'T SWAP SNDR/RCVR?           
         BZ    *+16                                                             
         MVC   RDNTFRID,RDNTFRID-RDLNNOTD(R7)   DON'T SWAP                      
         MVC   RDNTTOID,RDNTTOID-RDLNNOTD(R7)                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(X'20',RDNTDATE)                              
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,DUB                                                           
         AP    PACKOF4B,DUB(4)                                                  
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    SNDE10                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,RDNTDATE,(X'20',RDNTDATE),F'1'                       
*                                                                               
SNDE10   ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,RDNTTIME,L'HALF                                
*                                                                               
         MVC   RDNTRPCN,QREPCON                                                 
         MVC   RDNTRTRN,QRETURN                                                 
         EDIT  (B3,FULL+1),(3,RDNTEFLG),FILL=0                                  
*                                                                               
         CLC   =C'MKG',0(R7)       ERROR FOR MAKEGOOD?                          
         BE    SNDE30                                                           
         CLC   =C'VARHDR',0(R7)    ERROR FOR VARIOUS ORDER?                     
         BE    SNDE20                                                           
         CLC   =C'AGY',0(R7)       ERROR FOR AGY MESSAGE?                       
         BNE   SNDE24                                                           
         CLC   =C'AGYHDR',0(R7)                                                 
         BNE   SNDE22                                                           
***************                                                                 
* RECEIVED DATE/TIME FOR  AGYHDR  TYPES (INCLUDING VARHDR)                      
***************                                                                 
SNDE20   MVC   RDNTTDTE,PAHDDATE-PAGYHDRD(R7)                                   
         MVC   RDNTTTIM,PAHDTIME-PAGYHDRD(R7)                                   
         B     SNDE28                                                           
***************                                                                 
* RECEIVED DATE/TIME FOR  AGYRCL  TYPES (INCLUDING AGYCAN)                      
***************                                                                 
SNDE22   MVC   RDNTTDTE,PARCDATE-PAGYRCLD(R7)                                   
         MVC   RDNTTTIM,PARCTIME-PAGYRCLD(R7)                                   
         B     SNDE28                                                           
***************                                                                 
* RECEIVED DATE/TIME FOR  NON-AGY & NON-MKG  TYPES (DEFAULT TYPE)               
***************                                                                 
SNDE24   MVC   RDNTTDTE,RDNTDATE-RDLNNOTD(R7)   RECEIVED DATE/TIME              
         MVC   RDNTTTIM,RDNTTIME-RDLNNOTD(R7)                                   
SNDE28   LA    R3,RDLNNOTL(R3)                                                  
         B     SNDE50                                                           
*                                                                               
         USING MDLNNOTD,R3                                                      
SNDE30   CLC   =C'MKGHDR',0(R7)    ERROR FOR MAKEGOOD OFFER                     
         BNE   SNDE35                                                           
         MVC   MDNTTDTE,MOHDDATE-MOFRHDRD(R7)                                   
         MVC   MDNTTTIM,MOHDTIME-MOFRHDRD(R7)                                   
         MVC   MDNTOFRI,MOHDOFRI-MOFRHDRD(R7)                                   
         MVC   MDNTSEQN,MOHDSEQN-MOFRHDRD(R7)                                   
         B     SNDE40                                                           
*                                                                               
SNDE35   MVC   MDNTTDTE,MOAPDATE-MOFRAPPD(R7)  ALL OTHER MAKEGOOD FORMS         
         MVC   MDNTTTIM,MOAPTIME-MOFRAPPD(R7)                                   
         MVC   MDNTOFRI,MOAPOFRI-MOFRAPPD(R7)                                   
         MVC   MDNTSEQN,MOAPSEQN-MOFRAPPD(R7)                                   
SNDE40   LA    R3,MDLNNOTL(R3)                                                  
         B     SNDE50                                                           
*                                                                               
SNDE50   MVC   0(2,R3),=X'0D25'    CRLF                                         
         LA    R3,2(R3)                                                         
*                                                                               
*****    CLI   QMED,C'R'           RADIO?                                       
*****    BNE   SNDE90              NO, DON'T WORRY ABOUT XM1 AND XM2            
*                                                                               
         LR    RE,R7               LOCATE AGYXM1 AND XM2, R7 SHOULD             
         SR    RF,RF                                                            
         ICM   RF,3,MQMSGLEN       MESSAGE LENGTH                               
SNDE55   CLC   0(6,RE),=C'AGYXM1'                                               
         BE    SNDE60                                                           
         AHI   RE,1                                                             
         BCT   RF,SNDE55                                                        
         DC    H'0'                DIE IF WE REACH THIS AS WE SHOULDN'T         
*                                                                               
SNDE60   MVC   0(PAGYXM1L,R3),0(RE)     COPY AGYXM1                             
         MVC   PAGYXM1L(2,R3),=X'0D25'  PUT OUT CRLF                            
         LA    R3,PAGYXM1L+2(R3)                                                
         AHI   RE,PAGYXM1L+2                                                    
*                                                                               
         MVC   0(PAGYXM2L,R3),0(RE)     COPY AGYXM2                             
         MVC   PAGYXM2L(2,R3),=X'0D25'  PUT OUT CRLF                            
         LA    R3,PAGYXM2L+2(R3)                                                
         AHI   RE,PAGYXM2L+2                                                    
*                                                                               
         CLC   =C'AGYXM3',0(RE)    DID THEY PROVIDE US WITH AGYXM3?             
         BNE   SNDE63              NO, THEY WEREN'T NICE ENOUGH                 
         MVC   0(PAGYXM3L,R3),0(RE)  COPY AGYXM3                                
         B     SNDE66                                                           
*                                                                               
         USING PAGYXM3D,R3                                                      
SNDE63   MVI   PAX3TID,C' '         SPACE-FILL LINE                             
         MVC   PAX3TID+1(PAGYXM3L-1),PAX3TID                                    
*                                                                               
         MVC   PAX3TID,=C'AGYXM3'                                               
         MVC   PAX3ORDR,6(RE)       COPY ORDER NUMBER FROM AGYXM2               
SNDE66   MVC   PAX3MESG,0(R7)       TELL SELLER WHAT MSG CAUSED ERROR           
***************                                                                 
* IF WE CAN SEND BACK THE STATION CALL LETTERS DO SO                            
***************                                                                 
         LA    RF,ROAPQSTA-RORDAPPD(R7)  ERROR FROM ONE OF THESE                
         CLC   =C'ORDAPP',0(R7)                                                 
         BE    SNDE70                                                           
         CLC   =C'CANAPP',0(R7)                                                 
         BE    SNDE70                                                           
         LA    RF,RORJQSTA-RORDREJD(R7)                                         
         CLC   =C'ORDREJ',0(R7)                                                 
         BE    SNDE70                                                           
         CLC   =C'CANREJ',0(R7)                                                 
         BE    SNDE70                                                           
         LA    RF,ROCFQSTA-RORDCFMD(R7)                                         
         CLC   =C'ORDCFM',0(R7)                                                 
         BE    SNDE70                                                           
         CLC   =C'CANCFM',0(R7)                                                 
         BE    SNDE70                                                           
         CLC   =C'ORDCAN',0(R7)                                                 
         BE    SNDE70                                                           
         LA    RF,RORCQSTA-RORDRCLD(R7)                                         
         CLC   =C'ORDRCL',0(R7)                                                 
         BNE   SNDE75              DON'T KNOW WHERE THE STATION IS              
*                                                                               
SNDE70   MVC   PAX3QSTA,0(RF)                                                   
*                                                                               
SNDE75   MVC   PAGYXM3L(2,R3),=X'0D25'    CRLF                                  
         LA    R3,PAGYXM3L+2(R3)                                                
         DROP  R3                                                               
*                                                                               
         USING PAGYTLRD,R3                                                      
         MVC   PATLTID,=C'AGYTLR'                                               
         MVC   PATLORDR,6(RE)       COPY ORDER NUMBER FROM AGYXM2               
         MVC   PATLNMRC,=C'000005'                                              
         MVC   PATLSPTS,=C'000000'                                              
         MVC   PATLTOTL,=10C'0'                                                 
         LA    R3,PAGYTLRL(R3)                                                  
         DROP  R3                                                               
*                                                                               
         MVC   0(2,R3),=X'0D25'    CRLF                                         
         LA    R3,2(R3)                                                         
*                                                                               
SNDE90   L     R2,ASPLAREA                                                      
         SR    R3,R2               R3 = L(MSG) WITH CRLF                        
*                                                                               
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CMQIO-COMFACSD(RF)                                            
         GOTOR (RF),DMCB,=CL8'PUT',ASPLAREA,(R3),0,0,DUB                        
*                                                                               
**** SWITCH OUT OF XA MODE (24-BIT ADDRESSING)                                  
SNDEX    BRAS  RE,OFF31            IN CASE WE WERE IN 31-BIT MODE               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SENDS A SUCCESS MESSAGE BACK TO SBS FOR CONFIRMATIONS AND REJECTIONS          
*                                                                               
* ON ENTRY:    R7                  A(DARE MESSAGE), BUT ATBUFF ALSO             
***********************************************************************         
SNDSUCSS NTR1  BASE=*,LABEL=*                                                   
         L     RE,ASPLAREA         PUT MSG TO BUFFER FOR MQPUT                  
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
         L     R3,ASPLAREA         USE ASPLAREA AS ERRNOT IS SMALL              
         MVC   0(16,R3),=CL16'RADIOEDIXML*****'                                 
         LA    R3,16(R3)                                                        
*                                                                               
         L     R7,ATBUFF           USE TBUFF FOR ERROR CREATION                 
         CLC   MQMSGLEN,=AL2(4064) IN 31-BIT MODE (MSG TOO LONG)?               
         BNH   SNDSC05                                                          
**** SET 31-BIT ADDRESSING                                                      
         BRAS  RE,ON31             YES, WE ARE                                  
*****                                                                           
SNDSC05  AHI   R7,8                BUMP PAST THE "REDIXML "                     
*****                                                                           
         USING RDLNNOTD,R3                                                      
         MVC   RDNTTID,=C'ERRNOT'  SPECIAL ACK ERROR NOTIFICATION               
         MVC   RDNTORDR,6(R7)      ORDER # IS ALWAYS 6 BYTES FROM BEG.          
         MVC   RDNTFRID,RDNTTOID-RDLNNOTD(R7)   SWAP TO/FROM                    
         MVC   RDNTTOID,RDNTFRID-RDLNNOTD(R7)                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(X'20',RDNTDATE)                              
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,DUB                                                           
         AP    PACKOF4B,DUB(4)                                                  
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    SNDSC10                                                          
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,RDNTDATE,(X'20',RDNTDATE),F'1'                       
*                                                                               
SNDSC10  ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,RDNTTIME,L'HALF                                
*                                                                               
         MVC   RDNTRPCN,QREPCON                                                 
         MVC   RDNTRTRN,QRETURN                                                 
         MVC   RDNTEFLG,=C'ACK'    ACKNOWLEDGEMENT TYPE                         
*                                                                               
         MVC   RDNTTDTE,RDNTDATE-RDLNNOTD(R7)   RECEIVED DATE/TIME              
         MVC   RDNTTTIM,RDNTTIME-RDLNNOTD(R7)                                   
         LA    R3,RDLNNOTL(R3)                                                  
*                                                                               
SNDSC50  MVC   0(2,R3),=X'0D25'    CRLF                                         
         LA    R3,2(R3)                                                         
*                                                                               
*****    CLI   QMED,C'R'           RADIO?                                       
*****    BNE   SNDSC90             NO, DON'T WORRY ABOUT XM1 AND XM2            
*                                                                               
         LR    RE,R7               LOCATE AGYXM1 AND XM2, R7 SHOULD             
         SR    RF,RF                                                            
         ICM   RF,3,MQMSGLEN       MESSAGE LENGTH                               
SNDSC55  CLC   0(6,RE),=C'AGYXM1'                                               
         BE    SNDSC60                                                          
         AHI   RE,1                                                             
         BCT   RF,SNDSC55                                                       
         DC    H'0'                DIE IF WE REACH THIS AS WE SHOULDN'T         
*                                                                               
SNDSC60  MVC   0(PAGYXM1L,R3),0(RE)     COPY AGYXM1                             
         MVC   PAGYXM1L(2,R3),=X'0D25'  PUT OUT CRLF                            
         LA    R3,PAGYXM1L+2(R3)                                                
         AHI   RE,PAGYXM1L+2                                                    
*                                                                               
         MVC   0(PAGYXM2L,R3),0(RE)     COPY AGYXM2                             
         MVC   PAGYXM2L(2,R3),=X'0D25'  PUT OUT CRLF                            
         LA    R3,PAGYXM2L+2(R3)                                                
         AHI   RE,PAGYXM2L+2                                                    
*                                                                               
         CLC   =C'AGYXM3',0(RE)    DID THEY PROVIDE US WITH AGYXM3?             
         BNE   SNDSC63             NO, THEY WEREN'T NICE ENOUGH                 
         MVC   0(PAGYXM3L,R3),0(RE)  COPY AGYXM3                                
         B     SNDSC66                                                          
*                                                                               
         USING PAGYXM3D,R3                                                      
SNDSC63  MVI   PAX3TID,C' '         SPACE-FILL LINE                             
         MVC   PAX3TID+1(PAGYXM3L-1),PAX3TID                                    
*                                                                               
         MVC   PAX3TID,=C'AGYXM3'                                               
         MVC   PAX3ORDR,6(RE)       COPY ORDER NUMBER FROM AGYXM2               
SNDSC66  MVC   PAX3MESG,0(R7)       TELL SELLER WHAT MSG CAUSED ERROR           
***************                                                                 
* IF WE CAN SEND BACK THE STATION CALL LETTERS DO SO                            
***************                                                                 
         LA    RF,RORJQSTA-RORDREJD(R7)                                         
         CLC   =C'ORDREJ',0(R7)                                                 
         BE    SNDSC70                                                          
         CLC   =C'CANREJ',0(R7)                                                 
         BE    SNDSC70                                                          
         LA    RF,ROCFQSTA-RORDCFMD(R7)                                         
         CLC   =C'ORDCFM',0(R7)                                                 
         BE    SNDSC70                                                          
         CLC   =C'CANCFM',0(R7)                                                 
         BE    SNDSC70                                                          
         CLC   =C'ORDCAN',0(R7)                                                 
         BNE   SNDSC75             DON'T KNOW WHERE THE STATION IS              
*                                                                               
SNDSC70  MVC   PAX3QSTA,0(RF)                                                   
*                                                                               
SNDSC75  MVC   PAGYXM3L(2,R3),=X'0D25'    CRLF                                  
         LA    R3,PAGYXM3L+2(R3)                                                
         DROP  R3                                                               
*                                                                               
         USING PAGYTLRD,R3                                                      
         MVC   PATLTID,=C'AGYTLR'                                               
         MVC   PATLORDR,6(RE)       COPY ORDER NUMBER                           
         MVC   PATLNMRC,=C'000005'                                              
         MVC   PATLSPTS,=C'000000'                                              
         MVC   PATLTOTL,=10C'0'                                                 
         LA    R3,PAGYTLRL(R3)                                                  
         DROP  R3                                                               
*                                                                               
         MVC   0(2,R3),=X'0D25'    CRLF                                         
         LA    R3,2(R3)                                                         
*                                                                               
SNDSC90  L     R2,ASPLAREA                                                      
         SR    R3,R2               R3 = L(MSG) WITH CRLF                        
*                                                                               
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CMQIO-COMFACSD(RF)                                            
         GOTOR (RF),DMCB,=CL8'PUT',ASPLAREA,(R3),0,0,DUB                        
*                                                                               
**** SWITCH OUT OF XA MODE (24-BIT ADDRESSING)                                  
SNDSCX   BRAS  RE,OFF31            IN CASE WE WERE IN 31-BIT MODE               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER RECALL ACKNOWLEDGEMENT                                    
***********************************************************************         
RCLAKNWL NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         MVI   QMED,C'T'                                                        
         TM    MISCFLG3,MF3RADIO   AM I RADIO?                                  
         BZ    RCLAK02                                                          
         MVI   QMED,C'R'                                                        
*                                                                               
         L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         ZIC   R2,0(R3)            R2 = L(ORDRCL HEADER)                        
         LA    R7,1(R3)            R7 = A(ORDRCL HEADER)                        
*                                                                               
         USING RORDRCLD,R7                                                      
RCLAKD   USING RTN2SNDR,RORCRTRN                                                
*                                                                               
RCLAK02  GOTO1 VHEXIN,DMCB,RCLAKD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,RCLAKD.RTNPWRCD                                           
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
         DROP  RCLAKD                                                           
*                                                                               
         MVC   QREPCON,RORCRPCN    COPY THESE VALUES                            
         MVC   QRETURN,RORCRTRN                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,RORCORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   RCLAKNO                                                          
*                                                                               
         MVC   USERID,RORCTOID                                                  
         BAS   RE,SWTCHSPT         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   RCLAKNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   RCLAKNO                                                          
*                                                                               
         NI    BITFLAG2,X'FF'-BF2SPNDG                                          
         NI    MISCFLG1,X'FF'-MF1NOXMT   ASSUME XMT ELEM EXISTS                 
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         LA    R2,DOIDCON          DON'T OVERWRITE CONTRACT IF REVISION         
         DROP  R6                                                               
*                                                                               
         SR    R0,R0                                                            
         MVI   REVISION,0                                                       
RCLAK10  CLI   0(R6),0                                                          
         BNE   RCLAK12                                                          
         LHI   R1,REFORDNT          ORD NOT TRANSMITTED                         
         BRAS  RE,SNDERROR                                                      
         B     RCLAKNO                                                          
*                                                                               
         USING DOSPELD,R6                                                       
RCLAK12  CLI   0(R6),DOSPELQ                                                    
         BNE   *+10                                                             
         MVC   REVISION,DOSPREVN   SAVE REVISION NUMBER                         
*                                                                               
         CLI   0(R6),DOXMTELQ                                                   
         BE    RCLAK16                                                          
RCLAK14  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RCLAK10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
RCLAK16  CLI   DOXMTSTA,QSNTPNDG   SEND PENDING?                                
         BNE   RCLAK18                                                          
         OI    BITFLAG2,BF2SPNDG   YES, THIS XMT ELEM HAS NO DEST ID            
         ST    R6,FULL     <====== DON'T CLOBBER THIS!!                         
         B     RCLAK14             GET THE NEXT XMT ELEM                        
*                                                                               
RCLAK18  CLI   DOXMTSTA,QSNTXCNF   SENT CANCELLED, PARTIAL CONFIRM?             
         BE    RCLAKNO                                                          
         CLI   DOXMTSTA,QSNTXREJ     OR   SEND CANCELLED, REJECTED?             
         BE    RCLAKNO                                                          
         CLI   DOXMTSTA,QTOBESNT     OR   TO BE SENT VIA SCRIPT?                
         BE    RCLAKNO             YES, LET THEM RESEND THE ORDER               
*                                                                               
         OC    DOXMTDID,DOXMTDID   PROBLEM IF NO DEST ID                        
         BNZ   RCLAK20                                                          
         IC    R0,1(R6)            CHECK FOR PREVIOUS XMT                       
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),DOXMTELQ      IS IT A XMT ELEM?                            
         BNE   RCLAK19                                                          
         OC    DOXMTDID,DOXMTDID   WAS THERE A DESTID?                          
         BNZ   RCLAKNO             YES: DO NOTHING.                             
         CLI   DOXMTSTA,QTOBESNT     OR   TO BE SENT VIA SCRIPT?                
         BE    RCLAKNO             YES, LET THEM RESEND THE ORDER               
RCLAK19  LHI   R1,*-T16300                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
RCLAK20  GOTO1 VDATCON,DMCB,(0,RORCDATE),(19,WORK)                              
         GOTO1 VHEXIN,DMCB,RORCTIME,WORK+L'DOXMTSTD,L'RORCTIME                  
         CLI   REVISION,0          ARE WE IN REVISION?                          
         BNE   *+10                YES                                          
         MVC   0(L'DOIDCON,R2),RORCRPCN    SAVE THE REP CONTRACT NUMBER         
*                                                                               
         OC    DOXMTSTD,DOXMTSTD   IF NO DATE OR TIME ALREADY                   
         BZ    RCLAK40                                                          
         OC    DOXMTSTT,DOXMTSTT                                                
         BZ    RCLAK30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTSTD,WORK              IS ELEM'S DATE MORE RECENT?           
         BH    RCLAKNO                    YES, IGNORE THIS RECORD               
         BL    RCLAK30                    NO, OLDER                             
         CLC   DOXMTSTT,WORK+L'DOXMTSTD    IS ELEM'S TIME MORE RECENT?          
         BNH   RCLAK30                    NO, CONTINUE PROCESSING               
         GOTO1 COMPTIME,DMCB,DOXMTSTD,WORK                                      
         BNE   RCLAKNO                 YES, NOT W/IN 60 MIN, IGNORE REC         
*                                                                               
RCLAK30  CLI   DOXMTSTA,QRECALL    AGENCY ORDER IN RECALL STATUS?               
         BE    RCLAK40                                                          
         CLI   DOXMTSTA,QCFMD      IF CONFIRMED                                 
         BE    RCLAKNO             THEN DON'T MARK FOR RECALL ACK.              
         CLI   DOXMTSTA,QRJCT      IF REJECTED                                  
         BE    RCLAKNO             THEN DON'T MARK FOR RECALL ACK.              
         CLI   DOXMTSTA,QUNDARE    IF UNDARED                                   
         BE    RCLAKNO             THEN DON'T MARK FOR RECALL ACK.              
         LHI   R1,REFCCHNG                                                      
         BRAS  RE,SNDERROR                                                      
         B     RCLAKNO                                                          
*                                                                               
RCLAK40  DS    0H                                                               
         CLI   RORCACCP,C'A'       RECALL, REP STATUS APPROVED?                 
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLAPPR   YES                                          
         B     RCLAK50                                                          
*                                                                               
         CLI   RORCACCP,C'D'       RECALL, REP STATUS DELIVERED?                
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLDELN   YES                                          
         B     RCLAK50                                                          
*                                                                               
         CLI   RORCACCP,C'T'       RECALL, REP STATUS TRANSMITTED?              
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLTRNS   YES                                          
         B     RCLAK50                                                          
*                                                                               
         CLI   RORCACCP,C'W'       RECALL, REP STATUS WIP?                      
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLWIP    YES                                          
         B     RCLAK50                                                          
*                                                                               
         CLI   RORCACCP,C'C'       RECALL, REP STATUS CONFIRMED?                
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLCONF   YES                                          
         B     RCLAK53                                                          
*                                                                               
         CLI   RORCACCP,C'R'       RECALL, REP STATUS REJECTED?                 
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLREJD   YES                                          
         B     RCLAK56                                                          
*                                                                               
         MVI   DOXMTSTA,QRCLUNKN   RECALL, REP STATUS UNKNOWN                   
         B     RCLAK53                                                          
*                                                                               
RCLAK50  MVC   SVSTAT,DOXMTSTA     SAVE FOR LATER                               
         TM    BITFLAG2,BF2SPNDG   WE HAD A SENT PENDING?                       
         BZ    RCLAK59                                                          
         L     R6,FULL       <==== THIS BETTER NOT GET CLOBBERED!!              
         MVI   DOXMTSTA,QTOBESNT                                                
         B     RCLAK59                                                          
*                                                                               
RCLAK53  MVC   SVSTAT,DOXMTSTA     SAVE FOR LATER                               
         TM    BITFLAG2,BF2SPNDG   WE HAD A SENT PENDING?                       
         BZ    RCLAK59                                                          
****     L     R6,FULL       <==== COMMENTED SO CONFIRM CAN SET STATUS          
****     MVI   DOXMTSTA,QSNTXCNF                                                
         NI    BITFLAG2,X'FF'-BF2SPNDG   NOT "TO BE SENT"                       
         B     RCLAK59                                                          
*                                                                               
RCLAK56  MVC   SVSTAT,DOXMTSTA     SAVE FOR LATER                               
         TM    BITFLAG2,BF2SPNDG   WE HAD A SENT PENDING?                       
         BZ    RCLAK99                                                          
         L     R6,FULL       <==== THIS BETTER NOT GET CLOBBERED!!              
         MVI   DOXMTSTA,QSNTXREJ                                                
         NI    BITFLAG2,X'FF'-BF2SPNDG   NOT "TO BE SENT"                       
*                                                                               
RCLAK59  MVC   DOXMTSTD,WORK                                                    
         MVC   DOXMTSTT,WORK+L'DOXMTSTD                                         
         DROP  R6                                                               
*                                                                               
RCLAK99  OI    MISCFLG1,MF1XMTUP                                                
*                                                                               
RCLAK100 NI    BITFLAG3,X'FF'-BF3SPNDG    NOT A SENT PENDING ORDER              
         NI    MISCFLG2,X'FF'-MF2DIDNM    NO DESTID..                           
         B     RCLAK103                                                         
*                                                                               
RCLAK102 OI    MISCFLG2,MF2DIDNM   FOUND A DESTID                               
RCLAK103 SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
RCLAK110 CLI   0(R6),0                                                          
         BNE   RCLAK115                                                         
         TM    MISCFLG1,MF1NOXMT   DOES IT HAVE XMT ELEMS?                      
         BZ    RCLAKNO             YES, THEN DON'T GIVE ERROR                   
         LHI   R1,REFORDNT         NEVER TRANSMITTED                            
         BRAS  RE,SNDERROR                                                      
         B     RCLAKNO                                                          
*                                                                               
RCLAK115 CLI   0(R6),DOSTELQ                                                    
         BE    RCLAK120                                                         
RCLAK116 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RCLAK110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
RCLAK120 TM    MISCFLG2,MF2DIDNM   DESTID FOUND?                                
         BNZ   RCLAK130            YES, SKIP THIS CHECK                         
         CLI   DOSTSTAT,DDLVRD     NO, DELIVERY NOTICE?                         
         BNE   RCLAK116            - NO, CHECK NEXT                             
         OC    DOSTIDNM,DOSTIDNM   - YES, DEST ID???                            
         BNZ   RCLAK102                - YES, WE FOUND IT!!                     
         LHI   R1,*-T16300             - NO, SEND ERROR!!                       
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
RCLAK130 CLI   DOSTSTAT,DDLVRD     SKIP DELIVERY NOTICE                         
         BE    RCLAK116                                                         
         CLI   DOSTSTAT,QSNTPNDG   SEND PENDING?                                
         BNE   RCLAK135                                                         
         OI    BITFLAG3,BF3SPNDG                                                
         B     RCLAK116                                                         
*                                                                               
RCLAK135 CLI   DOSTSTAT,QSNTXCNF   SEND CANCELLED, PARTIAL CONFIRM?             
         BE    RCLAKNO                                                          
         CLI   DOSTSTAT,QSNTXREJ     OR SEND CANCELLED, REJECTED?               
         BE    RCLAKNO                                                          
         CLI   DOSTSTAT,QTOBESNT     OR TO BE SENT VIA SCRIPT?                  
         BE    RCLAKNO                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RORCDATE),(19,WORK)                              
         GOTO1 VHEXIN,DMCB,RORCTIME,WORK+L'DOSTDATE,L'RORCTIME                  
         CLI   REVISION,0          ARE WE IN A REVISION?                        
         BNE   *+10                                                             
         MVC   0(L'DOIDCON,R2),RORCRPCN  SAVE THE REP CONTRACT NUMBER           
*                                                                               
         OC    DOSTTIME,DOSTTIME                                                
         BZ    RCLAK140                                                         
*                                                                               
         CLC   DOSTDATE,WORK              IS ELEM'S DATE MORE RECENT?           
         BH    RCLAKNO                    YES, IGNORE THIS RECORD               
         BL    RCLAK140                   NO, OLDER                             
         CLC   DOSTTIME,WORK+L'DOSTDATE    IS ELEM'S TIME MORE RECENT?          
         BNH   RCLAK140                   NO, CONTINUE PROCESSING               
         GOTO1 COMPTIME,DMCB,DOSTDATE,WORK                                      
         BNE   RCLAKNO                 YES, NOT W/IN 60 MIN, IGNORE REC         
*                                                                               
RCLAK140 CLI   DOSTSTAT,QRECALL                                                 
         BE    RCLAK145                                                         
         CLI   DOSTSTAT,DSENT         ORDER IN SENT STATUS AND SENT             
         BE    RCLAKERR                 BEFORE ORDRCL MESSAGE DATE/TIME         
         CLI   DOSTSTAT,QCFMD                                                   
         BE    RCLAKNO                                                          
         CLI   DOSTSTAT,QRJCT                                                   
         BE    RCLAKNO                                                          
         CLI   DOSTSTAT,QUNDARE                                                 
         BE    RCLAKNO                                                          
*                                                                               
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    RCLAKNO                    YES, THEN DON'T GIVE ERROR            
RCLAKERR LHI   R1,REFCCHNG                                                      
         BRAS  RE,SNDERROR                                                      
         B     RCLAKNO                                                          
         DROP  R6                                                               
*                                                                               
RCLAK145 DS    0H                                                               
         LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,WORK                                                    
         MVC   DOSTTIME,WORK+L'DOSTDATE                                         
*                                                                               
         CLI   RORCACCP,C'A'       RECALL, REP STATUS APPROVED?                 
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLAPPR   YES                                          
         B     RCLAK150                                                         
*                                                                               
         CLI   RORCACCP,C'D'       RECALL, REP STATUS DELIVERED?                
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLDELN   YES                                          
         B     RCLAK150                                                         
*                                                                               
         CLI   RORCACCP,C'T'       RECALL, REP STATUS TRANSMITTED?              
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLTRNS   YES                                          
         B     RCLAK150                                                         
*                                                                               
         CLI   RORCACCP,C'W'       RECALL, REP STATUS WIP?                      
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLWIP    YES                                          
         B     RCLAK150                                                         
*                                                                               
         CLI   RORCACCP,C'C'       RECALL, REP STATUS CONFIRMED?                
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLCONF   YES                                          
         B     RCLAK150                                                         
*                                                                               
         CLI   RORCACCP,C'R'       RECALL, REP STATUS REJECTED?                 
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLREJD   YES                                          
         B     RCLAK150                                                         
*                                                                               
         MVI   DOSTSTAT,QRCLUNKN   RECALL, REP STATUS UNKNOWN                   
*                                                                               
RCLAK150 L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
RCLAK155 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    RCLAK160         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    RCLAK160                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   RCLAK160                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RCLAK155                                                         
*                                                                               
RCLAK160 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
         MVC   SVSTAT,DOSTSTAT                                                  
         MVC   DOSAELEM(DOSTLNQ2),DOSTELEM                                      
*                                                                               
         TM    BITFLAG3,BF3SPNDG                                                
         BZ    RCLAK300                                                         
*                                                                               
         CLI   SVSTAT,QRCLUNKN     RECALL, REP STATUS UNKNOWN                   
         BE    *+12                                                             
         CLI   SVSTAT,QRCLCONF     RECALL, REP STATUS CONFIRMED?                
         BNE   RCLAK165            NO                                           
         NI    BITFLAG3,X'FF'-BF3SPNDG  YES: NOT "TO BE SENT"                   
         B     RCLAK300                                                         
*                                                                               
RCLAK165 MVI   DOSTSTAT,QTOBESNT                                                
         CLI   SVSTAT,QRCLREJD                                                  
         BNE   RCLAK170                                                         
         MVI   DOSTSTAT,QSNTXREJ                                                
         NI    BITFLAG3,X'FF'-BF3SPNDG  YES: NOT "TO BE SENT"                   
         DROP  R2                                                               
*                                                                               
RCLAK170 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
RCLAK200 TM    MISCFLG3,MF3RADIO   ARE WE RADIO?                                
         BZ    RCLAK300                                                         
*                                                                               
         ZIC   R2,0(R3)            BUMP TOP NEXT RECORD TYPE                    
         AR    R3,R2                                                            
*&&DO                                                                           
         CLC   =C'ORDSAL',1(R3)    SALESPERSON REASSIGNMENT?                    
         BNE   RCLAK300            NONE, JUST EXIT                              
         LA    R4,1(R3)            YES, WE HAVE ONE                             
         USING RORDSALD,R4                                                      
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6) X'01'                                       
         USING DOIDELD,R6                                                       
         XC    DOIDSPER,DOIDSPER                                                
         MVC   DOIDSPER(L'ROSPSALN),ROSPSALN   NO, UPDATE SALESPERSON           
         OC    DOIDSPER,=25C' '                                                 
         OI    MISCFLG3,MF3SALPR                                                
         DROP  R4                                                               
*                                                                               
RCLAK210 ZIC   R1,0(R3)                                                         
         AR    R3,R1               BUMP TO NEXT SAVED ENTRY                     
*                                                                               
RCLAK250 CLC   =C'ORDTLR',1(R3)                                                 
         BE    RCLAK300                                                         
         LHI   R1,*-T16300                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*&&                                                                             
RCLAK300 XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
         TM    MISCFLG3,MF3RADIO                                                
         BZ    *+12                                                             
         L     R3,AWRKRIOA                                                      
         LA    R7,3(R3)            R7 = A(ORDRCL HEADER)                        
*                                                                               
         CLI   RORCACCP,C'D'                                                    
         BNE   RCLAK310                                                         
         CLI   REVISION,0                                                       
         BE    RCLAK310            REGULAR RECALL                               
         TM    BITFLAG2,BF2SPNDG   THIS BIT IS NOW "TO BE SENT"                 
         BNZ   RCLAK310            YES!                                         
         TM    MISCFLG3,MF3RADIO   RADIO ORDER RECALLED?                        
         BNZ   RCLAK310            YES, NO AGYCAN NEEDED AS PER SKUI            
*&&DO*&& BRAS  RE,SNDAGYCN                                                      
*                                                                               
RCLAK310 TM    BITFLAG2,BF2SPNDG   THIS BIT IS NOW "TO BE SENT"                 
         BZ    RCLAK320            NO                                           
         BRAS  RE,SNDSCRPT                                                      
*                                                                               
RCLAK320 DS    0H                                                               
*                                                                               
RCLAKYES BRAS  RE,BLDCOLOR                                                      
         BAS   RE,DAREMAIL                                                      
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         J     YES                                                              
*                                                                               
RCLAKNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   RCLAK200                                                         
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         J     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALCULATES IF TWO DATE/TIME ARE WITHIN 16 MINUTES OF             
* EACH OHTER                                                                    
*                                                                               
* ON ENTRY:    PARAM 1             A(1ST DATE/TIME)                             
*              PARAM 2             A(2ND DATE/TIME)                             
*                                                                               
* ON EXIT:     CC                  YES - WITHIN 16 MINUTES                      
*                                                                               
* NOTE: DATE FORMAT IS JULIAN PWOS (CYYDDD)                                     
*       TIME FORMAT IS HEXIN MILITARY (HHMM)                                    
*                                                                               
* WARNING:     WORK GETS CLOBBERED                                              
***********************************************************************         
COMPTIME NTR1                                                                   
         L     R5,0(R1)                                                         
         L     R6,4(R1)                                                         
*                                                                               
         CLC   0(3,R5),0(R6)       SAME DATE?                                   
         BNE   CMPTMNO             NO, EXIT FOR NOW                             
*                                                                               
         MVC   BYTE,3(R5)          GET HOURS AND MULTIPLE BY 60 MINUTES         
         NI    BYTE,X'F0'                                                       
         ZIC   R1,BYTE                                                          
         SRL   R1,4                                                             
         MH    R1,=H'600'                                                       
         CVD   R1,DUB                                                           
         ZAP   PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,3(R5)                                                       
         NI    BYTE,X'0F'                                                       
         ZIC   R1,BYTE                                                          
         MH    R1,=H'60'                                                        
         CVD   R1,DUB                                                           
         AP    PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,4(R5)          ADD MINUTES TO HOUR-MINUTES                  
         NI    BYTE,X'F0'                                                       
         ZIC   R1,BYTE                                                          
         SRL   R1,4                                                             
         MH    R1,=H'10'                                                        
         CVD   R1,DUB                                                           
         AP    PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,4(R5)                                                       
         NI    BYTE,X'0F'                                                       
         ZIC   R1,BYTE                                                          
         CVD   R1,DUB                                                           
         AP    DUB,PACKOF4B                                                     
         CVB   RE,DUB                                                           
*                                                                               
         MVC   BYTE,3(R6)          GET HOURS AND MULTIPLE BY 60 MINUTES         
         NI    BYTE,X'F0'                                                       
         ZIC   R1,BYTE                                                          
         SRL   R1,4                                                             
         MH    R1,=H'600'                                                       
         CVD   R1,DUB                                                           
         ZAP   PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,3(R6)                                                       
         NI    BYTE,X'0F'                                                       
         ZIC   R1,BYTE                                                          
         MH    R1,=H'60'                                                        
         CVD   R1,DUB                                                           
         AP    PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,4(R6)          ADD MINUTES TO HOUR-MINUTES                  
         NI    BYTE,X'F0'                                                       
         ZIC   R1,BYTE                                                          
         SRL   R1,4                                                             
         MH    R1,=H'10'                                                        
         CVD   R1,DUB                                                           
         AP    PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,4(R6)                                                       
         NI    BYTE,X'0F'                                                       
         ZIC   R1,BYTE                                                          
         CVD   R1,DUB                                                           
         AP    DUB,PACKOF4B                                                     
         CVB   RF,DUB                                                           
*                                                                               
         SR    RE,RF               CALCULATE AND STORE DIFFERENCE IN            
         LPR   RE,RE                   MINUTES                                  
         ST    RE,DMCB                                                          
*                                                                               
         CH    RE,=H'60'    **  CHANGED TO 60!!! STUPID SB2 ENCODA!! **         
***      CH    RE,=H'16'           ARE (DATE/TIME)S WITHIN 16 MINUTES?          
         BNL   CMPTMNO             NO, SUX TO BE ENCODA                         
*                                                                               
CMPTMYES J     YES                                                              
*                                                                               
CMPTMNO  J     NO                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCRIPT SETUP FOR AUTO-SEND                                                    
***********************************************************************         
SNDSCRPT NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GETORDER         READ THE ORDER RECORD TO AIO1                
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         XR    R0,R0                                                            
SSCRP10  CLI   0(R6),0                                                          
         BE    SSCRP50                                                          
         CLI   0(R6),DOSPELQ       X'03' - SUPPLEMENTARY ID ELEMENT             
         BE    SSCRP15                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SSCRP10                                                          
*                                                                               
         USING DOSPELD,R6                                                       
SSCRP15  TM    DOSPFLG2,DOSPOMDT   ORDER PREVIOUSLY SENT FROM LINK?             
         BZ    SSCRP50             NO, NORMAL ORDXMT SCRIPT                     
         BAS   RE,WRKRCREA                                                      
*                                                                               
         L     R2,AIO2                                                          
         XC    0(256,R2),0(R2)                                                  
         LA    R2,4(R2)                                                         
         MVC   0(4,R2),=F'2101'                                                 
         MVC   4(6,R2),=C'SCRIPT'                                               
         MVC   10(8,R2),=C'SPLNKSND'                                            
         MVI   18(R2),C'I'                                                      
         MVC   30(5,R2),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R2),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R2),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R2),C'Y'                                                      
         L     R2,AIO2                                                          
         MVC   0(2,R2),=H'50'        46 + 4 BYTES FOR QSAM                      
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R2,AIO2                                                          
         XC    0(256,R2),0(R2)                                                  
         LA    R2,4(R2)                                                         
         MVC   0(4,R2),=F'2102'                                                 
         MVC   4(6,R2),=C'000001'                                               
         MVC   10(20,R2),=CL20'SIGN-ON INFORMATION '                            
*                                                                               
         MVC   30(8,R2),USERID                                                  
         TM    BITFLAG1,BF1PSSWD   PASSWORD REQUIRED FOR SPOT SYSTEM?           
         BZ    *+10                                                             
         MVC   38(3,R2),=C'DDS'    YES, THEN USE THE EVER USEFUL DDS            
         CLC   =C'TMNY',USERID     TMNY ID?                                     
         BNE   *+10                                                             
         MVC   38(3,R2),=C'DDS'    YES, DDS PASSWORD DOESN'T WORK               
         L     R2,AIO2                *** DDS WORK NOW ***                      
         MVC   0(2,R2),=H'50'        46 + 4 BYTES FOR QSAM                      
*&&DO                                                                           
         MVC   30(8,R2),=CL8'NEWSTYLE'                                          
         MVC   30+16(8,R2),USERID                                               
         MVC   30+16+8(8,R2),=CL8',DDSDARE'                                     
         MVC   30+60(4,R2),=C'T214'   USERID+PID (44 BYTES)                     
         L     R2,AIO2                                                          
         MVC   0(2,R2),=H'98'        94 + 4 BYTES FOR QSAM                      
*&&                                                                             
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R2,AIO2                                                          
         XC    0(256,R2),0(R2)                                                  
         LA    R2,4(R2)                                                         
         MVC   0(4,R2),=F'2102'                                                 
         MVC   4(6,R2),=C'000002'                                               
         MVC   10(20,R2),=CL20'SENDING WITH $LINK'                              
*                                                                               
         LA    R2,30(R2)                                                        
         MVC   0(1,R2),QMED                                                     
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   FULL,BINORDER       SHOW THE ORDER NUMBER                        
         XC    FULL,=4X'FF'                                                     
         TM    FULL,X'80'          NEW STYLE ORDER NUMBER?                      
         BZ    SSCRP20                                                          
         NI    FULL,X'FF'-X'80'                                                 
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         AP    DUB,=P'04000000'                                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  0(8,R2),DUB                                                      
         B     SSCRP30                                                          
*                                                                               
SSCRP20  L     R1,FULL                                                          
         AHI   R1,1                                                             
         ZICM  R1,FULL,2                                                        
         CVD   R1,DUB                                                           
         UNPK  0(4,R2),DUB                                                      
         OI    3(R2),X'F0'                                                      
*                                                                               
         ZICM  R3,FULL+2,2                                                      
         CVD   R3,DUB                                                           
         UNPK  4(4,R2),DUB                                                      
         OI    4+3(R2),X'F0'      SEQUENCE NUMBER                               
*                                                                               
SSCRP30  LA    R2,8(R2)                                                         
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         XR    R0,R0                                                            
SSCRP32  CLI   0(R6),0                                                          
         BE    SSCRP40                                                          
         CLI   0(R6),DOSTELQ       X'12' - TRANSMISSION ELEMENT                 
         BE    SSCRP38                                                          
SSCRP35  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SSCRP32                                                          
*                                                                               
         USING DOSTELD,R6                                                       
SSCRP38  CLI   DOSTSTAT,DSENT      WAS IT SENT?                                 
         BNE   SSCRP35                                                          
         CLI   DOSTLEN,DOSTLNQ7    DOES IT HAVE A FLAG BYTE?                    
         BNE   SSCRP40                                                          
         TM    DOSTTYP2,DOSTCNCL                                                
         BZ    SSCRP40                                                          
         MVI   0(R2),C'C'          MARK IT AS CANCEL                            
*                                                                               
SSCRP40  LA    R2,1(R2)                                                         
         L     RE,AIO2                                                          
         SR    R2,RE                                                            
         STH   R2,0(RE)            L'DATA + 4 BYTES FOR QSAM                    
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
         BAS   RE,WRKRCLOS                                                      
         B     SSCRPX                                                           
*                                                                               
SSCRP50  L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         MVC   BCLT,DOIDCLT                                                     
         GOTO1 VCLUNPK,DMCB,DOIDCLT,QCLT                                        
         MVC   QPRD1(1),DOIDPRD    SAVE BINARY PRODUCT CODES                    
         MVC   QPRD2(1),DOIDPRD2                                                
         MVC   BEST,DOIDEST                                                     
         EDIT  (B1,DOIDEST),(3,QEST1),FILL=0                                    
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         MVC   QFLTNUM,=C'  '                                                   
         MVI   QCSHTRDE,C' '                                                    
         CLI   DOIDFLTN,0                                                       
         BE    SSCRP60                                                          
         EDIT  (B1,DOIDFLTN),(2,QFLTNUM),FILL=0                                 
*                                                                               
SSCRP60  XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    SSCRP70                                                          
         CLI   0(R6),DOSPELQ       SUPP ID ELEM (X'03')?                        
         BH    SSCRP70                                                          
         BL    SSCRP60                                                          
         USING DOSPELD,R6                                                       
         CLI   DOSPLEN,DOSPLNQ                                                  
         BNH   SSCRP70                                                          
         CLI   DOSPTMTH,0          ANY METHOD WHATSOEVER?                       
         BE    SSCRP70                                                          
         CLC   =C'000',DOSPTDAT    ANY TRADE REP SPECIFIED?                     
         BNL   SSCRP70             NONE                                         
*                                                                               
         MVI   QCSHTRDE,C'C'                                                    
         TM    DOSPTMTH,X'40'      TRADE?                                       
         BZ    *+8                                                              
         MVI   QCSHTRDE,C'T'                                                    
         DROP  R6                                                               
*                                                                               
SSCRP70  BAS   RE,GETCLTRC         GET THE CLIENT RECORD                        
         BE    SSCRP80                                                          
         LHI   R1,*-T16300                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
SSCRP80  L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
         MVC   SVCPRF00,CPROF+0                                                 
         GOTO1 VCLUNPK,DMCB,(CPROF+6,BCLT),QCLT                                 
         MVC   SVCOFFC,COFFICE                                                  
         DROP  R6                                                               
*                                                                               
         MVC   BPRD,QPRD1                                                       
         GOTO1 GETQPRD,DMCB,(QPRD1,QPRD1)                                       
         GOTO1 GETQPRD,DMCB,(QPRD2,QPRD2)                                       
*                                                                               
         BAS   RE,WRKRCREA                                                      
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPORDXMT'                                            
         MVI   18(R1),C'I'                                                      
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'                                                      
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000001'                                               
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
*                                                                               
         MVC   30(8,R1),USERID                                                  
         TM    BITFLAG1,BF1PSSWD   PASSWORD REQUIRED FOR SPOT SYSTEM?           
         BZ    *+10                                                             
         MVC   38(3,R1),=C'DDS'    YES, THEN USE THE EVER USEFUL DDS            
         CLC   =C'TMNY',USERID     TMNY ID?                                     
         BNE   *+10                                                             
         MVC   38(3,R1),=C'DDS'    YES, DDS PASSWORD DOESN'T WORK               
         L     R1,AIO2                *** DDS WORK NOW ***                      
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
*&&DO                                                                           
         MVC   30(8,R1),=CL8'NEWSTYLE'                                          
         MVC   30+16(8,R1),USERID                                               
         MVC   30+16+8(8,R1),=CL8',DDSDARE'                                     
         MVC   30+60(4,R1),=C'T214'   USERID+PID (44 BYTES)                     
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'98'        94 + 4 BYTES FOR QSAM                      
*&&                                                                             
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000002'                                               
         MVC   10(20,R1),=CL20'$DAR TO ORDER/SEND'                              
         LA    R1,30(R1)                                                        
         USING LAYOUT2D,R1                                                      
         MVC   LAY2MED,QMED                                                     
         MVC   LAY2BYR,QBUYER                                                   
         MVC   LAY2MTHD,QCSHTRDE                                                
         MVC   LAY2CLT,QCLT                                                     
*                                                                               
         MVC   LAY2PRDS(3),QPRD1                                                
         CLI   QPRD2,C' '          ANY PIGGYBACK?                               
         BNH   *+14                                                             
         MVI   LAY2PRDS+3,C'-'     YES                                          
         MVC   LAY2PRDS+4(3),QPRD2                                              
*                                                                               
         MVC   LAY2EST(L'QEST1),QEST1                                           
         CLC   QFLTNUM,=C'00'                                                   
         BNH   *+14                                                             
         MVI   LAY2EST+3,C'-'                                                   
         MVC   LAY2EST+4(2),QFLTNUM                                             
*                                                                               
         MVC   LAY2STA,QSTA                                                     
         LA    R1,LAY2END                                                       
         DROP  R1                                                               
         L     RE,AIO2                                                          
         SR    R1,RE                                                            
         STH   R1,0(RE)            L'DATA + 4 BYTES FOR QSAM                    
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         BAS   RE,WRKRCLOS                                                      
*                                                                               
SSCRPX   L     RE,AWRKRIOA         RESET WORKER IO AREA FOR EDICT RECS          
         MVI   0(RE),0                                                          
         MVI   1(RE),2                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE MAKEGOOD ORDER CONFIRMATION                                     
***********************************************************************         
MKGDCNFM NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING MOFRCFMD,R7                                                      
MCNFMD   USING RTN2SNDR,MOCFRTNS                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   MCNFMD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,MCNFMD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,MCNFMD.RTNPWRCD                                           
         DROP  MCNFMD                                                           
*                                                                               
         MVC   QREPCON,MOCFRPCN    COPY THESE VALUES                            
         MVC   QRETURN,MOCFRTNS                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,MOCFORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   MCNFMNO                                                          
*                                                                               
         MVC   USERID,MOCFTOID                                                  
         BAS   RE,SWTCHSPT         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   MCNFMNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD IN AIO1                    
         BNE   MCNFMNO                                                          
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         MVC   BCLT,DOIDCLT                                                     
         GOTO1 VCLUNPK,DMCB,DOIDCLT,QCLT                                        
         MVC   QPRD1(1),DOIDPRD    SAVE BINARY PRODUCT CODES                    
         MVC   QPRD2(1),DOIDPRD2                                                
         MVC   BEST,DOIDEST                                                     
         EDIT  (B1,DOIDEST),(3,QEST1),FILL=0                                    
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         MVC   QFLTNUM,=C'  '                                                   
         MVI   QCSHTRDE,C' '                                                    
         CLI   DOIDFLTN,0                                                       
         BE    MCNFM02                                                          
         EDIT  (B1,DOIDFLTN),(2,QFLTNUM),FILL=0                                 
MCNFM02  MVC   QMGGROUP,MOCFOFRI   MAKEGOOD GROUP CODE                          
*                                                                               
MCNFM03  XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    MCNFM04                                                          
         CLI   0(R6),DOSPELQ       SUPP ID ELEM (X'03')?                        
         BH    MCNFM04                                                          
         BL    MCNFM03                                                          
*                                                                               
         USING DOSPELD,R6                                                       
         CLI   DOSPLEN,DOSPLNQ                                                  
         BNH   MCNFM04                                                          
         CLI   DOSPTMTH,0          ANY METHOD WHATSOEVER?                       
         BE    MCNFM04                                                          
         CLC   =C'000',DOSPTDAT    ANY TRADE REP SPECIFIED?                     
         BNL   MCNFM04             NONE                                         
         MVI   QCSHTRDE,C'C'                                                    
         TM    DOSPTMTH,X'40'      TRADE?                                       
         BZ    *+8                                                              
         MVI   QCSHTRDE,C'T'                                                    
         DROP  R6                                                               
*                                                                               
MCNFM04  BAS   RE,GETCLTRC         GET THE CLIENT RECORD                        
         BE    MCNFM05                                                          
         LHI   R1,*-T16300                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
MCNFM05  L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
         MVC   SVCPRF00,CPROF+0                                                 
         GOTO1 VCLUNPK,DMCB,(CPROF+6,BCLT),QCLT                                 
         MVC   SVCOFFC,COFFICE                                                  
         DROP  R6                                                               
*                                                                               
         MVC   BPRD,QPRD1                                                       
         GOTO1 GETQPRD,DMCB,(QPRD1,QPRD1)                                       
         GOTO1 GETQPRD,DMCB,(QPRD2,QPRD2)                                       
*                                                                               
         BAS   RE,ESTLOCKD         TEST IF ESTIMATE IS LOCKED                   
*                                                                               
         OC    ESTPW,ESTPW         ANY PW PCT?                                  
         BNZ   *+14                                                             
         OC    ESTCOST2,ESTCOST2   ANY COST2 PCT?                               
         BZ    *+8                                                              
         BAS   RE,GETPWC2          TEST IF PW LOCKED                            
*                                                                               
         TM    BITFLAG2,BF2ELCKD   ESTIMATE ALREADY LOCKED?                     
         BNZ   MCNFM05B            YES, DON'T NEED TO CHECK THE PROFILE         
*****                                                                           
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         MVC   TUSER,SIGNON2H      FOOL GETPROF                                 
         DROP  R1                                                               
*****                                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SDAR'    <=== NEED LOWER CASE 'S'                     
         NI    WORK,X'FF'-X'40'    MAKE IT LOWERCASE                            
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         CLI   SVCOFFC,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFFC                                               
*                                                                               
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CGETPROF-COMFACSD(RF)                                         
         XC    PROFDAR,PROFDAR                                                  
         GOTO1 (RF),DMCB,WORK,PROFDAR,VDATAMGR                                  
*                                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'S0OM'    OM PROFILE TELLS US IF DAR OR OM             
         MVC   WORK+4(2),AGENCY                                                 
         LA    R4,WORK+16                                                       
         GOTO1 (RF),DMCB,WORK,(R4)                                              
*****                                                                           
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         XC    TUSER,TUSER         RESET THE ID NUMBER IN THE UTL               
         DROP  R1                                                               
*****                                                                           
         LA    R1,DMCB                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PDARONHD,C'Y'       SET TO ON HOLD IF MAKEGOOD OKAY?             
         BNE   MCNFM05B                                                         
         OI    BITFLAG2,BF2ELCKD   YES, LOOKS LIKE ESTIMATE LOCKED              
*                                                                               
MCNFM05B BRAS  RE,GETNOTCE         READ MAKEGOOD NOTICE RECORD IN AIO1          
         BNE   MCNFMNO                                                          
*                                                                               
         L     R6,AIO1             LOOK FOR LASTEST STATUS                      
         LA    R6,MNRFRST-MNKEY(R6)                                             
         MVI   ELCODE,MNSTELQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   MCNFM10             ERROR, COULD BE A NEW NOTICE                 
         USING MNSTELD,R6                                                       
*                                                                               
MCNFM05C CLI   MNSTSTAT,MNSTDELV   DELIVERED STATUS?                            
         BNE   MCNFM05D                                                         
         BAS   RE,NEXTEL           THEN SEE IF DELIVERED APPROVAL               
         BNE   MCNFM10                                                          
         B     MCNFM05C                                                         
*                                                                               
MCNFM05D CLI   MNSTSTAT,MNSTGOIN   GOING TO BE OKAY?                            
         BE    MCNFMNO                                                          
         CLI   MNSTSTAT,MNSTOKAY         OKAYED?                                
         BE    MCNFMNO                                                          
         CLI   MNSTSTAT,MNSTHOLD         ON HOLD?                               
         BE    MCNFMNO             THEN DON'T PROCESS THIS AGAIN                
*                                                                               
         CLI   MNSTSTAT,MNSTAPP    APPROVED STATUS?                             
         BE    MCNFM15                                                          
         CLI   MNSTSTAT,MNSTSAPP   SELF APPLIED?                                
         BNE   MCNFM10                                                          
         NI    BITFLAG2,X'FF'-BF2ELCKD+BF2PWLCK  CLEAR THOSE FLAGS!             
         B     MCNFM20                                                          
         DROP  R6                                                               
MCNFM10  LHI   R1,REFMNAPP                                                      
         BRAS  RE,SNDERROR                                                      
         B     MCNFMNO                                                          
*                                                                               
MCNFM15  TM    BITFLAG2,BF2ELCKD+BF2PWLCK    LOCKED ALREADY?                    
         BNZ   MCNFM20                                                          
         OC    ESTLOKYM,ESTLOKYM   ANY LOCKOUT PERIODS?                         
         BZ    MCNFM20                                                          
*                                                                               
         MVC   HALF(2),ESTLOKYM                                                 
         NI    HALF+1,X'FF'-X'C0'  REMOVE PRIOR AND SUBSEQUENT BITS             
         L     R6,AIO1                                                          
         LA    R6,MNRFRST-MNKEY(R6)                                             
         MVI   ELCODE,MNMSELQ      LOOK FOR MISSED SPOTS (X'10')                
         BAS   RE,FIRSTEL                                                       
         BNE   MCNFM17             NO MISSED SPOTS, CHECK MG SPOTS              
         USING MNMSELD,R6                                                       
MCNFM15B GOTO1 VDATCON,DMCB,(8,MNMSBDAT),(3,FULL)                               
*                                                                               
         TM    ESTLOKYM+1,X'C0'    PRIOR OR SUBSEQUENT?                         
         BNZ   MCNFM15P            COULD BE EITHER                              
         CLC   HALF,FULL           MISSED DATE MATCHES MONTH?                   
         BNE   MCNFM15N            NO, CHEXT THE NEXT MISSED SPOT               
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
         B     MCNFM20                                                          
*                                                                               
MCNFM15N BAS   RE,NEXTEL           NO MORE MISSED SPOTS?                        
         BNE   MCNFM17             CHECK MG SPOTS THEN                          
         B     MCNFM15B            OTHERWISE GO BACK FOR MORE                   
*                                                                               
MCNFM15P TM    ESTLOKYM+1,X'80'    THE MONTH AND PRIOR?                         
         BZ    MCNFM15S            NO, THE MONTH AND SUBSEQUENT                 
         CLC   FULL(2),HALF        MISSED DATE <= LOCKED MONTH?                 
         BH    MCNFM15N            NO, HIGHER                                   
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
         B     MCNFM20                                                          
*                                                                               
MCNFM15S CLC   FULL(2),HALF        MISSED DATE >= LOCKED MONTH?                 
         BL    MCNFM15N            NO, CHEXT THE NEXT MISSED SPOT               
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
         B     MCNFM20                                                          
*                                                                               
MCNFM17  BRAS  RE,GETOFFER                                                      
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,MORFRST-MOKEY(R6)                                             
         MVI   ELCODE,MOBDELQ      LOOK FOR MAKEGOOD SPOTS (X'50')              
         BAS   RE,FIRSTEL                                                       
         BNE   MCNFM17X            NO MISSED SPOTS, CHECK MG SPOTS              
*                                                                               
         USING MOBDELD,R6                                                       
MCNFM17B GOTO1 VDATCON,DMCB,(8,MOBDBDAT),(8,WORK)                               
         MVC   WORK+8(2),=C'-('                                                 
         XR    R0,R0                                                            
         IC    R0,MOBDNWKS                                                      
         CVD   R0,DUB                                                           
         UNPK  WORK+10(2),DUB      CAN'T HAVE MORE THAN 99 WKS                  
         OI    WORK+11,X'F0'                                                    
         MVC   WORK+12(2),=C'W)'                                                
         LA    R0,WORK                                                          
         ST    R0,DMCB                                                          
         MVI   DMCB,14                                                          
         L     R1,SRPARMSD.SRQACOMF    R1 = A(COMFACS)                          
         USING COMFACSD,R1                                                      
         L     RF,CPERVAL                                                       
         DROP  R1                                                               
         GOTO1 (RF),DMCB,,(X'10',ELEM)  LESS 1 DAY FOR THE END DATE             
*                                                                               
         LA    R1,ELEM                                                          
         USING PERVALD,R1                                                       
         MVC   FULL(2),PVALBSTA    SAVE YM OF THE START DATE                    
         MVC   FULL+2(2),PVALBEND  SAVE YM OF THE END   DATE                    
         DROP  R1                                                               
*                                                                               
         TM    ESTLOKYM+1,X'C0'    PRIOR OR SUBSEQUENT?                         
         BNZ   MCNFM17P            COULD BE EITHER                              
         CLC   HALF,FULL           MAKEGOOD DATE MATCHES MONTH?                 
         BE    *+14                                                             
         CLC   HALF,FULL+2                                                      
         BNE   MCNFM17N            NO, CHEXT THE NEXT MAKEGOOD SPOT             
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
         B     MCNFM17X                                                         
*                                                                               
MCNFM17N BAS   RE,NEXTEL           NO MORE MISSED SPOTS?                        
         BNE   MCNFM17X            CHECK MG SPOTS THEN                          
         B     MCNFM17B            OTHERWISE GO BACK FOR MORE                   
*                                                                               
MCNFM17P TM    ESTLOKYM+1,X'80'    THE MONTH AND PRIOR?                         
         BZ    MCNFM17S            NO, THE MONTH AND SUBSEQUENT                 
         CLC   FULL(2),HALF        MAKEGOOD START DATE <= MONTH?                
         BH    MCNFM17N            NO, NEXT MAKEGOOD SPOT                       
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
         B     MCNFM17X                                                         
*                                                                               
MCNFM17S CLC   FULL+2(2),HALF      MAKEGOOD END DATE >= LOCKED MONTH?           
         BL    MCNFM17N            NO, NEXT MAKEGOOD SPOT                       
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
*                                                                               
MCNFM17X BRAS  RE,GETNOTCE         RESTORE THE NOTICE RECORD TO AIO1            
*                                                                               
MCNFM20  L     R6,AIO1             MIGHT OF SKIPPED DELIVERED STATUS            
         LA    R6,MNRFRST-MNKEY(R6)                                             
         MVI   ELCODE,MNSTELQ                                                   
         BAS   RE,FIRSTEL                                                       
         USING MNSTELD,R6                                                       
*                                                                               
         XC    ELEM,ELEM                                                        
MCNFM20D USING MNSTELD,ELEM                                                     
         MVI   MCNFM20D.MNSTEL,MNSTELQ                                          
         MVI   MCNFM20D.MNSTLEN,MNSTLENQ                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    MCNFM25                                                          
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,WORK,WORK,F'1'                                       
*                                                                               
MCNFM25  GOTO1 VDATCON,DMCB,(0,WORK),(19,MCNFM20D.MNSTDATE)                     
         L     R1,PACKOF4B                                                      
         SRL   R1,12                                                            
         STCM  R1,3,MCNFM20D.MNSTTIME                                           
*                                                                               
         MVI   MCNFM20D.MNSTSTAT,MNSTGOIN   GOING TO BE OKAYED                  
         TM    BITFLAG2,BF2ELCKD+BF2PWLCK                                       
         BZ    *+8                                                              
         MVI   MCNFM20D.MNSTSTAT,MNSTHOLD   HOLD, EST OR PW LOCKED              
         CLI   MNSTSTAT,MNSTSAPP            WAS IT SELF APPLIED?                
         BNE   *+8                                                              
         MVI   MCNFM20D.MNSTSTAT,MNSTOKAY   YES, THE OKAY!                      
*                                                                               
         MVC   SVSTAT,MCNFM20D.MNSTSTAT     SAVE FOR LATER                      
         MVI   THISISMG,C'Y'                                                    
         DROP  MCNFM20D                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)  ADD STATUS                    
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                  ESTIMATE OR PW LOCKED?                       
         TM    BITFLAG2,BF2ELCKD+BF2PWLCK                                       
         BNZ   MCNFMYES            YES, LET USER START SCRIPT PROCESS           
         CLI   SVSTAT,MNSTOKAY     SELF APPLIED? OKAY == SELFAPPLIED            
         BE    MCNFMYES            YES, MAKEGOOD ALREADY APPLIED                
         BAS   RE,WRKRCREA                                                      
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPMGEACC'                                            
         MVI   18(R1),C'I'                                                      
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'                                                      
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000001'                                               
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
*                                                                               
         MVC   30(8,R1),MOCFTOID                                                
         TM    BITFLAG1,BF1PSSWD   PASSWORD REQUIRED FOR SPOT SYSTEM?           
         BZ    *+10                                                             
         MVC   38(3,R1),=C'DDS'    YES, THEN USE THE EVER USEFUL DDS            
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
*&&DO                                                                           
         MVC   30(8,R1),=CL8'NEWSTYLE'                                          
         MVC   30+16(8,R1),MOCFTOID                                             
         MVC   30+16+8(9,R1),=CL9',$SPTDARE'                                    
         MVC   30+60(4,R1),=C'T214'   USERID+PID (44 BYTES)                     
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'98'        94 + 4 BYTES FOR QSAM                      
*&&                                                                             
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000002'                                               
         MVC   10(20,R1),=CL20'$SPTDARE TO SPOT/BUY'                            
         LA    R1,30(R1)                                                        
         USING LAYOUTD,R1                                                       
         MVC   LAYMED,QMED                                                      
         MVC   LAYBYR,QBUYER                                                    
         MVC   LAYCLT,QCLT                                                      
*                                                                               
         MVC   LAYPRD,QPRD1                                                     
         BRAS  RE,ISITPOLE         IS IT A BRAND ON A POL ESTIMATE?             
         BNE   *+10                NO                                           
         MVC   LAYPRD,=C'POL'      YES, MUST HAVE POL IN BUY HEADER             
*                                                                               
         MVC   LAYEST,QEST1                                                     
         MVC   LAYSTA,QSTA                                                      
*                                                                               
         MVC   LAYFLT,QFLTNUM      EXTRA FIELDS NEEDED BY MGEACC                
         MVC   LAYGRPCD,QMGGROUP                                                
         MVC   LAYCORT,QCSHTRDE                                                 
         MVC   LAYPRD1,QPRD1                                                    
         OC    LAYPRD1,=CL3'   '                                                
         MVC   LAYPRD2,QPRD2                                                    
         OC    LAYPRD2,=CL3'   '                                                
*                                                                               
         CLC   LAYPRD2,=CL3'   '   PIGGYBACK?                                   
         BNH   *+10                                                             
         MVC   LAYPRD,=C'POL'      YEAH, NEEDS TO BE POL FOR SPMGEACC           
*                                                                               
         LA    R1,LAYEND                                                        
         DROP  R1                                                               
         L     RE,AIO2                                                          
         SR    R1,RE                                                            
         STH   R1,0(RE)            L'DATA + 4 BYTES FOR QSAM                    
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         BAS   RE,WRKRCLOS                                                      
*                                                                               
         L     RE,AWRKRIOA         RESET WORKER IO AREA FOR EDICT RECS          
         MVI   0(RE),0                                                          
         MVI   1(RE),2                                                          
*                                                                               
MCNFMYES BRAS  RE,BLDCOLOR                                                      
         BAS   RE,DAREMAIL                                                      
         J     YES                                                              
*                                                                               
MCNFMNO  J     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* TEST IF ESTIMATE IS LOCKED                                                    
***********************************************************************         
ESTLOCKD NTR1                                                                   
         XC    ESTPW,ESTPW         CLEAR OUT ANY PW PCT                         
         XC    ESTCOST2,ESTCOST2   CLEAR OUT ANY COST2 PCT                      
         XC    ESTLOKYM,ESTLOKYM   CLEAR OUT ANY LOCK YEAR/MONTH                
         MVI   BYTE,0                                                           
         NI    BITFLAG2,X'FF'-BF2ELCKD-BF2PWLCK                                 
*                                                                               
ELCKD10  LA    R6,KEY                                                           
         USING EKEY,R6                                                          
         XC    KEY,KEY                                                          
         MVC   EKEYAM,BAGYMD       TEST IF POL EST LOCKED FIRST                 
         MVC   EKEYCLT,BCLT                                                     
*                                                                               
         CLI   BYTE,0                                                           
         BNE   *+14                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         B     ELCKD15                                                          
*                                                                               
         CLI   BYTE,1                                                           
         BNE   *+14                                                             
         MVC   EKEYPRD,QPRD1                                                    
         B     ELCKD15                                                          
*                                                                               
         CLI   BYTE,2                                                           
         BNE   *+10                                                             
         MVC   EKEYPRD,QPRD2                                                    
*                                                                               
ELCKD15  OC    EKEYPRD,EKEYPRD     CAN'T HAVE A NULL PRODUCT                    
         BZ    ELCKDX                                                           
*                                                                               
         MVC   EKEYEST,BEST                                                     
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(ELEN-EKEY),KEYSAVE                                           
         BNE   ELCKD20                                                          
         TM    KEY+L'EKEY,X'0C'    HELD OR LOCKED?                              
         BZ    *+12                                                             
         OI    BITFLAG2,BF2ELCKD   YES                                          
         B     ELCKDX                                                           
*                                                                               
         OC    ESTPW,ESTPW         DO WE HAVE PW PCT ALREADY?                   
         BNZ   ELCKD20             YES                                          
         OC    ESTCOST2,ESTCOST2   DO WE HAVE PW PCT ALREADY?                   
         BNZ   ELCKD20             YES                                          
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
         CLI   DMCB+8,0                                                         
         BE    ELCKD17                                                          
         LHI   R1,*-T16300         DELETED RECORD BUT KEY IS NOT                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
ELCKD17  L     R6,AIO1                                                          
         USING ESTHDR,R6                                                        
         MVC   ESTPW,EPWPCT        COPY THE PW PCT                              
         MVC   ESTCOST2,ECOST2     COPY THE COST2 PCT                           
*                                                                               
         OC    ESTLOKYM,ESTLOKYM                                                
         BNZ   *+10                                                             
         MVC   ESTLOKYM,ELOCKYM    COPY THE LOCKYM                              
         DROP  R6                                                               
*                                                                               
ELCKD20  CLI   BYTE,2              TEST ALL PRODUCT COMBINATIONS?               
         BNL   ELCKDX              YES, EST IS NOT LOCKED                       
*                                                                               
         ZIC   R1,BYTE             BUMP TO NEXT PRODUCT                         
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         B     ELCKD10                                                          
*                                                                               
ELCKDX   J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ PW STATUS RECORD                                              
***********************************************************************         
GETPWC2  NTR1                                                                   
         BAS   RE,FNDMRKT          FIND MARKET NUMBER FOR STATION               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,BAGYMD                                                   
         MVC   PWKCLT,BCLT                                                      
         MVC   PWKPRD,BPRD                                                      
         MVC   PWKEST,BEST                                                      
         MVC   PWKMKT,BMKTSTA                                                   
         DROP  R6                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE       TEST FOUND                                 
         BNE   GETPWX                IF NOT, CAN'T BE LOCKED                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO1                                                          
         USING PWRECD,R6                                                        
         TM    PWGNFLG,X'C0'         ANY BUY LOCKED BITS?                       
         BZ    *+8                                                              
         OI    BITFLAG2,BF2PWLCK     YES                                        
         DROP  R6                                                               
*                                                                               
GETPWX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ STATION RECORD FOR THE MARKET NUMBER                          
***********************************************************************         
FNDMRKT  NTR1                                                                   
         XC    KEY,KEY             READ STATION MASTER RECORD                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         LA    R6,KEY                                                           
         USING STAKEY,R6                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,QSTA                                                    
         CLI   STAKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
*                                                                               
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLT                                                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,STATION,KEY,AIO1                            
         L     R6,AIO1                                                          
         CLC   KEY(STAKCLT-STAKEY),0(R6)  SAME UPTO CLIENT?                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   STAKCLT,QCLT                                                     
         BE    FMRKT10                                                          
         LA    R6,KEY                                                           
         MVI   STAKCLT,C'0'                                                     
         MVC   STAKCLT+1(L'STAKCLT-1),STAKCLT                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,STATION,KEY,AIO1                            
         CLC   KEY(STAKFILL-STAKEY),0(R6)  SAME UPTO CLIENT?                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FMRKT10  L     R6,AIO1                                                          
         USING STARECD,R6                                                       
         GOTO1 VMSPACK,DMCB,SMKT,STAKCALL,BMKTSTA                               
         DROP  R6                                                               
*                                                                               
FMRKTX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FIGURES OUT IF THE ESTIMATE IS A POL ESTIMATE                                 
*                                                                               
* ON EXIT:     (CC)                EQ, BRAND ON A POL EST                       
*                                  NE, NOT BRAND ON POL EST                     
***********************************************************************         
ISITPOLE NTR1  BASE=*,LABEL=*                                                   
         CLI   SVCPRF00,C'0'       BRAND BUYING CLIENT?                         
         JNE   ISITPNO             NO                                           
*                                                                               
         XC    KEY,KEY             YES, DO WE HAVE A POL ESTIMATE?              
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BEST                                                     
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'EKEY),KEYSAVE                                              
         JNE   ISITPNO             DON'T HAVE A POL ESTIMATE                    
*                                                                               
ISITPYES J     YES                 GOT A POL ESTIMATE                           
*                                                                               
ISITPNO  J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE FAX DELIVERY NOTIFICATION                                       
***********************************************************************         
DAREDLFX NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING RDLNFAXD,R7                                                      
DLFAXD   USING RTN2SNDR,RDFXRTRN                                                
*                                                                               
         MVC   AGENCY,DLFAXD.RTNPWRCD                                           
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   DLFAXD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,DLFAXD.RTNAGYMD,BAGYMD,2                             
*                                                                               
         MVC   QRETURN,RDFXRTRN    COPY THIS VALUE                              
         NI    MISCFLG1,X'FF'-MF1XMTUP   NO UPDATES TO XMT ELEM                 
         DROP  DLFAXD                                                           
*                                                                               
         GOTO1 CALCORDR,DMCB,RDFXORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   DLFAXNO                                                          
*                                                                               
         MVC   USERID,RDFXTOID                                                  
         BAS   RE,SWTCHSPT         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   DLFAXNO             NO                                           
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   DLFAXNO                                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
DLFAX10  CLI   0(R6),0                                                          
         BNE   DLFAX15                                                          
         LHI   R1,REFORDNT         ORD NOT TRANSMITTED                          
         BRAS  RE,SNDERROR                                                      
         B     DLFAXNO                                                          
*                                                                               
DLFAX15  CLI   0(R6),DOXMTELQ                                                   
         BE    DLFAX20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DLFAX10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
DLFAX20  GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOXMTDND,L'RDFXTIME                   
*                                                                               
         OC    DOXMTDND,DOXMTDND   IF NO DELIVERY DATE OR TIME ALREADY          
         BZ    DLFAX30                                                          
         OC    DOXMTDNT,DOXMTDNT                                                
         BZ    DLFAX30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTDND,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    DLFAXNO                    YES, IGNORE THIS RECORD               
         BL    DLFAX30                    NO, OLDER                             
         CLC   DOXMTDNT,DUB+L'DOXMTDND    IS ELEM'S TIME MORE RECENT?           
         BNL   DLFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
DLFAX30  XC    DOXMTDND,DOXMTDND                                                
         MVC   DOXMTDNT,DLNFRID    SAVE WHERE DELNOT CAME FROM                  
*                                                                               
         MVC   DOXMTSTD,DUB                                                     
         MVC   DOXMTSTT,DUB+L'DOXMTDND                                          
         MVI   DOXMTSTA,QFAXDLVD                                                
         CLC   DOXMTDID,=X'FFFD'   IS IT ACTUALLY AN EMAIL?                     
         BNE   *+8                                                              
         MVI   DOXMTSTA,DEMDLVD                                                 
         MVC   SVSTAT,DOXMTSTA                                                  
         DROP  R6                                                               
*                                                                               
DLFAX99  OI    MISCFLG1,MF1XMTUP                                                
*                                                                               
DLFAX100 SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
DLFAX110 CLI   0(R6),0                                                          
         BNE   DLFAX115                                                         
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    DLFAXNO                    YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFORDNT         ORD NOT TRANSMITTED                          
         BRAS  RE,SNDERROR                                                      
         B     DLFAXNO                                                          
*                                                                               
DLFAX115 CLI   0(R6),DOSTELQ                                                    
         BE    DLFAX120                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DLFAX110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
DLFAX120 GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOSTDATE,L'RDFXTIME                   
*                                                                               
         CLI   DOSTSTAT,DFXSENT                                                 
         BE    DLFAX140                                                         
         CLI   DOSTSTAT,DFXRSNT                                                 
         BE    DLFAX140                                                         
         CLI   DOSTSTAT,DEMSENT                                                 
         BE    DLFAX140                                                         
*                                                                               
         CLC   DOSTDATE,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    DLFAXNO                    YES, IGNORE THIS RECORD               
         BL    DLFAX130                   NO, OLDER                             
         CLC   DOSTTIME,DUB+L'DOSTDATE    IS ELEM'S TIME MORE RECENT?           
         BNL   DLFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
DLFAX130 CLI   DOSTSTAT,DFXDLVD                                                 
         BE    *+12                                                             
         CLI   DOSTSTAT,DEMDLVD                                                 
         BNE   DLFAXNO                                                          
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVC   DOSTIDNM,DLNFRID                                                 
         MVC   SVSTAT,DOSTSTAT                                                  
         CLI   DOSTSTAT,DEMDLVD                                                 
         BNE   DLFAX190                                                         
         MVC   DOSTIDNM,=X'FFFD'                                                
         B     DLFAX190                                                         
*                                                                               
DLFAX140 LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
DLFXDOST USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DLFXDOST.DOSTEL,DOSTELQ                                          
         MVI   DLFXDOST.DOSTLEN,DOSTLNQ2                                        
         MVC   DLFXDOST.DOSTDATE,DUB                                            
         MVC   DLFXDOST.DOSTTIME,DUB+L'DOSTDATE                                 
         MVI   DLFXDOST.DOSTSTAT,DFXDLVD                                        
         MVC   DLFXDOST.DOSTIDNM,DLNFRID                                        
         CLI   DOSTSTAT,DEMSENT                                                 
         BNE   DLFAX145                                                         
         MVI   DLFXDOST.DOSTSTAT,DEMDLVD                                        
         MVC   DLFXDOST.DOSTIDNM,=X'FFFD'                                       
*                                                                               
DLFAX145 MVC   SVSTAT,DLFXDOST.DOSTSTAT                                         
         DROP  DLFXDOST,R6                                                      
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
DLFAX150 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    DLFAX160         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    DLFAX160                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   DLFAX160                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DLFAX150                                                         
*                                                                               
DLFAX160 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
DLFAX190 XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
DLFAXYES BRAS  RE,BLDCOLOR                                                      
         BAS   RE,DAREMAIL                                                      
         J     YES                                                              
*                                                                               
DLFAXNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   DLFAX190                                                         
         J     NO                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE FAX CANCELLATION                                                
***********************************************************************         
DARECNFX NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING RDLNFAXD,R7                                                      
CNFAXD   USING RTN2SNDR,RDFXRTRN                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   CNFAXD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,CNFAXD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,CNFAXD.RTNPWRCD                                           
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
         DROP  CNFAXD                                                           
*                                                                               
         MVC   QRETURN,RDFXRTRN    COPY THIS VALUE                              
*                                                                               
         GOTO1 CALCORDR,DMCB,RDFXORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   CNFAXNO                                                          
*                                                                               
         MVC   USERID,RDFXTOID                                                  
         BAS   RE,SWTCHSPT         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   CNFAXNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   CNFAXNO                                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT   ASSUME XMT ELEM EXISTS                 
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
CNFAX10  CLI   0(R6),0                                                          
         BNE   CNFAX15                                                          
         LHI   R1,REFORDNT          ORD NOT TRANSMITTED                         
         BRAS  RE,SNDERROR                                                      
         B     CNFAXNO                                                          
*                                                                               
CNFAX15  CLI   0(R6),DOXMTELQ                                                   
         BE    CNFAX20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFAX10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
CNFAX20  GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOXMTDND,L'RDFXTIME                   
*                                                                               
         OC    DOXMTDND,DOXMTDND   IF NO DELIVERY DATE OR TIME ALREADY          
         BZ    CNFAX30                                                          
         OC    DOXMTDNT,DOXMTDNT                                                
         BZ    CNFAX30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTDND,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    CNFAXNO                    YES, IGNORE THIS RECORD               
         BL    CNFAX30                    NO, OLDER                             
         CLC   DOXMTDNT,DUB+L'DOXMTDND    IS ELEM'S TIME MORE RECENT?           
         BNL   CNFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
CNFAX30  MVC   DOXMTDND,DUB                                                     
         MVC   DOXMTDNT,DUB+L'DOXMTDND                                          
*                                                                               
         MVC   DOXMTSTD,DOXMTDND                                                
         MVC   DOXMTSTT,DOXMTDNT                                                
         MVI   DOXMTSTA,QFAXCNCL                                                
         CLC   DOXMTDID,=X'FFFD'          IS AN EMAIL?                          
         BNE   *+8                                                              
         MVI   DOXMTSTA,QERRORED                                                
         MVC   SVSTAT,DOXMTSTA                                                  
         DROP  R6                                                               
*                                                                               
CNFAX99  OI    MISCFLG1,MF1XMTUP                                                
*                                                                               
CNFAX100 SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
CNFAX110 CLI   0(R6),0                                                          
         BNE   CNFAX115                                                         
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    CNFAXNO                    YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFORDNT         ORD NOT TRANSMITTED                          
         BRAS  RE,SNDERROR                                                      
         B     CNFAXNO                                                          
*                                                                               
CNFAX115 CLI   0(R6),DOSTELQ                                                    
         BE    CNFAX120                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFAX110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
CNFAX120 GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOSTDATE,L'RDFXTIME                   
*                                                                               
         CLI   DOSTSTAT,DFXSENT                                                 
         BE    CNFAX140                                                         
         CLI   DOSTSTAT,DFXRSNT                                                 
         BE    CNFAX140                                                         
         CLI   DOSTSTAT,DEMSENT                                                 
         BE    CNFAX140                                                         
*                                                                               
         CLC   DOSTDATE,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    CNFAXNO                    YES, IGNORE THIS RECORD               
         BL    CNFAX130                   NO, OLDER                             
         CLC   DOSTTIME,DUB+L'DOSTDATE    IS ELEM'S TIME MORE RECENT?           
         BNL   CNFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
CNFAX130 CLI   DOSTSTAT,QFAXCNCL                                                
         BE    CNFAX135                                                         
         CLI   DOSTSTAT,QERRORED                                                
         BNE   CNFAXNO                                                          
CNFAX135 MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVC   SVSTAT,DOSTSTAT                                                  
         B     CNFAX190                                                         
*                                                                               
CNFAX140 LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
CNFXDOST USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   CNFXDOST.DOSTEL,DOSTELQ                                          
         MVI   CNFXDOST.DOSTLEN,DOSTLNQ                                         
         MVC   CNFXDOST.DOSTDATE,DUB                                            
         MVC   CNFXDOST.DOSTTIME,DUB+L'DOSTDATE                                 
         MVI   CNFXDOST.DOSTSTAT,QFAXCNCL                                       
         CLI   DOSTSTAT,DEMSENT       ??? IS AN EMAIL???                        
         BNE   CNFAX145                                                         
         MVI   CNFXDOST.DOSTLEN,DOSTLNQ                                         
         MVI   CNFXDOST.DOSTSTAT,QERRORED   YES: SHOW AS ERROR                  
*                                                                               
CNFAX145 MVC   SVSTAT,CNFXDOST.DOSTSTAT                                         
         DROP  CNFXDOST,R6                                                      
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
CNFAX150 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    CNFAX160         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    CNFAX160                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   CNFAX160                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFAX150                                                         
*                                                                               
CNFAX160 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
CNFAX190 XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
CNFAXYES BRAS  RE,BLDCOLOR                                                      
         BAS   RE,DAREMAIL                                                      
         J     YES                                                              
*                                                                               
CNFAXNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   CNFAX190                                                         
         J     NO                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE FAX ERROR                                                       
***********************************************************************         
DAREERFX NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING RDLNFAXD,R7                                                      
ERFAXD   USING RTN2SNDR,RDFXRTRN                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   ERFAXD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,ERFAXD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,ERFAXD.RTNPWRCD                                           
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
         DROP  ERFAXD                                                           
*                                                                               
         MVC   QRETURN,RDFXRTRN    COPY THIS VALUE                              
*                                                                               
         GOTO1 CALCORDR,DMCB,RDFXORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   ERFAXNO                                                          
*                                                                               
         MVC   USERID,RDFXTOID                                                  
         BAS   RE,SWTCHSPT         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   ERFAXNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   ERFAXNO                                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
ERFAX10  CLI   0(R6),0                                                          
         BNE   ERFAX15                                                          
         LHI   R1,REFORDNT          ORD NOT TRANSMITTED                         
         BRAS  RE,SNDERROR                                                      
         B     ERFAXNO                                                          
*                                                                               
ERFAX15  CLI   0(R6),DOXMTELQ                                                   
         BE    ERFAX20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ERFAX10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
ERFAX20  GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOXMTDND,L'RDFXTIME                   
*                                                                               
         OC    DOXMTDND,DOXMTDND   IF NO DELIVERY DATE OR TIME ALREADY          
         BZ    ERFAX30                                                          
         OC    DOXMTDNT,DOXMTDNT                                                
         BZ    ERFAX30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTDND,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    ERFAXNO                    YES, IGNORE THIS RECORD               
         BL    ERFAX30                    NO, OLDER                             
         CLC   DOXMTDNT,DUB+L'DOXMTDND    IS ELEM'S TIME MORE RECENT?           
         BNL   ERFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
ERFAX30  MVC   DOXMTDND,DUB                                                     
         MVC   DOXMTDNT,DUB+L'DOXMTDND                                          
*                                                                               
         MVC   DOXMTSTD,DOXMTDND                                                
         MVC   DOXMTSTT,DOXMTDNT                                                
         MVI   DOXMTSTA,QERRORED                                                
         MVI   SVSTAT,QERRORED                                                  
         DROP  R6                                                               
*                                                                               
ERFAX99  OI    MISCFLG1,MF1XMTUP                                                
*                                                                               
ERFAX100 SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
ERFAX110 CLI   0(R6),0                                                          
         BNE   ERFAX115                                                         
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    ERFAXNO                    YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFORDNT         ORD NOT TRANSMITTED                          
         BRAS  RE,SNDERROR                                                      
         B     ERFAXNO                                                          
*                                                                               
ERFAX115 CLI   0(R6),DOSTELQ                                                    
         BE    ERFAX120                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ERFAX110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
ERFAX120 GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOSTDATE,L'RDFXTIME                   
*                                                                               
         CLI   DOSTSTAT,DFXSENT                                                 
         BE    ERFAX140                                                         
         CLI   DOSTSTAT,DFXRSNT                                                 
         BE    ERFAX140                                                         
         CLI   DOSTSTAT,DEMSENT                                                 
         BE    ERFAX140                                                         
*                                                                               
         CLC   DOSTDATE,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    ERFAXNO                    YES, IGNORE THIS RECORD               
         BL    ERFAX130                   NO, OLDER                             
         CLC   DOSTTIME,DUB+L'DOSTDATE    IS ELEM'S TIME MORE RECENT?           
         BNL   ERFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
ERFAX130 CLI   DOSTSTAT,QERRORED                                                
         BNE   ERFAXNO                                                          
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         B     ERFAX190                                                         
*                                                                               
ERFAX140 LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
ERFXDOST USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   ERFXDOST.DOSTEL,DOSTELQ                                          
         MVI   ERFXDOST.DOSTLEN,DOSTLNQ                                         
         MVC   ERFXDOST.DOSTDATE,DUB                                            
         MVC   ERFXDOST.DOSTTIME,DUB+L'DOSTDATE                                 
         MVI   ERFXDOST.DOSTSTAT,QERRORED                                       
         DROP  ERFXDOST,R6                                                      
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
ERFAX150 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    ERFAX160         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    ERFAX160                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   ERFAX160                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ERFAX150                                                         
*                                                                               
ERFAX160 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
ERFAX190 MVI   SVSTAT,QERRORED                                                  
*                                                                               
ERFAX300 L     R6,AIO1             REMOVE ANY OLD ERROR COMMENTS                
         USING DOKEY,R6                                                         
         XR    R0,R0                                                            
         LA    R6,DORFRST                                                       
ERFAX305 CLI   0(R6),0                                                          
         BE    ERFAX315                                                         
         CLI   0(R6),DOCOMELQ                                                   
         BE    ERFAX310                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ERFAX305                                                         
ERFAX310 GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6)                                     
         B     ERFAX305                                                         
*                                                                               
ERFAX315 LA    R2,ELEM             ADD THE ERROR COMMENT TO RECORD              
         USING DOCOMELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCOMEL,DOCOMELQ                                                 
         MVI   DOCOMLEN,DOCOMOVH+L'RDNTEFLG                                     
         MVC   DOCOMTXT(3),RDFXERR  COPY THE NUMBER FOR NOW                     
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
ERFAXYES BRAS  RE,BLDCOLOR                                                      
         BAS   RE,DAREMAIL                                                      
         J     YES                                                              
*                                                                               
ERFAXNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   ERFAX300                                                         
         J     NO                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE MAKEGOOD HEADER                                                 
***********************************************************************         
DAREMKGX NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING PMKGHDRD,R7                                                      
MKHDRD   USING RTN2SNDR,PMKGRTNS                                                
         MVI   QMED,C'T'                                                        
         CLI   MKHDRD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,MKHDRD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,MKHDRD.RTNPWRCD                                           
         DROP  MKHDRD                                                           
*                                                                               
         MVI   DMCB,X'01'          SWITCH TO SERVICE SYSTEM                     
         BAS   RE,SWTCHSYS           OTHERWISE DEATH IN CALLOV                  
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'01'                                                       
         GOTO1 VCALLOV,DMCB                  GET PHASE-LIST ADDRESS             
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                DIE ON ERROR                                 
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,SRPARMS             RUN $DMG (T16101)                  
*                                                                               
MKHDRYES J     YES                                                              
*                                                                               
MKHDRNO  J     NO                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS PROCESSES THE APPROVAL ORDER                                             
***********************************************************************         
APPROVAL NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)                                                         
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         MVI   QMED,C'T'                                                        
         TM    MISCFLG3,MF3RADIO   AM I RADIO?                                  
         BZ    APPRV10                                                          
         MVI   QMED,C'R'                                                        
*                                                                               
         L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         ZIC   R2,0(R3)            R2 = L(ORDER APPROVAL HEADER)                
         LA    R7,1(R3)            R7 = A(ORDER APPROVAL HEADER)                
*                                                                               
APPRV10  DS    0H                                                               
         USING RORDAPPD,R7                                                      
APPRVD   USING RTN2SNDR,ROAPRTRN                                                
         GOTO1 VHEXIN,DMCB,APPRVD.RTNAGYMD,BAGYMD,2                             
*                                                                               
APPRV20  DS    0H                                                               
         MVC   AGENCY,APPRVD.RTNPWRCD                                           
         NI    MISCFLG1,X'FF'-MF1XMTUP NO UPDATES TO XMT ELEM                   
         DROP  APPRVD                                                           
*                                                                               
         MVC   QREPCON,ROAPRPCN    COPY THESE VALUES                            
         MVC   QRETURN,ROAPRTRN                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,ROAPORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   APPRVNO                                                          
*                                                                               
         MVC   USERID,ROAPTOID                                                  
         BAS   RE,SWTCHSPT         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   APPRVNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   APPRVNO                                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT  ASSUME XMT ELEM EXISTS                  
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   DOIDCON,ROAPRPCN    SAVE THE REP CONTRACT NUMBER                 
         DROP  R6                                                               
*                                                                               
APPRV40  BRAS  RE,PTXMTVER                                                      
         BE    APPRV50                                                          
         LHI   R1,REFORDNT         ORDER NOT TRANSMITTED                        
         BRAS  RE,SNDERROR                                                      
         B     APPRVNO                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
APPRV50  GOTO1 VDATCON,DMCB,(0,ROAPDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,ROAPTIME,DUB+L'DOXMTSTD,L'ROAPTIME                   
*                                                                               
         OC    DOXMTSTD,DOXMTSTD   IF NO DATE OR TIME ALREADY                   
         BZ    APPRV90                                                          
         OC    DOXMTSTT,DOXMTSTT                                                
         BZ    APPRV60             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTSTD,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    APPRVNO                    YES, IGNORE THIS RECORD               
         BL    APPRV60                    NO, OLDER                             
         CLC   DOXMTSTT,DUB+L'DOXMTSTD    IS ELEM'S TIME MORE RECENT?           
         BNH   APPRV60                                                          
         CLC   QREPCON,=C'00000000'    HAVE A CONTRACT IN THE MESSAGE?          
         BNH   APPRVNO                                                          
         B     APPRV99                    UPDATE CONTRACT NUMBER                
*                                                                               
APPRV60  CLI   DOXMTSTA,QAPP       CAN'T CHANGE ANYTHING BUT APPROVED           
         BE    APPRV90                STATUS                                    
*                                                                               
         CLI   DOXMTSTA,QSNTPNDG   IF SENT PENDING, DON'T MARK X'11'            
         BE    APPRV100               BUT MARK X'12'                            
*                                                                               
         CLI   DOXMTSTA,QERRORED   ERROR STATUS?                                
         BNE   APPRV70                                                          
         BRAS  RE,GET30TXT         GET X'30' TEXT AND PUT IN BLOCK              
         BNE   APPRV70                                                          
         CLC   =C'READ TIMED OUT',BLOCK                                         
         BE    APPRV90             ALLOW THIS                                   
*                                                                               
APPRV70  CLI   DOXMTSTA,QRECALL    IF RECALLING, DON'T MARK X'11'               
         BL    APPRV80                BUT MARK X'12'                            
         BE    APPRV100            O/W RECALL STATUSES THEN DON'T MARK          
         CLI   DOXMTSTA,QRCLUNKN      RECORD AS APPROVED                        
         BNH   APPRVNO                                                          
         CLI   DOXMTSTA,QRCLTRNS                                                
         BE    APPRVNO                                                          
*                                                                               
APPRV80  LHI   R1,REFCCHNG                                                      
         BRAS  RE,SNDERROR                                                      
         B     APPRVNO                                                          
*                                                                               
APPRV90  MVC   DOXMTSTD,DUB                                                     
         MVC   DOXMTSTT,DUB+L'DOXMTSTD                                          
         MVI   DOXMTSTA,QAPP       APPROVED                                     
         MVI   SVSTAT,QAPP         SAVE FOR LATER                               
         DROP  R6                                                               
*                                                                               
APPRV99  OI    MISCFLG1,MF1XMTUP                                                
***************                                                                 
* TIME TO PROCESS THE STATUS HISTORY ELEMENTS                                   
***************                                                                 
APPRV100 NI    BITFLAG3,X'FF'-BF3RCLNG-BF3DLVRD                                 
*                                  WE KNOW WE HAVE TRANSMISSIONS                
         BRAS  RE,PTSTAVER         R6 = A(SEND CLUSTER)                         
*                                                                               
         SR    R0,R0                                                            
APPRV105 CLI   0(R6),DOSTELQ                                                    
         BE    APPRV120                                                         
APPRV110 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     APPRV105                                                         
*                                                                               
         USING DOSTELD,R6                                                       
APPRV120 CLI   DOSTSTAT,DDLVRD     IF DELIVERED, SKIP IT                        
         BNE   APPRV130                                                         
         OI    BITFLAG3,BF3DLVRD                                                
         ST    R6,FULL             DON'T CLOBBER FULL, NEED THIS!!!             
         B     APPRV110                                                         
*                                                                               
APPRV130 CLI   DOSTSTAT,QSNTPNDG   IS IT QSNTPNDG?                              
         BE    APPRV110            BUMP TO NEXT ONE                             
*                                                                               
         GOTO1 VDATCON,DMCB,(0,ROAPDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,ROAPTIME,DUB+L'DOSTDATE,L'ROAPTIME                   
*                                                                               
         CLI   DOSTSTAT,DSENT      RECENTLY SENT?                               
         BE    APPRV190            YES, PUT APPROVAL ABOVE SENT IN REC          
*                                                                               
         CLC   DOSTDATE,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    APPRVNO                    YES, IGNORE THIS RECORD               
         BL    APPRV140                   NO, OLDER                             
         CLC   DOSTTIME,DUB+L'DOSTTIME    IS ELEM'S TIME MORE RECENT?           
         BH    APPRVNO                    YES, IGNORE THIS REC                  
*                                                                               
APPRV140 CLI   DOSTSTAT,QAPP                                                    
         BE    APPRV180                                                         
*                                                                               
         CLI   DOSTSTAT,QERRORED   ERROR STATUS?                                
         BNE   APPRV150                                                         
         BRAS  RE,GET30TXT         GET X'30' TEXT AND PUT IN BLOCK              
         BNE   APPRV170             NO COMMENT                                  
         CLC   =C'READ TIMED OUT',BLOCK                                         
         BE    APPRV190            ALLOW THIS                                   
*                                                                               
APPRV150 CLI   DOSTSTAT,QRECALL    IF RECALL STATUSES THEN DON'T MARK           
         BL    APPRV170               RECORD AS APPROVED                        
         BNE   APPRV160                                                         
         OI    BITFLAG3,BF3RCLNG   RECALL WAS JUST SENT                         
         TM    BITFLAG3,BF3DLVRD   WAS IT ALREADY DELIVERD?                     
         BNZ   APPRV110            YES, FULL SAVED ALREADY                      
         ST    R6,FULL             DON'T CLOBBER FULL, WE NEED THIS!!!          
         B     APPRV110                                                         
*                                                                               
APPRV160 CLI   DOSTSTAT,QRCLUNKN   IF RECALL STATUSES THEN DON'T MARK           
         BNH   APPRVNO                RECORD AS APPROVED                        
         CLI   DOSTSTAT,QRCLTRNS                                                
         BE    APPRVNO                                                          
*                                                                               
APPRV170 TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    APPRVNO                    YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFCCHNG                CAN'T CHANGE THE RECORD               
         BRAS  RE,SNDERROR                                                      
         B     APPRVNO                                                          
*                                                                               
APPRV180 MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QAPP                                                    
         MVC   DOSAELEM(DOSTLNQ2),DOSTEL                                        
         B     APPRV245                                                         
         DROP  R6                                                               
*                                                                               
APPRV190 DS    0H                                                               
         TM    BITFLAG3,BF3DLVRD   WAS IT ALREADY DELIVERD?                     
         BO    APPRV230             YES                                         
*                                                                               
* ADD IN THE MISSING DELNOT BUT ONLY IF THE MSGVERNO IS CURRENT                 
*                                                                               
         LA    R4,DOSTELEM                                                      
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ2                                                 
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,DDLVRD                                                  
         MVI   SVSTAT,DDLVRD                                                    
                                                                                
         CLI   QMED,C'R'           CURRENTLY ONLY KATZ RADIO                    
         BE    *+14                AS THEY MIGHT NOT HAVE IDS ANYMORE           
         MVC   DOSTIDNM,DLNFRID    SAVE WHERE DELNOT CAME FROM                  
         B     APPRV220                                                         
                                                                                
         MVI   DOSTLEN,DOSTLNQ6    LONGER LENGTH FOR REP PREFIX/OFFICE          
         L     RE,AIO1                                                          
         LA    RE,DORFRST-DOKEY(RE)                                             
         XR    R0,R0                                                            
APPRV200 CLI   0(RE),0             LOOK FOR DOWIG ELEM                          
         BE    APPRV220            DON'T HAVE IT SO LEAVE BLANK FOR NOW         
*                                                                               
         CLI   0(RE),DOSPELQ       X'03'                                        
         BNE   *+14                                                             
         CLC   MSGVERNO,DOSPVER#-DOSPELD(RE)  PROCESSING SAME VERSION?          
         BNE   APPRV230                       NO, SKIP ADDING DELNOT            
*                                                                               
         CLI   0(RE),DOWIGELQ      X'50'                                        
         BE    APPRV210                                                         
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     APPRV200                                                         
                                                                                
         USING DOWIGELD,RE                                                      
APPRV210 MVC   DOSTDRPP,DOWIGRPP                                                
         MVC   DOSTDRPO,DOWIGRPO                                                
         DROP  R4,RE                                                            
                                                                                
         BRAS  RE,PTSTAVER         R6 = A(INSERT STATUS ELEM)                   
*                                                                               
APPRV220 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
         B     APPRV235                                                         
*                                                                               
APPRV230 LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
APPRV235 MVI   DOSTLEN,DOSTLNQ                                                  
         MVI   DOSTSTAT,QAPP                                                    
         MVC   DOSAELEM(DOSTLNQ2),DOSTEL                                        
         DROP  R4                                                               
*                                                                               
         BRAS  RE,PTSTAVER         R6 = A(INSERT STATUS ELEM)                   
*                                                                               
         TM    BITFLAG3,BF3RCLNG   DID WE PREVIOUSLY RECALL?                    
         BZ    APPRV240                                                         
         L     R6,FULL             YES, INSERT ELEM AT FULL                     
*                                                                               
APPRV240 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
APPRV245 MVI   SVSTAT,QAPP                                                      
*********                                                                       
* SALESPERSON REASSIGNMENT (ONLY FOR RADIO)                                     
*********                                                                       
APPRV250 TM    MISCFLG3,MF3RADIO   ARE WE RADIO?                                
         BZ    APPRV300                                                         
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
                                                                                
APPRV300 XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
         GOTO1 =A(SEEDCONN),DMCB,(RC),ROAPRPCN,RR=RELO                          
*                                                                               
APPRVYES BRAS  RE,BLDCOLOR                                                      
         BAS   RE,DAREMAIL                                                      
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         J     YES                                                              
*                                                                               
APPRVNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   APPRV300                                                         
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         J     NO                                                               
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOCATE WHERE TO PUT STATUS ELEM IN ORDER RECORD BASED ON VERSION #            
*                                                                               
* ON ENTRY:    AIO1                A(ORDER RECORD)                              
*              DOSTELEM            BUILT STATUS ELEMENT (X'12')                 
*                                                                               
* ON EXIT:     (R6)                A(TO INSERT ELEM)                            
***********************************************************************         
PTSTAVER NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,GTMSGVER         GET MESSAGE VERSION #                        
         NI    MISCFLG3,X'FF'-MF3SNDCN                                          
         SR    RF,RF                                                            
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         XR    R0,R0                                                            
*                                                                               
PTSTVLP  CLI   0(R6),0             WE SHOULD NEVER HIT END OF RECORD            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),DOSPELQ       WE GET THE ORDER'S VERSION FROM THIS         
         BE    PTSTV010                                                         
         CLI   0(R6),DORPELQ2      WE'RE CHECKING REP CHANGE ELEM               
         BE    PTSTV020              SO THAT RESPONSES WILL BE ABOVE            
         CLI   0(R6),DOSTELQ                                                    
         BE    PTSTV020                                                         
PTSTVBMP IC    R0,1(R6)            BUMP TO THE NEXT ELEMENT                     
         AR    R6,R0                                                            
         B     PTSTVLP                                                          
*                                                                               
         USING DOSPELD,R6                                                       
PTSTV010 XR    RE,RE               RE = ORDER'S SEND VERSION                    
         ICM   RE,3,DOSPVER#                                                    
*                                                                               
         CLM   RE,3,MSGVERNO       ORDER VER# ALREADY SAME AS MSG VER#?         
         BNE   PTSTVBMP                                                         
PTSTVD   USING DOSTELD,DOSTELEM                                                 
         CLI   PTSTVD.DOSTSTAT,QCFMD     MESSAGE IS A CONFIRM?                  
         BNE   PTSTVBMP                                                         
         OI    PTSTVD.DOSTTYPE,DCNFMVER  SO HWON KNOWS NEXT SEND IS A           
         B     PTSTVBMP                   BUMP IN THE REVISION #                
         DROP  PTSTVD                                                           
*                                                                               
PTSTV020 CLM   RE,3,MSGVERNO       ORDER'S VERSION = MESSAGE'S VERSION?         
         BNE   PTSTV030            NO                                           
         LR    RF,R6               YES SAVE INSRT ADDR & FIND SEND TYPE         
         AHI   RE,1                AND FORCE CHECK AT PTSTV020 TO FAIL          
*                                                                               
PTSTV030 CLI   0(R6),DORPELQ2      IF IT IS A REP CHANGE ELEMENT                
         BE    PTSTVBMP            THEN SKIP IT                                 
*                                                                               
         USING DOSTELD,R6                                                       
PTSTV040 CLI   DOSTSTAT,DSENT      ARE WE A SENT STATUS?                        
         BE    PTSTVX               YES                                         
         CLI   DOSTSTAT,DFXSENT    OR A FAX SENT STATUS?                        
         BNE   PTSTVBMP                                                         
*                                                                               
PTSTVX   LTR   RF,RF               TRYING TO FIND THE SEND TYPE?                
         BNZ   PTSTVX10            YES, LETS EXIT                               
         BCTR  RE,0                NO , DECREMENT SEND VERSION #                
         B     PTSTVBMP                                                         
*                                                                               
PTSTVX10 CLI   DOSTLEN,DOSTLNQ7    HAVE EXTENDED BIT FLAGS?                     
         BNE   PTSTVX20            NO                                           
         TM    DOSTTYP2,DOSTCNCL   CANCEL SENT FLAG?                            
         BZ    PTSTVX20            NO                                           
         OI    MISCFLG3,MF3SNDCN   YES                                          
PTSTVX20 LR    R6,RF               RESTORE R6                                   
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
***********************************************************************         
PTXMTVER NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,GTMSGVER         GET MESSAGE VERSION #                        
*                                                                               
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
PTXMV020 CLM   RE,3,MSGVERNO       ORDER'S VERSION = MESSAGE'S VERSION?         
         BE    PTXMVYES                                                         
         BCTR  RE,0                NO, DECREMENT SEND VERSION #                 
         B     PTXMVBMP                                                         
*                                                                               
PTXMVYES SR    RC,RC                                                            
PTXMVNO  LTR   RC,RC                                                            
         XIT1  REGS=(R6)                                                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET THE DARE XML ORDER MESSAGE VERSION #                                      
***********************************************************************         
GTMSGVER NTR1  BASE=*,LABEL=*                                                   
         XC    MSGVERNO,MSGVERNO   THE MESSAGE VERSION #                        
*                                                                               
         L     R3,AWRKRIOA                                                      
         XR    R1,R1                                                            
         ICM   R1,3,0(R3)                                                       
         LA    R3,2(R3)                                                         
         LA    R1,0(R1,R3)         R1 = A(BYTE AFTER ENTIRE MESSAGE)            
*                                                                               
         LR    R6,R3               R6=A(L'(L'ORDAPP + ORDAPP))                  
GTMSV010 XR    RE,RE                                                            
         IC    RE,0(R6)            RE = L'RECORD IN APPROVAL MESSAGE            
         AR    R6,RE                                                            
*                                                                               
         CLI   0(R6),0             LOOK FOR AGYXM2                              
         BE    GTMSVX              IF NONE, THEN DON'T FRET                     
         CLC   =C'ORDTLR',1(R6)                                                 
         BE    GTMSVX                                                           
         CLC   =C'AGYXM2',1(R6)                                                 
         BNE   GTMSV010                                                         
*                                                                               
         LA    RF,1(R6)                                                         
         USING PAGYXM2D,RF                                                      
         LA    RE,PAX2VER#+L'PAX2VER#-1                                         
         LA    RF,PAX2VER#                                                      
         DROP  RF                                                               
*                                                                               
GTMSV020 CLI   0(RE),C' '          GETTING LENGTH OF VERSION #                  
         BH    GTMSV030                                                         
         BCTR  RE,0                                                             
         CR    RE,RF                                                            
         BNL   GTMSV020                                                         
         DC    H'0'                NO LENGTH                                    
*                                                                               
GTMSV030 SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,MSGVERNO                                                    
*                                                                               
GTMSVX   J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS PROCESSES THE REJECTION ORDER                                            
***********************************************************************         
REJECT   NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)                                                         
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         ZIC   R2,0(R3)            R2 = L(ORDER REJECTION HEADER)               
         LA    R7,1(R3)            R7 = A(ORDER REJECTION HEADER)               
*****                                                                           
* CHECK FOR AMENDED STATUS!!!                                                   
*****                                                                           
         NI    MISCFLG3,X'FF'-MF3AMEND                                          
RJCT10   CLC   =C'ORDTLR',1(R3)                                                 
         BE    RJCT20                                                           
         CLC   =C'ORDCOM',1(R3)                                                 
         BE    RJCT15                                                           
         ZIC   R2,0(R3)                                                         
         AR    R3,R2                                                            
         LA    R7,1(R3)                                                         
         B     RJCT10                                                           
         USING RORDCOMD,R7                                                      
RJCT15   CLC   =C'*** AMEND ***',ROCMTEXT  AMENDED?                             
         BNE   *+8                                                              
         OI    MISCFLG3,MF3AMEND         YES!!                                  
         DROP  R7                                                               
*                                                                               
RJCT20   L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         ZIC   R2,0(R3)            R2 = L(ORDER REJECTION HEADER)               
         LA    R7,1(R3)            R7 = A(ORDER REJECTION HEADER)               
*********                                                                       
* ORDER REJECTION HEADER                                                        
*********                                                                       
         USING RORDREJD,R7                                                      
RJCTD    USING RTN2SNDR,RORJRTRN                                                
         GOTO1 VHEXIN,DMCB,RJCTD.RTNAGYMD,BAGYMD,2                              
         MVC   AGENCY,RJCTD.RTNPWRCD                                            
*                                                                               
         NI    MISCFLG3,X'FF'-MF3RADIO-MF3CANCL                                 
         CLC   RORJTID,=C'CANREJ'                                               
         BNE   *+8                                                              
         OI    MISCFLG3,MF3CANCL                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   RJCTD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                    
         BNE   *+12                                                             
         OI    MISCFLG3,MF3RADIO    RADIO!!                                     
         MVI   QMED,C'R'                                                        
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
         DROP  RJCTD                                                            
*                                                                               
         MVC   QREPCON,RORJRPCN    COPY THESE VALUES                            
         MVC   QRETURN,RORJRTRN                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,RORJORDR    CONVERT ORDER NUMBER TO BINARY         
         BNE   RJCTNO                                                           
*                                                                               
         MVC   USERID,RORJTOID                                                  
         BAS   RE,SWTCHSPT         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   RJCTNO                                                           
*                                                                               
         BRAS  RE,GETORDER                                                      
         BNE   RJCTNO                                                           
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         NI    BITFLAG1,X'FF'-BF1IGRCM                                          
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   DOIDCON,RORJRPCN                                                 
         DROP  R6                                                               
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),DOSPELQ       SUPPLEMENTAL ELEMENT                         
         BNE   RJCT30                                                           
*                                                                               
         USING DOSPELD,R6                                                       
         CLI   DOSPREVN,0                                                       
         BE    RJCT30                                                           
         CLC   =C'UNDARE',RORJRPCN                                              
         BNE   RJCT30                                                           
         LHI   R1,928          CAN'T UNDARE REVISIONS                           
         BRAS  RE,SNDERROR                                                      
*                                                                               
         LHI   R1,928                                                           
         O     R1,=X'80000000' SO AGY KNOWS ALSO                                
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT30   LR    R1,R6               SAVE ADDRESS OF DOSPEL                       
         BRAS  RE,PTXMTVER                                                      
         BE    RJCT35                                                           
         LHI   R1,REFORDNT          ORD NOT TRANSMITTED                         
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT35   CLC   MSGVERNO,DOSPVER#-DOSPELD(R1)                                    
         BE    *+8                                                              
         OI    BITFLAG1,BF1IGRCM   IGNORE COMMENT                               
*                                                                               
RJCT40   CLI   0(R6),DOXMTELQ                                                   
         BE    RJCT60                                                           
RJCT50   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RJCT40                                                           
*                                                                               
         USING DOXMTELD,R6                                                      
RJCT60   CLI   DOXMTSTA,QSNTPNDG                                                
         BNE   *+16                                                             
         MVI   DOXMTSTA,QSNTXREJ   CAN'T AUTO-SEND IF REJECTED                  
         OI    MISCFLG1,MF1XMTUP                                                
         B     RJCT50                                                           
*                                                                               
         CLI   DOXMTSTA,QSNTXREJ                                                
         BE    RJCT50                                                           
         CLI   DOXMTSTA,QSNTXCNF                                                
         BE    RJCT50                                                           
         CLI   DOXMTSTA,QTOBESNT                                                
         BE    RJCT50                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RORJDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RORJTIME,DUB+L'DOXMTSTD,L'RORJTIME                   
*                                                                               
         OC    DOXMTSTD,DOXMTSTD   IF NO DATE OR TIME ALREADY                   
         BZ    RJCT90                                                           
         OC    DOXMTSTT,DOXMTSTT                                                
         BZ    RJCT70              THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLI   DOXMTSTA,QRECALL                                                 
         BE    RJCT80                                                           
         CLI   DOXMTSTA,QRCLREJD                                                
         BE    RJCT80                                                           
*                                                                               
         CLC   DOXMTSTD,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    RJCTNO                     YES, IGNORE THIS RECORD               
         BL    RJCT70                     NO, OLDER                             
         CLC   DOXMTSTT,DUB+L'DOXMTSTD    IS ELEM'S TIME MORE RECENT?           
         BNH   RJCT70                                                           
         CLC   QREPCON,=C'00000000'    HAVE A CONTRACT IN THE REJECT?           
         BNH   RJCTNO                     NO, IGNORE THIS RECORD                
         B     RJCT99                     UPDATE CONTRACT NUMBER                
*                                                                               
RJCT70   CLI   DOXMTSTA,QNODARE    SKIP THE RESPONSE TO A NOTDARE               
         BNE   RJCT80                                                           
         CLC   =C'NOTDARE',RORJRPCN                                             
         BE    RJCTNO                                                           
         CLC   =C'UNDARE',RORJRPCN                                              
         BE    RJCTNO                                                           
         LHI   R1,REFBDCON      BAD CONTRACT                                    
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT80   CLI   DOXMTSTA,QAPP       CAN'T CHANGE ANYTHING BUT APPROVED,          
         BE    RJCT90                  RECALL STATUSES                          
*                                                                               
         CLI   DOXMTSTA,QRECALL                                                 
         BE    RJCT90                                                           
         CLI   DOXMTSTA,QRCLREJD                                                
         BE    RJCT90                                                           
         CLI   DOXMTSTA,QRCLUNKN                                                
         BE    RJCT90                                                           
*                                                                               
         CLI   DOXMTSTA,QERRORED   ERROR STATUS?                                
         BNE   RJCT85                                                           
         BRAS  RE,GET30TXT         GET X'30' TEXT AND PUT IN BLOCK              
         BNE   RJCT85                                                           
         CLC   =C'READ TIMED OUT',BLOCK                                         
         BE    RJCT90              ALLOW THIS                                   
*                                                                               
RJCT85   LHI   R1,REFCCHNG         CAN'T CHANGE THE STATUS                      
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT90   MVC   DOXMTSTD,DUB                                                     
         MVC   DOXMTSTT,DUB+L'DOXMTSTD                                          
         MVI   DOXMTSTA,QRJCT      REJECTED                                     
         CLC   =C'UNDARE',RORJRPCN                                              
         BNE   *+8                                                              
         MVI   DOXMTSTA,QUNDARE    UNDARED, NEVER LET USER SEND AGAIN           
         MVC   SVSTAT,DOXMTSTA                                                  
         DROP  R6                                                               
*                                                                               
RJCT99   OI    MISCFLG1,MF1XMTUP                                                
***********************************************************************         
* TIME TO PROCESS THE STATUS ELEMENTS (X'12')                                   
***********************************************************************         
RJCT100  NI    BITFLAG3,X'FF'-BF3SPNDG-BF3RCLRJ                                 
         SR    R0,R0                                                            
         BRAS  RE,PTSTAVER                                                      
*                                                                               
RJCT110  CLI   0(R6),DOSTELQ                                                    
         BE    RJCT120                                                          
RJCT115  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RJCT110                                                          
*                                                                               
         USING DOSTELD,R6                                                       
RJCT120  CLI   DOSTSTAT,DDLVRD                                                  
         BE    RJCT115                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RORJDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RORJTIME,DUB+L'DOXMTSTD,L'RORJTIME                   
*                                                                               
         CLI   DOSTSTAT,QSNTPNDG                                                
         BNE   RJCT130                                                          
         DROP  R6                                                               
*                                                                               
         OI    BITFLAG3,BF3SPNDG   SET THIS BIT                                 
         LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QSNTXREJ                                                
         DROP  R4                                                               
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
RJCT125  CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    RJCT126          AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    RJCT126                                                          
         CLI   0(R6),DOSTELQ                                                    
         BNL   RJCT126                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RJCT125                                                          
*                                                                               
RJCT126  GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
RJCT127  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RJCT115    BUMP AGAIN, WE ARE POINTING AT QSNTPNDG               
*                                                                               
         USING DOSTELD,R6                                                       
RJCT130  CLI   DOSTSTAT,QSNTXREJ   AM I QSNTXREJ?                               
         BE    RJCT165             YES, CAME FROM RCLAKNWL, SO JUST             
*                                     REJECT ORDER!!!                           
RJCT131  CLI   DOSTSTAT,QSNTXCNF                                                
         BE    RJCT115                                                          
         CLI   DOSTSTAT,QTOBESNT                                                
         BE    RJCT115                                                          
*                                                                               
         CLI   DOSTSTAT,DSENT                                                   
         BE    RJCT165                                                          
         OC    DOSTTIME,DOSTTIME                                                
         BZ    RJCT140                                                          
*                                                                               
         CLI   DOSTSTAT,QRECALL    IF IT'S RECALL STATUS                        
         BE    RJCT165                                                          
         CLI   DOSTSTAT,QRCLREJD   OR, NOT-RECALLED, REJECTED STATUS            
         BE    RJCT165             THEN WE CAN ADD REJECT ELEM                  
*                                                                               
RJCT135  CLC   DOSTDATE,DUB        ELSE, CHECK DATE/TIME MORE RECENT            
         BH    RJCTNO              IGNORE THIS RECORD                           
         BL    RJCT140                                                          
         CLC   DOSTTIME,DUB+L'DOSTDATE   IS ELEM'S TIME MORE RECENT?            
         BH    RJCTNO                    YES, IGNORE THIS RECORD.               
*                                                                               
RJCT140  CLI   DOSTSTAT,QNODARE    SKIP THE RESPONSE TO A NOTDARE               
         BNE   RJCT145                                                          
         CLC   =C'NOTDARE',RORJRPCN                                             
         BE    RJCTNO                                                           
         CLC   =C'UNDARE',RORJRPCN                                              
         BE    RJCTNO                                                           
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    RJCTNO                     YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFBDCON         BAD CONTRACT                                 
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT145  CLI   DOSTSTAT,QRJCT                                                   
         BNE   RJCT150                                                          
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   SVSTAT,QRJCT                                                     
         MVC   DOSAELEM(DOSTLNQ2),DOSTEL                                        
         B     RJCT200                                                          
*                                                                               
RJCT150  CLI   DOSTSTAT,QAPP                                                    
         BE    RJCT165                                                          
*                                                                               
         CLI   DOSTSTAT,QRECALL                                                 
         BE    RJCT165                                                          
         CLI   DOSTSTAT,QRCLREJD                                                
         BE    RJCT165                                                          
         CLI   DOSTSTAT,QRCLUNKN                                                
         BE    RJCT165                                                          
*                                                                               
         CLI   DOSTSTAT,QERRORED   ERROR STATUS?                                
         BNE   RJCT160                                                          
         BRAS  RE,GET30TXT         GET X'30' TEXT AND PUT IN BLOCK              
         BNE   RJCT160              NO COMMENT                                  
         CLC   =C'READ TIMED OUT',BLOCK                                         
         BE    RJCT165             ALLOW THIS                                   
         DROP  R6                                                               
*                                                                               
RJCT160  TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    RJCTNO                     YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFCCHNG         BAD CONTRACT                                 
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT165  LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QRJCT                                                   
*                                                                               
         MVC   DOSAELEM(DOSTLNQ2),DOSTELEM                                      
*                                                                               
         TM    MISCFLG3,MF3AMEND   AMENDED?                                     
         BZ    *+8                                                              
         MVI   DOSTLEN,DOSTLNQ3                                                 
*                                                                               
         TM    MISCFLG3,MF3CANCL   CANCEL REJECTED?                             
         BZ    *+12                                                             
         MVI   DOSTLEN,DOSTLNQ3                                                 
         OI    DOSTTYPE,DRJTCAN                                                 
*                                                                               
         CLC   =C'UNDARE',RORJRPCN                                              
         BNE   *+8                                                              
         MVI   DOSTSTAT,QUNDARE    UNDARED, NEVER LET USER SEND AGAIN           
*                                                                               
         MVC   SVSTAT,DOSTSTAT                                                  
         DROP  R4                                                               
*                                                                               
         BRAS  RE,PTSTAVER                                                      
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*********                                                                       
* SALESPERSON REASSIGNMENT (ONLY FOR RADIO)                                     
*********                                                                       
RJCT200  AR    R3,R2                                                            
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
         GOTO1 =A(SEEDCONN),DMCB,(RC),RORJRPCN,RR=RELO                          
         DROP  R7                                                               
*********                                                                       
* ORDER REJECTION COMMENT                                                       
*********                                                                       
RJCT210  CLC   =C'ORDCOM',1(R3)                                                 
         BNE   RJCT300                                                          
*                                                                               
         TM    BITFLAG1,BF1IGRCM   IGNORE COMMENT?                              
         BNZ   RJCT263             YES                                          
*                                                                               
         NI    BITFLAG1,X'FF'-BF1NWCOM                                          
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   KEY+DOKCMT-DOKEY,2  LOOK FOR REP COMMENT RECORD                  
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE  REP COMMENT RECORD DOESN'T EXIST?          
         BE    RJCT230                                                          
         OI    BITFLAG1,BF1NWCOM     NO, NEED A NEW REP COMMENT RECORD          
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEFL                                                                  
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   0(L'DOKEY,R6),KEYSAVE                                            
         MVC   DORAGY,AGENCY                                                    
         DROP  R6                                                               
         B     RJCT240                                                          
*                                                                               
RJCT230  MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
RJCT240  L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   DORLEN,=Y(DORFRST-DOKEY)  REMOVES ALL COMMENT ELEMENTS           
         LA    R6,DORFRST                                                       
         DROP  R6                                                               
*                                                                               
         LA    R2,ELEM             ADD THE COMMENTS TO RECORD                   
         USING DOCOMELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCOMEL,DOCOMELQ                                                 
*                                                                               
RJCT250  LA    R7,1(R3)                                                         
         USING RORDCOMD,R7                                                      
*                                                                               
         CLC   =C'** AMEND **',DOCOMTXT  SKIP THIS COMMENT                      
         BE    RJCT263                                                          
*                                                                               
         MVC   DOCOMTXT(4),ROCMBLIN   BUYLINE GOES IN COMMENT                   
         MVI   DOCOMTXT+5,C'-'                                                  
         ZIC   R1,0(R3)            L(LENGTH AND LINE)                           
         SH    R1,=Y(ROCMTEXT-RORDCOMD+2)                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DOCOMTXT+7(0),ROCMTEXT                                           
         OC    DOCOMTXT(L'ROCMTEXT+7),ALLSPCES                                  
*                                                                               
         LA    R1,DOCOMTXT+L'ROCMTEXT+7-1                                       
RJCT255  CLI   0(R1),C' '          CALCULATE LENGTH OF COMMENT                  
         BNE   RJCT260                                                          
         BCT   R1,RJCT255                                                       
         LHI   R1,*-T16300                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
RJCT260  LR    R0,R1                                                            
         SR    R0,R2                                                            
         AH    R0,=H'1'                                                         
         STC   R0,DOCOMLEN                                                      
*                                                                               
         ZIC   R1,DOCOMLIN         INCREMENT LINE NUMBER                        
         LA    R1,1(R1)                                                         
         STC   R1,DOCOMLIN                                                      
         DROP  R2                                                               
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(C'R',(R6))                         
         CLI   8(R1),C'R'          RECORD OVERFLOW?                             
         BE    *+12                                                             
         OI    BITFLAG1,BF1IGRCM   YES, IGNORE REST OF REP COMMENTS             
         B     RJCT263                                                          
*                                                                               
         ZIC   R0,ELEM+1                                                        
         AR    R6,R0               R6 = A(NEXT INSERTION ADDRESS)               
RJCT263  ZIC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         CLC   =C'ORDCOM',1(R3)    GOT ANOTHER COMMENT LINE?                    
         BNE   RJCT266                                                          
         TM    BITFLAG1,BF1IGRCM   YES, IGNORE IT?                              
         BZ    RJCT250                  NO                                      
         B     RJCT263                  YES                                     
*                                                                               
RJCT266  L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
*                                                                               
         TM    BITFLAG1,BF1IGRCM   IGNORE COMMENT?                              
         BNZ   RJCT300                                                          
*                                                                               
         TM    BITFLAG1,BF1NWCOM   NEW REP COMMENT RECORD?                      
         BZ    RJCT270                                                          
         MVC   AIO,AIO1                                                         
         BRAS  RE,ADD                                                           
         B     RJCT300                                                          
*                                                                               
RJCT270  MVC   AIO,AIO1                                                         
         BRAS  RE,PUT                                                           
*                                                                               
RJCT300  XR    R0,R0                                                            
RJCT305  CLC   =C'AGYXM',1(R3)     IGNORE MESSAGES STARTING WITH AGYXM          
         BNE   RJCT310                                                          
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     RJCT305                                                          
*                                                                               
RJCT310  CLC   =C'ORDTLR',1(R3)                                                 
         BE    RJCTYES                                                          
         LHI   R1,*-T16300                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
RJCTYES  BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
         BRAS  RE,BLDCOLOR                                                      
         BAS   RE,DAREMAIL                                                      
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         BRAS  RE,SNDSUCSS         SEND SBS SUCCESS MESSAGE                     
         J     YES                                                              
*                                                                               
RJCTNO   TM    MISCFLG1,MF1XMTUP                                                
         BNZ   RJCT200                                                          
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS PROCESSES THE CONFIRMATION ORDER                                         
***********************************************************************         
CONFIRM  NTR1  BASE=*,LABEL=*                                                   
*        NMOD1 0,**CNFM**                                                       
         L     RC,0(R1)                                                         
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         ZIC   R2,0(R3)            R2 = L(ORDER CONFIRMATION HEADER)            
         LA    R7,1(R3)            R7 = A(ORDER CONFIRMATION HEADER)            
*********                                                                       
* ORDER CONFIRMATION HEADER                                                     
*********                                                                       
         USING RORDCFMD,R7                                                      
CNFMD    USING RTN2SNDR,ROCFRTRN                                                
*                                                                               
         NI    MISCFLG3,X'FF'-MF3RADIO-MF3CANCL                                 
         CLC   ROCFTID,=C'ORDCAN'                                               
         BE    CNFM03                                                           
         CLC   ROCFTID,=C'CANCFM'                                               
         BNE   *+8                                                              
CNFM03   OI    MISCFLG3,MF3CANCL                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   CNFMD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                    
         BNE   *+12                                                             
         OI    MISCFLG3,MF3RADIO    RADIO!!                                     
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,CNFMD.RTNAGYMD,BAGYMD,2                              
         MVC   AGENCY,CNFMD.RTNPWRCD                                            
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
         DROP  CNFMD                                                            
*                                                                               
         MVC   QREPCON,ROCFRPCN    COPY THESE VALUES                            
         MVC   QRETURN,ROCFRTRN                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,ROCFORDR    CONVERT ORDER NUMBER TO BINARY         
         BNE   CNFMNO                                                           
*                                                                               
         MVC   USERID,ROCFTOID                                                  
         BAS   RE,SWTCHSPT         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   CNFMNO                                                           
***************                                                                 
* GET OM PROFILE                                                                
***************                                                                 
         BRAS  RE,GETORDER                                                      
         BNE   CNFMNO                                                           
***                                                                             
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
         USING DOIDELD,R6                                                       
         MVC   BCLT,DOIDCLT                                                     
*                                                                               
         BAS   RE,GETCLTRC         GET THE CLIENT RECORD                        
         BE    CNFM05                                                           
         LHI   R1,*-T16300                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
         DROP  R6                                                               
*                                                                               
CNFM05   L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
         MVC   SVCPRF00,CPROF+0                                                 
         GOTO1 VCLUNPK,DMCB,(CPROF+6,BCLT),QCLT                                 
         MVC   SVCOFFC,COFFICE                                                  
         DROP  R6                                                               
*                                                                               
* GET OM PARITAL CONFIRM WORKFLOW PROFILE?                                      
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0OM'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
*                                                                               
         MVC   WORK+7(3),QCLT                                                   
         CLI   SVCOFFC,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFFC                                               
*****                                                                           
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         MVC   TUSER,SIGNON2H      FOOL GETPROF                                 
         DROP  R1                                                               
*****                                                                           
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CGETPROF-COMFACSD(RF)                                         
         XC    PROFOM,PROFOM                                                    
         GOTO1 (RF),DMCB,WORK,PROFOM,VDATAMGR                                   
*****                                                                           
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         XC    TUSER,TUSER         RESET THE ID NUMBER IN THE UTL               
         DROP  R1                                                               
***************                                                                 
* GOT OM PROFILE!! RESTORE AIO1 TO ORDER RECORD!!                               
***************                                                                 
         BRAS  RE,GETORDER                                                      
         BNE   CNFMNO                                                           
*                                                                               
         XC    POLORDER,POLORDER                                                
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         OI    DORSTAT,X'01'       SET CONFIRMED BIT ON                         
CNFM10   LA    R6,DORFRST                                                       
         USING DOIDELD,R6                                                       
         MVC   DOIDCON,ROCFRPCN                                                 
*                                                                               
         LLC   R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
*                                                                               
CNFM15   CLI   0(R6),DOSPELQ       DO WE HAVE A 2NDARY ID ELEM?                 
         BE    CNFM20                                                           
         LHI   R1,*-T16300         ALL POL ORDERS SHOULD HAVE THIS ELEM         
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
         USING DOSPELD,R6                                                       
CNFM20   MVC   REVISION,DOSPREVN   SAVE THE REVISION                            
         DROP  R6                                                               
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         NI    BITFLAG2,X'FF'-BF2SPNDG                                          
         SR    R0,R0                                                            
*                                                                               
         LR    R1,R6                                                            
         BRAS  RE,PTXMTVER                                                      
         BE    CNFM25                                                           
         LHI   R1,REFORDNT          ORD NOT TRANSMITTED                         
         BRAS  RE,SNDERROR                                                      
         B     CNFMNO                                                           
*                                                                               
CNFM25   CLC   MSGVERNO,DOSPVER#-DOSPELD(R1)                                    
         BE    *+8                                                              
         OI    BITFLAG1,BF1IGRCM   IGNORE COMMENT                               
*                                                                               
CNFM30   CLI   0(R6),DOXMTELQ                                                   
         BE    CNFM40                                                           
CNFM35   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM30                                                           
*                                                                               
         USING DOXMTELD,R6                                                      
CNFM40   CLI   DOXMTSTA,QSNTPNDG                                                
         BNE   CNFM45                                                           
         OI    BITFLAG2,BF2SPNDG   SEND PENDING                                 
         MVI   DOXMTSTA,QTOBESNT   CAN'T AUTO-SEND IF PARTIAL CONFIRM           
         MVI   SVSTAT,QTOBESNT                                                  
         OI    MISCFLG1,MF1XMTUP                                                
         B     CNFM35                                                           
*                                                                               
CNFM45   CLI   DOXMTSTA,QSNTXREJ                                                
         BE    CNFM35                                                           
         CLI   DOXMTSTA,QSNTXCNF                                                
         BE    CNFM35                                                           
         CLI   DOXMTSTA,QTOBESNT                                                
         BE    CNFM35                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(0,ROCFDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,ROCFTIME,DUB+L'DOXMTSTD,L'ROCFTIME                   
*                                                                               
         OC    DOXMTSTD,DOXMTSTD   IF NO DATE OR TIME ALREADY                   
         BZ    CNFM75                                                           
         OC    DOXMTSTT,DOXMTSTT                                                
         BZ    CNFM55              THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLI   DOXMTSTA,QRECALL    IF IT IS A RECALL STATUS                     
         BE    CNFM55                                                           
         CLI   DOXMTSTA,QRCLCONF                                                
         BE    CNFM55                                                           
*                                                                               
CNFM50   CLC   DOXMTSTD,DUB        ELSE CHECK DATE/TIME MORE RECENT             
         BH    CNFMNO              IGNORE THIS RECORD                           
         BL    CNFM55                                                           
         CLC   DOXMTSTT,DUB+L'DOXMTSTD    IS ELEM'S TIME MORE RECENT?           
         BH    CNFMNO                     YES, IGNORE THIS REC                  
*                                                                               
CNFM55   CLI   DOXMTSTA,QAPP       CAN'T CHANGE ANYTHING BUT APPROVED           
         BE    CNFM75                  OR RECALL STATUSES                       
*                                                                               
         CLI   DOXMTSTA,QRECALL                                                 
         BE    CNFM75                                                           
         CLI   DOXMTSTA,QRCLCONF                                                
         BE    CNFM75                                                           
         CLI   DOXMTSTA,QRCLUNKN     RECALL UNKNOWN, LET CONFIRM THRU           
         BE    CNFM75                                                           
*                                                                               
         CLI   DOXMTSTA,QERRORED   ERROR STATUS?                                
         BNE   CNFM70                                                           
         BRAS  RE,GET30TXT         GET X'30' TEXT AND PUT IN BLOCK              
         BNE   CNFM70                                                           
         CLC   =C'READ TIMED OUT',BLOCK                                         
         BE    CNFM75              ALLOW THIS                                   
*                                                                               
CNFM70   LHI   R1,REFCCHNG                                                      
         BRAS  RE,SNDERROR                                                      
         B     CNFMNO                                                           
*                                                                               
CNFM75   MVC   DOXMTSTD,DUB                                                     
         MVC   DOXMTSTT,DUB+L'DOXMTSTD                                          
*                                                                               
         MVI   DOXMTSTA,QCFMD      CONFIRMED                                    
         OC    POLORDER,POLORDER                                                
         BZ    *+8                                                              
         MVI   DOXMTSTA,QCFMDPND   CONFIRM PENDING                              
*                                                                               
         TM    BITFLAG2,BF2SPNDG                                                
         BNZ   *+10                                                             
         MVC   SVSTAT,DOXMTSTA                                                  
         DROP  R6                                                               
*                                                                               
CNFM99   OI    MISCFLG1,MF1XMTUP                                                
***********************************************************************         
* TIME TO PROCESS THE STATUS ELEMENTS (X'12')                                   
***********************************************************************         
CNFM100  SR    R0,R0                                                            
         NI    BITFLAG3,X'FF'-BF3SPNDG-BF3RCLCF-BF3CNFMD                        
         BRAS  RE,PTSTAVER                                                      
*                                                                               
CNFM110  CLI   0(R6),DOSTELQ                                                    
         BE    CNFM120                                                          
CNFM115  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM110                                                          
*                                                                               
         USING DOSTELD,R6                                                       
CNFM120  CLI   DOSTSTAT,DDLVRD                                                  
         BE    CNFM115                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(0,ROCFDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,ROCFTIME,DUB+L'DOSTDATE,L'ROCFTIME                   
*                                                                               
         CLI   DOSTSTAT,QSNTPNDG                                                
         BE    CNFM120A                                                         
         DROP  R6                                                               
*                                                                               
         TM    BITFLAG3,BF3RCLCF   DID I COME BACK TO CHK FOR QSNTPNDG          
         BNZ   CNFM199             YES, DIDN'T FIND IT, EXIT                    
         B     CNFM130             NO                                           
*                                                                               
CNFM120A OI    BITFLAG3,BF3SPNDG                                                
         LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QTOBESNT                                                
         MVI   SVSTAT,QTOBESNT                                                  
         DROP  R4                                                               
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
CNFM125  CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    CNFM126          AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    CNFM126                                                          
         CLI   0(R6),DOSTELQ                                                    
         BNL   CNFM126                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM125                                                          
*                                                                               
CNFM126  GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
         TM    BITFLAG3,BF3RCLCF   DID I COME BACK TO CHK FOR QSNTPNDG          
         BNZ   CNFM199             YES, FOUND IT, NO NEED TO CONTINUE           
*                                                                               
CNFM127  SR    R0,R0     CALLING HERE WILL BUMP YOU TWICE!!                     
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         ST    R6,FULL             <=== NEED THIS, DO NOT CLOBBER!!!            
         B     CNFM115   BUMP AGAIN, WE ARE POINTING AT QSNTPNDG                
*                                                                               
         USING DOSTELD,R6                                                       
CNFM130  CLI   DOSTSTAT,QSNTXREJ                                                
         BE    CNFM115                                                          
         CLI   DOSTSTAT,QSNTXCNF                                                
         BE    CNFM115                                                          
         CLI   DOSTSTAT,QTOBESNT                                                
         BE    CNFM115                                                          
*                                                                               
         CLI   DOSTSTAT,DSENT                                                   
         BE    CNFM165                                                          
         OC    DOSTTIME,DOSTTIME                                                
         BZ    CNFM140                                                          
*                                                                               
         CLI   DOSTSTAT,QRECALL    IF IT'S RECALL STATUS                        
         BE    CNFM165                                                          
         CLI   DOSTSTAT,QRCLCONF                                                
         BE    CNFM165             THEN WE CAN ADD CONFIRM STATUS               
*                                                                               
CNFM135  CLC   DOSTDATE,DUB        ELSE, CHECK DATE/TIME MORE RECENT?           
         BH    CNFMNO              IGNORE THIS RECORD.                          
         BL    CNFM140                                                          
         CLC   DOSTTIME,DUB+L'DOSTDATE   IS ELEM''S TIME MORE RECENT?           
         BH    CNFMNO                    YES, IGNORE THIS RECORD.               
*                                                                               
CNFM140  CLI   DOSTSTAT,QCFMD                                                   
         BE    CNFM145                                                          
         CLI   DOSTSTAT,QCFMDPND                                                
         BNE   CNFM150                                                          
CNFM145  MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVC   DOSAELEM(DOSTLNQ2),DOSTEL                                        
         TM    BITFLAG3,BF3SPNDG                                                
         BNZ   CNFM200                                                          
         MVC   SVSTAT,DOSTSTAT                                                  
         B     CNFM200                                                          
*                                                                               
CNFM150  CLI   DOSTSTAT,QAPP                                                    
         BE    CNFM165                                                          
*                                                                               
         CLI   DOSTSTAT,QRECALL                                                 
         BE    CNFM165                                                          
         CLI   DOSTSTAT,QRCLCONF                                                
         BE    CNFM165                                                          
         CLI   DOSTSTAT,QRCLUNKN                                                
         BE    CNFM165                                                          
*                                                                               
         CLI   DOSTSTAT,QERRORED   ERROR STATUS?                                
         BNE   CNFM160                                                          
         BRAS  RE,GET30TXT         GET X'30' TEXT AND PUT IN BLOCK              
         BNE   CNFM160              NO COMMENT                                  
         CLC   =C'READ TIMED OUT',BLOCK                                         
         BE    CNFM165             ALLOW THIS                                   
*                                                                               
CNFM160  DS    0H                                                               
         LHI   R1,REFCCHNG         CONFIRM TYPE DOESN'T MATCH SEND TYPE         
         BRAS  RE,SNDERROR                                                      
         B     CNFMNO10                                                         
*                                                                               
CNFM165  DS    0H                                                               
         CLI   DOSTSTAT,QRCLCONF   AM I RECALL CONFIRM?                         
         BNE   CNFM166                                                          
         OI    BITFLAG3,BF3RCLCF                                                
         DROP  R6                                                               
*                                                                               
CNFM166  LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ3                                                 
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QCFMD                                                   
*                                                                               
         TM    MISCFLG3,MF3CANCL+MF3SNDCN                                       
         BO    CNFM167                                                          
         BZ    CNFM168                                                          
         LHI   R1,BADCFMTP         CONFIRM TYPE DOESN'T MATCH SEND TYPE         
         BRAS  RE,SNDERROR                                                      
         B     CNFMNO10                                                         
CNFM167  OI    DOSTTYPE,DCNFMCAN                                                
*                                                                               
CNFM168  MVC   DOSAELEM(DOSTLNQ2),DOSTELEM                                      
         OC    POLORDER,POLORDER                                                
         BZ    *+12                                                             
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVI   DOSTSTAT,QCFMDPND   CONFIRM PENDING                              
*                                                                               
         L     R6,FULL             POINT TO STATUS BEFORE QTOBESNT              
         TM    BITFLAG3,BF3SPNDG   DO I WANT TO ADD IT THERE?                   
         BNZ   CNFM180             YES                                          
         MVC   SVSTAT,DOSTSTAT                                                  
         DROP  R4                                                               
*                                                                               
         BRAS  RE,PTSTAVER                                                      
*                                                                               
CNFM180  GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
         TM    BITFLAG3,BF3RCLCF   WAS IT QRCLCONF BEFORE?                      
         BNZ   CNFM127             YES, MUST CHECK FOR QSNTPNDG                 
*                                                                               
CNFM199  DS    0H                                                               
*********                                                                       
* ORDER LINE NUMBER EQUIVALENTS                                                 
*********                                                                       
CNFM200  AR    R3,R2                                                            
         NI    BITFLAG2,X'FF'-BF2CNFCM                                          
         MVI   QCSHTRDE,C' '                                                    
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,DOSPELQ                                                   
         MVI   ELCDHI,DOSPELQ                                                   
         BRAS  RE,BNEXTEL                                                       
         BNE   CNFM210                                                          
         USING DOSPELD,R6                                                       
         NI    DOSPFLG1,X'FF'-DOSPCFCM   MIGHT NOT BE A PARTIAL CONFM           
*                                                                               
         CLC   =C'000',DOSPTDAT    ANY TRADE REP SPECIFIED?                     
         BNL   CNFM210             NONE                                         
         MVI   QCSHTRDE,C'C'                                                    
         TM    DOSPTMTH,X'40'                                                   
         BZ    *+8                                                              
         MVI   QCSHTRDE,C'T'                                                    
         DROP  R6                                                               
*                                                                               
CNFM210  CLC   =C'ORDURL',1(R3)    SKIP ORDURL'S FOR NOW                        
         BE    CNFM290                                                          
*                                                                               
         CLC   =C'ORDCOM',1(R3)    SKIP ORDCOM'S FOR NOW                        
         BNE   CNFM250                                                          
         TM    BITFLAG2,BF2CNFCM                                                
         BNZ   CNFM290                                                          
         OI    BITFLAG2,BF2CNFCM   WE HAVE CONFIRM WITH COMMENTS                
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         SR    R0,R0                                                            
CNFM215  CLI   0(R6),0             SUPPLEMENTARY ID ELEMENT?                    
         BE    CNFM245                                                          
         CLI   0(R6),DOSPELQ                                                    
         BH    CNFM245             NONE                                         
         BE    CNFM220             YES, TURN ON THE APPROPRIATE BIT             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM215                                                          
*                                                                               
         USING DOSPELD,R6                                                       
CNFM220  OI    DOSPFLG1,DOSPCFCM   GOT A CONFIRM WITH COMMENTS                  
*                                                                               
         TM    BITFLAG2,BF2SPNDG   SEND PENDING AS WELL?                        
         BZ    CNFM230                                                          
CNFM225  IC    R0,1(R6)            NEED TO FIND XMT ELEM WITH QSNTPNDG          
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CNFM230                                                          
         CLI   0(R6),DOXMTELQ                                                   
         BNE   CNFM225                                                          
         USING DOXMTELD,R6                                                      
         CLI   DOXMTSTA,QTOBESNT   GOING TO SEND IT OUT?                        
         BNE   CNFM230                                                          
         MVI   DOXMTSTA,QSNTXCNF   NO CAN DO, BUYER SHOULD READ CMNTS           
         MVI   SVSTAT,QSNTXCNF                                                  
         NI    BITFLAG2,X'FF'-BF2SPNDG    NOT PENDING ANYMORE                   
         OI    MISCFLG1,MF1XMTUP                                                
         DROP  R6                                                               
*                                                                               
CNFM230  L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         SR    R0,R0                                                            
CNFM235  CLI   0(R6),0                                                          
         BE    CNFM290                                                          
         CLI   0(R6),DOSTELQ     NEED TO FIND THE X'12' ELEM W/QSNTPNDG         
         BE    CNFM240                                                          
CNFM237  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM235                                                          
*                                                                               
         USING DOSTELD,R6                                                       
*                                                                               
CNFM240  CLI   DOSTSTAT,QTOBESNT                                                
         BNE   CNFM242                                                          
*                                                                               
         TM    BITFLAG2,BF2SPNDG   SEND PENDING AS WELL?                        
         BZ    CNFM237                                                          
*                                                                               
         MVC   ELEM(L'DOSTSTAT),0(R6)                                           
         GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6)   DELETE IT!!                       
         DROP  R6                                                               
*                           AFTER THIS DELETE, R6 = A(QCFMD ELEM)               
         LR    R3,R6               SAVE ADDR OF CONFIRMED                       
         LA    R4,DOSTELEM        AND ADD QSNTXCNF AFTER QCFMD!!!               
         USING DOSTELD,R4                                                       
         MVI   DOSTSTAT,QSNTXCNF                                                
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
         DROP  R4                                                               
*                                                                               
         MVI   SVSTAT,QSNTXCNF                                                  
         NI    BITFLAG3,X'FF'-BF3SPNDG                                          
         LR    R6,R3                                                            
*                                                                               
         USING DOSTELD,R6                                                       
CNFM242  CLI   DOSTSTAT,QCFMD                                                   
         BNE   CNFM290                                                          
         CLI   DOSTLEN,DOSTLNQ3                                                 
         BNE   CNFM290                                                          
         OI    DOSTTYPE,DCNFMCOM                                                
         B     CNFM290                                                          
         DROP  R6                                                               
*                                                                               
CNFM245  XC    ELEM,ELEM           R6 = A(TO INSERT SUPP ELEM IN REC)           
         LA    R1,ELEM                                                          
         USING DOSPELD,R1                                                       
         MVI   DOSPEL,DOSPELQ                                                   
         MVI   DOSPLEN,DOSPLNQ                                                  
         OI    DOSPFLG1,DOSPCFCM                                                
         DROP  R1                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
         B     CNFM290                                                          
*                                                                               
CNFM250  CLC   =C'ORDLIN',1(R3)                                                 
         BNE   CNFM295                                                          
*                                                                               
CNFM255  LA    R4,1(R3)                                                         
         USING RORDLIND,R4                                                      
         PACK  DUB,ROLNBLIN        CONVERT THE BUYLINE TO BINARY                
         CVB   R1,DUB                                                           
         STH   R1,HALF                                                          
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
CNFM260  CLI   0(R6),0             END OF RECORD                                
         BE    CNFM290             AGY BYLN NOT IN ORD, DON'T SEND ERR          
*                                                                               
         CLI   HALF,0              HAVE 2 BYTE BUYLINE                          
         BNE   CNFM265              YES, SKIP X'22' ELEMENTS                    
         CLI   0(R6),DOBUYELQ      FIND CORRECT X'22' 1-BT BUYLN ELEM           
         BNE   CNFM265              NO                                          
         CLC   HALF+1(1),DOBUYSPT-DOBUYELD(R6)                                  
         BE    CNFM270                                                          
         B     CNFM267                                                          
*                                                                               
CNFM265  CLI   0(R6),DOBY2ELQ      FIND CORRECT X'23' 2-BT BUYLN ELEM           
         BNE   CNFM267              NO                                          
         CLC   HALF,DOBY2SPT-DOBY2ELD(R6)                                       
         BE    CNFM270                                                          
CNFM267  LLC   R0,1(R6)            CHECK NEXT ELEM IN RECORD                    
         AR    R6,R0                                                            
         B     CNFM260                                                          
*                                                                               
CNFM270  XC    ELEM,ELEM           COPY THE BUYLINE ELEMENT                     
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         LLC   RF,ELEM+1                                                        
         LA    RF,ELEM(RF)                                                      
*                                                                               
         LA    RE,ROLNRLIN         STICK IN REP BUYLINES                        
         LA    R0,10               MAX OF 10 IN EACH ORDLIN                     
*                                                                               
CNFM275  OC    0(L'ROLNRLIN/10,RE),0(RE)     END OF REP BUYLINE LIST?           
         BZ    CNFM280                                                          
         CLC   0(L'ROLNRLIN/10,RE),ALLSPCES                                     
         BE    CNFM280             YES                                          
         CLC   0(L'ROLNRLIN/10,RE),=4C'0'    ZEROS                              
         BE    CNFM280             YES                                          
         PACK  DUB,0(L'ROLNRLIN/10,RE)                                          
         CVB   R1,DUB                                                           
         STCM  R1,3,0(RF)                                                       
         LA    RF,L'DOBUYREP(RF)     PUT REP BUYLINE IN OUR ELEMENT AND         
         LA    RE,L'ROLNRLIN/10(RE)    CHECK NEXT REP BUYLINE                   
         LLC   R1,0(R3)            GET LENGTH OF THIS ORDCOM                    
         LA    R1,0(R1,R3)         ADDRESS AFTER THIS ORDLIN                    
         CR    RE,R1                                                            
         BNL   CNFM280                                                          
         BCT   R0,CNFM275                                                       
*                                                                               
CNFM280  LA    RE,ELEM             CALCULATE LENGTH OF THE BUYLINE ELEM         
         SR    RF,RE                                                            
         STC   RF,ELEM+1                                                        
*                                                                               
         CLM   RF,1,1(R6)          ELEMENT DIDN'T CHANGE IN SIZE?               
         BNE   CNFM285                                                          
         LLC   R1,ELEM+1           NO, MIGHT BE DIFFERENT BUYLINES, SO          
         BCTR  R1,0                    COPY IT OVER TO OLD ELEMENT              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),ELEM                                                     
         B     CNFM290                                                          
*                                                                               
CNFM285  GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6),(R6)                                
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
*                                                                               
CNFM290  LLC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     CNFM210                                                          
         DROP  R4                                                               
*********                                                                       
* DONE WITH THE ORDER RECORD                                                    
*********                                                                       
CNFM295  XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUT                                                           
*                                                                               
         GOTO1 =A(SEEDCONN),DMCB,(RC),ROCFRPCN,RR=RELO                          
*                                                                               
         XR    R0,R0                                                            
CNFM296  CLC   =C'AGYXM',1(R3)     IGNORE MESSAGES STARTING WITH AGYXM          
         BNE   CNFM297                                                          
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     CNFM296                                                          
*                                                                               
CNFM297  CLC   =C'ORDTLR',1(R3)                                                 
         BE    CNFM299                                                          
         L     R1,=AL4(*-T16300)                                                
*        LHI   R1,*-T16300                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
CNFM299  BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*********                                                                       
* PROCESS ORDER CONFIRMATION COMMENT AND URL                                    
*********                                                                       
CNFM300  L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         LLC   R2,0(R3)            R2 = L(ORDER CONFIRMATION HEADER)            
         LA    R7,1(R3)            R7 = A(ORDER CONFIRMATION HEADER)            
*                                                                               
         AR    R3,R2                                                            
*********                                                                       
* ORDER URL                                                                     
*********                                                                       
CNFM310  CLC   =C'ORDURL',1(R3)    DID WE GET A URL CONFIRMATION?               
         BNE   CNFM400             NO                                           
*                                                                               
         LA    R4,1(R3)            YES, WE HAVE ONE                             
         USING RORDURLD,R4                                                      
         NI    BITFLAG2,X'FF'-BF2NWURL                                          
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   KEY+DOKCMT-DOKEY,DOKCURLQ  LOOK FOR URL RECORD                   
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE  URL RECORD DOESN'T EXIST?                  
         BE    CNFM315                                                          
         OI    BITFLAG2,BF2NWURL     NO, NEED A NEW URL RECORD                  
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEFL                                                                  
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   0(L'DOKEY,R6),KEYSAVE                                            
         MVC   DORAGY,AGENCY                                                    
         DROP  R6                                                               
         B     CNFM320                                                          
*                                                                               
CNFM315  MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
CNFM320  L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
         DROP  R6                                                               
*                                                                               
         LA    R2,ELEM             ADD THE URL TO RECORD                        
         USING DOCM2ELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCM2EL,DOCM2ELQ                                                 
         MVC   DOCM2REV,REVISION                                                
         MVC   DOCM2TXT(L'ROURTEXT),ROURTEXT                                    
*                                                                               
         LA    R0,DOCM2OVH         LENGTH OF OVERHEAD BEFORE TEXT               
         CLI   ROURCONT,C'*'       MORE TO FOLLOW?                              
         BNE   CNFM330                                                          
         LLC   R1,0(R3)            BUMP TO NEXT                                 
         AR    R3,R1                                                            
*                                                                               
         LA    R4,1(R3)                                                         
         MVC   DOCM2TXT+L'ROURTEXT(L'ROURTEXT),ROURTEXT                         
         AHI   R0,L'ROURTEXT       ADD LENGTH OF FULL TEXT                      
*                                                                               
CNFM330  LA    R5,ROURTEXT+L'ROURTEXT-1                                         
CNFM335  CLI   0(R5),C' '                                                       
         BH    CNFM340                                                          
         BCT   R5,CNFM335                                                       
         L     R1,=AL4(*-T16300)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
CNFM340  LA    R5,1(R5)            BUMP 1 PAST LAST TO GET CORRECT LEN          
         LA    R1,ROURTEXT                                                      
         SR    R5,R1               SUBTRACT TO GET CORRECT LENGTH               
         AR    R0,R5               ADD TO PREVIOUS LENGTH IN R0                 
         STC   R0,DOCM2LEN                                                      
*                                                                               
CNFM350  GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(C'R',(R6))                         
*                                                                               
         L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
*                                                                               
         TM    BITFLAG2,BF2NWURL   NEW REP COMMENT RECORD?                      
         BZ    CNFM360                                                          
         MVC   AIO,AIO1                                                         
         BRAS  RE,ADD                                                           
         B     CNFM375                                                          
*                                                                               
CNFM360  MVC   AIO,AIO1                                                         
         BRAS  RE,PUT              ORDER COMMENTS FINISHED                      
*                                                                               
CNFM375  LLC   R1,0(R3)            BUMP TO NEXT                                 
         AR    R3,R1                                                            
*********                                                                       
* ORDER CONFIRMATION COMMENT                                                    
*********                                                                       
CNFM400  CLC   =C'ORDCOM',1(R3)                                                 
         BNE   CNFM455                                                          
*                                                                               
         TM    BITFLAG1,BF1IGRCM   IGNORE COMMENT?                              
         BNZ   CNFM440             YES                                          
*                                                                               
         NI    BITFLAG1,X'FF'-BF1NWCOM                                          
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   KEY+DOKCMT-DOKEY,2  LOOK FOR REP COMMENT RECORD                  
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE  REP COMMENT RECORD DOESN'T EXIST?          
         BE    CNFM410                                                          
         OI    BITFLAG1,BF1NWCOM     NO, NEED A NEW REP COMMENT RECORD          
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEFL                                                                  
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   0(L'DOKEY,R6),KEYSAVE                                            
         MVC   DORAGY,AGENCY                                                    
         DROP  R6                                                               
         B     CNFM420                                                          
*                                                                               
CNFM410  MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
CNFM420  L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   DORLEN,=Y(DORFRST-DOKEY)  REMOVES ALL COMMENT ELEMENTS           
         LA    R6,DORFRST                                                       
         DROP  R6                                                               
*                                                                               
         LA    R2,ELEM             ADD THE COMMENTS TO RECORD                   
         USING DOCOMELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCOMEL,DOCOMELQ                                                 
*                                                                               
CNFM425  LA    R4,1(R3)                                                         
         USING RORDCOMD,R4                                                      
*                                                                               
         MVC   DOCOMTXT(4),ROCMBLIN   BUYLINE GOES IN COMMENT                   
         MVI   DOCOMTXT+5,C'-'                                                  
         LLC   R1,0(R3)            L(LENGTH AND LINE)                           
         SH    R1,=Y(ROCMTEXT-RORDCOMD+2)                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DOCOMTXT+7(0),ROCMTEXT                                           
         OC    DOCOMTXT(L'ROCMTEXT+7),ALLSPCES                                  
         DROP  R4                                                               
*                                                                               
         LA    R1,DOCOMTXT+L'ROCMTEXT+7-1                                       
CNFM430  CLI   0(R1),C' '          CALCULATE LENGTH OF COMMENT                  
         BNE   CNFM435                                                          
         BCT   R1,CNFM430                                                       
         L     R1,=AL4(*-T16300)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
CNFM435  LR    R0,R1                                                            
         SR    R0,R2                                                            
         AH    R0,=H'1'                                                         
         STC   R0,DOCOMLEN                                                      
*                                                                               
         LLC   R1,DOCOMLIN         INCREMENT LINE NUMBER                        
         LA    R1,1(R1)                                                         
         STC   R1,DOCOMLIN                                                      
         DROP  R2                                                               
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(C'R',(R6))                         
         CLI   8(R1),C'R'          RECORD OVERFLOW?                             
         BE    *+12                                                             
         OI    BITFLAG1,BF1IGRCM   YES, IGNORE REST OF REP COMMENTS             
         B     CNFM440                                                          
*                                                                               
         LLC   R0,ELEM+1                                                        
         AR    R6,R0               R6 = A(NEXT INSERTION ADDRESS)               
CNFM440  LLC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         CLC   =C'ORDCOM',1(R3)    GOT ANOTHER COMMENT LINE?                    
         BNE   CNFM445                                                          
         TM    BITFLAG1,BF1IGRCM   YES, IGNORE IT?                              
         BZ    CNFM425                  NO                                      
         B     CNFM440                  YES                                     
*                                                                               
CNFM445  L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
*                                                                               
         TM    BITFLAG1,BF1IGRCM   IGNORE COMMENT?                              
         BNZ   CNFM455             YES                                          
*                                                                               
         TM    BITFLAG1,BF1NWCOM   NEW REP COMMENT RECORD?                      
         BZ    CNFM450                                                          
         MVC   AIO,AIO1                                                         
         BRAS  RE,ADD                                                           
         B     CNFM455                                                          
*                                                                               
CNFM450  MVC   AIO,AIO1                                                         
         BRAS  RE,PUT              ORDER COMMENTS FINISHED                      
*                                                                               
CNFM455  OC    POLORDER,POLORDER   NEED TO CHECK IF ALL BRANDS                  
         BZ    CNFM460             CONFIRMED?                                   
         BRAS  RE,CHKBORDS         YES                                          
         B     CNFMYES                                                          
*                                                                               
CNFM460  TM    BITFLAG2,BF2VARCN   VAR ORDER IS CONFIRMED NOW?                  
         BZ    CNFMYES             NO, NOTHING LEFT TO DO BUT LEAVE             
*                                                                               
         BRAS  RE,GETORDER                                                      
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         MVC   BCLT,DOIDCLT                                                     
         GOTO1 VCLUNPK,DMCB,BCLT,QCLT                                           
         MVC   BEST,DOIDEST                                                     
         EDIT  (B1,BEST),(3,QEST1),FILL=0                                       
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         MVC   QFLTNUM,=C'  '                                                   
         CLI   DOIDFLTN,0                                                       
         BE    CNFM465                                                          
         EDIT  (B1,DOIDFLTN),(2,QFLTNUM),FILL=0                                 
*                                                                               
CNFM465  L     R0,AIO2             BUILD PRDSLIST IN AIO2 FIRST                 
         LH    R1,=Y(IOA2-IOA1)                                                 
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     RE,AIO2                                                          
*                                                                               
CNFM470  CLI   0(R6),0                                                          
         BE    CNFM485                                                          
*                                                                               
         CLI   0(R6),DOVPRELQ                                                   
         BE    CNFM480                                                          
CNFM475  LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM470                                                          
*                                                                               
         USING DOVPRELD,R6                                                      
CNFM480  LLC   R1,DOVPRLEN                                                      
         SH    R1,=Y(DOVPROVH+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),DOVPRPRD                                                 
         LA    RE,1(R1,RE)                                                      
         B     CNFM475                                                          
*                                                                               
CNFM485  LR    R0,RE               SAVE LAST ADDRESS USED BY PRDSLIST           
         BAS   RE,GETCLTRC                                                      
         L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
*                                                                               
         LR    R1,R0               PUT PRDSLIST 10 BYTES AFTER CLT REC          
         L     R0,AIO2                                                          
         SR    R1,R0                                                            
         LA    R0,CLTHDRL+10(R6)                                                
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,CLTHDRL+10(R6)   R4 = A(1ST PRD PAIR) IN PRDSLIST             
*                                                                               
CNFM490  CLI   0(R4),0             ARE WE DONE WITH PRDSLIST?                   
         BE    CNFM500                                                          
         MVC   QPRD1,0(R4)                                                      
         MVC   QPRD2,1(R4)                                                      
*                                                                               
         GOTO1 GETQPRD,DMCB,(QPRD1,QPRD1)                                       
         GOTO1 GETQPRD,DMCB,(QPRD2,QPRD2)                                       
*                                                                               
         BRAS  RE,AUTOSCRP         GENERATE THE SCRIPT FILE                     
*                                                                               
         L     R0,AIO1                                                          
         AH    R0,=Y(IOA2-IOA1)                                                 
         LA    R4,2(R4)                                                         
         CR    R4,R0                                                            
         BL    CNFM490                                                          
*                                                                               
CNFM500  DS    0H                                                               
*                                                                               
CNFMYES  TM    BITFLAG2,BF2SPNDG                                                
         BZ    *+8                                                              
         BRAS  RE,SNDSCRPT                                                      
*                                                                               
         BRAS  RE,BLDCOLOR                                                      
         BAS   RE,DAREMAIL                                                      
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         BRAS  RE,SNDSUCSS         SEND SBS SUCCESS MESSAGE                     
         J     YES                                                              
*                                                                               
CNFMNO   TM    MISCFLG1,MF1XMTUP   HAVE I MADE CHANGES TO XMT ELEM?             
         BO    CNFM200             YES                                          
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
CNFMNO10 J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS GENERATES THE SCRIPT REC IN =NWK SO THAT IT WILL AUTOMATICALLY           
* SEND ORDERS FOR QPRD1,QPRD2                                                   
***********************************************************************         
AUTOSCRP NTR1  BASE=*,LABEL=*                                                   
*****                                                                           
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         MVC   TUSER,SIGNON2H      FOOL GETPROF                                 
         DROP  R1                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SDAR'    <=== NEED LOWER CASE 'S'                     
         NI    WORK,X'FF'-X'40'    MAKE IT LOWERCASE                            
         MVC   WORK+4(2),AGENCY                                                 
*                                                                               
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CGETPROF-COMFACSD(RF)                                         
         XC    PROFDAR,PROFDAR                                                  
         GOTO1 (RF),DMCB,WORK,PROFDAR,VDATAMGR                                  
*                                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'S0OM'    OM PROFILE TELLS US IF DAR OR OM             
         MVC   WORK+4(2),AGENCY                                                 
         LA    R4,WORK+16                                                       
         GOTO1 (RF),DMCB,WORK,(R4)                                              
*                                                                               
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         XC    TUSER,TUSER         RESET THE ID NUMBER                          
         DROP  R1                                                               
*                                                                               
         LA    R1,DMCB                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*****                                                                           
         BAS   RE,WRKRCREA                                                      
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPVARORD'                                            
         MVI   18(R1),C'I'                                                      
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'                                                      
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000001'                                               
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
         MVC   38(8,R1),=CL8'OM'    SIGN ON TO THE OM PROGRAM                   
*                                                                               
         MVC   30(8,R1),ROCFTOID                                                
         TM    BITFLAG1,BF1PSSWD   PASSWORD REQUIRED FOR SPOT SYSTEM?           
         BZ    *+10                                                             
         MVC   46(3,R1),=C'DDS'    YES, THEN USE THE EVER USEFUL DDS            
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'58'        54 + 4 BYTES FOR QSAM                      
*&&DO                                                                           
         MVC   30(8,R1),=CL8'NEWSTYLE'                                          
         MVC   30+16(8,R1),ROCFTOID                                             
         MVC   30+16+8(9,R1),=CL9',$SPTDARE'                                    
         MVC   30+60(4,R1),=C'T214'   USERID+PID (44 BYTES)                     
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'98'        94 + 4 BYTES FOR QSAM                      
*&&                                                                             
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000002'                                               
         MVC   10(20,R1),=CL20'$SPTDARE TO SPOT/DAR'                            
*                                                                               
         LA    R1,30(R1)                                                        
         USING LAYOUT2D,R1                                                      
         MVC   LAY2MED,QMED                                                     
         MVC   LAY2BYR,QBUYER                                                   
*                                                                               
         CLI   POMUSEOM,C'Y'                                                    
         BNE   *+10                                                             
         MVC   LAY2MTHD,QCSHTRDE                                                
*                                                                               
         MVC   LAY2CLT,QCLT                                                     
         MVC   LAY2STA,QSTA                                                     
         MVC   LAY2OORD,ROCFORDR                                                
*                                                                               
         MVC   LAY2PRDS(3),QPRD1                                                
         CLI   QPRD2,C' '          ANY PIGGYBACK?                               
         BNH   *+14                                                             
         MVI   LAY2PRDS+3,C'-'     YES                                          
         MVC   LAY2PRDS+4(3),QPRD2                                              
*                                                                               
         MVC   LAY2EST(L'QEST1),QEST1                                           
         CLC   QFLTNUM,=C'00'                                                   
         BNH   *+14                                                             
         MVI   LAY2EST+3,C'-'                                                   
         MVC   LAY2EST+4(2),QFLTNUM                                             
*                                                                               
         LA    R1,LAY2END                                                       
         DROP  R1                                                               
         L     RE,AIO2                                                          
         SR    R1,RE                                                            
         STH   R1,0(RE)            L'DATA + 4 BYTES FOR QSAM                    
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         BAS   RE,WRKRCLOS                                                      
*                                                                               
         L     RE,AWRKRIOA         RESET WORKER IO AREA FOR EDICT RECS          
         MVI   0(RE),0                                                          
         MVI   1(RE),2                                                          
ASCRPX   J     XIT                                                              
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECKS TO SEE IF ALL BRAND ORDERS OF A POL ORDER ARE CONFIRMED                
***********************************************************************         
CHKBORDS NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**BORD**'                                                    
*                                                                               
         NI    BITFLAG2,X'FF'-BF2ALLPD   NOT ALL PENDING CONFIRMED YET          
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
*                                                                               
CKBORD00 XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DOKEY,R6                                                         
         MVI   DVKTYPE,DVKTYPQ                                                  
         MVI   DVKSUBTY,DVKSTYPQ                                                
         MVC   DVKAGMD,BAGYMD                                                   
         MVC   DVKPORD,POLORDER                                                 
         DROP  R6                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
CKBORD07 CLC   KEY(DVKBORD-DOKEY),KEYSAVE   CHECKED ALL BRAND ORDERS?           
         BNE   CKBORD50            YES                                          
*                                                                               
         TM    BITFLAG2,BF2ALLPD   ALL BRANDS CONFIRM PENDING?                  
         BZ    *+8                                                              
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         TM    BITFLAG2,BF2ALLPD   ALL BRANDS CONFIRM PENDING?                  
         BZ    *+8                                                              
         OI    DORSTAT,X'01'       YES, MARK AS CONFIRMED                       
*                                                                               
         LA    R6,DORFRST                                                       
CKBORD10 CLI   0(R6),0                                                          
         BNE   CKBORD15                                                         
         L     R1,=A(*-T16300)                                                  
**>      LHI   R1,*-T16300                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
CKBORD15 CLI   0(R6),DOXMTELQ                                                   
         BE    CKBORD20                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CKBORD10                                                         
*                                                                               
         USING DOXMTELD,R6                                                      
CKBORD20 CLI   DOXMTSTA,QCFMDPND   MARKED AS CONFIRM PENDING?                   
         BNE   CKBORDX             NO, WAIT FOR CONFIRM FOR THIS ORDER          
*                                                                               
         TM    BITFLAG2,BF2ALLPD   YES, TIME TO MARK AS CONFIRMED?              
         BZ    CKBORD30                 NO, MORE BRANDS ARE POSSIBLE            
*                                                                               
         MVI   DOXMTSTA,QCFMD           YES, AND WRITE RECORD OUT               
*                                                                               
         MVC   DKEY,KEY                                                         
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
         MVC   KEY,DKEY                                                         
*                                                                               
CKBORD30 BRAS  RE,SEQ                                                           
         B     CKBORD07            CHECK NEXT BRAND ORDER                       
*                                                                               
CKBORD50 TM    BITFLAG2,BF2ALLPD   MARKED AS CONFIRM PENDING ALREADY?           
         BNZ   CKBORDX             YES, WE'RE DONE THEN                         
         OI    BITFLAG2,BF2ALLPD   NO, TIME TO CHANGE TO ALL CONFIRM            
         OI    MISCFLG1,MF1XMTUP                                                
         B     CKBORD00                                                         
*                                                                               
CKBO100  NI    BITFLAG2,X'FF'-BF2ALLPD   NOT ALL PENDING CONFIRMED YET          
         XC    CURDATTM,CURDATTM                                                
*                                                                               
CKBO110  XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DOKEY,R6                                                         
         MVI   DVKTYPE,DVKTYPQ                                                  
         MVI   DVKSUBTY,DVKSTYPQ                                                
         MVC   DVKAGMD,BAGYMD                                                   
         MVC   DVKPORD,POLORDER                                                 
         DROP  R6                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
CKBO117  CLC   KEY(DVKBORD-DOKEY),KEYSAVE   CHECKED ALL BRAND ORDERS?           
         BNE   CKBO150             YES                                          
*                                                                               
         TM    BITFLAG2,BF2ALLPD   ALL BRANDS CONFIRM PENDING?                  
         BZ    *+8                                                              
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         TM    BITFLAG2,BF2ALLPD   ALL BRANDS CONFIRM PENDING?                  
         BZ    *+8                                                              
         OI    DORSTAT,X'01'       YES, MARK AS CONFIRMED                       
*                                                                               
         LA    R6,DORFRST                                                       
CKBO120  CLI   0(R6),0                                                          
         BNE   CKBO125                                                          
         TM    MISCFLG1,MF1XMTUP   DID I UPDATE THE XMT?                        
         BNZ   CKBORDX             YES: DON'T SEND MESSAGE, EXIT!!!             
         L     R1,=A(*-T16300)                                                  
**>      LHI   R1,*-T16300                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
CKBO125  CLI   0(R6),DOSTELQ                                                    
         BE    CKBO130                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CKBO120                                                          
*                                                                               
         USING DOSTELD,R6                                                       
CKBO130  CLI   DOSTSTAT,QCFMDPND   MARKED AS CONFIRM PENDING?                   
         BNE   CKBORDX             NO, WAIT FOR CONFIRM FOR THIS ORDER          
*                                                                               
         TM    BITFLAG2,BF2ALLPD   YES, TIME TO MARK AS CONFIRMED?              
         BNZ   CKBO135                YES                                       
*                                     NO, MORE BRANDS ARE POSSIBLE              
         CLC   DOSTDATE,CURDATE       GET MOST RECENT DATE AND TIME             
         BH    CKBO132                                                          
         BL    CKBO140                                                          
         CLC   DOSTTIME,CURTIME                                                 
         BNH   CKBO140                                                          
CKBO132  MVC   CURDATTM,DOSTDATE                                                
         B     CKBO140                  NO, MORE BRANDS ARE POSSIBLE            
         DROP  R6                                                               
*                                                                               
CKBO135  LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ3                                                 
         MVC   DOSTDATE,CURDATE       MOVE IN MOST RECENT CNFMPDNG DATE         
         MVC   DOSTTIME,CURTIME       AND TIME                                  
         MVI   DOSTSTAT,QCFMD                                                   
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
CKBO137  CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    CKBO138          AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    CKBO138                                                          
         CLI   0(R6),DOSTELQ                                                    
         BNL   CKBO138                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CKBO137                                                          
*                                                                               
CKBO138  GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
         MVC   DKEY,KEY                                                         
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
         MVC   KEY,DKEY                                                         
*                                                                               
CKBO140  BRAS  RE,SEQ                                                           
         B     CKBO117             CHECK NEXT BRAND ORDER                       
*                                                                               
CKBO150  TM    BITFLAG2,BF2ALLPD   MARKED AS CONFIRM PENDING ALREADY?           
         BNZ   CKBORDX             YES, WE'RE DONE THEN                         
         OI    BITFLAG2,BF2ALLPD   NO, TIME TO CHANGE TO ALL CONFIRM            
         B     CKBO110                                                          
*                                                                               
CKBORDX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
CURDATTM DS    0XL7                                                             
CURDATE  DS    XL3                                                              
CURTIME  DS    XL2                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SEEDS THE CONTRACT NUMBER TO ALL THE BUYLINES THAT ARE           
* PART OF THE AGENCY ORDER.                                                     
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(REP CONTRACT NUMBER)                       
*              AIO1                A(AGENCY ORDER RECORD)                       
***********************************************************************         
SEEDCONN NTR1  BASE=*,LABEL=*                                                   
*        NMOD1 0,**SDCN**                                                       
         L     RC,0(R1)                                                         
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         L     R2,4(R1)                                                         
         MVC   CONTNUMB,0(R2)      MAKE A COPY OF REP CONTRACT NUMBER           
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         CLI   DOKCMT,0            DO WE HAVE A COMMENT RECORD HERE?            
         BE    SDCN00                                                           
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
*                                                                               
SDCN00   SR    R0,R0                                                            
         XC    TRDEMTHD(9),TRDEMTHD      CLEAR METHOD AND DATA                  
         LA    R6,DORFRST-DOKEY(R6)                                             
SDCN02   CLI   0(R6),0                                                          
         BE    SDCN10                                                           
         CLI   0(R6),DOSPELQ       GET TRADE METHOD AND SEE IF TRADE            
         BE    SDCN04                                                           
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SDCN02                                                           
*                                                                               
         USING DOSPELD,R6                                                       
SDCN04   CLI   DOSPLEN,DOSPLNQ                                                  
         BNH   SDCN10                                                           
         MVC   TRDEMTHD,DOSPTMTH   HOW TRADE IS DONE (LWRCASE IS CASH)          
         MVC   TRDEDATA,DOSPTDAT                                                
*                                                                               
SDCN10   L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         CLI   0(R6),1             MUST HAVE A DARE ID ELEMENT                  
         BE    SDCN15                                                           
         L     R1,=A(*-T16300)     WE SHOULD HAVE A DARE ID ELEMENT             
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                  CHECK IF WE CAN SEED CONTRACT NUMBER         
         USING DOIDELD,R6                                                       
SDCN15   LA    R4,KEY                                                           
         XC    KEY,KEY             READ CLIENT RECORD                           
         USING CLTHDRD,R4                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,DOIDCLT                                                  
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE   SAME BASE KEY?                            
         BE    SDCN20                                                           
         L     R1,=A(*-T16300)     WE SHOULD HAVE A DARE ID ELEMENT             
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
SDCN20   MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO2                                                         
         BRAS  RE,GET                                                           
         L     R4,AIO2                                                          
         CLI   CEXTRA+2,C'N'       CAN WE CHANGE THE ID?                        
         BE    SEEDC00                                                          
         CLI   CEXTRA+2,C'Y'                                                    
         BNE   SEEDCX              NO                                           
         DROP  R4                                                               
*                                                                               
SEEDC00  XC    KEY,KEY             READ STATION MASTER RECORD                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         XC    BMKTSTA,BMKTSTA                                                  
         MVC   BMKTSTA+2(L'DOISTA),DOISTA                                       
         GOTO1 VMSUNPK,DMCB,BMKTSTA,FULL,KEY+2                                  
         CLI   KEY+6,C' '          ANY BAND?                                    
         BH    *+8                                                              
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+7(2),AGENCY                                                  
         GOTO1 VCLUNPK,DMCB,DOIDCLT,KEY+9                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,STATION,KEY,AIO2                            
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         L     R4,AIO2                                                          
         USING STARECD,R4                                                       
         GOTO1 VMSPACK,DMCB,SMKT,STAKCALL,BMKTSTA                               
         DROP  R4                                                               
*                                                                               
         MVI   GBYACT,GBYINIT                                                   
         BRAS  RE,GOGETBUY                                                      
         MVC   SV1OR2,GBY1OR2                                                   
*                                                                               
         CLC   AGENCY,=C'SJ'       AGENCY SJ?                                   
         BNE   SEEDC05              NO                                          
         CLC   DOIDCLT,=X'CC2B'    CLIENT TBL?                                  
         BE    *+10                                                             
         CLC   DOIDCLT,=X'BCC9'    CLIENT PG0?                                  
         BE    *+10                 YES                                         
         CLC   DOIDCLT,=X'BCDA'    CLIENT PG1?                                  
         BNE   SEEDC05              NO, NONE OF THESE CLIENTS                   
         MVI   SV1OR2,2            TURN ON 2-BYTE BLN                           
*                                                                               
SEEDC05  XC    KEY,KEY             SET UP TO READ BUYLINES                      
         LA    R4,KEY                                                           
         USING BUYKEY,R4                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,DOIDCLT                                                  
         MVC   BUYKPRD,DOIDPRD                                                  
         MVC   PIGPRD,DOIDPRD2     SAVE PIGGY PRODUCT BINARY CODE               
         MVC   PIGEST,DOIDEST      THEN IT IS THE SAME ESTIMATE                 
*                                                                               
         MVC   BUYMSTA,BMKTSTA                                                  
         MVC   BUYKEST,DOIDEST                                                  
*                                                                               
         MVI   GBYACT,GBYHIGH                                                   
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(BUYKBUY-BUYKEY),KEYSAVE SAME BASE KEY?                       
         BNE   SEEDCX                      EXITING IS BETTER THAN DYING         
*                                                                               
         CLI   BUYKBUY,X'FF'       BRAND-POL BUY?                               
         LA    R4,KEYSAVE          KEYSAVE WILL BE THE KEY WE NEED              
         BNE   SEEDCX              NO, NO LONGER SUPPORTED                      
         MVI   BUYKBUY,X'FF'       YES, CHANGE KEYSAVE TO B-POL                 
         B     SEEDC15                                                          
*                                                                               
SEEDC10  CLI   0(R6),0             NO MORE BUYLINE ELEMENTS?                    
         BE    SEEDCX               YES                                         
         CLI   0(R6),DOBUYELQ      BUYLINE ELEMENT?                             
         BE    SEEDC20              YES, GO PROCESS IT                          
         CLI   0(R6),DOBY2ELQ      BUYLINE ELEMENT?                             
         BE    SEEDC25              YES, GO PROCESS IT                          
SEEDC15  LLC   R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     SEEDC10                                                          
*                                                                               
         USING DOBUYELD,R6                                                      
SEEDC20  CLI   BUYKBUY,X'FF'       BRAND-POL BUY?                               
         JNE   *+2                  NO, NON-POL NO LONGER SUPPORTED             
         MVC   BUYKBUY+2(L'DOBUYSPT),DOBUYSPT                                   
         B     SEEDC30                                                          
*&&DO                                                                           
         XC    BUYKBUY,BUYKBUY                                                  
         TM    DOBUYFLG,DOBUYFPG   PIGGYBACK PASSIVE?                           
         BNZ   SEEDC22                                                          
*                                                                               
         MVI   BUYKBUY+2,1         NO, ASSUME BUY REGULAR, NO P/B               
         TM    DOBUYFLG,DOBUYFPA   PIGGYBACK ACTIVE?                            
         BZ    *+8                                                              
         MVI   BUYKBUY+2,2         ASSUME BUY REGULAR, NO P/B                   
         MVC   BUYKBUY+1(L'DOBUYSPT),DOBUYSPT                                   
         B     SEEDC30                                                          
*                                                                               
SEEDC22  MVC   BUYKBUY(L'PIGPRD),PIGPRD         YES, PIGGY PASSIVE              
         MVC   BUYKBUY+1(L'PIGEST),PIGEST                                       
         MVC   BUYKBUY+2(L'DOBUYSPT),DOBUYSPT                                   
         B     SEEDC30                                                          
*&&                                                                             
         DROP  R6                                                               
*                                                                               
         USING DOBY2ELD,R6                                                      
SEEDC25  CLI   BUYKBUY,X'FF'       BRAND-POL BUY?                               
         JNE   *+2                  NO, NON-POL NO LONGER SUPPORTED             
         MVC   BUYKBUY+1(L'DOBY2SPT),DOBY2SPT                                   
         B     SEEDC30                                                          
*&&DO                                                                           
         XC    BUYKBUY,BUYKBUY                                                  
         TM    DOBY2FLG,DOBY2FPG   PIGGYBACK PASSIVE?                           
         BNZ   SEEDC27                                                          
*                                                                               
         MVI   BUYKBUY+2,1         NO, ASSUME BUY REGULAR, NO P/B               
         TM    DOBY2FLG,DOBY2FPA   PIGGYBACK ACTIVE?                            
         BZ    *+8                                                              
         MVI   BUYKBUY+2,2         ASSUME BUY REGULAR, NO P/B                   
         MVC   BUYKBUY+1(L'DOBUYSPT),DOBUYSPT                                   
         B     SEEDC30                                                          
*                                                                               
SEEDC27  MVC   BUYKBUY(L'PIGPRD),PIGPRD         YES, PIGGY PASSIVE              
         MVC   BUYKBUY+1(L'PIGEST),PIGEST                                       
         MVC   BUYKBUY+2(L'DOBUYSPT),DOBUYSPT                                   
*&&                                                                             
         DROP  R6                                                               
SEEDC30  CLC   KEY(BUYKBUY-BUYKEY),KEYSAVE MAKE SURE SAME BASE KEY              
         BNE   SEEDCX                      JUST EXIT BETTER THAN DYING          
         CLC   KEY(L'BUYKEY-1),KEYSAVE     HAVE A MATCH FOR BUYLINE?            
         BE    SEEDC40                     YES                                  
         DROP  R4                                                               
*                                                                               
SEEDC35  MVI   GBYACT,GBYSEQ                                                    
         BRAS  RE,SEQ              NO, CHECK NXT KEY                            
         B     SEEDC30                                                          
*                                                                               
SEEDC40  MVC   AIO,AIO2                                                         
         MVI   DMINBTS,X'88'                                                    
         MVI   GBYACT,GBYGET                                                    
         BRAS  RE,GET                                                           
         CLI   GBYERR,0                                                         
         BE    SEEDC42                                                          
         CLI   GBYERR,X'02'        RECORD DELETED?                              
         BE    SEEDC35             YES, READ NEXT RECORD                        
         DC    H'0'                                                             
*                                                                               
SEEDC42  L     R2,AIO2                                                          
         USING BUYRECD,R2                                                       
         MVC   BYTE,TRDEMTHD                                                    
         OI    BYTE,C' '                                                        
*                                                                               
         CLI   BYTE,C'R'           TRADE DETERMINED BY REP?                     
         BNE   SEEDC49X            NO                                           
         XR    R1,R1               YES,                                         
         ICM   R1,3,BDREP                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         CLC   WORK(3),TRDEDATA    BUY'S REP MATCHES TRADE REP?                 
         BNE   SEEDC45             NO                                           
         TM    TRDEMTHD,X'40'      YES, ORDER IS CASH?                          
         BZ    SEEDC35                  YES, NOT A CASH BUY                     
         B     SEEDC49X                                                         
*                                                                               
SEEDC45  TM    TRDEMTHD,X'40'      ORDER IS TRADE?                              
         BNZ   SEEDC35             YES, NOT A TRADE BUY                         
*                                                                               
SEEDC49X DS    0H                                                               
*  PAST THE TRADE METHOD TEST                                                   
         TM    BDCIND2,X'02'       TEST TRADE BUY                               
         BO    SEEDC35             DON'T CHANGE THE CONTRACT                    
         LA    R2,BDELEM           R2 = A(FIRST ELEMENT)                        
         SR    R0,R0                                                            
         DROP  R2                                                               
*                                                                               
SEEDC50  CLI   0(R2),0             END-OF-RECORD?                               
         BE    SEEDC60                                                          
         CLI   0(R2),X'70'         A X'70' OUT THERE ALREADY?                   
         BNL   SEEDC60                                                          
         IC    R0,1(R2)            YES, PUT REP CONTRACT # BEFORE               
         AR    R2,R0                                                            
         B     SEEDC50                                                          
*                                                                               
SEEDC60  CLI   0(R2),X'70'         HARDCODED BECAUSE NO REAL DSECT              
*&&DO                                                                           
         BNE   SEEDC70                                                          
         CLI   1(R2),15                                                         
         BL    SEEDC70                                                          
         MVC   3(L'CONTNUMB,R2),CONTNUMB   CHANGE THE ID                        
         B     SEEDC80                                                          
*&&                                                                             
         BE    SEEDCNXB            IF WE HAVE ONE, SKIP IT                      
*                                                                               
SEEDC70  XC    ELEM,ELEM           HARDCODED BECAUSE NO REAL DSECT              
         MVI   ELEM,X'70'                                                       
         MVI   ELEM+1,15                                                        
         MVC   ELEM+3(L'CONTNUMB),CONTNUMB                                      
         GOTO1 VRECUP,DMCB,(C'S',AIO2),ELEM,(R2)                                
*                                                                               
SEEDC80  MVC   WORK(L'KEY),KEY                                                  
         XC    KEY,KEY                                                          
         L     RE,AIO2                                                          
         MVC   KEY(L'BUYKEY),0(RE)                                              
         ST    RE,AIO                                                           
         MVI   GBYACT,GBYPUT                                                    
         BRAS  RE,PUT                                                           
         MVC   KEY,WORK            RE-ESTABLISH OUR PREVIOUS KEY                
*                                                                               
SEEDCNXB MVI   GBYACT,GBYSEQ                                                    
         BRAS  RE,SEQ              CHECK NEXT KEY                               
         B     SEEDC15                                                          
*                                                                               
SEEDCX   J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* READ THE ORDER RECORD/DELETE CURRENT COLOR PTR/ADD NEW COLOR PTR              
* SVSTAT HAS STATUS OF CURRENT NOTICE REC IN IO1                                
* ELEM CONTAINS STATUS ELEMENT FROM MAKEGOOD NOTICE RECORD                      
*=====================================================================*         
         SPACE 1                                                                
BLDCOLOR NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GETORDER         READ THE ORDER RECORD TO AIO1                
*                                                                               
         NI    MISCFLG2,X'FF'-MF2CNFCM                                          
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,DOSPELQ                                                   
         MVI   ELCDHI,DOSPELQ                                                   
         BRAS  RE,BNEXTEL                                                       
         USING DOSPELD,R6                                                       
         TM    DOSPFLG1,DOSPCFCM          CONFIRM WITH COMMENT?                 
         BZ    *+8                                                              
         OI    MISCFLG2,MF2CNFCM                                                
*                                                                               
BLDCLR3  MVI   BYTE,0                                                           
         CLI   THISISMG,C'Y'       IS THIS FOR A MAKEGOOD?                      
         BNE   BLDCLR5             NO, GET CURRENT ORDER STATUS                 
*                                                                               
*** CODE FOR MKGD ONLY             YES                                          
         BRAS  RE,MKGSTADR         R1 = A(STATUS TABLE)                         
         CLI   SVSTAT,0            WAS SVSTAT SET? ONLY FOR MG!!                
         BE    BLDCLRX             NO, EXIT                                     
         B     BLDCLR10                                                         
*                                                                               
*** CODE FOR ORDER ONLY                                                         
BLDCLR5  BRAS  RE,ORDSTADR         R1 = A(STATUS TABLE)                         
*                                                                               
         CLI   SVSTAT,0            WAS SVSTAT SET?                              
         BNE   BLDCLR10            YES,                                         
         L     R6,AIO1             NO, GET MOST CURRENT ORDSTAT                 
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,DOSTELQ                                                   
         MVI   ELCDHI,DOSTELQ                                                   
         BRAS  RE,BNEXTEL                                                       
         BNE   BLDCLRX                                                          
         USING DOSTELD,R6                                                       
         MVC   SVSTAT,DOSTSTAT     MOVE STATUS                                  
         DROP  R6                                                               
*                                                                               
         USING ORDD,R1                                                          
BLDCLR10 CLC   SVSTAT,ORDSTAT                                                   
         BNE   BLDCLR15                                                         
*                                                                               
         CLI   ORDTYP,0            IS TYPE BIT SET?                             
         BE    BLDCLR20                                                         
*                                                                               
         CLI   POMAUCFM,C'A'       PROFILE SET TO AUTO-CONFIRM?                 
         BE    BLDCLR12                                                         
         CLI   POMAUCFM,C'B'       PROFILE SET TO BUYER-CONFIRM?                
         BNE   BLDCLR20            ALL CONFIRMS ARE BLACK!!!                    
*                                   CHOOSE FIRST CNFM ENTRY IN TABLE            
BLDCLR12 TM    ORDTYP,ORDTCMTS            TEST WITH COMMENTS                    
         BNO   *+12                                                             
         TM    MISCFLG2,MF2CNFCM          ARE THERE COMMENTS ?                  
         BO    BLDCLR20                   YES, GOT IT                           
         TM    ORDTYP,ORDTNCMT            TEST WITH NO COMMENTS                 
         BNO   BLDCLR15                                                         
         TM    MISCFLG2,MF2CNFCM          ARE THERE COMMENTS ?                  
         BZ    BLDCLR20                   NO, GOT IT                            
*                                                                               
BLDCLR15 LA    R1,L'STATTAB(R1)                                                 
         CLI   0(R1),X'FF'                                                      
         BNE   BLDCLR10                                                         
         DC    H'0'                                                             
*                                                                               
BLDCLR20 MVC   BYTE,1(R1)          SAVE CORRESPONDING COLOR                     
         CLI   THISISMG,C'Y'       IS THIS FOR A MAKEGOOD?                      
         BNE   BLDCLR40                                                         
*                                                                               
         SR    R0,R0               LOOK FOR OUR MAKEGOOD COLOR                  
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,MGCOLELQ                                                  
         MVI   ELCDHI,MGCOLELQ                                                  
BLDCLR30 BRAS  RE,BNEXTEL          CAN'T FIND IT?                               
         BE    BLDCLR35                                                         
BLDCLR33 MVI   SVSTAT,0                                                         
         MVI   THISISMG,C'N'       NO LONGER A MAKEGOOD!!!                      
         B     BLDCLR3             RESTORE COLOR BASED ON DOSTSTAT AND          
*                                   AND MGCOLEL                                 
         USING MGCOLELD,R6                                                      
BLDCLR35 CLC   QMGGROUP,MGCOLCOD   FOUND OUR MAKEGOOD?                          
         BNE   BLDCLR30                                                         
         MVC   MGCOLCOL,BYTE       YES, UPDATE THE COLOR                        
         CLI   BYTE,C'K'           CAN WE DELETE THIS MAKEGOOD COLOR?           
         BNE   BLDCLR40                                                         
         GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6),(R6)   YES, DELETE                  
*                                                                               
BLDCLR40 CLI   BYTE,C'G'           TEST IF CURRENT CHANGE GREEN                 
         BE    BLDCLR75            YES - GREEN ALWAYS WINS                      
*                                                                               
         L     R6,AIO1             FIND MG GROUP COLOR ELEMENTS                 
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,MGCOLELQ                                                  
         MVI   ELCDHI,MGCOLELQ                                                  
*                                                                               
BLDCLR55 BRAS  RE,BNEXTEL                                                       
         BNE   BLDCLR62                                                         
         USING MGCOLEL,R6                                                       
         CLI   MGCOLCOL,C'G'                                                    
         BNE   BLDCLR60                                                         
         MVI   BYTE,C'G'           SET TO GREEN AND DONE                        
         B     BLDCLR75                                                         
*                                                                               
BLDCLR60 CLI   MGCOLCOL,C'R'                                                    
         BNE   BLDCLR55                                                         
         MVI   BYTE,C'R'                                                        
         B     BLDCLR55                                                         
         DROP  R6                                                               
*                                                                               
BLDCLR62 CLI   THISISMG,C'Y'       IS THIS FOR A MAKEGOOD?                      
         BNE   BLDCLR75            NO                                           
         CLI   BYTE,C'K'           YES, AM I BLACK? (NO MORE MG??)              
         BNE   BLDCLR74                                                         
*                                                                               
         CLI   POMAUCFM,C'A'       PROFILE SET TO AUTO-CONFIRM?                 
         BNE   BLDCLR33            NO                                           
         L     R6,AIO              ADD FULLY CONFIRMED STATUS!!                 
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,DOSTELQ                                                   
         MVI   ELCDHI,DOSTELQ                                                   
         BRAS  RE,BNEXTEL                                                       
         USING DOSTELD,R6                                                       
         CLI   DOSTSTAT,QCFMD      AM I CONFIRMED?                              
         BNE   BLDCLR33                                                         
*                                                                               
         CLI   DOSTLEN,DOSTLNQ3    DO I HAVE A TYPE FIELD?                      
         BE    BLDCLR65                                                         
         TM    MISCFLG2,MF2CNFCM                                                
         BZ    BLDCLR33                                                         
         B     BLDCLR67                                                         
BLDCLR65 TM    DOSTTYPE,DCNFMCOM   CONFIRMED WITH COMMENTS?                     
         BZ    BLDCLR69                                                         
         DROP  R6                                                               
*                                                                               
BLDCLR67 LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ3                                                 
         MVC   DOSTDATE(5),SVDATTIM  MOVE IN DATE/TIME OF MG CANCEL             
         MVI   DOSTSTAT,QCFMD                                                   
         OI    DOSTTYPE,DCNFMFUL                                                
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
BLDCLR69 NI    MISCFLG2,X'FF'-MF2CNFCM                                          
         B     BLDCLR33                                                         
         DROP  R4                                                               
*                                                                               
BLDCLR74 CLI   BYTE,C'G'           YES, IS MKGD GREEN?                          
         BNE   BLDCLR33            NO, CHK DOSTSTAT AND MGCOLEL FOR             
*                                      HIGHEST COLOR                            
BLDCLR75 L     R6,AIO1             FIND COLOR ELEMENT IN ORDER REC              
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
         MVI   K.DSCKTYPE,DSCKTYPQ                                              
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
BLDCLR82 GOTO1 VRECUP,DMCB,AIO1,(R6)    DELETE OLD COLOR ELEM                   
         BRAS  RE,BNEXTEL2              IN CASE ANY MORE AROUND                 
         BE    BLDCLR82                                                         
*                                                                               
BLDCLR85 GOTO1 VRECUP,DMCB,AIO1,ELEM+128,(R6)   ADD NEW                         
*                                                                               
         BRAS  RE,PUT              WRITE THE ORDER RECORD                       
***************                                                                 
* COMMENTED THIS CODE OUT BECAUSE EVEN A MKGCAN AFFECTS THE ORDER COLOR         
***************                                                                 
*&&DO                                                                           
         CLI   THISISMG,C'Y'       THIS IS FOR A MAKEGOOD?                      
         BNE   BLDCLR87            'C' IS CONFIRM FOR ORDERS                    
         CLI   SVSTAT,MNSTCAN      TEST CANCELLED                               
         BE    BLDCLRX             YES - JUST EXIT                              
*&&                                                                             
***************                                                                 
* COMMENTED THIS CODE OUT BECAUSE EVEN A MKGCAN AFFECTS THE ORDER COLOR         
***************                                                                 
K        USING DSCKTYPE,KEY                                                     
BLDCLR87 XC    KEY,KEY             CREATE NEW COLOR POINTER                     
         MVI   K.DSCKTYPE,DSCKTYPQ                                              
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
BLDCLR90 MVC   KEY,KEYSAVE         RESTORE                                      
         BRAS  RE,ADDDIR                                                        
*                                                                               
BLDCLRX  J     YES                                                              
*                                                                               
ORDSTADR BASR  R1,RE               POINT R1 TO STATUS TABLE                     
STATTAB  DS    0XL3                                                             
         DC    AL1(DSENT),C'G',AL1(0)                                           
         DC    AL1(DFXSENT),C'G',AL1(0)                                         
         DC    AL1(DEMSENT),C'G',AL1(0)                                         
         DC    AL1(QRJCT),C'G',AL1(0)                                           
         DC    AL1(QEMPTY),C'G',AL1(0)                                          
         DC    AL1(QERRORED),C'G',AL1(0)                                        
         DC    AL1(QFAXCNCL),C'G',AL1(0)                                        
         DC    AL1(QRCLAPPR),C'G',AL1(0)                                        
         DC    AL1(QRCLDELN),C'G',AL1(0)                                        
         DC    AL1(QRCLUNKN),C'G',AL1(0)                                        
         DC    AL1(QRCLTRNS),C'G',AL1(0)                                        
         DC    AL1(QRCLWIP),C'G',AL1(0)                                         
         DC    AL1(QSNTPNDG),C'G',AL1(0)                                        
         DC    AL1(QSNTXCNF),C'G',AL1(0)                                        
         DC    AL1(QSNTXREJ),C'G',AL1(0)                                        
         DC    AL1(QTOBESNT),C'G',AL1(0)                                        
*                                                                               
         DC    AL1(QAPP),C'R',AL1(0)                                            
         DC    AL1(QRECALL),C'R',AL1(0)                                         
         DC    AL1(QRCLCONF),C'R',AL1(0)                                        
         DC    AL1(QRCLREJD),C'R',AL1(0)                                        
         DC    AL1(MNSTDELV),C'R',AL1(0) B/C OF TRILOGY, WE NEED DELVRD         
         DC    AL1(DDLVRD),C'R',AL1(0)                                          
         DC    AL1(QFAXDLVD),C'R',AL1(0)                                        
         DC    AL1(DFXDLVD),C'R',AL1(0)                                         
         DC    AL1(DEMDLVD),C'R',AL1(0)                                         
*                                                                               
* DON'T CHANGE ORDER OF QCFMD, I HAVE CODE DEPENDING ON THIS ORDER!!            
         DC    AL1(QCFMD),C'K',AL1(ORDTNCMT) CONFIRMED W/O COMMENTS             
         DC    AL1(QCFMD),C'R',AL1(ORDTCMTS) CONFIRMED WITH COMMENTS            
*                                                                               
         DC    AL1(QBYRCNFM),C'K',AL1(0)                                        
         DC    AL1(QCFMDPND),C'K',AL1(0)                                        
         DC    AL1(QNODARE),C'K',AL1(0)                                         
         DC    AL1(QUNDARE),C'K',AL1(0)                                         
         DC    X'FF'                                                            
*                                                                               
MKGSTADR BASR  R1,RE               POINT R1 TO STATUS TABLE                     
         DC    AL1(MNSTNEW),C'G',AL1(0)                                         
         DC    AL1(MNSTAMND),C'G',AL1(0)                                        
         DC    AL1(MNSTGOIN),C'G',AL1(0)                                        
         DC    AL1(MNSTHOLD),C'G',AL1(0)                                        
         DC    AL1(MNSTERR),C'G',AL1(0)                                         
*                                                                               
         DC    AL1(MNSTAPP),C'R',AL1(0)                                         
         DC    AL1(MNSTREJ),C'R',AL1(0)                                         
         DC    AL1(MNSTCANM),C'R',AL1(0)                                        
         DC    AL1(MNSTDELV),C'R',AL1(0)                                        
         DC    AL1(MNSTSAPP),C'R',AL1(0)                                        
*                                                                               
         DC    AL1(MNSTCAN),C'K',AL1(0)                                         
         DC    AL1(MNSTOKAY),C'K',AL1(0)                                        
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY MAKEGOOD NOTICE RECORD AND PUTS IT IN AIO1                    
*                                                                               
* ON ENTRY:    BAGYMD              BINARY AGENCY/MEDIA                          
*              BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETNOTCE NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MNKEY,R4                                                         
         MVI   MNKTYPE,MNKTYPQ                                                  
         MVI   MNKSUBTY,MNKSTYPQ                                                
         MVC   MNKAGMD,BAGYMD                                                   
         MVC   MNKBYR,QBUYER                                                    
         MVC   MNKORDER,BINORDER                                                
         MVC   MNKGROUP,QMGGROUP                                                
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(L'MNKEY),KEYSAVE                                             
         BNE   GNTCENO                                                          
*********                                                                       
* READ NOTICE RECORD FOR UPDATE                                                 
*********                                                                       
         MVC   SVMKNDA,KEY+14      SAVE DISK ADDRESS FOR PASSIVES               
         MVI   DMINBTS,X'88'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
         CLI   DMCB+8,0            DELETED ?                                    
         BE    GNTCEYES                                                         
         B     GNTCENO             DO NOT DIE BECAUSE OF THIS!!!                
*&&DO                                                                           
         L     R1,=A(*-T16300)     DELETED RECORD BUT KEY IS NOT                
**>      LHI   R1,*-T16300         DELETED RECORD BUT KEY IS NOT                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*&&                                                                             
GNTCEYES J     YES                                                              
*                                                                               
GNTCENO  CLC   =C'ERRNOT',0(R7)           ERROR ON ERRNOT?                      
         JE    NO                                                               
         LHI   R1,REFMGDNE                OFFER DOES NOT EXIST                  
         BRAS  RE,SNDERROR                                                      
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY ORDER RECORD FOR UPDATE AND PUTS IT IN AIO1                   
*                                                                               
* ON ENTRY:    BAGYMD              BINARY AGENCY/MEDIA                          
*              BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETORDER NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DOKTYPE,DOKTYPQ                                                  
         MVI   DOKSUBTY,DOKSTYPQ                                                
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER                                                
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BNE   GORDRNO                                                          
         CLI   DOKCMT,0            WE HAVE A COMMENT INSTEAD?                   
         BNE   GORDRNO             NO GOOD, ERROR, BETTER THAN DC H'0'          
         DROP  R4                                                               
*********                                                                       
* READ ORDER RECORD FOR UPDATE                                                  
*********                                                                       
         MVC   SVORDDA,KEY+14      SAVE DISK ADDRESS FOR PASSIVES               
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVING VALUES FOR 0DBC XSPKEY                
         MVC   BCLT,DOIDCLT                                                     
         MVC   BPRD,DOIDPRD                                                     
         MVC   BEST,DOIDEST                                                     
*                                                                               
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,NEXTEL                                                        
         USING DOSPELD,R6                                                       
         MVC   BMKTSTA(2),DOSPMKT         MARKET                                
         DROP  R6                                                               
*                                                                               
GORDRYES J     YES                                                              
*                                                                               
GORDRNO  CLC   =C'ERRNOT',0(R7)           ERROR ON ERRNOT?                      
         JE    NO                                                               
         LHI   R1,REFORDNE          ORDER DOES NOT EXIST                        
         BRAS  RE,SNDERROR                                                      
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY MAKEGOOD OFFER RECORD AND PUTS IT IN AIO1                     
*                                                                               
* ON ENTRY:    BAGYMD              BINARY AGENCY/MEDIA                          
*              BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETOFFER NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MOKEY,R4                                                         
         MVI   MOKTYPE,MOKTYPQ                                                  
         MVI   MOKSUBTY,MOKSTYPQ                                                
         MVC   MOKAGMD,BAGYMD                                                   
         MVC   MOKORDER,BINORDER                                                
         MVC   MOKMGCD,QMGGROUP                                                 
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'MOKEY),KEYSAVE                                             
         BNE   GOFFRNO                                                          
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
         CLI   DMCB+8,0                                                         
         BE    GOFFRYES                                                         
         L     R1,=A(*-T16300)     DELETED RECORD BUT KEY IS NOT                
**>      LHI   R1,*-T16300         DELETED RECORD BUT KEY IS NOT                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
GOFFRYES J     YES                                                              
*                                                                               
GOFFRNO  CLC   =C'ERRNOT',0(R7)           ERROR ON ERRNOT?                      
         JE    NO                                                               
         LHI   R1,REFMGDNE          OFFER DOES NOT EXIST                        
         BRAS  RE,SNDERROR                                                      
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETS THE X'30' COMMENTS AND PUT IN BLOCK                                      
*  TYPICALLY, THIS IS COMMENTS BUT WHEN ORDER IS ERROR, ITS USED TO             
*  STORE THE ERROR TEXT FOR RADIO ORDERS                                        
*                                                                               
* ON ENTRY:    AIO                 HAS ORDER RECORD                             
* ON EXIT :    BLOCK  CONTAINS X'30' TEXT,  PRECEEDING SPACES REMOVED           
***********************************************************************         
GET30TXT NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
G30T10   CLI   0(R6),0             EOR?                                         
         JE    NO                   YES- EXIT WITH NO-CC                        
         CLI   0(R6),DOCOMELQ      X'30' COMMENT                                
         JE    G30T20               YES, LETS COPY IT                           
         LLC   R1,1(R6)             NO, GO GET NEXT ELEMENT                     
         AR    R6,R1                                                            
         J     G30T10                                                           
         USING DOCOMELD,R6                                                      
G30T20   LLC   R1,1(R6)                                                         
         AR    R1,R6               POINT R1 TO BYTE BEYOND THE ELEMENT          
         LA    RF,3(R6)            POINT RF TO BEGINING OF TEXT                 
G30T30   CR    RF,R1               END-OF-TEXT / HAVE EMPTY TEXT??              
         JNL   NO                   YES, THEN EXIT WITH NO-CC                   
         CLI   0(RF),C' '          HIGHER THAN SPACE?                           
         JH    G30T40              YES, LETS COPY IT                            
         AHI   RF,1                BUMP RF                                      
         J     G30T30                                                           
G30T40   SR    R1,RF               GET LENGTH OF TEXT                           
         BCTR  R1,0                 SETUP FOR EX INSTR                          
         EX    R1,G30TXT                                                        
         OC    BLOCK(L'GTSPACES),GTSPACES                                       
         J     YES                                                              
G30TXT   MVC   BLOCK(0),0(RF)      COPY TEXT TO BLOCK                           
GTSPACES DC    CL50' '                                                          
         LTORG                                                                  
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
         CLI   GBYACT,0                                                         
         JE    HIGH10                                                           
         CLI   GBYACT,GBYHIGH                                                   
         JNE   *+2                                                              
         BRAS  R1,GOGETBUY                                                      
*                                                                               
HIGH10   BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
HIGHREP  MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(REPDIR)                                                      
*                                                                               
SEQ      CLI   GBYACT,0                                                         
         JE    SEQ10                                                            
         CLI   GBYACT,GBYSEQ                                                    
         JNE   *+2                                                              
         BRAS  R1,GOGETBUY                                                      
*                                                                               
SEQ10    BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
SEQREP   BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(REPDIR)                                                      
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
GET      CLI   GBYACT,0                                                         
         JE    GET10                                                            
         CLI   GBYACT,GBYGET                                                    
         JNE   *+2                                                              
         BRAS  R1,GOGETBUY                                                      
*                                                                               
GET10    BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'               DSPL TO DISK ADDRESS                         
         SPACE 1                                                                
GETREP   BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(REPFILE)                                                     
         DC    H'28'               DSPL TO DISK ADDRESS                         
*                                                                               
PUT      CLI   GBYACT,0                                                         
         JE    PUT10                                                            
         CLI   GBYACT,GBYPUT                                                    
         JNE   *+2                                                              
         BRAS  R1,GOGETBUY                                                      
*                                                                               
PUT10    BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
         SPACE 1                                                                
PUTREP   BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(REPFILE)                                                     
         DC    H'28'               DSPL TO DISK ADDRESS                         
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
*                                                                               
GOGETBUY NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
*                                                                               
         MVC   GBYCOMF,SRPARMSD.SRQACOMF                                        
*                                                                               
         CLI   GBYACT,GBYINIT                                                   
         BNE   GOGETB10                                                         
         MVC   GBYAGY,AGENCY                                                    
         B     GOGETB50                                                         
*                                                                               
GOGETB10 MVC   GBYDMIN,DMINBTS                                                  
         MVI   GBYDMOUT,X'FF'                                                   
*                                                                               
         CLI   GBYACT,GBYHIGH                                                   
         BE    GOGETB20                                                         
         CLI   GBYACT,GBYSEQ                                                    
         BNE   GOGETB30                                                         
GOGETB20 LAY   R1,KEY                                                           
         ST    R1,GBYKEYIN                                                      
         ST    R1,GBYKEYOT                                                      
         MVC   GBY1OR2,SV1OR2                                                   
         B     GOGETB50                                                         
*                                                                               
GOGETB30 CLI   GBYACT,GBYGET                                                    
         BE    GOGETB40                                                         
         CLI   GBYACT,GBYPUT                                                    
         BNE   GOGETB50                                                         
GOGETB40 LA    RE,KEY+14                                                        
         ST    RE,GBYDA                                                         
         MVC   GBYIOA,AIO                                                       
         LA    RE,GBWORK                                                        
         ST    RE,GBYDMWRK                                                      
*        LA    RE,IOBRDLST                                                      
*        ST    RE,GBYPRDL                                                       
         MVC   GBY1OR2,SV1OR2                                                   
*                                                                               
GOGETB50 GOTOR AGETBUY,GETBLK                                                   
         MVI   GBYACT,0                                                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
DAREOBJD DSECT                                                                  
DOBJTID  DS    CL6                 TRANSMISSION ID                              
DOBJRIDN DS    XL1                 RETURN ID NUMBER                             
DOBJNXT  DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
*                                                                               
RELO     DS    A                                                                
SAVERD   DS    A                                                                
AUTL     DS    A                                                                
ATBUFF   DS    A                                                                
EDICTFL  DS    A                   A(EDICT FILE)                                
DAFILT   DS    A                   DISK ADDRESS FILTER                          
DSKAD    DS    A                   LAST PAGE'S LAST DISK ADDRESS                
AIO      DS    A                                                                
AIO1     DS    A                   A(IOAREA #1)                                 
AIO2     DS    A                   A(IOAREA #2)                                 
ASPLAREA DS    A                   A(SPOOL AREA)                                
AWRKRIOA DS    A                   A(IO AREA USED BY EDICT)                     
AWRKRBUF DS    A                   A(WORKER BUFFER AREA)                        
AHUGEBLK DS    A                   A(HUGE BLOCK)                                
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
VDARREPS DS    V                                                                
VMSPACK  DS    V                                                                
VMSUNPK  DS    V                                                                
VSWITCH  DS    V                                                                
*                                                                               
ASPOOL   DS    A                                                                
ASTAPACK DS    A                                                                
AREPFACS DS    A                                                                
AGETDARE DS    A                                                                
AGETBUY  DS    A                                                                
*                                                                               
SRPARMS  DS    8F                  SERVICE REQUEST PARAMETERS                   
EDCTFDSK DS    F                   EDICTFIL DISK ADDRESS   TTTTBB00             
EDCTFDSP DS    F                   EDICTFIL DISP ADDRESS   DD999999             
DMCB     DS    6F                                                               
FULL     DS    F                                                                
SVORDDA  DS    A                   SAVED DISK ADDRESS OF ORDER                  
SVMKNDA  DS    A                   SAVED DISK ADDRESS OF MKGD NOTICE            
*                                                                               
EDCTFRPT DS    H                   EDICTFIL PHYSICAL RECORDS PER TRACK          
EDCTFTPD DS    H                   EDICTFIL TRACKS PER DAY                      
EDCTFATQ EQU   40                     ON ADV                                    
EDCTFRTQ EQU   40                     ON REP                                    
EDCTFTTQ EQU   2                      ON TEST                                   
EDCTFMTQ EQU   2                      ON MEL                                    
EDCTRPBQ DS    H                   EDICTFIL LOGICAL RECORDS PER BLOCK           
EDCTLRCL DS    H                   EDICTFIL LOGICAL RECORD LENGTH               
EDCTFLST DS    H                   LAST TRACK FOR GIVEN DAY                     
FRSTTRK  DS    H                   FIRST TRAC FOR GIVEN DAY                     
RECLEN   DS    H                                                                
TERMNUM  DS    H                   TERMINAL NUMBER                              
USERNUM  DS    H                   USER ID NUMBER                               
TRNNUM   DS    H                   TRANSACTION COUNT                            
HALF     DS    H                                                                
DATADISP DS    H                                                                
MQMSGLEN DS    H                   L(MQ MESSAGE) (BE IN XA MODE > 4064)         
MSGVERNO DS    XL2                                                              
DMINBTS  DS    X                                                                
SVSTAT   DS    X                                                                
THISISMG DS    C                   FOR SVSTAT, IS THIS A MAKEGOOD ?             
*                                                                               
AGENCY   DS    XL2                 AGENCY POWER CODE                            
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
USERID   DS    CL10                CONTROL USER ID                              
REP      DS    CL10                REP 'FROM ID'                                
POWERCDE DS    CL2                 POWER CODE FOR REP READ                      
BAGYMD   DS    XL1                 BINARY AGENCY/MEDIA                          
BCLT     DS    XL2                 BINARY CLIENT CODE                           
BEST     DS    XL1                 BINARY ESTIMATE                              
BMKTSTA  DS    XL5                 BINARY MARKET STATION                        
BPRD     DS    XL1                                                              
BINORDER DS    0XL4                ORDER NUMBER (FF COMPLEMENT)                 
BINORDDT DS    XL2                   (YEAR-90)*1000 + JULIAN DAY                
BINORDSQ DS    XL2                   SEQUENCE NUMBER (0-9999)                   
POLORDER DS    XL4                 POL ORDER NUMBER (FF COMPLEMENT)             
SVAPRF07 DS    XL1                 SAVED COPY OF APROF+07                       
SVCPRF00 DS    XL1                 SAVED COPY OF CPROF+00                       
SVCOFFC  DS    XL1                 CLIENT OFFICE                                
CONTNUMB DS    CL8                 CONTRACT NUMBER                              
         DS    0F                                                               
PACKOF4B DS    PL4                 PACKED NUMBER OF 4 BYTES                     
JDTTODAY DS    PL4                 JULIAN DATE OF TODAY                         
SVDATTIM DS    CL5                 SAVED DATE AND TIME OF MG CANCEL             
QREPCON  DS    CL8                 COPY OF REP CONTRACT NUMBER                  
QRETURN  DS    CL16                COPY OF RETURN TO SENDER DATA                
*                                                                               
QMED     DS    CL1                 EBCDIC MEDIA                                 
QBUYER   DS    CL3                        BUYER CODE                            
QCLT     DS    CL3                        CLIENT                                
QPRD1    DS    CL3                        PRODUCT 1                             
QPRD2    DS    CL3                        PRODUCT 2                             
QEST1    DS    CL3                        ESTIMATE 1                            
QSTA     DS    CL8                        STATION                               
QMGGROUP DS    CL3                        MAKEGOOD GROUP CODE                   
QMNUMCD  DS    CL2                        MGE MAKEGOOD CODE (INTO BUY)          
QCSHTRDE DS    CL1                        CASH OR TRADE                         
QFLTNUM  DS    CL2                        FLIGHT NUMBER                         
*                                                                               
ESTPW    DS    CL3                 PW PERCENTAGE                                
ESTCOST2 DS    XL4                 COST2 PERCENTAGE                             
ESTLOKYM DS    XL2                 BYTE 0 - YEAR, BYTE 1 MONTH                  
*                                                 X'80' - PREVIOUS              
*                                                 X'40' - SUBSEQUENT            
*                                                                               
THESYSID DS    XL1                 SAVED SYSTEM ID                              
WHCHEDCT DS    XL1                 WHICH EDICT FILE (A)DV OR (R)EP              
*                                                                               
RECSKIP  DS    XL1                 # OF RECORDS TO BUMP INTO BLOCK              
RECNUM   DS    XL1                 # OF RECORDS INTO BLOCK                      
SPTSENUM DS    XL1                 SPOT SYSTEM SENUM                            
BYTE     DS    C                                                                
*                                                                               
PIGPRD   DS    XL1                 PIGGYBACK PRODUCT BINARY CODE                
PIGEST   DS    XL1                 PIGGYBACK ESTIMATE                           
*                                                                               
MISCFLG1 DS    XL1                 VARIOUS BIT FLAGS FOR X'11' ELEM             
MF1XMTUP EQU   X'80'                -  XMT HAS BEEN UPDATED MUST PUTREC         
MF1NOXMT EQU   X'40'                -  NO TRANSMISSION ELEMENT FOUND            
*                                                                               
MISCFLG2 DS    XL1                 VARIOUS BIT FLAGS FOR X'12' ELEM             
MF2DIDNM EQU   X'80'                -  DELIVERY NOTICE ID NUMBER FOUND          
MF2CNFCM EQU   X'40'                -  WE GOT A CONFIRM WITH COMMENT            
*                                                                               
MISCFLG3 DS    XL1                 VARIOUS BIT FLAGS                            
MF3RADIO EQU   X'80'                -  RADIO EDICT TRANSMISSION                 
MF3AMEND EQU   X'40'                -  AMENDED STATUS                           
*SPARE   EQU   X'20'                                                            
*SPARE   EQU   X'10'                                                            
*SPARE   EQU   X'01'                                                            
MF3CANCL EQU   X'02'                -  CANCELLED STATUS                         
MF3SNDCN EQU   X'01'                -  ORDER WAS SEND CANCELLED                 
*                                                                               
BITFLAG1 DS    XL1                 VARIOUS BIT FLAGS                            
BF1SKPTB EQU   X'80'                - SKIP TRACK/BLOCK                          
BF1SKIPB EQU   X'40'                - SKIP RECORD BUMP                          
BF1FRSTR EQU   X'20'                - FIRST RECORD IN BLOCK                     
BF1DPEND EQU   X'10'                - DARE RECORD PENDING                       
BF1NWCOM EQU   X'08'                - NEED TO ADD REP COMMENT RECORD            
BF1IGRCM EQU   X'04'                - IGNORE REST OF REP COMMENTS               
BF1PSSWD EQU   X'02'                - PASSWORD REQUIRED                         
BF1YSDAY EQU   X'01'                - USING PRIOR BUSINESS DAY'S INFO           
*                                                                               
BITFLAG2 DS    XL1                                                              
BF2ELCKD EQU   X'80'               ESTIMATE STATUS IS HOLD OR LOCKED            
BF2VARCN EQU   X'40'               THE VAR ORDER IS CONFIRMED                   
BF2ALLPD EQU   X'20'               ALL BRANDS ARE MARKED CONFIRM PENDNG         
BF2CNFCM EQU   X'10'               WE HAVE CONFIRM WITH COMMENTS                
BF2MGSEQ EQU   X'08'               WE HAVE MG SEQ NUMBER PROBLEM                
BF2PWLCK EQU   X'04'               PW BUY LOCKED                                
BF2SPNDG EQU   X'02'               ORDER IS UNDER SEND PENDING                  
BF2NWURL EQU   X'01'               NEED TO ADD A NEW URL RECORD                 
*                                                                               
BITFLAG3 DS    XL1               FLAGS FOR X'12' ELEMS                          
BF3SPNDG EQU   X'80'             - ORDER IS UNDER SEND PENDING                  
BF3RCLCF EQU   X'40'             - ORDER IS IN QRCLCONF, CHECK QSNTPNDG         
BF3CNFMD EQU   X'20'             - ORDER HAS BEEN CONFIRMED                     
BF3RCLRJ EQU   X'10'             - ORDER IS IN QRCLREJD, CHECK QSNTPNDG         
BF3RJCTD EQU   X'08'             - ORDER HAS BEEN REJECTED                      
BF3DLVRD EQU   X'04'             - ORDER WAS DELIVERED                          
BF3RCLNG EQU   X'02'             - ORDER IS BEING RECALLED                      
*                                                                               
DOBJNUMB DS    XL1                 DARE OBJECT NUMBER                           
DOBJDLNQ EQU   1                   DARE DELIVERY NOTIFICATION                   
DOBJOAPQ EQU   2                   DARE ORDER APPROVAL                          
DOBJORJQ EQU   3                   DARE ORDER REJECTION                         
DOBJOCFQ EQU   4                   DARE ORDER CONFIRMATION                      
DOBJERRQ EQU   5                   DARE ERROR NOTIFICATION                      
DOBJORAQ EQU   6                   DARE ORDER RECALL ACKNOWLEDGEMENT            
DOBJDFXQ EQU   7                   DARE FAX DELIVERY NOTIFICATION               
DOBJCFXQ EQU   8                   DARE FAX CANCELLATION                        
DOBJEFXQ EQU   9                   DARE FAX ERROR                               
*                                                                               
BTODAY   DS    XL3                 TODAY'S DATE IN BINARY (YMD)                 
PRIORDAT DS    XL3                 YESTERDAY'S OR SOME PRIOR DAY'S DATE         
SYSNAME  DS    CL3                 3 CHR FACPAK NAME                            
SYSN1    DS    CL1                 1 CHR FACPAK NAME                            
WRKFILNO DS    XL2                 WORK FILE NUMBER                             
WRKRECNO DS    F                   WORKER FILE RECORD NUMBER                    
AFACIDT  DS    A                   A(FACIDTAB) TABLE                            
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
DDA      DS    CL4                 DISK ADDRESS OF GETREC                       
DALAST   DS    CL1                 LAST DA FILE ADDED TO                        
RACTN    DS    CL1                 RECORD TYPE (COPY CHNG ADD)                  
LASTACTN DS    CL1                 LAST TYPE (COPY CHNG ADD)                    
DLNFRID  DS    XL2                 DELIVERY NOTICE SENDER ID NUM                
ROUTNGCD DS    CL5                 ROUTING CODE                                 
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
BLOCK    DS    CL255                                                            
*                                                                               
ACURELEM DS    F                   ADDRESS OF CURRENT ELEMENT                   
ELEM     DS    CL256                                                            
DOSTELEM DS    XL(DOSTLNQ6)        NEW STATUS ELEMENT                           
DOSAELEM DS    XL(DOSALNQ2)        NEW SALESPERSON REASSIGNMENT ELEMENT         
*                                                                               
SV1OR2   DS    X                                                                
GBWORK   DS    XL44                D/A LINKED FILES WORK AREA                   
         DS    0A                  ALIGNMENT                                    
GETBLK   DS    XL64                FOR SPGETBUYD                                
         ORG   GETBLK                                                           
       ++INCLUDE SPGETBUYD                                                      
         ORG   GETBLK+L'GETBLK                                                  
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
*                                                                               
HUGEBLCK DS    18432C              HUGE OUTPUT BLOCK                            
WORKX    EQU   *                                                                
*                                                                               
REPDKEYD DSECT                     DSECT TO USE OVER WRKRIOA IN AGYRCL          
RKEYDA   DS    XL4                 DISK ADDRESS OF AGENCY ORDER RECORD          
ROLDKEYS DS    XL800               TO REBUILD REP EDI PASSIVE KEYS              
RNEWKEYS DS    XL800                                                            
RKEYIO   DS    XL4000                                                           
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
       ++INCLUDE SRDXMFFD                                                       
         EJECT                                                                  
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
RDARRECD DSECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
RMKGRECD DSECT                                                                  
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE REPFACSQ                                                       
RAG2RECD DSECT                                                                  
       ++INCLUDE REGENAGY2                                                      
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE SPDARMKGDD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMKN                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMKO                                                     
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPSYSFAC                                                       
         EJECT                                                                  
       ++INCLUDE FACIDTABD                                                      
         EJECT                                                                  
ORDD     DSECT                                                                  
ORDSTAT  DS    XL1                 ORDER OR MG STATUS                           
ORDCOL   DS    XL1                 COLOR CODE                                   
ORDTYP   DS    XL1                 SECONDARY TYPE COMPARE                       
ORDTCMTS EQU   X'80'               WITH COMMENTS                                
ORDTNCMT EQU   X'40'               NO COMMENTS                                  
ORDLNQ   EQU   *-ORDD              LENGTH OF EACH ENTRY                         
*                                                                               
LAYOUTD  DSECT                                                                  
LAYMED   DS    CL1                 FIELDS MADE UP IN THE BUY SCREEN             
LAYBYR   DS    CL3                                                              
LAYCLT   DS    CL3                                                              
LAYPRD   DS    CL3                                                              
LAYEST   DS    CL3                                                              
LAYSTA   DS    CL8                                                              
*                                  EXTRA FIELDS NEEDED FOR MGEACC               
LAYFLT   DS    CL2                 - FLIGHT NUMBER                              
LAYGRPCD DS    CL3                 - MAKEGOOD GROUP CODE (FROM REP)             
LAYCORT  DS    CL1                 - CASH OR TRADE                              
LAYPRD1  DS    CL3                 - PRODUCT IF POL IN BUY HEADER               
LAYPRD2  DS    CL3                 - PIGGYBACK PRODUCT                          
LAYEND   EQU   *                                                                
*                                                                               
LAYOUT2D DSECT                                                                  
LAY2MED  DS    CL1                 FIELDS MADE UP IN THE DARE SCREEN            
LAY2BYR  DS    CL3                                                              
LAY2MTHD DS    CL1                 - CASH OR TRADE                              
LAY2CLT  DS    CL3                                                              
LAY2PRDS DS    CL7                                                              
LAY2EST  DS    CL6                                                              
LAY2STA  DS    CL8                                                              
LAY2OORD DS    CL8                                                              
LAY2END  EQU   *                                                                
*                                                                               
BUYINFD  DSECT                                                                  
STRTIM   DS    XL2                                                              
ENDTIM   DS    XL2                                                              
SPTLEN   DS    XL1                                                              
SPTCST   DS    XL3                                                              
SPTDAT   DS    XL2                                                              
LINNO    DS    XL1                                                              
BYINFLN  EQU   *-STRTIM                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SRDXM00   03/12/19'                                      
         END                                                                    
