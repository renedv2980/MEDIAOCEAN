*          DATA SET SPINF00S   AT LEVEL 009 AS OF 05/01/02                      
*PHASE T21A00A                                                                  
*=============================================================*                 
* 10JUN98 MHER IF AGENCY WI, RESTRICT ACCESS TO CLIENT RECORD *                 
*=============================================================*                 
         TITLE 'T21A00 - SPOTPAK INFO BASE'                                     
T21A00   CSECT                                                                  
         PRINT NOGEN                                                            
LINLEN   EQU   88                                                               
         NMODL WORKL,**T21A00,RR=R8                                             
         USING GENOLD,RC                                                        
         USING T21AFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         ST    R8,RELO                                                          
*                                                                               
ST       DS    0H                                                               
         BAS   RE,INITL                                                         
         MVC   RELO00,RELO        SET RELO VALUE IN WORK TOO                    
*                                                                               
         L     RF,0(R1)                                                         
         ST    RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID                                                       
         CH    R0,=H'12'                                                        
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY                          
         MVC   CURADDR,TIOBCURS    SAVE CURSOR ADDRESS                          
         DROP  RF                                                               
*                                                                               
         MVI   DMCB+7,QMSPACK      GET A(MSPACK) AND A(MSUNPK)                  
         BAS   RE,FLCALLOV                                                      
         MVC   VMSPACK,0(R1)                                                    
         MVI   DMCB+7,QMSUNPK                                                   
         BAS   RE,FLCALLOV                                                      
         MVC   VMSUNPK,0(R1)                                                    
         B     FL1                                                              
RELO     DC    A(0)                                                             
*                                                                               
FLCALLOV LR    R0,RE                                                            
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
FL1      FOUT  SINIFLTH            FORCE XMT OF MODIFIED FIELD                  
         EJECT                                                                  
         CLI   SVFIRST,0           IS THIS FIRST TIME IN                        
         BNE   FL1A                                                             
         MVI   SVFIRST,C'Y'                                                     
         XC    SVXFRSY,SVXFRSY                                                  
         XC    SVXFRPR,SVXFRPR                                                  
* SET STEREO FLAG (WILL SUPPRESS PFKEY DATA)                                    
         MVI   SVSTEREO,C'N'                                                    
         L     RF,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RE,0(R1)                                                         
         USING FACTSD,RE                                                        
         MVC   SVOVSYS,FAOVSYS     02=SPOT,03=NET                               
         TM    FATSTAT6,X'80'      TEST STEREO                                  
         BZ    *+8                                                              
         MVI   SVSTEREO,C'Y'                                                    
         DROP  RE                                                               
* TEST IF THIS IS A TRANSFER OF CONTROL FROM ANOTHER PROGRAM                    
FL1A     L     RF,VCOMFACS                                                      
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    FL1X                                                             
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',ELEM,24,GLVXCTL                               
         CLI   8(R1),0                                                          
         BNE   FL1X                                                             
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         TM    GLVXFLG1,GLV1RETN   TEST THIS IS A RETURN CALL                   
         BZ    FL1D                                                             
* ON RETURN NEED TO XMT ALL                                                     
         LA    R2,SINIRECH                                                      
         SR    R0,R0                                                            
FL1B     IC    R0,0(R2)                                                         
         CLI   0(R2),0                                                          
         BE    FL1C                                                             
         AR    R2,R0                                                            
         B     FL1B                                                             
*                                                                               
FL1C     MVC   0(3,R2),=X'000101'  FORCE XMT ALL                                
         B     EXIT                                                             
*                                                                               
FL1D     MVC   SVXFRSY,GLVXFRSY    SAVE CALLING SYS/PRG                         
         MVC   SVXFRPR,GLVXFRPR                                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 (RF),(R1),=C'GETF',SINIRECH,3,GLVXREC                            
         CLI   8(R1),0                                                          
         BNE   FL1X                FORGET ABOUT IT THEN                         
         GOTO1 (RF),(R1),=C'DELE'                                               
* KEY DATA IS IN PFM ELEMENT                                                    
         GOTO1 (RF),(R1),=C'GETF',SINIKEYH,24,GLVPFM                            
         CLI   8(R1),0                                                          
         BNE   FL1X                                                             
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
FL1X     CLI   PFKEY,0             IF PFKEY HIT, LEAVE PREV MESSAGE             
         BNE   *+10                                                             
         XC    SINMSG,SINMSG       CLEAR MESSAGE AREA                           
         FOUT  SINMSGH                                                          
         CLI   PFKEY,12            TEST THIS IS A RETURN REQUEST                
         BNE   FL1X2               NO                                           
         CLI   SVXFRSY,0           DO WE HAVE SOMEPLACE TO GO BACK TO           
         BE    FL1X2               NO - IGNORE                                  
         GOTO1 =A(CALLXFR),RR=RELO  BUILD RETURN CALL                           
         B     EXIT                                                             
*                                                                               
FL1X2    GOTO1 =A(SETPF),RR=RELO   ADD PFKEY DATA AS NEEDED                     
         EJECT                                                                  
NOTXCTL  LA    R2,SINIRECH         TEST RECORD PREV VALIDATED                   
         TM    4(R2),X'20'                                                      
         BO    FL10                                                             
*                                                                               
         MVI   PFKEY,0             THESE PEOPLE ARE IDIOTS                      
         XC    SINRMSG,SINRMSG                                                  
         FOUT  SINRMSGH                                                         
*                                                                               
         XC    SINKMSG,SINKMSG                                                  
         FOUT  SINKMSGH                                                         
*                                                                               
         NI    SINIKEYH+4,X'DF'    FORCE KEY VALIDATION                         
         XC    SVDATA,SVDATA                                                    
         MVC   SVAGYA,AGYALPHA                                                  
         MVI   ERRCD,INVERR                                                     
* VALIDATE RECORD                                                               
         CLI   5(R2),3                                                          
         BL    FLERR                                                            
         CLI   5(R2),8                                                          
         BH    FLERR                                                            
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
*                                                                               
         L     R7,=A(RECLIST)                                                   
         A     R7,RELO                                                          
         LH    R8,0(R7)                                                         
         L     R9,2(R7)                                                         
         A     R9,RELO                                                          
         LA    R7,6(R7)                                                         
*                                                                               
         EX    RE,FLCLC                                                         
         BE    FL2                                                              
         BXLE  R7,R8,*-8                                                        
         B     FLERR                                                            
FLCLC    CLC   8(0,R2),0(R7)                                                    
*                                                                               
FL2      MVC   SVREC,8(R7)                                                      
         MVC   SVOVLY,9(R7)                                                     
         MVC   SVKBLD,12(R7)       SAVE DSPL TO KEYBUILD ROUTINE                
*                                                                               
         MVI   ERRCD,ACCSERR                                                    
         CLI   T21AFFD+1,C'*'      TEST DDS TERM                                
         BE    FL4                                                              
*                                                                               
         CLC   =C'WI',AGYALPHA                                                  
         BNE   FL3                                                              
         CLI   SVOVLY,X'11'        TEST CLIENT RECORD                           
         BNE   FL3                                                              
         TM    T21AFFD+12,X'80'    IS IT DFB ?                                  
         BO    FL3                                                              
*                                                                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,2           WIM ONLY RESTRICTS SPOT ACCESS               
         BE    FLERR                                                            
         DROP  R1                                                               
*                                                                               
FL3      CLI   SVOVLY,36                                                        
         BH    FL4                                                              
         CLI   SVOVLY,X'34'                                                     
         BE    FL4                                                              
         CLI   SVOVLY,X'30'        TEST RESTRICTED REC                          
         BH    FLERR               YES - ERROR                                  
*                                                                               
         EJECT                                                                  
FL4      OI    4(R2),X'20'         SET REC VALID                                
         MVC   SINRMSG(6),=C'KEY IS'                                            
*                                                                               
         L     R7,=A(KEYLIST)       FIND KEY DESCRIPTION                        
         A     R7,RELO                                                          
         SR    R8,R8                                                            
FL6A     CLC   SVREC,0(R7)                                                      
         BE    FL6B                                                             
         IC    R8,1(R7)                                                         
         LA    R7,2(R7,R8)                                                      
         CLI   0(R7),X'FF'                                                      
         BNE   FL6A                                                             
         DC    H'0'                                                             
FL6B     IC    R8,1(R7)                                                         
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   SINRMSG+7(0),2(R7)  *EXECUTED*                                   
         EJECT                                                                  
* SET DUMMY ACTION                                                              
FL10     MVI   SVACT,C'C'                                                       
         MVC   WORK2,SVKEY         SAVE PREVIOUS KEY DATA                       
         EJECT                                                                  
*=============================================================*                 
* KEY VALIDATION                                              *                 
*=============================================================*                 
         SPACE 1                                                                
FL30     LA    R2,SINIKEYH                                                      
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BO    FL40                YES                                          
* KEY NOT VALIDATED                                                             
         LA    R2,SINIKEYH         RESTORE R2                                   
         XC    SINKMSG,SINKMSG                                                  
         FOUT  SINKMSGH                                                         
         MVI   ERRCD,MSSNGERR                                                   
         XC    PREVKEY,PREVKEY                                                  
         XC    SVKEY,SVKEY                                                      
         MVI   SVFMTSW,0           RESET FORMAT SWITCH                          
* LOAD KEY VALIDATION CODE (T21A01)                                             
         GOTO1 VCALLOV,DMCB,(1,0),(RA)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
         TM    ERRCD,X'F0'                                                      
         BO    DSPFLT                                                           
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*===================================================================*           
* KEY PREVIOUSLY VALIDATED                                                      
*===================================================================*           
         SPACE 1                                                                
FL40     CLI   PFKEY,0             KEY VALID - DID THEY HIT A PFKEY             
         BE    FL50                NO                                           
         CLI   PFKEY,2             TEST XFR TO FILE MAINT                       
         BNE   FL42                                                             
         GOTO1 =A(KEYBLD),RR=RELO                                               
         XC    SINMSG,SINMSG                                                    
         MVC   SINMSG(29),HITENTER   SET APPROPIRATE MESSAGE                    
         OC    PREVKEY,PREVKEY                                                  
         BNZ   *+10                                                             
         MVC   SINMSG(29),NOMORDTA                                              
         B     EXIT                                                             
*                                                                               
FL42     CLI   PFKEY,12            TEST RETURN                                  
         BNE   FL50                                                             
* NEED TO DO RETURN CALL                                                        
         B     FL50                                                             
         EJECT                                                                  
FL50     BAS   R9,CLRPROT                                                       
         LA    R0,8                CLEAR RECORD AREA                            
         LA    R1,REC                                                           
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         SR    RE,RE               HASH TOTAL FOR FILTERS                       
         SR    R0,R0                                                            
         LA    R1,SINIFLT                                                       
         LA    RF,64                                                            
FL51     IC    R0,0(R1)                                                         
         AR    RE,R0                                                            
         LA    R1,1(R1)                                                         
         BCT   RF,FL51                                                          
         C     RE,FLTHASH          FILTERS EQUAL                                
         BE    *+10                                                             
         XC    PREVKEY,PREVKEY      NO - RESET                                  
         ST    RE,FLTHASH                                                       
         CLC   SVKEY(26),WORK2     SAME KEY                                     
         BE    *+10                                                             
         XC    PREVKEY,PREVKEY     NO - CLEAR PREVIOUS KEY                      
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB(1),SVOVLY                                                   
         GOTO1 VCALLOV,DMCB,,(RA)                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB             GET OVERLAY ADDRESS                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
         CLI   ERRAREA,0                                                        
         BE    FL58                                                             
         NI    SINIRECH+4,X'DF'    TURN REVALIDATION                            
         B     EXXMOD                                                           
         EJECT                                                                  
FL58     MVC   SINMSG(29),HITENTER                                              
         OC    PREVKEY,PREVKEY                                                  
         BNZ   EXIT                                                             
         NI    SINIKEYH+4,X'DF'    TURN OFF FIELD VALIDATED                     
         MVC   SINMSG(29),NOMORDTA                                              
         B     EXIT                                                             
         EJECT                                                                  
DSPFLT   BAS   R9,CLRPROT                                                       
         NI    SINIRECH+4,X'DF'    FORCE RE-EDIT OF RECORD                      
         L     RE,=A(FLTDESC)      DISPLAY VALID FILTERS                        
         A     RE,RELO             RELOCATE                                     
         SPACE 1                                                                
* FIND FILTERS FOR THIS PROGRAM                                                 
DSPFLT1  CLC   SVOVLY,0(RE)                                                     
         BE    DSPFLT2                                                          
         CLC   0(2,RE),=X'FFFF'                                                 
         BE    EXXMOD                                                           
         MVC   HALF,1(RE)                                                       
         AH    RE,HALF                                                          
         B     DSPFLT1                                                          
         SPACE 1                                                                
* FOUND FILTERS NOW DISPLAY THEM                                                
DSPFLT2  ST    R2,FULL                                                          
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA(19),=C'VALID FILTER FIELDS'                              
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA(19),=19C'-'                                              
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         SPACE 2                                                                
* SEND FILTER DESCRIPTIONS                                                      
         LA    RE,3(RE)                                                         
DSPFLT3  CLI   0(RE),X'FF'                                                      
         BE    DSPEX                                                            
         SR    RF,RF                                                            
         IC    RF,0(RE)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),2(RE)                                                 
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         IC    RF,0(RE)                                                         
         AH    RF,=H'2'                                                         
         AR    RE,RF                                                            
         B     DSPFLT3                                                          
         SPACE 2                                                                
DSPEX    L     R2,FULL                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
FLERR    GOTO1 ERROR                                                            
         B     EXXMOD                                                           
*                                                                               
EXIT     DS    0H                                                               
*                                                                               
EXXMOD   DS    0H                                                               
         BAS   RE,GFACT                                                         
         XMOD1 1                                                                
*                                                                               
HITENTER DC    CL29'** HIT ENTER FOR MORE DATA **'                              
NOMORDTA DC    CL29'** END OF REQUESTED DATA **'                                
         EJECT                                                                  
FNDUF    TM    1(R2),X'20'         TEST PROTECTED                               
         BCR   8,RE                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         DC    H'0'                                                             
         EJECT                                                                  
CLRPROT  LA    R2,SINHDRH                                                       
*                                                                               
CLRPR2   TM    1(R2),X'20'         TEST PROT                                    
         BZ    CLRPR8                                                           
         CLI   FLDLEN,LINLEN                                                    
         BNE   CLRPR8                                                           
*                                                                               
CLRPR4   SR    RE,RE                                                            
         IC    RE,FLDLEN                                                        
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    FLDDATA,FLDDATA    * EXECUTED *                                  
         BZ    CLRPR6                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA,FLDDATA    * EXECUTED *                                  
         FOUT  (R2)                                                             
*                                                                               
CLRPR6   LA    R2,9(RE,R2)                                                      
         B     CLRPR10                                                          
*                                                                               
CLRPR8   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
CLRPR10  CLI   0(R2),0                                                          
         BNE   CLRPR2                                                           
*                                                                               
CLRPROTX BR    R9                                                               
         EJECT                                                                  
INITL    DS    0H                                                               
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
* CLEAR WORK AREA                                                               
         LA    R4,8(RC)                                                         
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LA    R0,256                                                           
INIT2    CR    R5,R0                                                            
         BNH   INIT4                                                            
         XC    0(256,R4),0(R4)                                                  
         AR    R4,R0                                                            
         SR    R5,R0                                                            
         B     INIT2                                                            
INIT4    BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)  * EXECUTED *                                      
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         LM    R2,R4,0(R1)                                                      
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD          A(FIRST INPUT FIELD HDR)                     
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD          A(LAST INPUT FIELD HDR)                      
         MVC   NUMFLD,4(R2)        NUMBER OF INPUT FIELDS                       
*                                                                               
         ST    R3,VTWA                                                          
INIT8    MVC   VDATAMGR(80),0(R4)  FACILITY LIST                                
         MVC   VCOMFACS,16(R1)     ASSUMES R1 UNCHANGED                         
         LR    RA,R3                                                            
         MVC   AGYALPHA,14(RA)                                                  
         LA    R3,64(R3)           PRESET ERROR MSG ADDRESS                     
         ST    R3,ERRAREA                                                       
         MVI   DMINBTS,X'C0'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
* SET UP COMMON FACILITY LINKAGES                                               
         LA    R6,SPCOMMON                                                      
         SR    R7,R7                                                            
         LA    R8,ERROR                                                         
         LA    R0,SPCOMCNT                                                      
*                                                                               
INIT10   ST    R6,0(R8)                                                         
         STC   R7,0(R8)                                                         
         LA    R7,4(R7)                                                         
         LA    R8,4(R8)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
SPCOMMON NTR1  BASE=BASERB                                                      
*                                                                               
         SRL   RF,24                                                            
         B     SPCOMTAB(RF)                                                     
*                                                                               
SPCOMTAB B     SPERROR             X'00'                                        
         B     SPANY               X'04'                                        
         B     SPMOVE              X'08'                                        
         B     SPPACK              X'0C'                                        
         B     SPREAD              X'10'                                        
         B     SPSEQ               X'14'                                        
         B     SPHIGH              X'18'                                        
         B     SPADD               X'1C'                                        
         B     SPDIR               X'20'                                        
         B     SPRDSTA             X'24'                                        
         B     SPSTA               X'28'                                        
         B     SPGETREC            X'2C'                                        
         B     SPPUTREC            X'30'                                        
         B     SPADDREC            X'34'                                        
         B     SPFIL               X'3C'                                        
         DC    5AL4(0)             X'40/44/48/4C/50' RESERVED                   
SPCOMUSR DC    9AL4(0)   ** USER ROUTINES ORIGIN HERE WITH X'54' **             
SPCOMCNT EQU   (*-SPCOMTAB)/4      NUMBER OF ENTRIES                            
         SPACE 2                                                                
SPCOMXIT XIT1                                                                   
         EJECT                                                                  
SPERROR  L     R4,ERRAREA                                                       
         CLI   ERRCD,NEWERRS       IS THIS A 2 CHAR ERROR                       
         BNE   SPERR10                                                          
*                                                                               
         XC    WORK,WORK           DEFINE CONTROL BLOCK                         
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         MVC   GTMTYP,GTMERR       SET MESSAGE TYPE TO ERRO                     
         MVC   GTMSGNO,NERRCD      AND MESSAGE NUMBER                           
         MVI   GTMSYS,2            AND MESSAGE SYSTEM                           
         L     RF,VCOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),WORK           PUT OUT SYSTEM MESSAGE                       
         B     SPERR12                                                          
*                                                                               
SPERR10  MVC   DMCB+20(4),VDATAMGR                                              
         MVI   DMCB+20,2                                                        
         GOTO1 VGETMSG,DMCB+12,(ERRCD,8(R4)),(X'FF',DMCB)                       
*                                                                               
SPERR12  FOUT  (R4)                                                             
         MVI   ERRAREA,X'FF'                                                    
*                                                                               
         OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
         BAS   RE,GFACT            SETS TIOB FOR STEREO                         
*                                                                               
         L     RD,BASERD           RETURN TO * BASE *                           
         B     SPCOMXIT                                                         
         EJECT                                                                  
SPANY    CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         MVI   ERRCD,1                                                          
         B     SPERROR                                                          
*                                                                               
ANY2     TM    4(R2),X'10'                                                      
         BZ    SPCOMXIT                                                         
         MVI   ERRCD,3                                                          
         B     SPERROR                                                          
         EJECT                                                                  
SPPACK   SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1               EXIT ON ZERO LENGTH                          
         BZ    PACKX                                                            
         TM    4(R2),X'08'         OR NON-NUMERIC                               
         BZ    PACKX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+12                                                          
         CVB   R0,DUB                                                           
         B     PACKX                                                            
         PACK  DUB,8(0,R2)   * EXECUTED *                                       
*                                                                               
PACKX    XIT1  REGS=(R0,R1)                                                     
         SPACE 2                                                                
SPMOVE   MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    SPCOMXIT                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     PACKX                                                            
*                                                                               
         MVC   WORK(0),8(R2) * EXECUTED *                                       
         EJECT                                                                  
SPREAD   MVC   COMMAND,=C'DMREAD'                                               
         B     SPDIR                                                            
SPSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         B     SPDIR                                                            
SPHIGH   MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     SPDIR                                                            
SPADD    MVC   COMMAND,=C'DMADD'                                                
         B     SPDIR                                                            
SPWRITE  MVC   COMMAND,=C'DMWRT'                                                
*                                                                               
SPDIR    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTDIR',KEY,KEY               
*                                                                               
SPDIRX   TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         BZ    SPDIRX2             NO ERROR                                     
         CLI   COMMAND+2,C'R'      TEST READ COMMAND                            
         BE    *+6                                                              
         DC    H'0'                FORCE RECOVERY ON ADD/WRITE ERROR            
*                                                                               
SPDIRX2  MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,8(R1)                                                       
         BZ    SPCOMXIT                                                         
* DATAMGR ERROR HAS OCCURRED                                                    
         MVI   ERRCD,0                                                          
         B     SPERROR                                                          
         EJECT                                                                  
SPRDSTA  MVC   COMMAND,=C'DMREAD'                                               
*                                                                               
SPSTA    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'STATION',KEY,AREC             
*                                                                               
         B     SPDIRX                                                           
         EJECT                                                                  
SPGETREC MVC   COMMAND,=C'GETREC'                                               
         B     SPFIL                                                            
SPPUTREC MVC   COMMAND,=C'PUTREC'                                               
         B     SPFIL                                                            
SPADDREC MVC   COMMAND,=C'ADDREC'                                               
*                                                                               
SPFIL    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTFILE',KEY+14,AREC,X        
               DMWORK                                                           
*                                                                               
         CLI   COMMAND,C'G'        TEST GETREC                                  
         BE    SPDIRX2                                                          
         TM    8(R1),X'D0'         TEST EOF OR ERROR                            
         BZ    SPDIRX2                                                          
         DC    H'0'                FORCE RECOVERY ON ADD/WRITE ERRORS           
*===================== END ++INCLUDE SPGENEROL ======================*          
         EJECT                                                                  
COMRUTIN DS    0H                                                               
         ORG   SPCOMUSR                                                         
         B     GETFLTR             USER 1                                       
         B     FRMTALPH            USER 2                                       
         ORG   COMRUTIN                                                         
         EJECT                                                                  
*                                                                               
* GET A FIELD FROM THE FILTER FIELD                                             
*                                                                               
*        PARAMETER 1     BYTE   0 = L'DATA                                      
*                        BYTE 1-3 = A(DATA)                                     
*                                                                               
*        PARAMETER 2     BYTE   0 = L'SEARCH FIELD                              
*                        BYTE 1-3 = A(SEARCH FIELD)                             
*                                                                               
*                                    OUTPUT - A(FIELD) OR A(0)                  
*                                                                               
GETFLTR  LM    R6,R7,0(R1)         SET PARAMS                                   
         LA    R6,0(R6)            CLEAR HIGH ORDER BITS                        
         LA    R7,0(R7)                                                         
         SR    R8,R8                                                            
         IC    R8,0(R1)            SET FIELD END                                
         AR    R8,R6                                                            
         SR    R9,R9                                                            
         IC    R9,4(R1)                                                         
         BCTR  R9,0                                                             
* REGISTER ASSIGNMENTS - R6=A(DATA)                                             
*                        R7=A(SEARCH DATA)                                      
*                        R8=A(END OF DATA)                                      
*                        R9=L'SEARCH DATA                                       
*                                                                               
GETFLD1  CLI   0(R6),0             FIND FIRST FIELD                             
         BE    GFNODATA                                                         
         CLI   0(R6),C' '                                                       
         BNE   GETFLD2             FOUND FIRST FIELD                            
         LA    R6,1(R6)            NEXT COLUMN                                  
         CR    R6,R8                                                            
         BH    GFNODATA                                                         
         B     GETFLD1                                                          
GETFLD2  EX    R9,*+8              SEE IF CORRECT FIELD                         
         B     *+10                                                             
         CLC   0(0,R6),0(R7)      *EXECUTED                                     
         BE    GFDATA                                                           
GETFLD3  LA    R6,1(R6)                                                         
         CLI   0(R6),C','                                                       
         BNE   *+12                                                             
         LA    R6,1(R6)                                                         
         B     GETFLD2                                                          
         CLI   0(R6),0                                                          
         BE    GFNODATA                                                         
         CLI   0(R6),C' '                                                       
         BE    GFNODATA                                                         
         CR    R6,R8                                                            
         BL    GETFLD3                                                          
GFNODATA XC    4(4,R1),4(R1)       DATA NOT FOUND                               
         B     SPCOMXIT                                                         
GFDATA   ST    R6,4(R1)            DATA FOUND                                   
         B     SPCOMXIT                                                         
         EJECT                                                                  
*                                                                               
* FORMAT TABLE IN ALPHABETIC COLUMNS                                            
*                                                                               
*        PARAMETER 1     BYTE   0 = L'EACH ENTRY                                
*                        BYTE 1-3 = A(TABLE)                                    
*        PARAMETER 2     BYTE 0-3 = NUMBER OF ENTRIES IN TABLE                  
*        PARAMETER 3     BYTE 0-3 = NUMBER OF ROWS TO BE FORMATTED              
*                                                                               
*        PARAMETER 4     BYTE   0 = NUMBER OF COLUMNS TO BE FORMATTED           
*                        BYTE 1-3 = A(FORMAT LIST)                              
*                                                                               
*        OUTPUT FORMAT LIST (ONE ENTRY FOR EACH COLUMN)                         
*                BYTE  0 = NUMBER OF ENTRIES FOR THIS COLUMN                    
*                BYTE 1-3= A(START FIELD FOR THIS COLUMN)                       
*                                                                               
*               A(TABLE) IS SET TO NEXT ENTRY POINT                             
*               NUMBER OF ENTRIES IS SET TO NUMBER OF ENTRIES LEFT IN           
*                TABLE.                                                         
*                                                                               
FRMTALPH L     R6,12(R1)           CLEAR FORMAT LIST                            
         LA    R6,0(R6)                                                         
         SR    R7,R7                                                            
         IC    R7,12(R1)                                                        
         XC    0(4,R6),0(R6)                                                    
         LA    R6,4(R6)                                                         
         BCT   R7,*-10                                                          
*                                                                               
         SR    RF,RF               GET END OF TABLE                             
         IC    RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         MH    RF,6(R1)            NUMBER OF ENTRIES X ENTRY LENGTH             
         L     R6,0(R1)                                                         
         LA    RF,0(RF,R6)         END OF TABLE                                 
*                                                                               
         LA    R6,0(R6)            A(TABLE START)                               
         L     R7,12(R1)           A(FORMAT LIST)                               
         SR    R8,R8                                                            
         IC    R8,12(R1)           NUMBER OF COLUMNS                            
FA1      ST    R6,0(R7)            SAVE COLUMN ADDRESS                          
         SR    R4,R4                                                            
         CLC   4(4,R1),8(R1)       NUMBER OF ENTRIES GT NO OF ROWS              
         BH    FA2                                                              
         MVC   0(1,R7),7(R1)        NO - SET ROWS FOR THIS COLUMN               
         XC    4(4,R1),4(R1)       SET ENTRIES LEFT TO ZERO                     
         LR    R6,RF                                                            
         B     FAEX                                                             
FA2      MVC   0(1,R7),11(R1)      SET TO NUMBER OF ROWS                        
         SR    R9,R9                                                            
         IC    R9,0(R1)                                                         
         MH    R9,10(R1)           X ENTRY LENGTH                               
         LA    R6,0(R9,R6)         SET TO NEXT COLUMN ADDRESS                   
         LA    R7,4(R7)                                                         
         L     R9,4(R1)            DECREMENT NUMBER OF ENTRIES                  
         S     R9,8(R1)                                                         
         ST    R9,4(R1)            SAVE ENTRIES LEFT                            
         BCT   R8,FA1              GET NEXT COLUMN ADDRESS                      
*                                                                               
FAEX     IC    RF,0(R1)            SAVE A(NEXT START)                           
         ST    R6,0(R1)                                                         
         STC   RF,0(R1)                                                         
         B     SPCOMXIT                                                         
         EJECT                                                                  
* SUBROUTINE CALLS GETFACT TO SEE IF USER SIGNED ON VIA STEREO                  
* IF SO - SET SUB SCREEN NUMBER IN FATIOB                                       
         SPACE 1                                                                
GFACT    NTR1                                                                   
         XC    DMCB,DMCB                                                        
         L     RF,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB                                                        
         L     R2,0(R1)                                                         
         USING FACTSD,R2                                                        
         TM    FATSTAT6,TST6FLSH                                                
         BNO   TFX                                                              
         DROP  R2                                                               
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSCRN   SET TIOBCNT TO SCREEN NUMBER                 
         MVI   TIOBAID,0                                                        
         XC    TIOBCNT,TIOBCNT                                                  
         ZIC   R2,SVOVLY                                                        
         STC   R2,TIOBCNT                                                       
         OI    TIOBINDS,TIOBSUBS   SET TIOBAID TO SUB SCREEN NUMBER             
         ZIC   R2,SVSUBSC                                                       
         STC   R2,TIOBAID                                                       
*                                                                               
TFX      XIT1                                                                   
         DROP  RF                                                               
         EJECT                                                                  
*=============================================================*                 
* SET PFKEY DATA AT END OF SCREEN IF ROOM AVAILABLE           *                 
*=============================================================*                 
         SPACE 1                                                                
SETPF    NTR1  BASE=*,LABEL=*                                                   
         CLI   SVSTEREO,C'Y'                                                    
         BE    SETPFX                                                           
         OC    SVKBLD,SVKBLD       TEST NO PFKEY THIS SCREEN                    
         BZ    SETPFX                                                           
*                                                                               
         LA    R2,SINIKEYH                                                      
SETPF2   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SETPF4                                                           
         CLC   2(2,R2),=AL2(23*80)   PAST ROW 24/COL 1                          
         BNH   SETPF2                NO                                         
* IF IN LINE 24, THINK ABOUT WHERE THE FIELD ENDS !                             
         SR    RE,RE                                                            
         IC    RE,0(R2)            LENGTH OF FLDHDR                             
         SH    RE,=H'8'                                                         
         TM    1(R2),X'02'         TEST EXTENDED FLDHDR                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         SR    RF,RF                                                            
         ICM   RF,3,2(R2)          GET FIELD START                              
         AR    RE,RF               GIVES END + 1                                
         CLM   RE,3,STRFLDH+2      COMPARE TO MY START POSN                     
         BL    SETPF2                                                           
         B     SETPFX                                                           
*                                                                               
SETPF4   MVC   0(STRFLDX-STRFLDH+3,R2),STRFLDH  (INCLUDE X'000101')             
         OC    SVXFRSY,SVXFRSY     TEST XFRCTL FROM ANOTHER PROGRAM             
         BZ    SETPFX                                                           
         MVC   0(STRFLDX2-STRFLDH2+3,R2),STRFLDH2                               
*                                                                               
SETPFX   XIT1                                                                   
*                                                                               
STRFLDH  DC    AL1(STRFLDX-STRFLDH)                                             
         DC    X'20'               PROTECTED                                    
         DC    AL2(23*80+60)       ROW 24/COL 60                                
         DC    X'00'               INDS                                         
         DC    X'00'               INPUT LENGTH                                 
         DC    X'80'               TRANSMIT                                     
         DC    AL1(STRFLDX-STRFLDH-8)  DATA LENGTH                              
         DC    C'PF2=',X'C6899385' (FILE)                                       
STRFLDX  EQU   *                                                                
         DC    X'000101'                                                        
*                                                                               
STRFLDH2 DC    AL1(STRFLDX2-STRFLDH2)                                           
         DC    X'20'               PROTECTED                                    
         DC    AL2(23*80+60)       ROW 24/COL 40                                
         DC    X'00'               INDS                                         
         DC    X'00'               INPUT LENGTH                                 
         DC    X'80'               TRANSMIT                                     
         DC    AL1(STRFLDX2-STRFLDH2-8)  DATA LENGTH                            
         DC    C'PF2=',X'C6899385' (SPOT FILE)                                  
         DC    C' PF12=',X'D985A3A49995'   (RETURN))                            
STRFLDX2 EQU   *                                                                
         DC    X'000101'                                                        
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* BUILD A TRANSFER CONTROL RETURN CALL                           *              
*================================================================*              
         SPACE 1                                                                
CALLXFR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'SPO'                                                 
         CLI   SVOVSYS,2           2=SPOT,3=NET                                 
         BNE   *+10                                                             
         MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXFRPR,=C'FIL'                                                 
         MVC   GLVXTOSY,SVXFRSY                                                 
         MVC   GLVXTOPR,SVXFRPR                                                 
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
*                                                                               
         L     RF,VCOMFACS                                                      
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,24,GLVXCTL                               
*                                                                               
         XC    SINMSG,SINMSG                                                    
         MVC   SINMSG(23),=C'** BACK TO SPOT INFO **'                           
         OI    SINMSG+6,X'80'      TRANSMIT                                     
         LA    R2,SINIRECH                                                      
         OI    6(R2),X'40'         POSITION CURSOR                              
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* BUILD KEY FOR TRANSFER TO FILE MAINT                           *              
*================================================================*              
         SPACE 1                                                                
KEYBLD   NTR1  BASE=*,LABEL=*                                                   
         TM    SINIKEYH+4,X'20'    TEST KEY DATA VALID                          
         BO    KB2                                                              
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOKEYDTA)                                            
         GOTO1 ERROR                                                            
*                                                                               
KB2      ICM   RE,15,SVKBLD        GET DSPL TO KEYBLD ROUTINE                   
         BNZ   KB4                                                              
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOXFR)                                               
         GOTO1 ERROR                                                            
*                                                                               
KB4      XC    WORK,WORK           CLEAR KEY BUILD AREA                         
         A     RE,RELO00           RELOCATE                                     
         BR    RE                                                               
*                                                                               
KCLT     MVC   WORK(1),SVEBCMED    MEDIA                                        
         MVI   WORK+1,C','                                                      
         CLI   SVREC,X'11'         TEST CLTHDR                                  
         BNE   KPRD                                                             
         MVC   COLS(4),=AL1(01,27,53,255)                                       
         BAS   RE,GETROW           POINT R1 TO DATA IN TWA                      
         MVC   WORK+2(3),0(R1)     MOVE SELECTED COLUMN                         
         OC    WORK+2(3),SPACES                                                 
         B     KEYBLDX                                                          
*                                                                               
KPRD     MVC   WORK+2(3),SVEBCCLT                                               
         LA    R4,WORK+4          ADJUST FOR 2 CHAR CLIENT                      
         BAS   RE,BACKUP                                                        
         CLI   SVREC,X'12'        TEST PRD REC                                  
         BNE   KEST                                                             
* DIG OUT THE PRODUCT CODE                                                      
         MVC   COLS(4),=AL1(01,27,53,255)                                       
         BAS   RE,GETROW                                                        
         MVC   0(3,R4),0(R1)       MOVE PRODUCT                                 
         B     KEYBLDX                                                          
*                                                                               
* NEED TO MOVE THE LAST DISPLAYED PRODUCT ABOVE CURSOR                          
*                                                                               
KEST     CLC   =C'ALL',SVEBCPRD                                                 
         BNE   KEST10                                                           
         CLI   SVDATOPT,C'N'       TEST DATA=NAME TYPE                          
         BNE   KEST2                                                            
         MVC   COLS(2),=AL1(1,255)                                              
         BAS   RE,GETROW                                                        
         MVC   0(3,R4),0(R1)                                                    
         LA    R1,33(R1)           POINT TO EST IN COL 35                       
         B     KEST20                                                           
*                                                                               
KEST2    CLI   SVDATOPT,C'D'       TEST DEMO TYPE DISPLAY                       
         BNE   KEST4                                                            
         MVC   COLS(2),=AL1(1,255)                                              
         BAS   RE,GETROW                                                        
         MVC   0(3,R4),0(R1)                                                    
         LA    R1,25(R1)           POINT TO EST IN COL 27                       
         B     KEST20                                                           
*                                                                               
KEST4    CLI   SVDATOPT,C'C'       TEST CTRL TYPE DISPLAY                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   COLS(3),=AL1(1,40,255)                                           
         BAS   RE,GETROW                                                        
         MVC   0(3,R4),0(R1)       USE PRD FROM SCREEN                          
         LA    R1,31(R1)           POINT TO EST                                 
         B     KEST20                                                           
*                                                                               
KEST10   MVC   0(3,R4),SVEBCPRD    SINGLE PRODUCT                               
         MVC   COLS(2),=AL1(26,255)                                             
         CLI   SVDATOPT,C'N'       TEST DATA=NAME                               
         BNE   *+10                                                             
         MVC   COLS(2),=AL1(34,255)  SET COL 35                                 
         BAS   RE,GETROW                                                        
*                                                                               
KEST20   LA    R4,3(R4)                                                         
         BAS   RE,BACKUP                                                        
*                                                                               
         PACK  DUB,0(3,R1)           NEED 3 DIGITS                              
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         B     KEYBLDX                                                          
*                                                                               
KPGR     MVC   WORK(1),SVEBCMED                                                 
         MVI   WORK+1,C','                                                      
         MVC   WORK+2(3),SVEBCCLT                                               
         LA    R4,WORK+4          ADJUST FOR 2 CHAR CLIENT                      
         BAS   RE,BACKUP                                                        
*                                                                               
         MVC   COLS(2),=AL1(0,255)  GROUP CODE IS IN COL 1                      
         BAS   RE,GETROW                                                        
         MVC   0(1,R4),0(R1)       MOVE GROUP CODE                              
         MVC   1(3,R4),2(R1)       MOVE GROUP NUMBER                            
         B     KEYBLDX                                                          
*                                                                               
KPGRD    MVC   WORK(1),SVEBCMED                                                 
         MVI   WORK+1,C','                                                      
*                                                                               
         MVC   COLS(2),=AL1(29,255)  GROUP CODE IS IN COL 30                    
         BAS   RE,GETROW                                                        
* DIG OUT THE CLIENT CODE WHICH MAY BE ON A LINE ABOVE                          
KPGRD2   CLI   9(R2),C' '          IS ANYTHING THERE                            
         BH    KPGRD4                                                           
         SH    R2,=AL2(LINLEN)                                                  
         B     KPGRD2                                                           
*                                                                               
KPGRD4   MVC   WORK+2(3),9(R2)                                                  
         LA    R4,WORK+4                                                        
         BAS   RE,BACKUP                                                        
*                                                                               
         MVC   0(1,R4),0(R1)       MOVE GROUP CODE AFTER CLIENT                 
         B     KEYBLDX                                                          
*                                                                               
KMGR     MVC   WORK(1),SVEBCMED                                                 
         MVI   WORK+1,C','                                                      
         MVC   WORK+2(3),=C'ALL'                                                
         CLI   SVEBCCLT,C' '                                                    
         BNH   *+10                                                             
         MVC   WORK+2(3),SVEBCCLT                                               
         LA    R4,WORK+4                                                        
         BAS   RE,BACKUP                                                        
         MVC   COLS(2),=AL1(0,255)  PRDGRP IN COL 1                             
         BAS   RE,GETROW                                                        
         MVC   0(3,R4),=C'ALL'     FORCE PGR=ALL                                
         CLI   0(R1),C' '          TEST NO PRDGRP                               
         BNH   KMGR2                                                            
         MVC   0(1,R4),0(R1)       PGR CODE                                     
         MVC   1(3,R4),2(R1)       PGR NUMBER                                   
KMGR2    LA    R4,4(R4)                                                         
         BAS   RE,BACKUP                                                        
         MVC   0(5,R4),6(R1)       MGRP CODE/NUMBER                             
         B     KEYBLDX                                                          
*                                                                               
KAGY     MVC   COLS(2),=AL1(65,255) AGYCODE IN COL 66                           
         BAS   RE,GETROW                                                        
         MVC   WORK(2),0(R1)                                                    
         B     KEYBLDX                                                          
*                                                                               
KDPT     MVC   WORK(2),SVAGYA      NOTE DISPLAY AS COLUMNS                      
         MVI   WORK+2,C','                                                      
         MVC   WORK+3(1),SVEBCMED                                               
         MVI   WORK+4,C','                                                      
         MVC   COLS(8),=AL1(1,10,19,28,37,46,55,255)                            
         BAS   RE,GETROW                                                        
         SR    R1,R2               GET DSPL FROM FLDHDR                         
KDPT2    CLC   =C'MENU',9(R2)      IS THIS THE 'MENU' ROW                       
         BE    KDPT4                                                            
         SH    R2,=AL2(LINLEN)                                                  
         B     KDPT2                                                            
KDPT4    AR    R1,R2               POINT TO 'MENU X'                            
         MVC   WORK+5(1),5(R1)                                                  
         B     KEYBLDX                                                          
*                                                                               
KMENU    MVC   WORK(1),SVEBCMED                                                 
         MVI   WORK+1,C','                                                      
         MVC   COLS(2),=AL1(1,255)                                              
         BAS   RE,GETROW                                                        
*                                                                               
KMENU2   MVC   WORK+2(4),0(R1)                                                  
         CLI   0(R1),C' '          TEST IT IS THERE                             
         BH    KEYBLDX             YES - DONE                                   
         SH    R1,=AL2(LINLEN)                                                  
         B     KMENU2                                                           
*                                                                               
KEYBLDX  L     RF,VCOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',WORK,24,GLVPFM                                
* PUT OUT RECORD NAME                                                           
         GOTO1 (RF),(R1),=C'PUTF',SINIRECH,,GLVXREC                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         CLI   SVOVSYS,3           2=SPOT,3=NET                                 
         BE    KEYBLDX2                                                         
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         B     KEYBLDX4                                                         
*                                                                               
KEYBLDX2 MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXTOSY,=C'NET'                                                 
*                                                                               
KEYBLDX4 MVC   GLVXFRPR,=C'INF'                                                 
         MVC   GLVXTOPR,=C'FIL'                                                 
         OI    GLVXFLG1,GLV1GOTO                                                
         OI    GLVXFLG1,GLV1SEPS                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK,14,GLVXCTL                               
* FIX THE MESSAGE !                                                             
         OI    SINIRECH+6,X'40'    CURSOR TO REC FIELD                          
         XIT1                                                                   
*                                                                               
BACKUP   CLI   0(R4),C' '          FIND LAST CHAR AND INSERT A ','              
         BH    *+8                                                              
         BCT   R4,BACKUP                                                        
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         BR    RE                                                               
*                                                                               
GETROW   DS    0H                                                               
         LA    R2,SINHDRH          POINT TO FIRST DISPLAY LINE                  
         SR    R0,R0                                                            
*                                                                               
GETROW1  IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   10(R2),C'-'         FIND THE LINE WITH '---'                     
         BNE   GETROW1                                                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    R0,10                                                            
         LA    R1,10(R2)                                                        
GETROW1A CLI   0(R1),C' '                                                       
         BH    GETROW1X                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,GETROW1A                                                      
* POINT TO NEXT ROW                                                             
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
GETROW1X CLC   CURADDR,2(R2)       CURSOR ADDR TO FIELD START                   
         BNL   GETROW4                                                          
GETROW2  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(PUTCURS)                                             
         GOTO1 ERROR                                                            
*                                                                               
GETROW4  LR    RF,R2               SAVE PREVIOUS FIELD ADDRESS                  
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    GETROW2             THIS SHOULDN'T HAPPEN                        
         CLC   CURADDR,2(R2)                                                    
         BH    GETROW4                                                          
         BE    *+6                 EQUAL - USE CURRENT ROW                      
         LR    R2,RF               LOW - USE PREVIOUS ROW                       
* CALCULATE ROW/COLUMN                                                          
         LH    R0,CURADDR                                                       
         SRDA  R0,32                                                            
         D     R0,=F'80'           R0 HAS COL-1 ON/R1 HAS ROW NUMBER            
* POINT R1 TO COLUMN DATA                                                       
         LA    RF,COLS             POINT TO FIRST COL DSPL                      
*                                                                               
GETROW6  SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    R1,8(R2,R1)         POINT TO CODE                                
         CLM   R0,1,1(RF)          ACTUAL DSPL TO NEXT COL DSPL                 
         BLR   RE                  IF LOW, USE THIS DATA                        
         LA    RF,1(RF)            ELSE POINT TO NEXT COL DSPL                  
         CLI   0(RF),X'FF'                                                      
         BNE   GETROW6                                                          
         BR    RE                  ON RETURN R1 POINTS TO CODE                  
         LTORG                                                                  
         EJECT                                                                  
* RECLIST ENTRIES ARE  00-07 RECORD NAME                                        
*                      08    RECORD NUMBER                                      
*                      09    OVERLAY NUMBER                                     
*                      10    SPARE                                              
*                      11    SPARE                                              
*                      12    A(KEY BUILD ROUTINE FOR FILE)                      
         CNOP  2,4                                                              
RECLIST  DC    H'16'                                                            
         DC    A(RECLISTX-1)                                                    
*                                                                               
         DC    CL8'CLIENT  ',X'11110080',AL4(KCLT)                              
         DC    CL8'CLTHDR  ',X'11110080',AL4(KCLT)                              
         DC    CL8'PRODUCT ',X'12120000',AL4(KCLT)                              
         DC    CL8'PRDHDR  ',X'12120000',AL4(KCLT)                              
         DC    CL8'ESTIMATE',X'13130000',AL4(KCLT)                              
         DC    CL8'ESTHDR  ',X'13130000',AL4(KCLT)                              
*                                                                               
         DC    CL8'PGROUP  ',X'14140000',AL4(KPGR)                              
         DC    CL8'PGRDEF  ',X'15150000',AL4(KPGRD)                             
         DC    CL8'MGROUP  ',X'16160000',AL4(KMGR)                              
         DC    CL8'MGRDEF  ',X'17170000',AL4(0)    <<< NOTE NO PFKEY            
*                                                                               
         DC    CL8'BILLREC ',X'18180000',AL4(0)                                 
         DC    CL8'BILL    ',X'18180000',AL4(0)                                 
*                                                                               
         DC    CL8'A2COM   ',X'1D1D0000',AL4(0)                                 
         DC    CL8'A3COM   ',X'1D1D0000',AL4(0)                                 
         DC    CL8'BCOM    ',X'1D1D0000',AL4(0)                                 
         DC    CL8'MCOM    ',X'1D1D0000',AL4(0)                                 
         DC    CL8'RSCOM   ',X'1D1D0000',AL4(0)                                 
         DC    CL8'SDR     ',X'1F1D0000',AL4(0)                                 
*                                                                               
         DC    CL8'AGYHDR  ',X'32320000',AL4(KAGY)                              
         DC    CL8'DPTHDR  ',X'34340000',AL4(KDPT)                              
         DC    CL8'EQUHDR  ',X'35350000',AL4(0)   <<< NOTE NO PFKEY             
*                                                                               
         DC    CL8'MENU    ',X'44440000',AL4(KMENU)                             
*                                                                               
         DC    CL8'MARKET  ',X'45450000',AL4(0)                                 
         DC    CL8'STATION ',X'46460000',AL4(0)                                 
         DC    CL8'REP     ',X'47470000',AL4(0)                                 
RECLISTX EQU   *                                                                
         SPACE 2                                                                
         EJECT                                                                  
KEYLIST  DS    0H                                                               
         DC    X'11',AL1(L'K11)                                                 
K11      DC    C'MEDIA,(START AT *CLIENT*)'                                     
         DC    X'12',AL1(L'K12)                                                 
K12      DC    C'MEDIA,CLIENT,(START AT *PRODUCT*)'                             
         DC    X'13',AL1(L'K13)                                                 
K13      DC    C'MED,CLT,PRD,(START AT *ESTIMATE*)'                             
         DC    X'14',AL1(L'K14)                                                 
K14      DC    C'MEDIA,CLIENT,PRD GRP'                                          
         DC    X'15',AL1(L'K15)                                                 
K15      DC    C'MEDIA,(CLIENT),(PGR ID)'                                       
         DC    X'16',AL1(L'K16)                                                 
K16      DC    C'MEDIA,CLIENT,(PRD GROUP),MKT GRP'                              
         DC    X'17',AL1(L'K17)                                                 
K17      DC    C'MEDIA,CLIENT,(PRD GRP),MGRP ID'                                
         DC    X'18',AL1(L'K18)                                                 
K18      DC    C'MEDIA,CLIENT,PRODUCT'                                          
         DC    X'1D',AL1(L'K1D)                                                 
K1D      DC    C'MED,(CLT),(PRD),(EST)'                                         
         DC    X'1F',AL1(L'K1F)                                                 
K1F      DC    C'MED,(CLT),(PRD),(EST),(STA)'                                   
*                                                                               
         DC    X'32',AL1(L'K32)                                                 
K32      DC    C'NOT NEEDED'                                                    
         DC    X'34',AL1(L'K34)                                                 
K34      DC    C'MEDIA'                                                         
         DC    X'35',AL1(L'K35)                                                 
K35      DC    C'MEDIA,CLIENT'                                                  
*                                                                               
         DC    X'37',AL1(L'K37)                                                 
K37      DC    C'MEDIA,REP'                                                     
*                                                                               
         DC    X'44',AL1(L'K44)                                                 
K44      DC    C'MEDIA,(START AT *MENU*)'                                       
*                                                                               
         DC    X'45',AL1(L'K45)                                                 
K45      DC    C'MEDIA,MARKET'                                                  
*                                                                               
         DC    X'46',AL1(L'K46)                                                 
K46      DC    C'MEDIA,STATION'                                                 
*                                                                               
         DC    X'47',AL1(L'K47)                                                 
K47      DC    C'MEDIA,(START AT *REP NUMBER*)'                                 
*                                                                               
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
FLTDESC  DS    0D                                                               
*                                                                               
* FILTERS LIST                                                                  
*        0 = OVERLAY NUMBER   X'FF' FOR END OF LIST                             
*      1-2 = DISPLACEMENT TO NEXT OVERLAY DESCRIPTION                           
*                                                                               
*   VARIBALE NUMBER OF DESCRIPTION FIELDS                                       
*        0 = LENGTH OF DESCRIPTION OR X'FF' FOR END OF LIST                     
*        1 = LENGTH OF KEY WORD                                                 
*      2-39= DESCRIPTION                                                        
F11      DC    X'11'                                                            
         DC    AL2(F12-F11)                                                     
*                                                                               
         DC    AL1(L'F11A)                                                      
         DC    AL1(5)                                                           
F11A     DC    C'DATA=PROF,PROD'                                                
         DC    AL1(L'F11B)                                                      
         DC    AL1(4)                                                           
F11B     DC    C'CHARNN=N'                                                      
         DC    X'FF'                                                            
         SPACE 2                                                                
F12      DS    0C                                                               
F13      DC    X'13'                                                            
         DC    AL2(F14-F13)                                                     
*                                                                               
         DC    AL1(L'F13A)                                                      
         DC    AL1(8)                                                           
F13A     DC    C'DISPLAY=L,H'                                                   
*                                                                               
         DC    AL1(L'F13B)                                                      
         DC    AL1(4)                                                           
F13B     DC    C'EST=NNN,NNN-NNN,NO'                                            
*                                                                               
         DC    AL1(L'F13C)                                                      
         DC    AL1(5)                                                           
F13C     DC    C'DATE=MMMDD/YY-MMMDD/YY'                                        
*                                                                               
         DC    AL1(L'F13D)                                                      
         DC    AL1(5)                                                           
F13D     DC    C'DEMO=NNN'                                                      
*                                                                               
         DC    AL1(L'F13E)                                                      
         DC    AL1(5)                                                           
F13E     DC    C'DATA=DATE,COPY,NAME,MENU,DEMO'                                 
*                                                                               
         DC    AL1(L'F13F)                                                      
         DC    AL1(4)                                                           
F13F     DC    C'REQ=Y/N'                                                       
*                                                                               
         DC    AL1(L'F13G)                                                      
         DC    AL1(4)                                                           
F13G     DC    C'NMG=Y/N'                                                       
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
F14      DC    X'14'                                                            
         DC    AL2(F15-F14)                                                     
*                                                                               
         DC    AL1(L'F14A)                                                      
         DC    AL1(5)                                                           
F14A     DC    C'DATA=PROD'                                                     
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
F15      DS    0C                                                               
F16      DC    X'14'                                                            
         DC    AL2(F17-F16)                                                     
*                                                                               
         DC    AL1(L'F16A)                                                      
         DC    AL1(5)                                                           
F16A     DC    C'DATA=MARKET'                                                   
F17      DS    0C                                                               
F18      DC    X'18'                                                            
         DC    AL2(F32-F18)                                                     
*                                                                               
         DC    AL1(L'F18A)                                                      
         DC    AL1(4)                                                           
F18A     DC    C'EST=NNN,NNN-NNN,NO'                                            
*                                                                               
         DC    AL1(L'F18B)                                                      
         DC    AL1(4)                                                           
F18B     DC    C'MOS=MMM/YY'                                                    
*                                                                               
         DC    AL1(L'F18C)                                                      
         DC    AL1(7)                                                           
F18C     DC    C'BILLMO=MMM/YY'                                                 
*                                                                               
         DC    AL1(L'F18D)                                                      
         DC    AL1(6)                                                           
F18D     DC    C'INVNO=NNNNNN,GTNNNNNN,LTNNNNNN'                                
*                                                                               
         DC    AL1(L'F18E)                                                      
         DC    AL1(9)                                                           
F18E     DC    C'BILLDATE=MMMDD/YY'                                             
*                                                                               
         DC    AL1(L'F18F)                                                      
         DC    AL1(9)                                                           
F18F     DC    C'BILLTYPE= 4-7,AOR'                                             
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
F32      DC    X'32'                                                            
         DC    AL2(F33-F32)                                                     
*                                                                               
         DC    AL1(L'F32A)                                                      
         DC    AL1(7)                                                           
F32A     DC    C'CHARNN=N'                                                      
         SPACE 2                                                                
F33      DS    0C                                                               
         SPACE 2                                                                
F34      DS    0C                                                               
         SPACE 2                                                                
F35      DS    0C                                                               
         SPACE 2                                                                
F37      DS    0C                                                               
*                                                                               
F45      DC    X'45'                                                            
         DC    AL2(F46-F45)                                                     
*                                                                               
         DC    AL1(L'F45A)                                                      
         DC    AL1(3)                                                           
F45A     DC    C'TZ=1-4'                                                        
*                                                                               
         DC    AL1(L'F45B)                                                      
         DC    AL1(4)                                                           
F45B     DC    C'SWP=1-8'                                                       
*                                                                               
         DC    AL1(L'F45C)                                                      
         DC    AL1(5)                                                           
F45C     DC    C'DATA=TZ,SWP,RANK,HOMES,NTA,WEIGHT,SHARE,NSI,CSI,BBM,ARX        
               B'                                                               
*                                                                               
         DC    X'FF'                                                            
*                                                                               
F46      DC    X'46'                                                            
         DC    AL2(F47-F46)                                                     
*                                                                               
         DC    AL1(L'F46A)                                                      
         DC    AL1(4)                                                           
F46A     DC    C'PAY=NNN'                                                       
*                                                                               
         DC    AL1(L'F46B)                                                      
         DC    AL1(4)                                                           
F46B     DC    C'TIM=NNN'                                                       
*                                                                               
         DC    AL1(L'F46C)                                                      
         DC    AL1(4)                                                           
F46C     DC    C'AFF=NNN'                                                       
*                                                                               
         DC    AL1(L'F46D)                                                      
         DC    AL1(5)                                                           
F46D     DC    C'TYPE=N'                                                        
*                                                                               
         DC    AL1(L'F46E)                                                      
         DC    AL1(4)                                                           
F46E     DC    C'CLI=NNN'                                                       
*                                                                               
         DC    AL1(L'F46F)                                                      
         DC    AL1(4)                                                           
F46F     DC    C'MKT=NNNN'                                                      
*                                                                               
         DC    AL1(L'F46G)                                                      
         DC    AL1(5)                                                           
F46G     DC    C'DATA=MKT,PAY,TIM,TRA,TWX,FAX,TAX,SIZE,AFF,TYPE,CHA'            
*                                                                               
         DC    X'FF'                                                            
*                                                                               
F47      DC    X'47'                                                            
         DC    AL2(F48-F47)                                                     
*                                                                               
         DC    AL1(L'F47A)                                                      
         DC    AL1(4)                                                           
F47A     DC    C'SYND'                                                          
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
F48      DS    0C                                                               
         SPACE 2                                                                
         DC    X'FFFF'                                                          
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
* DDCOREQUS                                                                     
* DDCOMFACS                                                                     
* FAFACTS                                                                       
* FAUTL                                                                         
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
 END                                                                            
