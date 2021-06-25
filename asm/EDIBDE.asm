*          DATA SET EDIBDE     AT LEVEL 106 AS OF 04/25/14                      
*PHASE EDIBDEA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SCANNER                                                                
*INCLUDE BDESND                                                                 
*INCLUDE CPUINFO                                                                
*                                                                               
EDIBDF   START                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIBDF*,WORK=VWRKAREA                                         
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         L     RA,ACOMMON                                                       
         LHI   R9,4096                                                          
         AR    R9,RA                                                            
         LHI   R6,4096                                                          
         AR    R6,R9                                                            
         USING COMMON,RA,R9,R6                                                  
         L     R7,VCPRINT                                                       
         USING DPRINT,R7                                                        
         ST    RD,SAVERD           SET EXIT FROM CHAIN RD                       
*                                                                               
         L     RC,AWORKD           GET SOME W/S FOR THIS MODULE                 
         USING WORKD,RC                                                         
         BRAS  RE,INIT             INITIALISE IT ALL                            
         BNE   MAINX                                                            
*                                                                               
MAIN02   BRAS  RE,WAIT             ISSUE 'GET' AND WAIT FOR A MESSAGE           
         BNE   MAINX               OPERATOR REQUESTED CANCEL JOB                
*                                                                               
         OC    DATALEN,DATALEN     EMPTY MESSAGE?                               
         BZ    MAIN02              YES - SKIP THIS ONE, GET NEXT ONE            
         BRAS  RE,SENDBDF          SEND BDF                                     
*                                                                               
         B     MAIN02              NEXT ONE                                     
*                                                                               
MAINX    BRAS  RE,MQCLS            FREE ANY MQ CONNECTIONS THAT EXIST           
*                                                                               
         B     XBASE                                                            
*                                                                               
VWRKAREA DC    V(WORKAREA)                                                      
ACOMMON  DC    A(COMMON)                                                        
AWORKD   DC    A(WORKD)                                                         
         EJECT                                                                  
***********************************************************************         
* SEND MESSAGE OUT                                                    *         
***********************************************************************         
         SPACE 1                                                                
SENDBDF  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRFLDS          CLEAN UP BEFORE PROCESSING THIS MSG          
         SAM31                                                                  
*                                                                               
         L     R4,AMSGIN           INPUT MESSAGE BUFFER                         
         USING MSGHDRD,R4                                                       
         MVC   USERID,MSGUSR       GET LOCAL COPIES OF HEADER INFO              
         MVC   USERIDH,MSGUSRNO                                                 
         MVC   RPTSUBID,MSGPQSUB                                                
         MVC   RPTPQNUM,MSGPQNUM                                                
         MVC   RPTHTIME,MSGHTIME                                                
         MVC   RPTLOGNO,MSGLOGNO                                                
         MVC   RPTPQTYP,MSGPQTYP                                                
*                                                                               
         MVC   MSG01(8),USERID         PRINT USER ID                            
         MVC   MSG01+10(3),RPTSUBID    PRINT PQ SUBID                           
         GOTO1 VHEXIN,DMCB,RPTPQNUM,WORK,4,0                                    
         EDIT  (2,WORK),(5,MSG01+15),ALIGN=LEFT                                 
*                                                                               
         GOTO1 VHEXIN,DMCB,USERIDH,USERIDNO,4,0                                 
*                                                                               
         PACK  DUB,MSGHLEN         GO PAST HEADER                               
         CVB   R0,DUB                                                           
         AR    R4,R0               R4 = FIRST LINE OF MESSAGE                   
         DROP  R4                                                               
         SAM24                                                                  
*                                                                               
SBF020   BRAS  RE,GETNXTL          GET NEXT LINE INTO R / RLEN                  
         BNE   SBF030              EOB                                          
*                                                                               
         CLI   GOTHDR,C'Y'         TEST ALREADY HAVE HEADER                     
         BE    *+12                YES                                          
         BRAS  RE,PRCHDR           PROCESS THIS CARD AS IF HEADER               
         B     SBF020              NEXT CARD                                    
*                                                                               
         CLC   =C'++DDS',R+1       IS THIS A DDS CONTROL CARD                   
         BNE   *+12                NO - THEN WE ARE INTO THE FAX PROPER         
         BRAS  RE,PRCDDS                                                        
         B     SBF020                                                           
*                                                                               
SBF030   CLI   GOTHDR,C'Y'         TEST ALREADY HAVE HEADER                     
         BE    SBF040                                                           
         BRAS  RE,NOHDR            PRT WARNING MSG                              
         MVI   ACTION,ACTJNKQ      MARK REPORT UNSENDABLE                       
         BRAS  RE,POSTSENT                                                      
         B     EXITOK                                                           
*                                  POINT R4 BACK TO A(1ST DATA LINE)            
SBF040   SH    R4,RLEN                                                          
         AHI   R4,-4               4 BYTES FOR THE LENGTH IN CHARS              
*                                                                               
         BRAS  RE,BLDCREF          REFERENCE NUMBER RETURNS IN CUSTREF          
         BRAS  RE,PRCDDSC          PROCESS ALL ++DDS CARDS                      
*                                                                               
         BRAS  RE,FINDDEST         GET EDICT CONTROL RECORD INFO                
         BE    SBF050              YES                                          
         MVI   ACTION,ACTJNKQ      NO - MARK REPORT UNSENDABLE                  
         BRAS  RE,POSTSENT                                                      
         B     EXITOK                                                           
*                                                                               
SBF050   BRAS  RE,BLDFILN          BUILD THE FILENAME                           
         BRAS  RE,GETBDEP          LOG THE BDE PARMS                            
*                                                                               
         CLI   HDR+37,C'D'         IS A DSN SUPPLIED?                           
         BE    SBF070                                                           
         CLI   HDR+37,C'H'         IS A HFS FILE SUPPLIED?                      
         BE    SBF070                                                           
*                                                                               
         BRAS  RE,BLDDATF          BUILD FILE AND SEND IT VIA BSS               
         BE    SBF125                                                           
*                                  CAN'T PROCESS FILE                           
         LA    R2,MQBACK           PUT BACK THE MQ MSG FOR RETRY                
         BRAS  RE,CALLMQ                                                        
         CLI   HFSSPCOK,C'N'       HFS SPACE PROBLEM?                           
         BE    *+6                 YES                                          
         DC    H'0'                ABEND FOR ALL OTHER ERROR FOR NOW            
         GOTO1 VDMGR,DMCB,=C'OPMSG',=C'AUTONOTE*US-MF_FAC_NOTIFY:BDE No+        
               space left in the HFS device.      '                             
         GOTO1 VLOGIO,DMCB,1,=C'No space left in the HFS device.     BD+        
               F subtask will shutdown.  Try again later.'                      
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)             WTOR MESSAGE                  
         MVI   OPERSTOP,C'Y'        SHUTDOWN FOR NOW                            
         B     SBFX                                                             
*                                                                               
SBF070   DS    0H                                                               
         BRAS  RE,INTRC            SET RC=START IN RCFILE                       
         BRAS  RE,BATCH                                                         
*                                                                               
         CLC   =C'RC=START',WORK                                                
         BNE   SBF100                                                           
         GOTO1 VDMGR,DMCB,=C'OPMSG',=C'AUTONOTE*US-MF_FAC_NOTIFY:BDF RE+        
               XX EXEC FAILED, PLEASE CHECK MYSTDOUT FOR ERROR'                 
*                                                                               
         LA    R2,MQBACK           NEED TO PUT BACK THE MQ MSG                  
         BRAS  RE,CALLMQ                                                        
         MVI   OPERSTOP,C'Y'                                                    
         DC    H'0'                ABEND FOR NOW                                
         B     SBFX                                                             
*                                                                               
SBF100   DS    0H                                                               
         CLC   =C'RC=COMPLETED',WORK                                            
         BNE   SBF130                                                           
*                                                                               
SBF125   DS    0H                                                               
         MVI   ACTION,ACTADDQ              MARK SENT                            
         BRAS  RE,POSTSENT                                                      
         B     SBFX                                                             
*                                                                               
SBF130   MVC   PLINE(20),=CL20'CANT_SEND_REPORT'                                
         MVC   PLINE+30(32),WORK                                                
         BRAS  RE,PRNT                                                          
         MVI   ERRCODE,EDFERDNQ    DSN ERROR                                    
         CLC   =C'RC=DSN ERROR',WORK                                            
         BE    SBF140              ALRET COMP ROOM ANYWAY                       
*                                                                               
*                                  "RC=ICONV Error'                             
         MVI   ERRCODE,EDFERNDQ    NO DOC ERROR/ICONV ERROR                     
*                                                                               
SBF140   DS    0H                                                               
         CLC   =C'GRPMAIX',HDR+15                                               
         BNE   SBF145                                                           
         LA    R2,MQBACK           NEED TO PUT BACK THE MQ MSG                  
         BRAS  RE,CALLMQ                                                        
         GOTO1 VDMGR,DMCB,=C'OPMSG',=C'AUTONOTE*US-MF_FAC_NOTIFY,JSHAN:+        
               Cannot send GroupM report! Please check.    '                    
         GOTO1 VLOGIO,DMCB,1,=C'Cannot skip GRPMAIX report. Try again l+        
               ater.         '                                                  
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)          WTOR MESSAGE                     
         MVI   OPERSTOP,C'Y'                   SHUTDOWN FOR NOW                 
         B     SBFX                                                             
*                                                                               
SBF145   DS    0H                                                               
         CLC   =C'KRGNY     SMO',MSG01                                          
         BNE   SBF150                                                           
         MVC   WORK2,SPACESL                                                    
         MVC   WORK2(45),=CL45'CANNOT OPEN DATATSET, CHECK IF IT IS IN +        
               USE: '                                                           
         MVC   WORK2+45(55),DSN                                                 
         GOTO1 VLOGIO,DMCB,1,(100,WORK2)                                        
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)          WTOR MESSAGE                     
*                                   (REPLY, CONTINUE TO NEXT WTOR---)           
*                                                                               
SBF150   MVC   WORK2,SPACESL                                                    
         MVC   WORK2(12),=CL12'BDF REPORT: '                                    
         MVC   WORK2+12(L'MSG01),MSG01                                          
         MVC   WORK2+12+L'MSG01+1(70),=CL70'CAN''T BE SENT!  REPLY: (R)+        
               TO RETRY, (S) TO SKIP, (P) TO STOP       '                       
         GOTO1 VLOGIO,DMCB,1,(100,WORK2)                                        
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)             WTOR MESSAGE                  
*                                                                               
         CLI   BYTE,C'S'           SKIP?                                        
         BE    SBF160                                                           
*                                                                               
         CLI   BYTE,C'P'           STOP?                                        
         BNE   SBF155                                                           
         MVI   OPERSTOP,C'Y'                                                    
         LA    R2,MQBACK           PUT BACK THE MQ MSG FOR RETRY                
         BRAS  RE,CALLMQ                                                        
         B     SBFX                                                             
*                                                                               
SBF155   LA    R2,MQBACK           PUT BACK THE MQ MSG FOR RETRY                
         BRAS  RE,CALLMQ                                                        
*        CLI   BYTE,C'R'           RETRY?                                       
*        BE    SBFX                                                             
         B     SBFX                ASSUME RETRY WHEN INVALID REPLY              
*                                                                               
SBF160   EQU   *                                                                
         MVI   ACTION,ACTJNKQ      MARK REPORT UNSENDABLE                       
         BRAS  RE,POSTSENT                                                      
*                                  BUILD FAILURE NOTIFICATION MESSAGE           
         CLC   BDEFN,SPACESL                                                    
         BL    SBFX                NO EMAIL ADDRESS                             
*                                                                               
         MVC   WORK2,SPACESL                                                    
         MVC   WORK2(9),=CL9'AUTONOTE*'                                         
         MVC   WORK2+9(L'BDEFN),BDEFN                                           
*                                                                               
         LA    RE,WORK2+9+L'BDEFN         PT TO THE LAST CHAR                   
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         B     *-10                       RE = A(LAST NON-BLANK CHAR)           
*                                                                               
         MVC   1(21,RE),=CL21':BDF REPORT FAILED - '                            
         MVC   22(L'MSG01,RE),MSG01       REPORT USERID, SUBID & #              
*                                                                               
         OC    WORK2,SPACESL                                                    
         GOTO1 VDMGR,DMCB,=C'OPMSG',(100,WORK2)                                 
*                                                                               
SBFX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BLDDATF  NTR1  BASE=*,LABEL=*                                                   
         MVC   PLINE(30),=CL30'begin  BUILD DATA FILE'                          
         BRAS  RE,PRNT                                                          
*                                                                               
         XC    WORK,WORK                                                        
         CLI   ETYPE,C'A'                                                       
         BE    BDA220                                                           
         CLI   ETYPE,C'R'                                                       
         BE    BDA220                                                           
         MVC   WORK(11),=CL11'/u/bdeftpt/'     TEST                             
         LA    RE,11                                                            
         LA    R1,WORK+11                                                       
         B     BDA225                                                           
BDA220   MVC   WORK(10),=CL10'/u/bdeftp/'      LIVE                             
         LA    RE,10                                                            
         LA    R1,WORK+10                                                       
*                                                                               
BDA225   AHI   RE,L'CUSTREF                                                     
         ST    RE,DFLEN                                                         
         MVC   0(L'CUSTREF,R1),CUSTREF                                          
*                                                                               
*************tracing************************************                        
         MVC   PLINE(10),=CL10'WORK ='                                          
         MVC   PLINE+10(50),WORK                                                
         BRAS  RE,PRNT                                                          
*************tracing************************************                        
*                                                                               
         SAM31                                                                  
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR       USER READ/WRITE, GROUP READ,               
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IROTH           OTHER READ             
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_CREAT+O_RDWR+O_TRUNC                                  
         L     RF,BPX1OPN                                                       
         CALL  (15),                 OPEN A FILE  BPX1OPN              X        
               (DFLEN,               INPUT: PATHNAME LENGTH            X        
               WORK,                 INPUT: PATHNAME                   X        
               O_FLAGS,              INPUT: ACCESS            BPXYOPNF X        
               S_MODE,               INPUT: MODE    BPXYMODE, BPXYFTYP X        
               RETVAL,               RETURN VALUE:-1 OR FILE DESCRIPTO X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
         ST    RF,FILEDESC         SAVE FILE DESCRIPTOR                         
         SAM24                                                                  
*                                                                               
BDAT30   DS    0H                                                               
         GOTO1 VHEXIN,DMCB,RPTPQTYP,PQTYPE,2,0                                  
         TM    PQTYPE,QLTYDL       DOWNLOADABLE REPORT?                         
         BO    BDAT30D                                                          
         BRAS  RE,GETNXTL          GET NEXT LINE INTO R / RLEN                  
         BNE   BDAT100             EOB                                          
         B     BDAT50                                                           
BDAT30D  BRAS  RE,GETNXTLD         GET NEXT LINE INTO R / RLEN                  
         BNE   BDAT100             EOB   (DOWNLOADABLE REPORT)                  
*                                                                               
BDAT50   EQU   *                                                                
*************tracing************************************                        
         MVC   PLINE(10),=CL10'RLEN ='                                          
         MVC   PLINE+10(L'RLEN),RLEN                                            
         BRAS  RE,PRNT                                                          
         MVC   PLINE(10),=CL10'R ='                                             
         MVC   PLINE+10(100),R                                                  
         BRAS  RE,PRNT                                                          
         SAM31                                                                  
         MVC   PLINE(10),=CL10'R4='                                             
         MVC   PLINE+10(100),0(R4)                                              
         SAM24                                                                  
         BRAS  RE,PRNT                                                          
*************tracing************************************                        
         LH    RE,RLEN                                                          
         AHI   RE,-2                                                            
         L     R8,=A(R+1)                                                       
         AR    R8,RE               PT TO THE LAST CHAR IN THIS RECORD           
*                                  OMIT THE TAILING BLANKS                      
         CLI   0(R8),C' '                                                       
         BH    *+10                                                             
         BCTR  R8,0                                                             
         BCT   RE,*-10                                                          
         LA    R8,1(R8)                                                         
*                                                                               
         CLI   BDEOP,DESTBUIXQ     UNIX OS?                                     
         BNE   BDAT60              NO CARRIAGE RETURN FOR UNIX OS               
         MVI   0(R8),X'15'         NEW LINE                                     
         LA    R8,1(R8)                                                         
         B     BDAT70                                                           
*                                                                               
BDAT60   MVI   0(R8),X'0D'         CARRIAGE RETURN                              
         MVI   1(R8),X'15'         NEW LINE                                     
         LA    R8,2(R8)                                                         
*                                                                               
BDAT70   S     R8,=A(R+1)          IGNORE THE CC CHAR                           
         ST    R8,BUFLEN                                                        
         MVC   BUFADDR,=A(R+1)                                                  
         MVC   ALET,=F'0'                                                       
*                                                                               
         CLI   BDECA,C'Y'          CONVERT TO ASCII?                            
         BNE   BDAT80              DON'T DO THE TRANSLATION                     
*                                                                               
         LHI   R0,X'FF'         PREPARE FOR TRE(SET TEST VALUE=X'FF')           
         L     RE,=A(R+1)          TARGET                                       
         LR    RF,R8               LENGTH                                       
         L     R1,=A(ASCIITBL)     EBCDIC TO ASCII TABLE                        
         TRE   RE,R1               TRANSLATE FROM EBCDIC TO ASCII               
         BO    *-4                                                              
*                                                                               
BDAT80   DS    0H                                                               
         SAM31                                                                  
         L     RF,BPX1WRT                                                       
         CALL  (15),                 WRITE TO A FILE                   X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               BUFADDR,              INPUT: ->BUFFER                   X        
               ALET,                 INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO WRIT    X        
               RETVAL,               RETURN VALUE: -1 OR BYTES WRIT    X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   BDAT90                                                           
*                                                                               
         CLC   RETCODE,=XL4'00000085'  NO SPACE LEFT ON THE DEVICE              
         BE    *+6                                                              
         DC    H'0'                CAN'T WRITE TO HFS FILE!                     
         MVI   HFSSPCOK,C'N'       HFS SPACE PROBLEM                            
*                                                                               
BDAT90   DS    0H                                                               
         SAM24                                                                  
         CLI   HFSSPCOK,C'N'       HFS SPACE OKAY?                              
         BE    BDATERR             NO, EXIT                                     
         B     BDAT30                                                           
*                                                                               
BDAT100  DS    0H                                                               
         SAM31                                                                  
         L     RF,BPX1CLO                                                       
         CALL  (15),                 CLOSE A FILE                      X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               RETVAL,               RETURN VALUE: 0 OR -1             X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
         SAM24                                                                  
*                                                                               
         MVC   PLINE(30),=CL30'END OF BUILD DATA FILE'                          
         BRAS  RE,PRNT                                                          
*                                                                               
         BRAS  RE,CALLBSND                                                      
         B     BDATYES                                                          
*                                                                               
BDATERR  DS    0H                                                               
         SAM31                                                                  
         L     RF,BPX1CLO                                                       
         CALL  (15),                 CLOSE A FILE                      X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               RETVAL,               RETURN VALUE: 0 OR -1             X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL   TEST RETVAL           BL    PSEUD            
         BNL   *+6                                                              
         DC    H'0'                                                             
         SAM24                                                                  
*                                                                               
         MVC   PLINE(30),=CL30'ERROR--BUILD DATA FILE'                          
         BRAS  RE,PRNT                                                          
         B     BDATNO                                                           
*                                                                               
BDATYES  CR    RB,RB               SET CC EQUAL                                 
         B     BDATX                                                            
BDATNO   LTR   RB,RB               SET CC NOT EQUAL                             
BDATX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BLDFILN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   FILENAME,SPACESL                                                 
*                                                                               
BFN30    MVC   FILENAME(L'FLNAME),FLNAME                                        
         LA    RE,FILENAME+L'FLNAME+1                                           
*                                                                               
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         B     *-10                                                             
*                                  **********************************           
         LA    R1,1(,RE)           SAVE FILENAME LENGTH W/O EXT IN R1           
         LAY   RF,FILENAME              FOR LATER USE                           
         SR    R1,RF               **********************************           
*                                                                               
*                                                                               
         CLC   =C'.NONE',FLEXT                                                  
         BE    BFN40               SKIP ANY FILE EXT.                           
*                                                                               
         MVI   1(RE),C'.'                                                       
         MVC   2(L'FLEXT,RE),FLEXT                                              
*                                                                               
         AHI   RE,L'FLEXT+2                                                     
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         B     *-10                                                             
*                                                                               
BFN40    AHI   RE,1                                                             
         S     RE,=A(FILENAME)                                                  
         ST    RE,FNLEN                                                         
*                                  **********************************           
         CLC   DFFILN(12),FILENAME ONLY TR FILENAME, NOT INCLUDE EXT            
         BNE   BFN50                   IF DEFAULT FILENAME IS USED.             
         LR    RE,R1               USE LENGTH W/O EXT                           
*                                  **********************************           
*                                                                               
*                                  TR CHARS THAT MAY CAUSE PROBLEM              
BFN50    BCTR  RE,0                                                             
         LAY   RF,TRTABLE                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         TR    FILENAME(0),0(RF)                                                
*                                                                               
BFNX     B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CALLBSND NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   RE,SUBJECT          REPLACE & IN SUBJECT WITH +                  
         LA    RF,L'SUBJECT-1                                                   
CBSND20  CLI   0(RE),C'&&'                                                      
         JNE   CBSND20N                                                         
         MVI   0(RE),C'+'                                                       
CBSND20N AHI   RE,1                                                             
         BCT   RF,CBSND20                                                       
*                                                                               
         LAY   R3,BDECBLK                                                       
         USING BDESNDD,R3                                                       
         MVC   BSNCNM,BDECN         BDE COMMAN NAME  (CL40)                     
         MVC   BSNENC,BDEEN         BDE ENCRYPTION   (CL1)                      
         MVC   BSNCMP,BDECM         BDE COMPRESS Y/N (CL1)                      
         MVC   BSNQMGR,BQMGRNAM     QMGR BDE SENDER  (CL48)                     
         MVC   BSNQNAM,BQUENAME     QUEUE BDE SENDER (CL48)                     
         MVC   BSNAHCON,=A(HCONN)                                               
         MVC   BSNSUB,SPACES                                                    
         OC    SUBJECT,SPACES                                                   
         MVC   BSNSUB(L'SUBJECT),SUBJECT     SUBJECT (CL60)                     
         MVC   BSNRID,SPACES                                                    
         MVC   BSNRID(L'CUSTREF),CUSTREF     REQUEST ID (CL18)                  
         MVC   BSNAPN,SPACES                                                    
         MVC   BSNAPN(10),=CL10'EDICT-BDF'   APPLICATION NAME (CL10)            
         MVC   BSNFIL,SPACES                                                    
         MVC   BSNFIL(L'FILENAME),FILENAME   FILENAME(CL71)                     
         MVI   BSNINFS,C'S'         FILE ALREADY in /u/bdeftp(t)                
         MVC   BSNINFN(L'CUSTREF),CUSTREF    CUSTREF AS FILENAME(CL18)          
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(BDESND),DMCB,=C'OPEN',BDECBLK,BSNDDQ,0,0,0                    
         TM    DMCB+12,X'80'                                                    
         JO    *+2                                                              
*                                                                               
         GOTO1 =V(BDESND),DMCB,=C'CLOSE',0,0,0,0,0                              
         TM    DMCB+12,X'80'                                                    
         JO    *+2                                                              
*                                                                               
CBSNDX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GETBDEP  NTR1   BASE=*,LABEL=*                                                  
***THIS ROUTINE IS JUST FOR TRACTING ONLY****************************           
*                                                                               
         MVC   CR00P1,BDECN                                                     
         MVC   CR01P1,SUBJECT                                                   
         MVC   CR01P2,CUSTREF                                                   
         MVC   CR02P1(L'DSN),DSN                                                
         MVC   CR03P1(L'FILENAME),FILENAME                                      
         MVC   CR04P1(1),BDECM                                                  
         MVC   CR05P1(1),BDESF                                                  
*                                                                               
         CLI   BDEEN,DESTBNOQ                                                   
         BNE   *+14                                                             
         MVC   CR06P1(10),=CL10'NO_CRYPT'                                       
         B     GBP10X                                                           
         CLI   BDEEN,DESTBBFQ                                                   
         BNE   *+14                                                             
         MVC   CR06P1(10),=CL10'BLOWFISH'                                       
         B     GBP10X                                                           
         CLI   BDEEN,DESTB3DQ                                                   
         BNE   *+10                                                             
         MVC   CR06P1(10),=CL10'TRIPLE_DES'                                     
GBP10X   DS    0H                                                               
*                                                                               
         LA    R3,CR00                                                          
GBP30    CLI   0(R3),X'FF'                                                      
         BE    GBP30X                                                           
         MVC   P,SPACES                                                         
         ZIC   R2,0(R3)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P(0),1(R3)                                                       
         LA    R3,2(R2,R3)                                                      
         GOTO1 =V(PRINTER)                                                      
         B     GBP30                                                            
GBP30X   DS    0H                                                               
*                                                                               
GBPX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
INTRC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,PHLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RCFILE(0),PATH                                                   
*                                                                               
         LA    RE,RCFILE+1(RE)                                                  
         MVC   0(12,RE),=C'/return_code'                                        
         L     RE,PHLEN                                                         
         AHI   RE,12                                                            
         ST    RE,RCFLEN                                                        
         CHI   RE,L'RCFILE                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SAM31                                                                  
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR       USER READ/WRITE, GROUP READ,               
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IROTH        OTHER READ                
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_CREAT+O_RDWR+O_TRUNC                                  
         L     RF,BPX1OPN                                                       
         CALL  (15),                 OPEN A FILE  BPX1OPN              X        
               (RCFLEN,              INPUT: PATHNAME LENGTH            X        
               RCFILE,               INPUT: PATHNAME                   X        
               O_FLAGS,              INPUT: ACCESS            BPXYOPNF X        
               S_MODE,               INPUT: MODE    BPXYMODE, BPXYFTYP X        
               RETVAL,               RETURN VALUE:-1 OR FILE DESCRIPTO X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
         ST    RF,FILEDESC           SAVE FILE DESCRIPTOR                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(8),=CL8'RC=START'                                           
         MVC   BUFLEN,=F'8'          READ BUFFER LENGTH                         
         MVC   BUFADDR,=A(WORK)                                                 
         L     RF,BPX1WRT                                                       
         CALL  (15),                 OPEN A FILE  BPX1RED              X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               BUFADDR,              ->BUFFER TO READ INTO             X        
               BUFFALET,             INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO READ    X        
               RETVAL,               RETURN VALUE: 0, -1, OR CHAR COUN X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BPX1CLO                                                       
         CALL  (15),                 CLOSE A FILE                      X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               RETVAL,               RETURN VALUE: 0 OR -1             X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SAM24                                                                  
*                                                                               
IRCX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CHKRC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,PHLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RCFILE(0),PATH                                                   
*                                                                               
         LA    RE,RCFILE+1(RE)                                                  
         MVC   0(12,RE),=C'/return_code'                                        
         L     RE,PHLEN                                                         
         AHI   RE,12                                                            
         ST    RE,RCFLEN                                                        
         CHI   RE,L'RCFILE                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
*                                                                               
         XC    S_MODE,S_MODE                                                    
         MVI   S_MODE2,S_IRUSR       USER READ/WRITE, GROUP READ,               
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IROTH        OTHER READ                
         XC    O_FLAGS(OPNF#LENGTH),O_FLAGS                                     
         MVI   O_FLAGS4,O_RDONLY                                                
         L     RF,BPX1OPN                                                       
         CALL  (15),                 OPEN A FILE  BPX1OPN              X        
               (RCFLEN,              INPUT: PATHNAME LENGTH            X        
               RCFILE,               INPUT: PATHNAME                   X        
               O_FLAGS,              INPUT: ACCESS            BPXYOPNF X        
               S_MODE,               INPUT: MODE    BPXYMODE, BPXYFTYP X        
               RETVAL,               RETURN VALUE:-1 OR FILE DESCRIPTO X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
         ST    RF,FILEDESC           SAVE FILE DESCRIPTOR                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   BUFLEN,=A(L'WORK)     READ BUFFER LENGTH                         
         MVC   BUFADDR,=A(WORK)                                                 
         L     RF,BPX1RED                                                       
         CALL  (15),                 OPEN A FILE  BPX1RED              X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               BUFADDR,              ->BUFFER TO READ INTO             X        
               BUFFALET,             INPUT: BUFFER ALET                X        
               BUFLEN,               INPUT: NUMBER OF BYTES TO READ    X        
               RETVAL,               RETURN VALUE: 0, -1, OR CHAR COUN X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BPX1CLO                                                       
         CALL  (15),                 CLOSE A FILE                      X        
               (FILEDESC,            INPUT: FILE DESCRIPTOR            X        
               RETVAL,               RETURN VALUE: 0 OR -1             X        
               RETCODE,              RETURN CODE                       X        
               RSNCODE),             REASON CODE                       X        
               VL,MF=(E,PLIST)                                                  
         ICM   RF,B'1111',RETVAL     TEST RETVAL           BL    PSEUD          
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH BACK TO 24-BIT MODE                   
*                                                                               
CRCX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BATCH    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   PARM,C' '                                                        
         MVC   PARM+1(200),PARM                                                 
         MVC   PARM+200(200),PARM                                               
*                                                                               
         MVC   PARM+2(3),=C'SH '                                                
         L     RE,PHLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PARM+5(0),PATH                                                   
*                                                                               
         LA    RE,PARM+6(RE)                                                    
         MVC   0(15,RE),=C'/newmain2.rex '''                                    
         AHI   RE,15                                                            
*                                                                               
         L     RF,FNLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FILENAME                                                 
*                                                                               
         LA    RE,1(RE,RF)                                                      
         MVC   0(2,RE),=C''' '                                                  
         AHI   RE,2                                                             
*                                                                               
         OC    BDECA,BDECA                                                      
         BNZ   *+8                                                              
         MVI   BDECA,C'N'                                                       
         MVC   0(L'BDECA,RE),BDECA                                              
         AHI   RE,L'BDECA+1                                                     
*                                                                               
         OC    BDECP,BDECP                                                      
         BNZ   *+10                                                             
         MVC   BDECP,=CL10'ISO8859-1'   USE DEFAULT CODE PAGE                   
         MVC   0(L'BDECP,RE),BDECP                                              
         AHI   RE,L'BDECP+1                                                     
*                                                                               
         OC    BDEOP,BDEOP                                                      
         BNZ   *+8                                                              
         MVI   BDEOP,C'?'                                                       
         MVC   0(L'BDEOP,RE),BDEOP                                              
         AHI   RE,L'BDEOP+1                                                     
*                                                                               
         CLI   HDR+37,C'D'         IS A DSN SUPPLIED?                           
         BE    BAT40                                                            
         CLI   HDR+37,C'H'         IS A HFS FILE SUPPLIED?                      
         BE    BAT40                                                            
*                                                                               
         MVC   0(3,RE),=CL3'X X'   NO DSNFLAG AND DSN                           
         AHI   RE,4                                                             
         B     BAT50                                                            
*                                                                               
BAT40    MVC   0(1,RE),HDR+37                                                   
         MVC   2(L'DSN,RE),DSN                                                  
         AHI   RE,L'DSN+3                                                       
*                                                                               
BAT50    OC    BDEBI,BDEBI                                                      
         BNZ   *+8                                                              
         MVI   BDEBI,C'N'                                                       
         MVC   0(L'BDEBI,RE),BDEBI    BINARY FLAG                               
         AHI   RE,L'BDEBI+1                                                     
*                                                                               
         MVC   0(L'CUSTREF,RE),CUSTREF  USE CUSTOM REF# AS FILENAME             
         AHI   RE,L'CUSTREF+1                                                   
*                                                                               
         OC    BDESF,BDESF                                                      
         BNZ   *+8                                                              
         MVI   BDESF,C'N'                                                       
         MVC   0(L'BDESF,RE),BDESF    DELETE_AFTER_SENT FLAG                    
         AHI   RE,L'BDESF+1                                                     
*                                                                               
         MVC   0(2,RE),=CL2'>>'                                                 
         AHI   RE,2                                                             
*                                                                               
         L     R1,PHLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),PATH                                                     
         A     RE,PHLEN                                                         
*                                                                               
         MVC   0(9,RE),=CL9'/mystdout'                                          
         AHI   RE,9                                                             
*                                                                               
         S     RE,=A(PARM)                                                      
         SHI   RE,2                -2 BYTES FOR PARM LEN                        
         STH   RE,PARM             STORE THE LEN OF PARM                        
*                                                                               
         LINKX EP=BPXBATCH,PARAM=(PARM),VL=1,ERRET=ERRET                        
         ST    RF,FULL                                                          
*                                                                               
*&&DO                                                                           
         XC    BPXECB,BPXECB        clear ecb                                   
         ATTACH EP=ASMREXX,ECB=BPXECB,PARAM=PARM,SZERO=NO                       
         LTR   RF,RF                                                            
         BZ    BAT71                                                            
         DC    H'0'                                                             
BAT71    ST    R1,TCBADDR                                                       
         WAIT  ECB=BPXECB                                                       
         DETACH TCBADDR                                                         
*                                                                               
*                                                                               
         MVC   P(35),=CL35'Return code from BPXBATCH:'                          
         GOTO1 =V(HEXOUT),DMCB,BPXECB,P+35,4,=C'TOG'                            
         GOTO1 VPRINTER                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,BPXECB+1     IGNORE 1ST BYTE                                
         BZ    BAT90             NO ERRORS                                      
*                                                                               
         CHI   RF,X'0100'                                                       
         BL    BAT80                                                            
         CHI   RF,X'0F00'                                                       
         BNH   BATX              X'100' - X'F00' REXX ERROR, CHK RCFILE         
BAT80    EQU   *                                                                
         MVC   P(35),=CL35'BPXBATCH ERROR'                                      
         GOTO1 VPRINTER                                                         
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
BAT90    DS    0H                                                               
         BRAS  RE,CHKRC            CHECK RC FILE                                
         CLC   =C'RC=COMPLETED',WORK                                            
         BNE   BATX                                                             
         BRAS  RE,CALLBSND         DON'T SEND MQ MSG IF ANY ERROR               
*                                                                               
BATX     XIT1                                                                   
BPXECB   DC    F'0'                                                             
TCBADDR  DS    A                                                                
ERRET    DC    H'0'                                                             
PARM     DS    CL400                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CLEAR ALL FIELDS BEFORE STARTING PROCESSING                         *         
***********************************************************************         
         SPACE 1                                                                
CLRFLDS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DSN,DSN                                                          
         XC    HDR,HDR                                                          
         MVI   SUBJECT,C'%'        MUST HAVE SOMETHING IN SUBJECT FIELD         
         MVC   FLEXT,SPACESL                                                    
         MVC   FLEXT(3),=C'htm'                                                 
         MVI   HFSSPCOK,C'Y'       ASSUME HFS SPACE IS OKAY                     
         MVI   GOTHDR,C'N'                                                      
*                                                                               
         L     R2,ALINETAB         CLEAR OUT THE ++DDS LINES                    
         USING LINETABD,R2                                                      
CFL20    XC    LINELEN,LINELEN                                                  
         MVC   LINEIN,SPACESL                                                   
         AHI   R2,LINETABL                                                      
         CLI   LINEID,X'FF'                                                     
         BNE   CFL20                                                            
         DROP  R2                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET NEXT LINE INTO R AND RLEN                                       *         
* NTRY: R4 = CURRENT BUFFER POINTER                                   *         
***********************************************************************         
         SPACE 1                                                                
GETNXTL  NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
         XC    RLEN,RLEN           WE SET LENGTH OF THIS LINE HERE              
         MVC   R(L'SPACESL),SPACESL       PAD ID WITH SPACES                    
*                                                                               
         LR    R0,R4               CHECK NOT PAST END OF BUFFER                 
         S     R0,AMSGIN                                                        
         C     R0,DATALEN                                                       
         BL    GNXL20              EOB                                          
         SAM24                                                                  
         B     EXITL                                                            
*                                                                               
GNXL20   PACK  DUB,0(4,R4)         GET LENGTH OF THIS LINE                      
         CVB   R1,DUB                                                           
         AHI   R4,4                GO PAST LENGTH                               
*                                                                               
         LR    R0,R4               COPY LINE INTO R AND LEN INTO RLEN           
         STH   R1,RLEN                                                          
         LA    RE,R                                                             
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         TR    R+1(255),TRTAB      TRANSLATE OUT UNPRINTABLES                   
         AH    R4,RLEN                                                          
*                                                                               
         LHI   R0,81                                                            
         CLI   HDR+34,C'W'         WIDE REPORT?                                 
         BNE   *+8                                                              
         LHI   R0,133                                                           
         CH    R0,RLEN                                                          
         BH    *+8                                                              
         STH   R0,RLEN                                                          
*                                                                               
         SAM24                                                                  
*                                                                               
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
* GET NEXT LINE INTO R AND RLEN     (FOR DOWNLOADABLE REPORT ONLY)    *         
* NTRY: R4 = CURRENT BUFFER POINTER                                   *         
***********************************************************************         
         SPACE 1                                                                
GETNXTLD NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
         LR    R2,R4               CHECK NOT PAST END OF BUFFER                 
         S     R2,AMSGIN                                                        
         C     R2,DATALEN                                                       
         BNL   GNLDBAD             PAST END OF BUFFER                           
*                                                                               
         CLI   1(R4),C':'          END OF REPORT?                               
         BE    GNLDBAD             YES                                          
         CLC   =C'*** END OF DDS MESSAGE ***',1(R4)                             
         BE    GNLDBAD             YES                                          
*                                                                               
         XC    RLEN,RLEN           WE SET LENGTH OF THIS LINE HERE              
         LA    R2,R                                                             
         ST    R2,RNEXT            NEXT AVALIABLE AREA IN R                     
         LHI   R3,L'R                                                           
         LLC   RF,SPACES                                                        
         MVCL  R2,RE               BLANK OUT THE AREA                           
*                                                                               
*                                                                               
*                                                                               
GNLD20   DS    0H                                                               
         PACK  DUB,0(4,R4)         GET LENGTH OF THIS LINE                      
         CVB   R3,DUB                                                           
         AHI   R4,4                GO PAST LENGTH                               
*                                                                               
         LR    R2,R4               COPY/MERGE LINE INTO R                       
         STH   R3,HALF                                                          
         L     RE,RNEXT                                                         
         LR    RF,R3                                                            
         MVCL  RE,R2               NO WORRY ABOUT CC, TRE WILL HANDLE           
*                                                                               
         AH    R4,HALF             BUMP UP THE MQ BUFFER POINTER                
         LH    RE,RLEN             ADD UP THE LENGTH OF R                       
         AH    RE,HALF                                                          
         STH   RE,RLEN                                                          
*                                                                               
         L     RF,RNEXT                                                         
         LR    RE,RF                                                            
         AH    RE,HALF             BUMP UP THE R POINTER                        
         ST    RE,RNEXT                                                         
*                                                                               
*                                  RF = A(START OF RECORD)                      
*                                  RE = A(JUST PAST END OF RECORD)              
         LHI   R0,X'5E'            SEMICOLON (END OF LOGICAL RECORD)            
         SRST  RE,RF               SEARCH FOR SEMICOLON IN RECORD               
         BC    4,GNLD80            SEMICOLON FOUND, NO MORE MERGE               
*                                  NOT - GO GET NEXT LINE                       
*                                                                               
*                        BUT MAKE SURE THE MQ BUF IS GOOD FIRST                 
         LR    R2,R4               CHECK NOT PAST END OF BUFFER                 
         S     R2,AMSGIN                                                        
         C     R2,DATALEN                                                       
         BNL   GNLDBAD             PAST END OF BUFFER                           
         CLI   1(R4),C':'          END OF REPORT?                               
         BE    GNLDBAD             YES - LAST LINE IN R IS DROPPED              
         CLC   =C'*** END OF DDS MESSAGE ***',1(R4)                             
         BE    GNLDBAD             YES - LAST LINE IN R IS DROPPED              
         B     GNLD20              FINALLY, GO GET NEXT LINE                    
*                                                                               
*                                                                               
GNLD80   DS    0H                                                               
         LHI   R2,1000                                                          
         CH    R2,RLEN                                                          
         BH    *+8                                                              
         STH   R2,RLEN                                                          
*                                                                               
         LHI   R2,X'FF'         PREPARE FOR TRE(SET TEST VALUE=X'FF')           
         LAY   RE,R                TARGET                                       
         LH    RF,RLEN             LENGTH                                       
         LAY   R3,TRTAB                                                         
         TRE   RE,R3               TRANSLATE OUT UNPRINTABLES                   
         BO    *-4                      (NOTE: PRTL CC CNVT TO ' ')             
*                                                                               
GNLDOK   SAM24                                                                  
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R4)                                                        
*                                                                               
GNLDBAD  SAM24                     PAST END OF BUFFER                           
         B     EXITL                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
TRTAB    DS    0XL256                                                           
         DC    X'40404040404040404040404040404040' 00-0F                        
         DC    X'40404040404040404040404040404040' 10-1F                        
         DC    X'40404040404040404040404040404040' 20-2F                        
         DC    X'40404040404040404040404040404040' 30-3F                        
         DC    X'404040404040404040404A4B4C4D4E4F' 40-4F                        
         DC    X'504040404040404040405A5B5C5D5E5F' 50-5F                        
         DC    X'606140404040404040406A6B6C6D6E6F' 60-6F                        
         DC    X'404040404040404040797A7B7C7D7E7F' 70-7F                        
         DC    X'40818283848586878889404040404040' 80-8F                        
         DC    X'4091929394959697989940404040409F' 90-9F                        
         DC    X'40A1A2A3A4A5A6A7A8A9404040404040' A0-AF                        
         DC    X'40404040404040404040404040404040' B0-BF                        
         DC    X'C0C1C2C3C4C5C6C7C8C9404040404040' C0-CF                        
         DC    X'D0D1D2D3D4D5D6D7D8D9404040404040' D0-DF                        
         DC    X'E040E2E3E4E5E6E7E8E9404040404040' E0-EF                        
         DC    X'F0F1F2F3F4F5F6F7F8F9404040404040' F0-FF                        
         EJECT                                                                  
***********************************************************************         
* MISSING *HDR RECORD, PRINT WARNING MESSAGE                          *         
***********************************************************************         
         SPACE 1                                                                
NOHDR    NTR1  BASE=*,LABEL=*                                                   
         MVC   HDRMUID,USERID                                                   
         MVC   HDRMSUB,RPTSUBID                                                 
*                                                                               
         GOTO1 VHEXIN,DMCB,RPTPQNUM,HALF,4,0                                    
         SR    R0,R0                                                            
         ICM   R0,3,HALF                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  HDRMNUM,DUB                                                      
*                                                                               
         MVC   PLINE(HDRMISSQ),HDRMISS                                          
         BRAS  RE,PRNT                                                          
         B     EXIT                CONTINUE                                     
*                                                                               
HDRMISS  DC    C'Missing Header, skip report: '                                 
HDRMUID  DS    CL8                                                              
         DC    C','                                                             
HDRMSUB  DS    CL3                                                              
         DC    C','                                                             
HDRMNUM  DS    CL5                                                              
HDRMISSQ EQU   *-HDRMISS                                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS *HDR* CARD - THIS SHOULD BE THE FIRST CARD                  *         
***********************************************************************         
         SPACE 1                                                                
PRCHDR   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'*HDR*',R+5       IS THIS THE HEADER                           
         BNE   EXITL               NO - LOOP UNTIL YOU GET IT                   
         MVI   GOTHDR,C'Y'                                                      
*                                                                               
         LH    R1,RLEN             SAVE HEADER STRIPPING OFF THE CC             
         AHI   R1,-1                                                            
         STH   R1,HDRLEN                                                        
         LA    R0,HDR                                                           
         LA    RE,R+1                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
PCHRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS ++DDS CARD(S)                                               *         
***********************************************************************         
         SPACE 1                                                                
PRCDDS   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ALINETAB         NOW DEAL WITH OTHER CONTROL CARDS            
         USING LINETABD,R2         THESE SHOULD BE SINGLETONS                   
*                                                                               
PDDS20   CLC   LINEID,R+12         MATCH CONTROL CARD?                          
         BE    PDDS40              YES                                          
         AHI   R2,LINETABL                                                      
         CLI   LINEID,X'FF'        EOT?                                         
         BNE   PDDS20              NO - NEXT CARD                               
*                                                                               
         MVI   FERN,20             UNKNOWN ++DDS CARD                           
         BRAS  RE,ERRMSG                                                        
         MVC   PLINE,R+1                                                        
         BRAS  RE,PRNT                                                          
         B     EXITL               IGNORE IT FOR NOW                            
*                                                                               
PDDS40   LH    R1,RLEN             MOVE FROM R  - STRIP OFF LEADING CC          
         AHI   R1,-1                                                            
         STH   R1,LINELEN                                                       
         LA    R0,LINEIN                                                        
         LA    RE,R+1                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     EXIT                GET NEXT CARD                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WAIT UNTIL POSTED OR A MESSAGE ARRIVES                   *         
***********************************************************************         
         SPACE 1                                                                
WAIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
WAIT02   XC    GETECB,GETECB       CLEAR SIGNAL ECB                             
         XC    MSGDESC_MSGID,MSGDESC_MSGID                                      
         XC    MSGDESC_CORRELID,MSGDESC_CORRELID                                
*                                                                               
         LHI   RF,MQGMO_SET_SIGNAL+MQGMO_ACCEPT_TRUNCATED_MSG                   
         ST    RF,GETOPTS_OPTIONS                                               
         LA    RF,GETECB           TELL IT TO SIGNAL AND SET ECB                
         ST    RF,GETOPTS_SIGNAL1                                               
         LHI   RF,MQWI_UNLIMITED                                                
         ST    RF,GETOPTS_WAITINTERVAL                                          
*                                                                               
         MVC   MQGETBUF,AMSGIN     MESSAGE                                      
         SAM31                                                                  
         L     RE,AMSGIN                                                        
         L     RF,MAXMSGLN         CLEAR BUFFER                                 
         XCEFL                                                                  
         SAM24                                                                  
*                                                                               
         LA    R2,MQGET                                                         
         BRAS  RE,CALLMQ           CALL MQ TO FLAG WHEN MESSAGE ARRIVES         
         BE    EXITOK              YOU DON'T NEED TO WAIT                       
         BH    WAIT04              NEED TO WAIT                                 
         DC    H'0'                ERROR ON GET                                 
*                                                                               
WAIT04   WAIT  1,ECBLIST=ECBLST                                                 
*                                                                               
         L     RF,AOPERECB                                                      
         TM    0(RF),X'40'         OPERATOR INTERRUPT                           
         BZ    *+8                                                              
         BRAS  RE,CHKOPER                                                       
         CLI   OPERSTOP,C'Y'       OPERATOR STOP REQUESTED?                     
         BE    EXITL               YES                                          
*                                                                               
         TM    GETECB,X'40'        MQ SIGNALS MESSAGE ARRIVED                   
         BO    WAIT02              YES - GO GET IT                              
*                                                                               
         B     WAIT04              UNKNOWN COMMAND, GO BACK TO WAIT             
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,1                                                             
         BRAS  RE,SYSMESS          STARTING INITIALISE                          
*                                                                               
         LR    R0,RC                                                            
         LHI   R1,WORKL                                                         
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR NBASE W/S                              
*                                                                               
         ZAP   LINE,P99                                                         
         MVC   TITLE,VCTITLE                                                    
*                                                                               
         BRAS  RE,GETCARDS         READ IN INPUT CARDS                          
         BL    EXITL                                                            
         BRAS  RE,VALCARDS         VALIDATE INPUT CARDS                         
         BL    EXITL                                                            
         BRAS  RE,OPENCTFL         OPEN CTFILE IF NOT DONE YET                  
*                                                                               
         ZAP   LINE,P99                                                         
         MVC   TITLE,DWTITLE                                                    
*                                                                               
         BRAS  RE,MQINIT                                                        
         BL    EXITL                                                            
         BRAS  RE,SETOPS           SET UP OPERATOR INPUT                        
         BL    EXITL                                                            
*                                                                               
         L     R0,MAXMSGLN         TWO BUFFERS REQUIRED INPUT + OUTPUT          
         SLL   R0,1                                                             
*                                                                               
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    INIT02                                                           
         LHI   R0,21                                                            
         BRAS  RE,SYSMESS          GETMAIN FAILED                               
         ABEND 911,DUMP                                                         
*                                                                               
INIT02   ST    R1,AMSGIN           A(INPUT BUFFER)                              
         A     R1,MAXMSGLN                                                      
         ST    R1,AMSGOUT          A(OUTPUT BUFFER)                             
*                                                                               
         BRAS  RE,BDFINIT          JVM PIPI/HFS INITIALIZATION                  
*                                                                               
         LHI   R0,2                                                             
         BRAS  RE,SYSMESS          COMPLETED INITIALISE                         
*                                                                               
         B     EXITOK                                                           
*                                                                               
VCTITLE  DC    CL(L'TITLE)'Input cards to MQ EDI BDF job'                       
DWTITLE  DC    CL(L'TITLE)'MQ EDI BDF send output log'                          
         EJECT                                                                  
***********************************************************************         
* INITIALISE JVM PIPI/HFS                                             *         
***********************************************************************         
         SPACE 1                                                                
BDFINIT  NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
         L     R1,16(,0)       CVT - COMMON VECTOR TABLE                        
         L     R1,544(R1)      CSRTABLE                                         
         L     R1,24(R1)       CSR SLOT                                         
         MVC   BPX1OPN,BPX1OPNQ(R1)     ADDRESS OF THE SERVICE BPX1OPN          
         MVC   BPX1CLO,BPX1CLOQ(R1)     ADDRESS OF THE SERVICE BPX1CLO          
         MVC   BPX1WRT,BPX1WRTQ(R1)     ADDRESS OF THE SERVICE BPX1WRT          
         MVC   BPX1RED,BPX1REDQ(R1)     ADDRESS OF THE SERVICE BPX1RED          
         SAM24                                                                  
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE MQ QUEUES                                                *         
* NOTE: IN A DELIBERATE ATTEMPT TO SIMPLIFY THE NAMES OF THE MQ       *         
*       QUEUE MANAGER, THE MASTER QUEUE AND LOG QUEUES ARE ALL HARD   *         
***********************************************************************         
         SPACE 1                                                                
MQINIT   NTR1  BASE=*,LABEL=*                                                   
         MVI   CONOK,C'N'                                                       
         LHI   R0,9                                                             
         BRAS  RE,SYSMESS          BEGINNING MQ INITIALISE                      
*                                                                               
         MVC   INPQ_OBJECTNAME,QINPUTA                                          
         MVC   LOGQ_OBJECTNAME,QLOGA                                            
         CLI   ETYPE,C'A'          ADV QUEUES                                   
         BE    MQINI50                                                          
*                                                                               
         CLI   ETYPE,C'R'          REP QUEUES                                   
         BNE   MQINI30                                                          
         MVC   INPQ_OBJECTNAME,QINPUTR                                          
         MVC   LOGQ_OBJECTNAME,QLOGR                                            
*                                                                               
MQINI30  CLI   ETYPE,C'Q'          FQA QUEUES                                   
         BNE   MQINI40                                                          
         MVC   INPQ_OBJECTNAME,QINPUTQ                                          
         MVC   LOGQ_OBJECTNAME,QLOGQ                                            
*                                                                               
MQINI40  CLI   ETYPE,C'T'          TEST QUEUES                                  
         BNE   MQINI50                                                          
         MVC   INPQ_OBJECTNAME,QINPUTT                                          
         MVC   LOGQ_OBJECTNAME,QLOGT                                            
*                                                                               
MQINI50  EQU   *                                                                
*                                                                               
         OC    INPUTQ,INPUTQ       ANY INPUT QUEUE GIVEN?                       
         BZ    *+10                                                             
         MVC   INPQ_OBJECTNAME,INPUTQ                                           
         OC    ELOGQ,ELOGQ           ANY EDICT LOG QUEUE GIVEN?                 
         BZ    *+10                                                             
         MVC   LOGQ_OBJECTNAME,ELOGQ                                            
*                                                                               
         MVC   PLINE(20),=CL20'Queue Manager: '                                 
         MVC   PLINE+21(L'QMGR),QMGR                                            
         BRAS  RE,PRNT                                                          
         MVC   PLINE(20),=CL20'Input Queue:'                                    
         MVC   PLINE+21(L'INPQ_OBJECTNAME),INPQ_OBJECTNAME                      
         BRAS  RE,PRNT                                                          
         MVC   PLINE(20),=CL20'Logging Queue:'                                  
         MVC   PLINE+21(L'LOGQ_OBJECTNAME),LOGQ_OBJECTNAME                      
         BRAS  RE,PRNT                                                          
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF EXTERNAL ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,AMQCT                                                         
         LOAD  DE=CSQBGET                                                       
         ST    R0,AMQGET                                                        
         LOAD  DE=CSQBBACK                                                      
         ST    R0,AMQBACK                                                       
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,AMQOPENQ                                                      
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,AMQCMIT                                                       
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,AMQCLOSE                                                      
         LOAD  DE=CSQBDISC                                                      
         ST    R0,AMQDISC                                                       
         LOAD  DE=CSQBPUT                                                       
         ST    R0,AMQPUT                                                        
*                                                                               
         LA    R2,MQCT                                                          
         BRAS  RE,CALLMQ           CONNECT TO MQ                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,11                                                            
         BRAS  RE,SYSMESS          CONNECTED TO MQ QMGR                         
*                                                                               
         LA    RF,INPQ             SET QUEUE OBJECT FOR INPUT QUEUE             
         ST    RF,MQOPNQNM                                                      
         LA    RF,INPQHOB          SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
         LHI   RF,MQOO_INPUT_AS_Q_DEF+MQOO_SAVE_ALL_CONTEXT                     
         ST    RF,OPN_OPTS                                                      
         LA    R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN MASTER QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,LOGQ             SET QUEUE OBJECT FOR LOG QUEUE               
         ST    RF,MQOPNQNM                                                      
         LA    RF,LOGQHOB          SET A(RETURN FOR HOBJ)                       
         ST    RF,MQOPNHOB                                                      
         LHI   RF,MQOO_OUTPUT+MQOO_PASS_ALL_CONTEXT                             
         ST    RF,OPN_OPTS                                                      
         LA    R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN OUTPUT QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,12                                                            
         BRAS  RE,SYSMESS          OPENED MASTER QUEUE                          
*                                                                               
         LHI   R0,10                                                            
         BRAS  RE,SYSMESS          COMPLETED MQ INITIALISE                      
         MVI   CONOK,C'Y'                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* QUEUE INFORMATION                                                   *         
***********************************************************************         
         SPACE 1                                                                
*                                 DEFAULT INPUT                                 
QINPUTA  DC    CL48'DDS.BDEFTP.ADV.LOCALQ'                                      
QINPUTR  DC    CL48'DDS.BDEFTP.REP.LOCALQ'                                      
QINPUTT  DC    CL48'DDS.BDEFTP.TST.LOCALQ'                                      
QINPUTQ  DC    CL48'DDS.BDEFTP.FQA.LOCALQ'                                      
*                                                                               
*                                  QUEUE FOR EDICT FILE STATUS UPDATE           
QLOGA    DC    CL48'MQ1P.BDEFTP.ADV.LOCALQ'                                     
QLOGR    DC    CL48'MQ1P.BDEFTP.REP.LOCALQ'                                     
QLOGQ    DC    CL48'MQ1P.BDEFTP.FQA.LOCALQ'                                     
QLOGT    DC    CL48'MQ1P.EZFAX.TEST.LOCALQ'                                     
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISCONNECT FROM MQ CLEANLY                               *         
***********************************************************************         
         SPACE 1                                                                
MQCLS    NTR1  BASE=*,LABEL=*                                                   
         CLI   CONOK,C'Y'                                                       
         BNE   EXITOK                                                           
*                                                                               
         LHI   R0,14                                                            
         BRAS  RE,SYSMESS          CLOSING QUEUES                               
*                                                                               
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LA    RF,INPQHOB                                                       
         ST    RF,MQCLSHOB                                                      
         LA    R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE INPUT QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LA    RF,LOGQHOB                                                       
         ST    RF,MQCLSHOB                                                      
         LA    R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           CLOSE LOG QUEUE                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,16                                                            
         BRAS  RE,SYSMESS          CLOSED MQ MASTER QUEUE                       
         LA    R2,MQDISC                                                        
         BRAS  RE,CALLMQ           DISCONNECT MQ QMGR                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,15                                                            
         BRAS  RE,SYSMESS          COMPLETED MQ DEALLOCATION                    
         MVI   CONOK,C'N'                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL MQ                                                  *         
* NTRY: R2          = A(PARAMETER STRUCTURE)                          *         
* EXIT: MQ_CC       = MQ COMPLETION CODE                              *         
*       MQ_RC       = MQ REASON CODE                                  *         
*       CC SET EQ     WHEN CALL OKAY                                  *         
***********************************************************************         
         SPACE 1                                                                
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
*                                                                               
         L     RF,16(R2)           RF = A(MQ ROUTINE)                           
         LA    R3,20(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         SAM24                                                                  
         CLC   MQ_CC,=A(MQCC_OK)                                                
         BE    CMQ02                                                            
*                                                                               
         LA    RF,MQGET            MQGET CAN HAVE NZ CC                         
         CR    R2,RF               IF SO THE FOLLOWING 2 FIELDS ARE SET         
         BNE   CMQ04                                                            
         CLC   MQ_CC,=A(MQCC_WARNING)                                           
         BNE   CMQ04                                                            
         CLC   MQ_RC,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                           
         BNE   CMQ04                                                            
*                                                                               
         CLI   TRACE,C'N'                                                       
         BE    EXITH                                                            
         MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(35),=CL35'WAITING FOR NEW MESSAGE'                      
         BRAS  RE,PRNT                                                          
         B     EXITH               FLAG THAT WE NEED TO WAIT                    
*                                                                               
CMQ02    CLI   TRACE,C'N'                                                       
         BE    EXITOK                                                           
         MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(12),=C'Completed ok'                                    
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
CMQ04    MVI   PLINE,C'+'                                                       
         MVC   PLINE+1(16),0(R2)                                                
         MVC   PLINE+20(09),=C'**ERROR**'                                       
         MVC   PLINE+30(08),=CL08'Warning '                                     
         CLC   MQ_CC,=A(MQCC_WARNING)                                           
         BE    CMQ06                                                            
         MVC   PLINE+30(08),=CL08'Failed  '                                     
         CLC   MQ_CC,=A(MQCC_FAILED)                                            
         BE    CMQ06                                                            
         MVC   PLINE+30(08),=CL08'Unknown '                                     
         EDIT  MQ_CC,(7,PLINE+38),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
CMQ06    MVC   PLINE+46(3),=C'RC='                                              
         EDIT  MQ_RC,(5,PLINE+49),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
         L     RF,AWHY                                                          
CMQ08    CLI   0(RF),X'FF'         SEE IF WE HAVE TEXT FOR THE PROBLEM          
         BE    CMQ10                                                            
         CLC   MQ_RC,0(RF)                                                      
         BE    *+12                                                             
         AHI   RF,28                                                            
         B     CMQ08                                                            
*                                                                               
         MVC   PLINE+60(24),4(RF)                                               
*                                                                               
CMQ10    BRAS  RE,PRNT                                                          
         CLC   MQ_RC,=A(MQRC_SIGNAL_OUTSTANDING)                                
         BE    EXITH               FORCE IT TO WAIT                             
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
         SPACE 1                                                                
SETOPS   NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,7                                                             
         BRAS  RE,SYSMESS          BEGINNING SETTING OPERATOR COMMS             
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         MVC   ECBLST,AOPERECB                                                  
*                                                                               
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
*                                                                               
         LHI   R0,8                                                             
         BRAS  RE,SYSMESS          COMPLETED SETTING OPERATOR COMMS             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ IN ALL INPUT CARDS                                             *         
* FORMAT INTO A SCANNER BLOCK AND SAVE FOR VALIDATION                 *         
***********************************************************************         
         SPACE 1                                                                
GETCARDS NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,3                                                             
         BRAS  RE,SYSMESS          STARTING READING INPUT CARDS                 
*                                                                               
         L     R3,ACARDSIN                                                      
GCD02    GOTO1 VCARDS,PLIST,CARDIO,=C'RE00'                                     
         MVC   PLINE(L'CARDIO),CARDIO                                           
         BRAS  RE,PRNT             ALWAYS PRINT THE LINE                        
         CLI   CARDIO,C'*'         IGNORE COMMENTS                              
         BE    GCD02                                                            
         CLC   =C'/*',CARDIO       END OF CARDS?                                
         BE    GCDX                YES                                          
*                                                                               
         CLI   0(R3),X'FF'         OVERFLOW                                     
         BNE   GCD04                                                            
         MVI   FERN,05             TOO MANY PARAMETERS                          
         BRAS  RE,ERRMSG                                                        
         B     EXITL                                                            
*                                                                               
GCD04    MVC   0(L'CARDIO,R3),CARDIO                                            
         AHI   R3,L'CARDIO                                                      
         B     GCD02               NEXT                                         
*                                                                               
GCDX     MVI   0(R3),X'FF'         FLAG END OF CARDS                            
         LHI   R0,4                                                             
         BRAS  RE,SYSMESS          COMPLETED READING INPUT CARDS                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS IN SCANNER BLOCK                           *         
***********************************************************************         
         SPACE 1                                                                
VALCARDS NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,5                                                             
         BRAS  RE,SYSMESS          STARTING VALIDATE INPUT CARDS                
         MVI   GOTERR,C'N'                                                      
*                                                                               
         L     R3,ACARDSIN                                                      
VCD02    CLI   0(R3),X'FF'                                                      
         BE    VCDX                                                             
*                                                                               
         L     R4,ACARDTAB         TABLE OF SUPPORTED PARAMETERS                
         USING CARDTABD,R4                                                      
VCD04    CLI   0(R4),255           END OF TABLE?                                
         BE    VCD08                                                            
         TM    CFLAG,CFBIG         BIG CARDS ARE NOT SCANNED                    
         BZ    VCD06                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,CMAX                                                          
         LR    R5,RF                                                            
         AR    R5,R3                                                            
         AHI   R5,2                R5=A(INPUT VALUE) AFTER =                    
*                                                                               
         EX    RF,*+8              MATCH TEXT STRING                            
         BE    VCD20                                                            
         CLC   CARDTXT(0),0(R3)                                                 
*                                                                               
VCD06    AHI   R4,CARDTABL         TRY NEXT ENTRY                               
         B     VCD04                                                            
*                                                                               
VCD08    GOTO1 VSCANNER,PLIST,(C'C',0(R3)),ASCANTAB,0                           
         XR    R2,R2                                                            
         ICM   R2,1,4(R1)          RF=NUMBER OF INPUT PARAMETERS                
         BNZ   VCD10                                                            
         MVC   PLINE(L'CARDIO),0(R3)                                            
         BRAS  RE,PRNT                                                          
         MVI   FERN,04                                                          
         BRAS  RE,ERRMSG                                                        
         B     VCD18                                                            
*                                                                               
VCD10    L     R5,ASCANTAB                                                      
         USING SCANBLKD,R5                                                      
*                                                                               
VCD12    XR    RF,RF               RECONSTRUCT INPUT PARAMETER                  
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),SC1STFLD                                                
         LA    RE,PLINE+1(RF)                                                   
*                                                                               
         CLI   SC2NDLEN,0          ANY SECOND HALF TO PARAMETER                 
         BE    VCD14                                                            
         MVI   0(RE),C'='                                                       
         MVC   1(L'SC2NDFLD,RE),SC2NDFLD                                        
*                                                                               
VCD14    BRAS  RE,PRNT             PRINT RECONSTRUCTED PARAMETER                
*                                                                               
         L     R4,ACARDTAB         TABLE OF SUPPORTED PARAMETERS                
         USING CARDTABD,R4                                                      
         XR    RF,RF                                                            
         IC    RF,SC1STLEN         LENGTH OF PARAMETER                          
         BCTR  RF,0                                                             
*                                                                               
VCD16    CLI   0(R4),255           END OF TABLE?                                
         BNE   *+16                                                             
         MVI   FERN,01             FLAG UNKNOWN PARAMETER CARD                  
         BRAS  RE,ERRMSG                                                        
         B     VCD22                                                            
*                                                                               
         CLM   RF,1,CMIN           CHECK LENGTHS ARE OK                         
         BL    VCD18                                                            
         CLM   RF,1,CMAX                                                        
         BH    VCD18                                                            
         EX    RF,*+8              MATCH TEXT STRING                            
         BE    VCD20                                                            
         CLC   SC1STFLD(0),CARDTXT                                              
*                                                                               
VCD18    AHI   R4,CARDTABL         TRY NEXT ENTRY                               
         B     VCD16                                                            
*                                                                               
VCD20    ICM   RF,15,CARDVAL       GO AND VALIDATE THIS INPUT                   
         BASR  RE,RF                                                            
         BE    *+8                                                              
         BRAS  RE,ERRMSG           PRINT ERROR MESSAGE                          
*                                                                               
         TM    CFLAG,CFBIG         BIG CARDS?                                   
         BO    VCD24               YES - GO TO NEXT LINE                        
*                                                                               
VCD22    AHI   R5,SCBLKLQ          NEXT LINE IN SCANTAB                         
         BCT   R2,VCD12            ANY MORE PARMS INPUT?                        
*                                                                               
VCD24    AHI   R3,L'CARDIO                                                      
         B     VCD02                                                            
*                                                                               
VCDX     ZAP   LINE,P99            FORCE PAGE THROW                             
         LHI   R0,6                                                             
         BRAS  RE,SYSMESS          ENDED VALIDATE INPUT CARDS                   
*                                                                               
         CLI   GOTERR,C'Y'         SET CC BASED ON ERRORS                       
         BNE   EXITOK                                                           
         B     EXITL                                                            
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DSPACE= CARD                                          *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R5                                                      
VCDSPACE NTR1  BASE=*,LABEL=*                                                   
         CLI   SC2NDLEN,1                                                       
         BE    *+12                                                             
         MVI   FERN,02                                                          
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,ASSB                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),SC2NDFLD                          
*                                                                               
         MVC   ETYPE,SSODSPAC-SSOOFF(RF)                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DDSIO= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCDDSIO  NTR1  BASE=*,LABEL=*                                                   
         CLI   SC2NDLEN,8                                                       
         BNH   *+12                                                             
         MVI   FERN,03                                                          
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,VDDSIO                                                     
         MVC   0(8,RF),SC2NDFLD                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DRTEST= CARD                                          *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCDR     NTR1  BASE=*,LABEL=*                                                   
         MVC   DRTEST,SC2NDFLD                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DEBUG= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCDEBUG  NTR1  BASE=*,LABEL=*                                                   
         MVC   DEBUG,SC2NDFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF TRACE= CARD                                           *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCTRACE  NTR1  BASE=*,LABEL=*                                                   
         MVC   TRACE,SC2NDFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF QMGR= OVERRIDE CARD                                   *         
* NTRY: R5     = A(INPUT VALUE)                                       *         
***********************************************************************         
         SPACE 1                                                                
VCQMGR   NTR1  BASE=*,LABEL=*                                                   
         MVC   QMGR,0(R5)                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF BDEQ= OVERRIDE CARD                                   *         
* NTRY: R5     = A(INPUT VALUE)                                       *         
***********************************************************************         
         SPACE 1                                                                
VCBDEQ   NTR1  BASE=*,LABEL=*                                                   
         MVC   BQUENAME,0(R5)                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF PATH= CARD                                            *         
* NTRY: R5     = A(INPUT VALUE)                                       *         
***********************************************************************         
         SPACE 1                                                                
VCPATH   NTR1  BASE=*,LABEL=*                                                   
         MVC   PATH,0(R5)                                                       
*                                                                               
         LA    RF,PATH                                                          
         LA    RE,L'PATH(RF)                                                    
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         B     *-10                                                             
         LA    RE,1(RE)                                                         
         SR    RE,RF                                                            
         ST    RE,PHLEN                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF EDICTTYPE=  CARD                                      *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCEDTYPE NTR1  BASE=*,LABEL=*                                                   
         MVC   EDICTTYP,SC2NDFLD                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF TYPE=CARD (DEFAULT IS 'Y' UNLESS YOU HAVE AN 'N')     *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCTYPE   NTR1  BASE=*,LABEL=*                                                   
         MVC   FACTYPE,SC2NDFLD                                                 
         CLI   FACTYPE,C'A'                                                     
         BE    EXITOK                                                           
         CLI   FACTYPE,C'R'                                                     
         BE    EXITOK                                                           
         CLI   FACTYPE,C'T'                                                     
         BE    EXITOK                                                           
         CLI   FACTYPE,C'Q'                                                     
         BE    EXITOK                                                           
         MVI   FERN,19                                                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF SENDERID= CARD  (1 CHAR)                              *         
* NTRY: R5     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
VCSNDRID NTR1  BASE=*,LABEL=*                                                   
         MVC   SENDERID,SC2NDFLD                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF INPUTQ= CARD  (CL48)                                  *         
* NTRY: R5     = A(INPUT VALUE)                                       *         
***********************************************************************         
         SPACE 1                                                                
VCINPQ   NTR1  BASE=*,LABEL=*                                                   
         MVC   INPUTQ,0(R5)                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF LOGQ= CARD  (CL48)                                    *         
* NTRY: R5     = A(INPUT VALUE)                                       *         
***********************************************************************         
         SPACE 1                                                                
VCLOGQ   NTR1  BASE=*,LABEL=*                                                   
         MVC   ELOGQ,0(R5)                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR MESSAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
ERRMSG   NTR1  BASE=*,LABEL=*                                                   
         MVI   GOTERR,C'Y'         SET GOT AN ERROR                             
*                                                                               
         LA    RF,ERRND                                                         
         CLI   FERN,0              UNDEFINED ERROR                              
         BE    ERRM02                                                           
*                                                                               
         XR    RF,RF               INDEX INTO ERROR TABLE                       
         IC    RF,FERN                                                          
         BCTR  RF,0                                                             
         MHI   RF,L'ERRMSGS                                                     
         A     RF,AERRMSG          RF = A(ERROR MESSAGE)                        
*                                                                               
ERRM02   MVC   PLINE,SPACESL                                                    
         MVC   PLINE(L'ERRHDR),ERRHDR                                           
         MVC   PLINE+L'ERRHDR(L'ERRMSGS),0(RF)                                  
         BRAS  RE,PRNT                                                          
*                                                                               
         MVI   FERN,0                                                           
         B     EXITOK                                                           
*                                                                               
ERRHDR   DC    C'@  *** ERROR *** '                                             
ERRND    DC    CL45'Improperly defined error'                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PLINE TO A PRINT LINE                             *         
***********************************************************************         
         SPACE 1                                                                
PRNT     NTR1  BASE=*,LABEL=*                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   P+00(2),PRNTTIME                                                 
         MVI   P+02,C':'                                                        
         MVC   P+03(2),PRNTTIME+2                                               
         MVI   P+05,C':'                                                        
         MVC   P+06(2),PRNTTIME+4                                               
         MVI   P+08,C'.'                                                        
         MVC   P+09(2),PRNTTIME+6                                               
         MVC   P+12(L'PLINE),PLINE                                              
         MVC   PLINE,SPACESL                                                    
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
*                                                                               
PRNTDUB  DS    D                   FOR PRNT ROUTINE ONLY                        
PRNTTIME DS    CL9                 FOR PRNT ROUTINE ONLY                        
         EJECT                                                                  
***********************************************************************         
* PUT A MESSAGE TO THE OPERATOR CONSOLE AND OPTIONALLY GET A REPLY.   *         
* NTRY: R0        = MESSAGE NUMBER                                    *         
*       XTRAMESS  = OPTIONAL CL8 TO BE INSERTED INTO MESSAGE          *         
*       XTRAMES2  = OPTIONAL CL16 TO BE INSERTED INTO MESSAGE         *         
*       R0 ==  0  = SPECIAL MESSAGE AT 0(R1)                          *         
*                                                                     *         
* 1. IF THE FIRST CHARACTER OF THE MESSAGE IS AN 'X' JUST PUT OUT THE *         
* MESSAGE                                                             *         
*                                                                     *         
* 2. IF IT IS A NUMBER THIS IS THE # OF CHARACTERS FOR THE RESPONSE   *         
* RESPONSE IS RETURNED IN 'REPLY' - "DUMP" AND "EOJ" HANDLED IN HERE  *         
*                                                                     *         
* 3. ELSE THIS IS A MULTILINE MESSAGE AND IT LOOPS UNTIL (1) OR (2)   *         
***********************************************************************         
         SPACE 1                                                                
SYSMESS  NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         AHI   R0,-1                                                            
         BM    SYM02               R0 == 0 MEANS R1=A(MESSAGE)                  
*                                                                               
         MHI   R0,L'SYSMSGS                                                     
         L     R2,ASYSMSGS                                                      
         AR    R2,R0               R2=A(SYSMSGS ENTRY)                          
*                                                                               
SYM02    MVC   MESSAGE,SPACESL     BUILD MESSAGE                                
         MVC   MESSAGE(8),=CL08'EDPQSCAN'                                       
         MVC   MESSAGE+9(L'SYSMSGS-1),1(R2)                                     
*                                                                               
         LA    R0,MESSAGE          NOW REPLACE SUBSTITUTE CHARS                 
         LA    R1,MESSAGE+L'MESSAGE-1                                           
SYM04    CR    R0,R1                                                            
         BE    SYM10                                                            
         CLC   0(18,R1),=18C'X'                                                 
         BE    SYM08                                                            
         CLC   0(8,R1),=18C'X'                                                  
         BE    SYM06                                                            
         BCT   R1,SYM04                                                         
         DC    H'0'                                                             
*                                                                               
SYM06    MVC   0(8,R1),XTRAMESS                                                 
         B     SYM10                                                            
*                                                                               
SYM08    MVC   0(18,R1),XTRAMES2                                                
         B     SYM10                                                            
*                                                                               
SYM10    CLI   0(R2),C'0'                                                       
         BH    SYM12                                                            
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'MESSAGE,MESSAGE)                      
         CLI   0(R2),C'X'                                                       
         BE    SYM16                                                            
         AHI   R2,L'SYSMSGS        SPECIAL MULTILINE MESSAGE                    
         B     SYM10                                                            
*                                                                               
SYM12    GOTO1 VLOGIO,DMCB,1,(L'MESSAGE,MESSAGE)                                
                                                                                
         XR    R0,R0                                                            
         ICM   R0,1,0(R2)                                                       
         N     R0,=X'0000000F'                                                  
         GOTO1 VLOGIO,DMCB,0,((R0),REPLY)                                       
*                                                                               
         CLC   REPLY(4),=C'DUMP'   CHECK FOR DUMP REPLY                         
         BNE   SYM14                                                            
         ABEND 666,DUMP                                                         
*                                                                               
SYM14    CLC   REPLY(3),=C'EOJ'    CHECK FOR EOJ REPLY                          
         BNE   SYM16                                                            
         ABEND 666                                                              
*                                                                               
SYM16    MVC   XTRAMESS,SPACESL    CLEAR THESE OUT                              
         MVC   XTRAMES2,SPACESL                                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OPEN CONTROL FILE IF REQUIRED                            *         
***********************************************************************         
         SPACE 1                                                                
OPENCTFL NTR1  BASE=*,LABEL=*                                                   
         CLI   CTOPEN,C'Y'                                                      
         BE    EXITOK                                                           
         MVI   CTOPEN,C'Y'                                                      
         GOTO1 VDMGR,DMCB,DMOPEN,CONTROL,CTFLIST,AIO,0                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'       *         
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.      *         
***********************************************************************         
         SPACE 1                                                                
CHKOPER  NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,19                                                            
         BRAS  RE,SYSMESS          OPERATOR COMMAND MESSAGE                     
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CH02                                                             
         MVI   OPERSTOP,C'Y'       YES -- SET STOP FLAG                         
         LHI   R0,20                                                            
         BRAS  RE,SYSMESS          OUTPUT OPERATOR STOP MESSAGE                 
         B     CHX                                                              
*                                                                               
CH02     CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT DID THE OPERATOR DO?                    
*                                                                               
         LA    R4,COMMTAB                                                       
         USING COMMTABD,R4                                                      
         XR    RF,RF                                                            
CH04     CLI   0(R4),X'FF'         EOT                                          
         BE    CHBAD               BAD COMMAND                                  
*                                                                               
         IC    RF,COMMLEN          GET MINIMUM LENGTH                           
         CH    RF,CIBDATLN         CHECK STRING LENGTH                          
         BL    CH06                                                             
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8              MATCH COMMAND IN TABLE                       
         BE    CH08                PASS                                         
         CLC   COMMCMD(0),CIBDATA                                               
*                                                                               
CH06     AHI   R4,COMMTABL         NEXT ENTRY                                   
         B     CH04                                                             
*                                                                               
CH08     ICM   RF,15,COMMRTN       GET PROCESSING ROUTINE                       
         BASR  RE,RF                                                            
         B     CHX                                                              
*                                                                               
CHBAD    LHI   R0,21                                                            
         BRAS  RE,SYSMESS          OUTPUT UNKNOWN COMMAND MESSAGE               
*                                                                               
CHX      L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
COMMTAB  DC    CL8'????????',AL1(8,0,0,0),AL4(OPDUMMY)                          
         DC    X'FF'                                                            
*                                                                               
OPDUMMY  BR    RE                                                               
*                                                                               
COMMTABD DSECT                     COVERS COMMTAB ABOVE                         
COMMCMD  DS    CL8                 INPUT COMMAND                                
COMMLEN  DS    X                   MINIMUM LENGTH                               
         DS    XL3                                                              
COMMRTN  DS    AL4                 A(PROCESSING ROUTINE)                        
COMMTABL EQU   *-COMMTABD                                                       
*                                                                               
EDIBDF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SENT MQ MESSAGE TO LOG FOR THIS DESTINATION                         *         
***********************************************************************         
         SPACE 1                                                                
POSTSENT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ALINETAB                                                      
         USING LINETAB,R5                                                       
*                                                                               
         XC    WORK,WORK           USE WORK+WORK2 FOR STATUS MESSAGE            
         XC    WORK2,WORK2                                                      
         LA    R3,WORK                                                          
*                                                                               
         MVC   0(3,R3),=C'ADD'                                                  
         CLI   ACTION,ACTJNKQ      MARK REPORT UNSENDABLE                       
         BNE   *+10                YES                                          
         MVC   0(3,R3),=C'JNK'                                                  
         AHI   R3,3                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         BRAS  RE,BLDCREF          REFERENCE NUMBER RETURNS IN CUSTREF          
*                                                                               
         MVC   0(L'CUSTREF,R3),CUSTREF                                          
         AHI   R3,L'CUSTREF                                                     
         BRAS  RE,ADDCRLF                                                       
*                                  SET METH=BDF (P)                             
         MVI   0(R3),EDIBDFQ                                                    
         AHI   R3,1                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(9,R3),FLNAME      FILE NAME                                    
         AHI   R3,9                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVI   0(R3),EDFDSTEQ      DESTINATION TYPE EDICT= KEY                  
         AHI   R3,1                                                             
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(80,R3),HDR        WE WANT ALL THE *HDR* CARD                   
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(80,R3),TRN        WE WANT ALL THE ++TRN CARD                   
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         OC    MQNLEN,MQNLEN                                                    
         BZ    *+14                                                             
         MVC   0(80,R3),MQN        WE WANT ALL THE ++MQN CARD IF THERE          
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         OC    PQSLEN,PQSLEN                                                    
         BZ    *+14                                                             
         MVC   0(80,R3),PQS        WE WANT ALL THE ++PQS CARD IF THERE          
         AHI   R3,80                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(25,R3),HDR+9      DESTINATION                                  
         AHI   R3,25                                                            
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(16,R3),HDR+9      DESTINATION                                  
         AHI   R3,16                                                            
         BRAS  RE,ADDCRLF          NO FORMATED DEST                             
*                                                                               
         MVC   0(L'RPTPQTYP,R3),RPTPQTYP                                        
         AHI   R3,L'RPTPQTYP                                                    
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   0(L'SENDERID,R3),SENDERID    BDE SENDER ID                       
         AHI   R3,L'SENDERID                                                    
         BRAS  RE,ADDCRLF                                                       
*                                                                               
         MVC   MQPUTQ,=A(LOGQHOB)                                               
         LA    RF,WORK                                                          
         ST    RF,MQPUTBUF                                                      
         SR    R3,RF                                                            
         ST    R3,DATALEN                                                       
*                                                                               
         LA    R2,MQPUT                                                         
         BRAS  RE,CALLMQ                                                        
*                                                                               
         LA    R2,MQCMIT           MQ COMMIT ALL QUEUES                         
         BRAS  RE,CALLMQ             includes queue for BDESND                  
*                                                                               
         XC    ACTION,ACTION                                                    
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET DATA FROM EDICT RECORD FOR RECEIVER (EDICT=??????)              *         
***********************************************************************         
         SPACE 1                                                                
FINDDEST NTR1  BASE=*,LABEL=*                                                   
         LA    R5,KEY                                                           
         USING EDIKEYD,R5                                                       
         XC    EDIKEY,EDIKEY       CLEAR KEY                                    
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
         MVC   EDINAME,HDR+15      USER ALPHA                                   
         GOTO1 VDMGR,DMCB,DMRDHI,CTFILE,(R5),AIO,0                              
*                                                                               
         L     R5,AIO                                                           
         CLC   EDIKEY,KEY                                                       
         BNE   EXITL                                                            
         DROP  R5                                                               
*                                                                               
         L     R4,AIO                                                           
         AHI   R4,28                                                            
         XR    RF,RF                                                            
         USING EDILNKD,R4                                                       
*                                                                               
FD20     CLI   EDILNKEL,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   EDILNKEL,EDILNKEQ                                                
         BE    *+12                                                             
         IC    RF,EDILNKLN                                                      
         BXH   R4,RF,FD20                                                       
*                                                                               
         MVC   BDECN,EDIBDECN      BDE COMMAN NAME                              
         MVC   BDEOP,EDIBDEOP      BDE RECEIVER'S OPERATING SYSTEM              
         MVC   BDEEN,EDIBDEEN      BDE ENCRYPTION (NONE,BLOWFISH,3DES)          
         MVC   BDECM,EDIBDECM      BDE COMPRESS (Y/N)                           
         MVC   BDESF,EDIBDESF      BDE DELETE SENT FILE (Y/N)                   
         MVC   BDECA,EDIBDECA      BDE CONVERT TO ASCII (Y/N)                   
         MVC   BDECP,EDIBDECP      BDE CODE PAGE                                
         MVC   BDEFN,EDIBDEFN      BDE FAILURE NOTIFICATION EMAIL ADDR          
         MVC   BDEBI,EDIBDEBI      BDE BINARY DATA TRANSFER (Y/N)               
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ALL SAVED ++DDS CONTROL CARDS                               *         
***********************************************************************         
         SPACE 1                                                                
PRCDDSC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ALINETAB                                                      
         USING LINETAB,R5                                                       
*                                                                               
         MVC   SUBJECT,SUB+15      SUBJECT OF FILE                              
         LA    RE,L'SUBJECT                                                     
PDC10    CLI   SUBJECT,C'#'        1ST CHAR OF SUB CAN'T BE #                   
         BE    PDC20                                                            
         CLI   SUBJECT,C' '        REMOVE LEADING SPACES                        
         BNE   PDC20X                                                           
         MVC   SUBJECT(L'SUBJECT-1),SUBJECT+1                                   
         MVI   SUBJECT+L'SUBJECT-1,C' '                                         
         BCT   RE,PDC10                                                         
PDC20    MVI   SUBJECT,C'%'                                                     
PDC20X   EQU   *                                                                
*                                                                               
         MVC   FLNAME,FIL+15       FILENAME                                     
*                                                                               
         MVC   FLEXT,EXT+15        FILE TYPE EXTENSION                          
*                                                                               
         CLC   EXT,SPACESL         ANY EXT GIVEN?                               
         BNE   PDC40               YES                                          
         CLI   TRN+9,C'A'          NO - USE TRN+9(3) FOR EXT IF ALL CHR         
         BL    PDC40                                                            
         CLI   TRN+10,C'A'                                                      
         BL    PDC40                                                            
         CLI   TRN+11,C'A'                                                      
         BL    PDC40                                                            
         MVC   FLEXT(3),TRN+9      SAVE IT FOR DEFAULT FILE EXT                 
PDC40    EQU   *                                                                
*                                                                               
         MVC   DSN,DSN_C+15                                                     
*                                                                               
         CLC   FLNAME,SPACESL      ANY FILENAME?                                
         BH    *+8                 YES                                          
         BRAS  RE,BLDDFLN          NO - BUILD DEFAULT FILENAME W/O EXT          
*                                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD DEFAULT FILENAME W/O EXT                                      *         
* NTRY: HDR, USERIDNO, CUSTREF                                        *         
* EXIT: FLNAME = DEFAULT FILENAME                                     *         
***********************************************************************         
         SPACE 1                                                                
BLDDFLN  NTR1  BASE=*,LABEL=*                                                   
         MVC   DFFEDX,ETYPE        A/R/T/Q                                      
*                                                                               
         MVC   DFFEDKEY,HDR+15                                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,USERIDNO       USERID #                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DFFUIDN,DUB                                                      
*                                                                               
         MVC   DFFREFN,CUSTREF+9                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(11,DFFDATE)                                  
         MVI   DFFDATE+5,C'-'                                                   
*                                                                               
         MVC   FLNAME,DFFILN                                                    
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CREATE UNIQUE REFERENCE NUMBER FOR ETI FILE                         *         
* EXIT: CUSTREF   = REFERENCE NUMBER                                  *         
***********************************************************************         
         SPACE 1                                                                
BLDCREF  NTR1  BASE=*,LABEL=*                                                   
         MVC   CUSTREF,SPACESL                                                  
         LA    R2,CUSTREF                                                       
         MVI   0(R2),C'1'          VERSION 1                                    
         MVC   1(1,R2),FACTYPE     ADV/REP                                      
         CLI   DRTEST,C'Y'                                                      
         BNE   *+8                                                              
         MVI   0(R2),C'D'          DR                                           
         AHI   R2,2                                                             
*                                                                               
         MVC   0(L'USERIDH,R2),USERIDH                                          
         AHI   R2,L'USERIDH                                                     
         MVC   0(L'RPTSUBID,R2),RPTSUBID                                        
         AHI   R2,L'RPTSUBID                                                    
         MVC   0(L'RPTPQNUM,R2),RPTPQNUM                                        
         AHI   R2,L'RPTPQNUM                                                    
         MVC   0(L'RPTHTIME,R2),RPTHTIME                                        
         AHI   R2,L'RPTHTIME                                                    
         MVC   0(L'RPTLOGNO,R2),RPTLOGNO                                        
         AHI   R2,L'RPTLOGNO                                                    
*                                                                               
         MVI   RPTLDSTS,X'01'                                                   
         OI    RPTLDSTS,X'80'      FLAG AS LAST DESTINATION                     
*                                                                               
         GOTO1 VHEXOUT,DMCB,RPTLDSTS,0(R2),L'RPTLDSTS                           
         AHI   R2,L'RPTLDSTS*2                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* COMMON STORAGE/ROUTINES                                             *         
***********************************************************************         
         SPACE 1                                                                
         ORG   EDIBDF+(((*-EDIBDF)/4096)+1)*4096                                
COMMON   DC    CL8'*COMMON*'                                                    
SAVERD   DC    F'0'                                                             
*                                                                               
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
EXITR3   CR    RB,RB                                                            
         XIT1  REGS=(R3)                                                        
*                                                                               
XBASE    L     RD,SAVERD                                                        
         XBASE ,                                                                
*                                                                               
ADDCRLF  MVC   0(L'CRLF,R3),CRLF                                                
         AHI   R3,L'CRLF                                                        
         BR    RE                                                               
*                                                                               
CRLF     DC    X'0D25'             EBCDIC <CRLF>                                
*                                                                               
LINESUP  NTR1  ,                   INCREMENT LINE COUNT AND LOG IF REQ          
         L     R0,NUMLINES                                                      
         AHI   R0,1                                                             
         ST    R0,NUMLINES                                                      
*                                                                               
         CLI   TRACE,C'F'          FULL TRACE PRINTS ALL LINES                  
         BNE   EXITOK                                                           
         ICM   RF,15,LASTOUT                                                    
         BNZ   *+8                                                              
         L     RF,AMSGOUT                                                       
         ST    R3,LASTOUT                                                       
*                                                                               
         LR    R1,R3                                                            
         SR    R1,RF                                                            
         BM    EXITOK                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),0(RF)                                                   
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
*                                                                               
*        DDSFTP-EDICTX-EDICTKEY-UID00000-REFNUM##-MMMDD-YY                      
DFFILN   DS    0CL60               DEFAULT FILENAME W/O EXT                     
         DC    CL6'DDSFTP'                                                      
         DC    C'-'                                                             
         DC    CL5'EDICT'                                                       
DFFEDX   DC    C'X'                                                             
         DC    C'-'                                                             
DFFEDKEY DC    CL8'EDICTKEY'                                                    
         DC    C'-'                                                             
         DC    CL3'UID'                                                         
DFFUIDN  DC    CL5'00000'                                                       
         DC    C'-'                                                             
         DC    C'REF'                                                           
DFFREFN  DC    CL5'00000'                                                       
         DC    C'-'                                                             
DFFDATE  DC    CL8'MMMDD-YY'                                                    
         DC    (*-DFFILN)C' '                                                   
*                                                                               
         SPACE 1                                                                
ARZERO   DC    16F'0'                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'HCONN==>'                                                    
HCONN    DC    F'0',F'0'           MQ QMGR CONNECTION HANDLE                    
*                                                                               
         DC    CL8'QMGR===>'                                                    
QMGR     DC    CL48'MQ1P'                                                       
*                                                                               
         DC    CL8'MSGBUFFS'                                                    
AMSGIN   DC    A(0)                INPUT MESSAGE BUFFER                         
AMSGOUT  DC    A(0)                OUTPUT MESSAGE BUFFER                        
MAXMSGLN DC    A(1024*1024)         MAX MESSAGE LENGTH                          
*                                                                               
         LTORG                                                                  
*                                                                               
TRACE    DC    C'N'                                                             
GOTHDR   DC    C'N'                                                             
DRTEST   DC    C'N'                                                             
DEBUG    DC    C'N'                                                             
FACTYPE  DC    CL3'ADV'            USED ONLY FOR BUILDING CUSTREF               
EDICTTYP DC    C' '                2ND CHAR FOR EDICT TYPE                      
*                                                                               
VCPRINT  DC    V(CPRINT)                                                        
VCARDS   DC    V(CARDS)                                                         
VDDSIO   DC    V(DDSIO)                                                         
VDMGR    DC    V(DATAMGR)                                                       
VDADDS   DC    V(DADDS)                                                         
VDATCON  DC    V(DATCON)                                                        
VHELLO   DC    V(HELLO)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VLOGIO   DC    V(LOGIO)                                                         
VNUMVAL  DC    V(NUMVAL)                                                        
VSCANNER DC    V(SCANNER)                                                       
*                                                                               
ASYSMSGS DC    A(SYSMSGS)                                                       
ALINETAB DC    A(LINETAB)                                                       
ASCANTAB DC    A(SCANTAB)                                                       
AERRMSG  DC    A(ERRMSGS)                                                       
ACARDTAB DC    A(CARDTAB)                                                       
ACARDSIN DC    A(CARDSIN)                                                       
*                                                                               
AWHY     DC    A(MQ_REASON_CODE_TABLE)                                          
ASSB     DC    A(SSB)                                                           
AUTL     DC    A(UTL)                                                           
AIO      DC    A(IO)                                                            
*                                                                               
LASTOUT  DC    A(0)                                                             
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
ACOMM    DC    A(0)                A(COMMUNICATIONS PARAMETER LIST)             
*                                                                               
MESSAGE  DS    CL60                                                             
XTRAMESS DS    XL8'00'                                                          
XTRAMES2 DS    XL18'00'                                                         
*                                                                               
DMOPEN   DC    CL8'DMOPEN  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
GENDIR   DC    CL8'GENDIR  '                                                    
GENFIL   DC    CL8'GENFIL  '                                                    
*                                                                               
CTOPEN   DC    C'N'                                                             
CONTROL  DC    CL8'CONTROL '                                                    
CTFLIST  DC    C'NCTFILE NGENDIR NGENFIL X'                                     
*                                                                               
P99      DC    P'99'                                                            
*                                                                               
         DS    0D                                                               
         DC    CL08'ECBLIST*'                                                   
ECBLST   DC    A(0)                A(OPERATOR ECB IS STORED HERE)               
         DC    X'00',AL3(GETECB)                                                
         DC    X'80',AL3(TIMERECB) TIMER POP                                    
         DC    CL08'ECBLISTX'                                                   
*                                                                               
GETECB   DC    A(0),CL12'<===GETECB'                                            
TIMERECB DC    A(0),CL12'<===TIMERECB'                                          
*                                                                               
         DC    CL8'HDRLEN=>'                                                    
HDRLEN   DS    H                                                                
HDR      DS    XL256                                                            
*                                                                               
STARS    DC    80C'*'                                                           
SPACESL  DC    256C' '                                                          
*                                                                               
*&&DO                                                                           
ERRMSG1  DS    0C                                                               
         DC    C'AUTONOTE*YYUNN:'                                               
ERRMSG1R DS    CL(L'JUNKRPT+L'DSTAEDST)                                         
ERRMSG1Q EQU   *-ERRMSG1                                                        
*&&                                                                             
*                                                                               
RNEXT    DS    A                                                                
         DC    CL8'RLEN===>'                                                    
RLEN     DS    H                                                                
R        DS    XL1000                                                           
         EJECT                                                                  
***********************************************************************         
* HFS, PIPI LITERALS                                                  *         
***********************************************************************         
         SPACE 2                                                                
BPX1OPNQ EQU   156                                                              
BPX1CLOQ EQU   72                                                               
BPX1REDQ EQU   176                                                              
BPX1WRTQ EQU   220                                                              
BPX1OPN  DS    A                                                                
BPX1CLO  DS    A                                                                
BPX1WRT  DS    A                                                                
BPX1RED  DS    A                                                                
*                                                                               
CMDFILE  DS    CL60                                                             
DATAFILE DS    CL60                                                             
         DC    CL8'*PATH*'                                                      
PATH     DS    CL60                                                             
RCFILE   DS    CL60                RETURN CODE FILE NAME                        
BUFFALET DC    F'0'                                                             
CFLEN    DS    F                                                                
DFLEN    DS    F                                                                
PHLEN    DS    F                                                                
RCFLEN   DS    F                                                                
SOFLEN   DS    F                                                                
BUFADDR  DS    A                                                                
BUFLEN   DS    F                                                                
ALET     DS    F                                                                
PLIST    DS    13A              CALL PARMLIST WORK AREA                         
RETCODE  DS    F                RETURN CODE (ERRNO)                             
RSNCODE  DS    F                REASON CODE                                     
RETVAL   DS    F                RETURN VALUE (0, -1 OR OTHER)                   
FILEDESC DS    F                FILE DESCRIPTOR                                 
         BPXYFTYP DSECT=NO                                                      
         BPXYOPNF DSECT=NO                                                      
         BPXYMODE DSECT=NO                                                      
         EJECT                                                                  
*                                                                               
TABLNTRY DS    XL(XMTTBLQ)         SAVED XMIT TABLE KEY                         
OPERSTOP DC    C'N'                'Y' IF OPERATOR WANTS TO STOP                
HFSSPCOK DC    C'Y'                'Y' IF HFS SPACE IS OKAY                     
TRACEFLG DS    C                   'Y' IF DETAILED TRACE WANTED                 
*                                                                               
FLNAME   DS    CL60                                                             
FLEXT    DS    CL10                                                             
FNLEN    DS    F                                                                
FILENAME DS    CL71                                                             
SUBJECT  DS    CL60                                                             
DSN      DS    CL60                DSN TO BE SENT INSTEAD OF PQ REPORT          
*                                                                               
*                                                                               
BDECBLK  DC    (BSNDDQ)X'00'                                                    
*                                                                               
         DC    C'*BDEPARM*'                                                     
CR00     DC    X'00',C'COMMONNAME='                                             
CR00P1   DC    CL60' '                             COMMAN NAME PARM             
CR01     DC    X'00',C'SUBJECT='                                                
CR01P1   DC    CL60' ',C' '                        SUBJECT PARM                 
CR01P2   DC    CL27' '                             REFERENCE # PARM             
CR02     DC    X'00',C'INPUT FILE='                                             
CR02P1   DC    CL60' '                             INPUT FILE                   
CR03     DC    X'00',C'FILENAME='                                               
CR03P1   DC    CL80' '                             FILENAME PARM                
CR04     DC    X'00',C'PRECOMPRESS='                                            
CR04P1   DC    CL60' '                             PRECOMPRESS PARM             
CR05     DC    X'00',C'DELSENTFILES='                                           
CR05P1   DC    CL60' '                             DELSENTFILES PARM            
CR06     DC    X'00',C'ENCRYPTION='                                             
CR06P1   DC    CL60' '                             ENCRYPTION  PARM             
CR07     EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         ORG   CR00                                                             
         DC    AL1(CR01-*-1)                                                    
         ORG   CR01                                                             
         DC    AL1(CR02-*-1)                                                    
         ORG   CR02                                                             
         DC    AL1(CR03-*-1)                                                    
         ORG   CR03                                                             
         DC    AL1(CR04-*-1)                                                    
         ORG   CR04                                                             
         DC    AL1(CR05-*-1)                                                    
         ORG   CR05                                                             
         DC    AL1(CR06-*-1)                                                    
         ORG   CR06                                                             
         DC    AL1(CR07-*-1)                                                    
         ORG                                                                    
         EJECT                                                                  
TRTABLE  DS    0XL256                                                           
*                                                                               
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'000102030405060708090A0B0C0D0E0F' 00-0F                        
         DC    X'101112131440161718191A1B1C1D1E1F' 10-1F                        
         DC    X'202122232425262728292A2B2C2D2E2F' 20-2F                        
         DC    X'303132333435363738393A3B3C3D3E3F' 30-3F                        
         DC    X'6D4142434445464748494A4B4C4D4E4F' 40-4F                        
         DC    X'505152535455565758595A5B5C5D5E5F' 50-5F                        
         DC    X'606D62636465666768696A6B6C6D6E6F' 60-6F                        
         DC    X'707172737475767778797A6C7C6D7E7F' 70-7F                        
         DC    X'808182838485868788898A8B8C8D8E8F' 80-8F                        
         DC    X'909192939495969798999A9B9C9D9E9F' 90-9F                        
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF' A0-AF                        
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF' B0-BF                        
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF' C0-CF                        
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF' D0-DF                        
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF' E0-EF                        
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF' F0-FF                        
         EJECT                                                                  
ASCIITBL DS    0XL256      EBCDIC TO ASCII TABLE (CODE PAGE 1252)               
*                          12/28/04 GIVEN FROM DAVID SCALESE                    
*                                                                               
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'00010203CF09D37FD4D5C30B0C0D0E0F' 00-0F                        
         DC    X'10111213C70A08C91819CCCD831DD21F' 10-1F                        
         DC    X'81821C84860A171B89919295A2050607' 20-2F                        
         DC    X'E0EE16E5D01EEA048AF6C6C21415C11A' 30-3F                        
         DC    X'2020E180EB909FE2AB8BA22E3C282B7C' 40-4F                        
         DC    X'26A95E9CDBA599E3A89E21242A293BAC' 50-5F                        
         DC    X'2D2FDFDC9ADDDE989DACA62C255F3E3F' 60-6F                        
         DC    X'D78894B0B1B2FCD6FB603A2340273D22' 70-7F                        
         DC    X'F861626364656667686996A4F3AFAEC5' 80-8F                        
         DC    X'8C6A6B6C6D6E6F7071729787CE93F180' 90-9F                        
         DC    X'C87E737475767778797AEFC0DA5BF2F9' A0-AF                        
         DC    X'B5B6FDB7B8B9E6BBBCBD5BD9BF5DA8C4' B0-BF                        
         DC    X'7B414243444546474849A8A8BEA8A8A8' C0-CF                        
         DC    X'7D4A4B4C4D4E4F505152A1AD81A8A38F' D0-DF                        
         DC    X'5C20535455565758595AA0858EA8A8D1' E0-EF                        
         DC    X'30313233343536373839B3A89AA8A7A8' F0-FF                        
         EJECT                                                                  
***********************************************************************         
* PARAMETER LISTS TO FACILITATE MQ CALLS                              *         
* CL16 EBCDIC  ROUTINE NAME                                           *         
* A    A(ROUTINE)                                                     *         
* PARAMETERS (STANDARD IBM FORMAT)                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
MQCT     DC    CL16'MQ QMGR connect'                                            
AMQCT    DC    A(0)                                                             
         DC    A(QMGR)                                                          
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQOPENQ  DC    CL16'MQ Open queue'                                              
AMQOPENQ DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQOPNQNM DC    A(0)                CMQODA                                       
         DC    A(OPN_OPTS)                                                      
MQOPNHOB DC    A(0)                HANDLE TO OBJECT (RETURNED)                  
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQCMIT   DC    CL16'MQ Commit'                                                  
AMQCMIT  DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQCLOSE  DC    CL16'MQ Close Queue'                                             
AMQCLOSE DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQCLSHOB DC    A(0)                                                             
         DC    A(CLS_OPTS)                                                      
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQDISC   DC    CL16'MQ Disconnect'                                              
AMQDISC  DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQGET    DC    CL16'MQ Get'                                                     
AMQGET   DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(INPQHOB)          QUEUE HOB                                    
         DC    A(MSGDESC)                                                       
         DC    A(GETOPTS)                                                       
         DC    A(MAXMSGLN)         MAX BUFFER LENGTH                            
MQGETBUF DC    A(0)                BUFFER ADDRESS                               
         DC    A(DATALEN)                                                       
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQBACK   DC    CL16'MQ Back'                                                    
AMQBACK  DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQPUT    DC    CL16'MQ Put'                                                     
AMQPUT   DC    A(0)                                                             
         DC    A(HCONN)                                                         
MQPUTQ   DC    A(LOGQHOB)          QUEUE NAME                                   
         DC    A(MSGDESC)                                                       
         DC    A(PUTOPTS)                                                       
MQPUTLEN DC    A(DATALEN)                                                       
MQPUTBUF DC    A(0)                                                             
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
OPN_OPTS DS    F                   MQOPEN OPTIONS                               
CLS_OPTS DS    F                   MQCLOSE OPTIONS                              
MQ_CC    DS    F                   COMPLETION CODE                              
MQ_RC    DS    F                   QUALIFIES COMPLETION CODE                    
DATALEN  DS    F                   LENGTH OF THE MESSAGE                        
*                                                                               
         DS    0D                                                               
         DC    CL8'INPQ===>'                                                    
INPQHOB  DC    F'0',C'<==>'                                                     
INPQ     CMQODA  LIST=YES          OBJECT DESCRIPTOR                            
*                                                                               
         DS    0D                                                               
         DC    CL8'LOGQ===>'                                                    
LOGQHOB  DC    F'0',C'<==>'                                                     
LOGQ     CMQODA  LIST=YES          OBJECT DESCRIPTOR                            
*                                                                               
         DS    0D                                                               
         DC    CL8'MSGDESC>'                                                    
MSGDESC  CMQMDA  LIST=YES          MESSAGE DESCRIPTOR                           
*                                                                               
         DS    0D                                                               
         DC    CL8'GETOPTS>'                                                    
GETOPTS  CMQGMOA LIST=YES          GET MESSAGE OPTIONS                          
*                                                                               
         DS    0D                                                               
         DC    CL8'PUTOPTS>'                                                    
PUTOPTS  CMQPMOA DSECT=NO,LIST=YES PUT MESSAGE OPTIONS                          
         EJECT                                                                  
***********************************************************************         
* NON-ADDRESSIBLE STORAGE AREAS                                       *         
***********************************************************************         
         DS    0D                                                               
         DC    CL16'SCANTAB+SCANTAB+'                                           
SCANTAB  DS    (PARMCNTQ)CL(SCBLKLQ)                                            
         DC    CL16'SCANTAB-SCANTAB-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'SSB+SSB+SSB+SSB+'                                           
SSB      DC    XL2'0000',X'FF',1021X'00'                                        
         DC    CL16'SSB-SSB-SSB-SSB-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'UTL+UTL+UTL+UTL+'                                           
UTL      DC    F'0',X'0A',251X'00'                                              
         DC    CL16'UTL-UTL-UTL-UTL-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'+IO++IO++IO++IO+'                                           
IO       DC    2048X'00'           CTFILE/GENFIL I/O AREA                       
         DC    CL16'-IO--IO--IO--IO-'                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* INCLUDED DATA                                                       *         
***********************************************************************         
         SPACE 1                                                                
*DDMQREASON                        MQ REASON CODES                              
         PRINT OFF                                                              
       ++INCLUDE DDMQREASON                                                     
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* VALID INPUT CARD TABLE                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
CARDTAB  DC    CL10'DSPACE    ',AL1(5,5,0,0),AL4(VCDSPACE)                      
         DC    CL10'DDSIO     ',AL1(4,4,0,0),AL4(VCDDSIO)                       
         DC    CL10'DEBUG     ',AL1(4,4,0,0),AL4(VCDEBUG)                       
         DC    CL10'DRTEST    ',AL1(5,5,0,0),AL4(VCDR)                          
         DC    CL10'TRACE     ',AL1(4,4,0,0),AL4(VCTRACE)                       
         DC    CL10'TYPE      ',AL1(3,3,0,0),AL4(VCTYPE)                        
         DC    CL10'QMGR      ',AL1(3,3,CFBIG,0),AL4(VCQMGR)                    
         DC    CL10'PATH      ',AL1(3,3,CFBIG,0),AL4(VCPATH)                    
         DC    CL10'EDICTTYPE ',AL1(8,8,0,0),AL4(VCEDTYPE)                      
         DC    CL10'SENDERID  ',AL1(7,7,0,0),AL4(VCSNDRID)                      
         DC    CL10'INPUTQ    ',AL1(5,5,CFBIG,0),AL4(VCINPQ)                    
         DC    CL10'LOGQ      ',AL1(3,3,CFBIG,0),AL4(VCLOGQ)                    
         DC    CL10'BDEQ      ',AL1(3,3,CFBIG,0),AL4(VCBDEQ)                    
         DC    X'FF'                                                            
*                                                                               
CARDSIN  DC    15CL80' '                                                        
         DC    X'FF'                                                            
*                                                                               
CARDTABD DSECT                     INPUT CARD PARAMETERS                        
CARDTXT  DS    CL10                TEXT                                         
CMIN     DS    X                   MIN LEN FOR EXECUTE (-1)                     
CMAX     DS    X                   MAX LEN FOR EXECUTE (-1)                     
CFLAG    DS    X                   FLAGS                                        
CFBIG    EQU   X'80'                                                            
         DS    X                   N/D                                          
CARDVAL  DS    AL4                 A(VALIDATION ROUTINE)                        
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
EDIBDF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                       *         
***********************************************************************         
         SPACE 1                                                                
ERRMSGS  DS    0CL45                                                            
EMSG01   DC    CL45'Undefined parameter card                    '               
EMSG02   DC    CL45'DSPACE= parameter can only be 1 byte long   '               
EMSG03   DC    CL45'DDSIO= parameter cannot be more than 8 long '               
EMSG04   DC    CL45'Input line is not valid for SCANNER         '               
EMSG05   DC    CL45'Too many parameters in total                '               
EMSG06   DC    CL45'The parameter must be a number              '               
EMSG07   DC    CL45'This userid is not valid                    '               
EMSG08   DC    CL45'This userid has no 02 element - this is bad '               
EMSG09   DC    CL45'Too many filters - this one is ignored      '               
EMSG10   DC    CL45'This user has no EDICT record               '               
EMSG11   DC    CL45'Unable to open print queue report           '               
EMSG12   DC    CL45'Unable to write first line to PQ            '               
EMSG13   DC    CL45'Unable to write page eject to PQ            '               
EMSG14   DC    CL45'Print line is too long to handle            '               
EMSG15   DC    CL45'Unable to write print line to PQ            '               
EMSG16   DC    CL45'Unable to close print queue report          '               
EMSG17   DC    CL45'Must erase partial PQ report                '               
EMSG18   DC    CL45'Unable to find incoming EDICT user id       '               
EMSG19   DC    CL45'TYPE= must be A(DV) or R(EP)                '               
EMSG20   DC    CL45'Unknown ++DDS card - please check           '               
         EJECT                                                                  
***********************************************************************         
* CONSOLE MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                     *         
***********************************************************************         
         SPACE 1                                                                
SYSMSGS  DS    0CL50                                                            
SMSG01   DC    CL50'XBeginning initialisation                      '            
SMSG02   DC    CL50'XCompleted initialisation                      '            
SMSG03   DC    CL50'XBeginning reading input cards                 '            
SMSG04   DC    CL50'XCompleted reading input cards                 '            
SMSG05   DC    CL50'XBeginning validating input cards              '            
SMSG06   DC    CL50'XCompleted validating input cards              '            
SMSG07   DC    CL50'XBeginning setting operator comms              '            
SMSG08   DC    CL50'XCompleted setting operator comms              '            
SMSG09   DC    CL50'XBeginning MQ Initialisation                   '            
SMSG10   DC    CL50'XCompleted MQ Initialisation                   '            
SMSG11   DC    CL50'XObtained  MQ QMGR handle                      '            
SMSG12   DC    CL50'XOpened    MQ Master Queue                     '            
SMSG13   DC    CL50'XOpened    MQ Log Queue                        '            
SMSG14   DC    CL50'XBeginning MQ Deallocation                     '            
SMSG15   DC    CL50'XCompleted MQ Deallocation                     '            
SMSG16   DC    CL50'XClosed    MQ Master Queue                     '            
SMSG17   DC    CL50'XClosed    MQ Log Queue                        '            
SMSG18   DC    CL50'X** WARNING ** Logging Suppressed              '            
SMSG19   DC    CL50'XIncoming operator command                     '            
SMSG20   DC    CL50'X** WARNING ** Operator requested "STOP"       '            
SMSG21   DC    CL50'X** WARNING ** Unknown operator command ignored'            
SMSG22   DC    CL50'XOpened all subsidiary MQ Queues               '            
SMSG23   DC    CL50'XClosed all subsidiary MQ Queues               '            
SMSG24   DC    CL50'XNUMPRTS inadequate - abending call programmer '            
SMSG25   DC    CL50'X** ERROR ** Unknown userid XXXXXXXXXXXXXXXXXX '            
SMSG26   DC    CL50'XBeginning building PQ list                    '            
SMSG27   DC    CL50'XCompleted building PQ list                    '            
SMSG28   DC    CL50'X** ERROR ** Mailbox XXXXXXXX is unknown       '            
SMSG29   DC    CL50'4** ERROR ** DSTTAB size inadequate must ABEND '            
SMSG30   DC    CL50'XEASYLINK -- Transmissions suspended (MQ issue)'            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* W/S AREA                                                            *         
***********************************************************************         
         SPACE 1                                                                
         ORG   EDIBDF+(((*-EDIBDF)/4096)+1)*4096                                
         DC    CL8'*WORKD**'                                                    
WORKD    DS    0X                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
STRTTIME DS    F                                                                
*                                                                               
DMCB     DS    8F                                                               
*                                                                               
PACKOF4B DS    PL4                 KEEP FULLWORD ALIGNED                        
WORK     DS    XL256                                                            
WORK2    DS    XL256                                                            
MSG01    DS    CL20                                                             
*                                                                               
NUMLINES DS    F                   NUMBER OF TRANSMITTED LINES/REPORT           
NUMPAGES DS    H                   PAGE COUNTER FOR FAX TRANSMISSION            
*                                                                               
REQUESTR DS    CL3                                                              
AGYPOWER DS    CL2                 AGENCY POWER CODE                            
SVAGYPOW DS    CL2                 SAVED AGENCY POWER CODE                      
AGYNAME  DS    CL33                                                             
AGYADDR  DS    CL33                                                             
EASYDATA DS    XL21                EASYLINK-SPECIFIC TRANSFER DATA              
ROUTCODE DS    CL5                 DARE ROUTING CODE                            
MEDIA    DS    C                   DARE MEDIA                                   
REPCODE  DS    CL3                 DARE REP CODE                                
CUSTREF  DS    CL27                VFUUUUSSSNNNNNTTTTTTTTLLLLSS                 
ERRCODE  DS    X                                                                
*                                                                               
BDECN    DS    CL(L'DESTBDECN)     BDE COMMAN NAME                              
BDEOP    DS    CL(L'DESTBDEOP)     BDE RECEIVER'S OPERATING SYSTEM              
BDEEN    DS    CL(L'DESTBDEEN)     BDE ENCRYPTION (NONE,BLOWFISH,3DES)          
BDECM    DS    CL(L'DESTBDECM)     BDE COMPRESS (Y/N)                           
BDESF    DS    CL(L'DESTBDESF)     BDE DELETE SENT FILE (Y/N)                   
BDECA    DS    CL(L'DESTBDECA)     BDE CONVERT TO ASCII (Y/N)                   
BDECP    DS    CL(L'DESTBDECP)     BDE CODE PAGE                                
BDEFN    DS    CL(L'DESTBDEFN)     BDE FAILURE NOTIFICATION EMAIL ADDR          
BDEBI    DS    CL(L'DESTBDEBI)     BDE BINARY DATA TRANSFER (Y/N)               
*                                                                               
*                                                                               
CONOK    DS    X                                                                
PLINE    DS    CL100               OUTPUT PRINT LINE                            
REPLY    DS    CL8                                                              
CARDIO   DS    CL80                                                             
FERN     DS    X                                                                
ETYPE    DS    X                                                                
GOTERR   DS    X                                                                
PARMCNT  DS    X                   NO OF I/P CARDS                              
PARMCNTQ EQU   100                 MAX NUMBER OF I/P CARDS                      
KEY      DS    XL64                FOR CTFILE/GENDIR READS                      
*                                                                               
ACTION   DS    X                   ACTION(S)                                    
ACTJNKQ  EQU   C'J'                 UNSENDABLE                                  
ACTADDQ  EQU   C'A'                 ADD                                         
*                                                                               
SENDERID DS    CL1                 BDE SENDER ID                                
*                                                                               
INPUTQ   DS    CL48                INPUT MQ QUEUE                               
ELOGQ    DS    CL48                EDICT STATUS OUTBOUNE MQ QUEUE               
*                                                                               
BQMGRNAM DS    CL48                BDE SNDR SERVICE MQ Q MANAGER NAME           
BQUENAME DS    CL48                BDE SNDR SERVICE QUEUE NAME                  
*                                                                               
USERID   DS    CL8                 REPORT USERID (ALPHA)                        
USERIDH  DS    CL4                 REPORT USERID (HEX AS CHARS)                 
USERIDNO DS    XL2                 REPORT USERID                                
RPTSUBID DS    CL3                 REPORT SUB-ID                                
RPTPQNUM DS    CL4                 REPORT REFERENCE NUMBER                      
RPTHTIME DS    CL8                 REPORT CREATION DATE/TIME                    
RPTLOGNO DS    CL4                 LOGICAL REPORT SEQ. WITHIN PHYSICAL          
RPTLDSTS DS    XL1                 LOG. REP. DEST. SEQ. WITHIN PHYSICAL         
RPTPQTYP DS    XL2                 PQ REPORT TYPE                               
PQTYPE   DS    X                   PQ REPORT TYPE IN HEX                        
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL16'ENTRYPTSENTRYPTS'                                           
ENTRYPTS DC    Y((ENTRYLSQ-ENTRYSTQ)/60) NUMBER OF TABLE ENTRIES                
         DC    H'60'                     MUST REMAIN AS 60                      
ENTRYSTQ EQU   *                                                                
CSQBBACK DC    CL8'CSQBBACK'                                                    
         DC    XL52'00'                                                         
CSQBCLOS DC    CL8'CSQBCLOS'                                                    
         DC    XL52'00'                                                         
CSQBCOMM DC    CL8'CSQBCOMM'                                                    
         DC    XL52'00'                                                         
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
         DC    XL52'00'                                                         
CSQBGET  DC    CL8'CSQBGET'                                                     
         DC    XL52'00'                                                         
CSQBOPEN DC    CL8'CSQBOPEN'                                                    
         DC    XL52'00'                                                         
CSQBPUT  DC    CL8'CSQBPUT'                                                     
         DC    XL52'00'                                                         
ENTRYLSQ EQU   *                                                                
*                                                                               
         CMQA    LIST=YES,EQUONLY=NO                                            
***********************************************************************         
* TABLE OF INPUT LINES ON DDS CONTROL CARDS                           *         
***********************************************************************         
         SPACE 1                                                                
LINETAB  DS    0F                                                               
         DC    CL3'SUB',X'00'                                                   
SUBLEN   DS    H                                                                
SUB      DS    XL256                                                            
         DC    CL3'FIL',X'00'                                                   
FILLEN   DS    H                                                                
FIL      DS    XL256                                                            
         DC    CL3'EXT',X'00'                                                   
EXTLEN   DS    H                                                                
EXT      DS    XL256                                                            
         DC    CL3'DSN',X'00'                                                   
DSNLEN   DS    H                                                                
DSN_C    DS    XL256                                                            
         DC    CL3'TRN',X'00'                                                   
TRNLEN   DS    H                                                                
TRN      DS    XL256                                                            
         DC    CL3'MQN',X'00'                                                   
MQNLEN   DS    H                                                                
MQN      DS    XL256                                                            
         DC    CL3'PQS',X'00'                                                   
PQSLEN   DS    H                                                                
PQS      DS    XL256                                                            
         DC    CL3'FTP',X'00'                                                   
FTPLEN   DS    H                                                                
FTP      DS    XL256                                                            
         DC    X'FF'                                                            
*                                                                               
LINETABD DSECT                                                                  
LINEID   DS    CL3                                                              
         DS    C                                                                
LINELEN  DS    H                                                                
LINEIN   DS    CL256                                                            
LINETABL EQU   *-LINETABD                                                       
*                                                                               
EDIBDF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* RD CHAIN CSECT                                                      *         
***********************************************************************         
         SPACE 1                                                                
WORKAREA CSECT                                                                  
         DC    200000X'00'                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DDEDICTFIL                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDEDICTFIL                                                     
         PRINT ON                                                               
* DDEDICTWRK                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDEDICTWRK                                                     
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* CTGENEDICT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
* EDISCANHDR                                                                    
         PRINT OFF                                                              
       ++INCLUDE EDISCANHDR                                                     
         PRINT ON                                                               
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
* DDBDESNDD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBDESNDD                                                      
         PRINT ON                                                               
* IEZCIB                                                                        
         PRINT OFF                                                              
         DSECT                                                                  
         IEZCIB                                                                 
         PRINT ON                                                               
* IEZCOM                                                                        
         PRINT OFF                                                              
         IEZCOM                                                                 
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'106EDIBDE    04/25/14'                                      
         END                                                                    
