*          DATA SET CTDFAR     AT LEVEL 008 AS OF 07/09/19                      
*PHASE CTDFARB                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE CHOPPER                                                                
*INCLUDE CTKEYOUT                                                               
*INCLUDE CUREDIT                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE FATABOFF                                                               
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE UNTIME                                                                 
         EJECT                                                                  
         TITLE 'CONTROL DAILY FILE ACTIVITY REPORT'                             
         PRINT NOGEN                                                            
CTDFAR   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKL,**CTDFAR,VREGSVE,CLEAR=YES                                 
         USING WORKD,RC            LOCAL W/S                                    
         L     RA,ACOMMON                                                       
         USING COMMON,RA                                                        
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         USING PLINED,P                                                         
         L     R8,VBOXAREA                                                      
         USING BOXD,R8                                                          
         ST    RD,SAVERD                                                        
         ST    R1,ACMRG            SAVE MVS PARMS                               
         SAM31                                                                  
         IAZXJSAB READ,JOBNAME=JOB,JOBID=JOBNUM                                 
         SAM24                                                                  
*                                                                               
         LA    R0,PBLOCK           CLEAR PBLOCK TO SPACES                       
         LHI   R1,PBLOCKL                                                       
         LHI   RF,C' '                                                          
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
*                                                                               
         LHI   RF,ELADDS-WORKD                                                  
         AR    RF,RC                                                            
         ST    RF,AELADDS                                                       
*                                                                               
         BRAS  RE,GETUPSI          GET UPSI PARAMETERS                          
         BRAS  RE,GETCARD          GET ALL INPUT CARDS                          
         BNE   MAIN06                                                           
*                                                                               
         MVC   SORTCARD,DEF_SORT   SET A DEFAULT SORT OPTION                    
         BRAS  RE,VALCARD          VALIDATE INPUT CARDS                         
         BNE   MAIN40                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMOPEN,DMSYS,DMFLIST                               
         GOTO1 VDATAMGR,DMCB,DMDTF,CTRCVR                                       
         L     RE,12(R1)                                                        
         NILH  GRE,X'00FF'         Clear high order byte (file number)          
         ST    RE,@CRCVDTF         Save DTF of CTRCVR file                      
         BRAS  RE,MAXBLKS                                                       
*                                                                               
         CLI   INPTYPE,C'T'        INPUT IS FROM AN OLD TAPE                    
         BNE   MAIN02              NO                                           
         BRAS  RE,READFILE                                                      
         BE    MAIN04                                                           
         B     XBASE                                                            
*                                                                               
MAIN02   BRAS  RE,WRTFILE                                                       
         BNE   MAIN06                                                           
*                                                                               
MAIN04   BRAS  RE,PRNTACCS         PRINT INPUT TOTALS                           
         BNE   MAIN06                                                           
         CLI   SORTACTV,C'Y'       SORT ACTIVATED?                              
         BNE   MAIN06              NO                                           
*                                                                               
         BRAS  RE,PROCRECS         PROCESS SORTED RECORDS                       
         BNE   MAIN06                                                           
         BRAS  RE,SORTTOT          PRINT TOTALS AT END                          
         BNE   MAIN06                                                           
*                                                                               
MAIN06   CLI   SORTACTV,C'Y'       DOING A SORT?                                
         BNE   MAIN08              NO                                           
         GOTO1 VSORTER,PLIST,END   END SORT                                     
*                                                                               
MAIN08   CURED INPERRS,(6,ERMSGNUM),0,ZERO=NOBLANK                              
         MVC   OPMSGJOB,JOB                                                     
         MVC   OPMSGJB#,JOBNUM                                                  
         MVC   ERMSGJOB,JOB                                                     
         MVC   ERMSGJB#,JOBNUM                                                  
*&&US*&& CLI   ERASESW,YES                                                      
*&&US*&& BE    MAIN10                                                           
         CP    INPERRS,PZERO       TEST FOR INPUT ERRORS                        
         BE    MAIN40                                                           
         WTO   TEXT=ERMSG                                                       
         B     MAIN40                                                           
*&&US                                                                           
MAIN10   MVC   ERMSGXTA,MSGNOTER   "Not erased"                                 
         CP    INPERRS,PZERO       TEST FOR INPUT ERRORS                        
         BE    MAIN30                                                           
         XC    REPLY,REPLY                                                      
         XC    @ECB,@ECB                                                        
         WTOR  TEXT=(OPMSG,REPLY,L'REPLY,@ECB,ROUTCDE=(1))                      
         WAIT  ECB=@ECB                                                         
         CLI   REPLY,ERASEQ                                                     
         BE    MAIN30              YES, ERASE FILE AND SET EOF TO ZERO          
         CLI   REPLY,CANCELQ                                                    
         BE    MAIN20              No close file and dump                       
         CLI   REPLY,NO                                                         
         BNE   MAIN10              Try again                                    
MAIN20   WTO   TEXT=ERMSG                                                       
         B     MAIN40                                                           
*                                                                               
MAIN30   MVC   ERMSGXTA,MSGWASER   "Was erased"                                 
         GOTO1 VDMOD000,ERSPARS,A(DMEXT),,,(X'A4',0)                            
         GOTO1 VDADDS,ERSPARS,A(WTERASE)                                        
         GOTO1 VDADDS,ERSPARS,A(FNDEOF)                                         
         GOTO1 VDATAMGR,DMCBENQ,(0,ENQCTRL),(C'D',=C'CTRL')                     
                                                                                
MAIN40   GOTO1 VDATAMGR,DMCB,DMCLOSE,DMSYS,DMFLIST                              
         CLI   REPLY,CANCELQ                                                    
         JE    *+2                                                              
         CLI   FERN,0                                                           
         BE    XBASE                                                            
         BRAS  RE,ERRMSG                                                        
         B     XBASE                                                            
*                                                                               
VREGSVE  DC    V(REGSAVE)                                                       
ACOMMON  DC    A(COMMON)                                                        
@ECB     DS    F                                                                
REPLY    DS    C                                                                
ERASEQ   EQU   C'E'                                                             
CANCELQ  EQU   C'C'                                                             
*                                                                               
* ERROR/RESPONSE message                                                        
*                                                                               
OPMSG    DC    0H                                                               
OPLEN    DC    AL2(OPMSGLN)                                                     
OPMSGTXT DC    0C                                                               
OPMSGJOB DC    CL8'XXXXXXXX'                                                    
         DC    C' '                                                             
OPMSGJB# DC    CL8'JOB00000'                                                    
         DC    C' '                                                             
         DC    C'DISK ERRORS! '                                                 
         DC    C'Call programmer. '                                             
         DC    C'C-Cancel job '                                                 
         DC    C'E-Erase recovery '                                             
         DC    C'N-Do not erase'                                                
OPMSGLN  EQU   *-OPMSGTXT                                                       
*                                                                               
* ERASE message                                                                 
*                                                                               
ERMSG    DC    0C                                                               
ERLEN    DC    AL2(ERMSGLN)                                                     
ERMSGTXT DC    0C                                                               
ERMSGJOB DC    CL8'XXXXXXXX'                                                    
         DC    C' '                                                             
ERMSGJB# DC    CL8'JOB00000'                                                    
         DC    C' '                                                             
ERMSGNUM DC    C'       '                                                       
         DC    C'Recovery File Read Errors - '                                  
ERMSGXTA DC    CL25' '                                                          
ERMSGLN  EQU   *-ERMSGTXT                                                       
*                                                                               
MSGNOTER DC    CL25'File Not Erased !!!!'                                       
MSGWASER DC    CL25'File Erased !!!!'                                           
*                                                                               
         EJECT                                                                  
**********************************************************************          
* READ IN ALL INPUT CARDS AND SAVE FOR VALIDATION                    *          
**********************************************************************          
GETCARD  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,SCANTAB          SAVE IN SCANTAB FOR PROCESSING               
         USING SCANBLKD,R3                                                      
*                                                                               
GCD01    GOTO1 VCARDS,PLIST,CARDIO,=C'RE00'                                     
         CLI   CARDIO,C'*'         IGNORE COMMENTS                              
         BE    GCD01                                                            
         CLC   =C'/*',CARDIO       END OF CARDS?                                
         BE    EXITOK              YES                                          
*                                                                               
GCD02    CLC   =C'PATCH=',CARDIO   PATCH CARD?                                  
         BNE   GCD03                                                            
         XC    FULL,FULL           FORMAT OF CARD IS PATCH=XXXXXX XXXX          
         GOTO1 VHEXIN,PLIST,CARDIO+6,FULL+1,6                                   
         CLC   PLIST+12(4),=F'3'                                                
         BNE   GCD06               MUST BE 6 HEX DIGITS FOR ADDR.               
*                                                                               
         LA    R7,CARDIO+79        FIND LENGTH OF PATCH DATA                    
         LH    R4,=H'-1'                                                        
         LA    R5,CARDIO+12                                                     
         CLI   0(R7),C' '                                                       
         BNE   *+12                                                             
         BXH   R7,R4,*-8                                                        
         B     GCD06                                                            
*                                                                               
         SR    R7,R5               L'PATCH DATA IN R1                           
         GOTO1 VHEXIN,PLIST,CARDIO+13,DUB,(R7)                                  
         ICM   R1,15,PLIST+12                                                   
         BZ    GCD06               ZERO IS NOT ALLOWED                          
         BCTR  R1,0                                                             
         L     R5,FULL             PATCH DISPLACEMENT IN R5                     
         AR    R5,R7               R5=A(AREA TO BE PATCHED)                     
         MVC   0(0,R5),DUB                                                      
         EX    R1,*-6              MOVE IN THE PATCH DATA                       
         B     GCD01                                                            
*                                                                               
GCD03    CLC   =C'DDSIO=',CARDIO   DDSIO=XXX... TO SET THE DDSIO                
         BNE   GCD04                                                            
         L     RF,=V(DDSIO)        OVERRIDE DATAMGR LOAD MODULE NAME            
         MVC   0(8,RF),CARDIO+6                                                 
         B     GCD01                                                            
*                                                                               
GCD04    CLC   =C'DSPACE=',CARDIO  DSPACE=X TO SET THE DATA SPACE               
         BNE   GCD05                                                            
         L     RF,=A(SSB)                                                       
         USING SSBD,RF                                                          
         MVC   SSODSPAC,CARDIO+7                                                
         B     GCD01                                                            
         DROP  RF                                                               
*                                                                               
GCD05    MVC   CARDIO+60(20),SPACES    FIX FOR GRABAGE ON END CARD              
         GOTO1 VSCANNER,PLIST,(C'C',CARDIO),((R3))                              
         XR    RF,RF                                                            
         ICM   RF,1,4(R1)          RF=NUMBER OF INPUT PARAMETERS                
         BZ    GCD06                                                            
*                                                                               
         XR    R0,R0               INCREMENT NUMBER OF PARAMETERS               
         IC    R0,PARMCNT                                                       
         AR    R0,RF                                                            
         CHI   R0,PARMCNTQ                                                      
         BH    GCD06                                                            
         STC   R0,PARMCNT                                                       
*                                                                               
         MHI   RF,SCBLKLQ          GO TO NEXT FREE SLOT IN SCANTAB              
         AR    R3,RF                                                            
         B     GCD01                                                            
*                                                                               
GCD06    MVI   FERN,8              INVALID PARAMETER CARD                       
         BRAS  RE,ERRMSG                                                        
         DC    H'00'               FAIL ON THIS                                 
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS IN SCANTAB (FROM GETCARD)                  *         
***********************************************************************         
VALCARD  NTR1  BASE=*,LABEL=*                                                   
         MVC   TITLE,VCTITLE                                                    
         XR    R2,R2                                                            
         ICM   R2,1,PARMCNT                                                     
         BNZ   VCD02                                                            
         OI    OUTTYPE,CHNGOUT     DEFAULT - SHOW CHANGES ONLY                  
         OI    FILTERS,STDONLY               STANDARD O/P                       
         B     EXITOK                                                           
*                                                                               
VCD02    LA    R3,SCANTAB                                                       
         USING SCANBLKD,R3                                                      
*                                                                               
VCD04    XR    RF,RF               RECONSTRUCT INPUT PARAMETER                  
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
         MVC   PBLOCK(0),SC1STFLD                                               
         EX    RF,*-6                                                           
         LA    RE,PBLOCK+1(RF)                                                  
*                                                                               
         CLI   SC2NDLEN,0          SECOND HALF TO PARAMETER                     
         BE    VCD06                                                            
         MVI   0(RE),C'='                                                       
         MVC   1(L'SC2NDFLD,RE),SC2NDFLD                                        
*                                                                               
VCD06    BRAS  RE,PRNT                                                          
*                                                                               
         L     R4,ACARDTAB         TABLE OF SUPPORTED PARAMETERS                
         USING CARDTABD,R4                                                      
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
*                                                                               
VCD08    CLI   0(R4),255           END OF TABLE?                                
         BNE   *+12                                                             
         MVI   FERN,13             UNKNOWN PARAMETER CARD                       
         B     EXITL                                                            
*                                                                               
         CLM   RF,1,CMIN           CHECK LENGTHS ARE OK                         
         BL    VCD10                                                            
         CLM   RF,1,CMAX                                                        
         BH    VCD10                                                            
         EX    RF,*+8              MATCH TEXT STRING                            
         BE    VCD12                                                            
         CLC   SC1STFLD(0),CARDTXT                                              
*                                                                               
VCD10    AHI   R4,CARDTABL         TRY NEXT ENTRY                               
         B     VCD08                                                            
*                                                                               
VCD12    ICM   RF,15,CARDVAL       GO AND VALIDATE THIS INPUT                   
         BASR  RE,RF                                                            
         BE    VCD20                                                            
         BRAS  RE,ERRMSG                                                        
         DC    H'00'                                                            
*                                                                               
VCD20    AHI   R3,SCBLKLQ          NEXT LINE IN SCANTAB                         
         BCT   R2,VCD04            ANY MORE PARMS INPUT?                        
*                                                                               
         TM    OUTTYPE,X'FC'       WERE OUTPUT PARAMETERS SET?                  
         BNZ   *+8                                                              
         OI    OUTTYPE,CHNGOUT     DEFAULT - SHOW CHANGES ONLY                  
         OC    FILTERS,FILTERS     WERE ANY INPUT PARMS SET?                    
         BNZ   *+8                                                              
         OI    FILTERS,STDONLY     DEFAULT TO STANDARD O/P                      
         B     EXITOK              ERROR                                        
         DROP  R3,R4                                                            
*                                                                               
VCTITLE  DC    CL(L'TITLE)'Input Cards to Daily File Activity'                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE LUID=XXXXXXXX PARAMETER                                   *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALUID   NTR1  BASE=*,LABEL=*                                                   
         OC    V_TCODE,V_TCODE                                                  
         BZ    *+12                                                             
         MVI   FERN,14             PARAMETER ALREADY SET                        
         B     EXITL                                                            
*                                                                               
         L     R2,AIOTEMP                                                       
         USING CTTREC,R2                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKTID,SC2NDFLD                                                 
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R2),(R2)                            
         CLI   8(R1),0             TEST IF RECORD FOUND                         
         BE    *+12                                                             
         MVI   FERN,2              SET ERROR MESSAGE                            
         B     EXITL                                                            
*                                                                               
         GOTO1 VHELLO,PLIST,(C'G',CTFILE),(3,(R2)),0,0,0,0                      
         CLI   12(R1),0                                                         
         BE    *+12                                                             
         MVI   FERN,2              SET ERROR MESSAGE                            
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,12(R1)                                                     
         MVC   V_TCODE,2(RF)                                                    
         OI    FILTERS,LUIND       SET FILTERING BY LUID                        
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE TERMINAL=XXXX PARAMETER                                   *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALTRM   NTR1  BASE=*,LABEL=*                                                   
         OC    V_TCODE,V_TCODE                                                  
         BZ    *+12                                                             
         MVI   FERN,14             PARAMETER ALREADY SET                        
         B     EXITL                                                            
*                                                                               
         TM    SC2NDVAL,SCNUMQ     NUMERIC?                                     
         BNZ   *+12                                                             
         MVI   FERN,17             PARAMETER MUST BE A NUMBER                   
         B     EXITL                                                            
*                                                                               
         ICM   R1,15,SC2NDNUM                                                   
         C     R1,=X'0000FFFF'                                                  
         BL    *+12                                                             
         MVI   FERN,16             NUMBER ENTERED IS TOO BIG                    
         B     EXITL                                                            
*                                                                               
         L     R2,AIOTEMP                                                       
         USING CTTREC,R2                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         STH   R1,23(R2)                                                        
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,AIOTEMP,AIOTEMP                      
         CLI   8(R1),0             TEST RECORD FOUND                            
         BE    *+12                                                             
         MVI   FERN,1                                                           
         B     EXITL                                                            
*                                                                               
         MVC   V_TCODE,23(R2)      SAVE TERMINAL NUMBER TO FILTER               
         OI    FILTERS,LUIND                                                    
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE SORTRECS = XXXX PARAMETER                                 *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALSORT  NTR1  BASE=*,LABEL=*                                                   
         OC    SORTFILT,SORTFILT                                                
         BZ    *+12                                                             
         MVI   FERN,14             PARAMETER ALREADY SET                        
         B     EXITL                                                            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,9                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         LA    R1,SRTINPS                                                       
*                                                                               
VSRT02   CLI   0(R1),255           END OF TABLE                                 
         BNE   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         EX    RF,*+8                                                           
         BE    VSRT04                                                           
         CLC   SC2NDFLD(0),0(R1)                                                
         AHI   R1,L'SRTINPS                                                     
         B     VSRT02                                                           
*                                                                               
VSRT04   MVC   SORTFILT,7(R1)                                                   
         B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
SRTINPS  DS    0XL8                                                             
         DC    CL7'ALL    ',AL1(SRTFALL)                                        
         DC    CL7'NONE   ',AL1(SRTFNONE)                                       
         DC    CL7'STD    ',AL1(SRTFSTD)                                        
         DC    AL1(255)                                                         
         EJECT                                                                  
**********************************************************************          
* VALIDATE USERNAME=XXXXXXXXXX PARAMETER                             *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALNID   NTR1  BASE=*,LABEL=*                                                   
         OC    V_USRID,V_USRID                                                  
         BZ    *+12                                                             
         MVI   FERN,14             PARAMETER ALREADY SET                        
         B     EXITL                                                            
*                                                                               
         L     R2,AIOTEMP                                                       
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SC2NDFLD                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,AIOTEMP,AIOTEMP                      
         CLI   8(R1),0             TEST IF RECORD FOUND                         
         BE    *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         GOTO1 VHELLO,PLIST,(C'G',CTFILE),(2,AIOTEMP),0,0,0,0                   
         CLI   12(R1),0            ELEMENT FOUND?                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,12(R1)                                                        
         MVC   V_USRID,2(RF)                                                    
         OI    FILTERS,USRIND      SET USERID FLAG OK ON                        
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE USERID = XXXX PARAMETER                                   *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALID    NTR1  BASE=*,LABEL=*                                                   
         OC    V_USRID,V_USRID                                                  
         BZ    *+12                                                             
         MVI   FERN,14             PARAMETER ALREADY SET                        
         B     EXITL                                                            
*                                                                               
         TM    SC2NDVAL,SCNUMQ     NUMERIC?                                     
         BNZ   *+12                                                             
         MVI   FERN,15             PARAMETER MUST BE A NUMBER                   
         B     EXITL                                                            
*                                                                               
         ICM   R1,15,SC2NDNUM                                                   
         C     R1,=X'0000FFFF'                                                  
         BL    *+12                                                             
         MVI   FERN,16             NUMBER ENTERED IS TOO BIG                    
         B     EXITL                                                            
*                                                                               
         L     R2,AIOTEMP                                                       
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         STCM  R1,3,CTIKNUM                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,AIOTEMP,AIOTEMP                      
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         MVI   FERN,3              SET ERROR MESSAGE                            
         B     EXITL                                                            
*                                                                               
         MVC   V_USRID,CTIKNUM                                                  
         OI    FILTERS,USRIND      SET USERID OK FLAG                           
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGYBIN = XX PARAMETER                                      *         
* NTRY: R3     = A(SCANBLK)                                           *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALAGY   NTR1  BASE=*,LABEL=*                                                   
         OC    V_AGYID,V_AGYID                                                  
         BZ    *+12                                                             
         MVI   FERN,14             PARAMETER ALREADY SET                        
         B     EXITL                                                            
*                                                                               
         TM    SC2NDVAL,SCNUMQ     NUMERIC?                                     
         BNZ   *+12                                                             
         MVI   FERN,18             PARAMETER MUST BE A NUMBER                   
         B     EXITL                                                            
*                                                                               
         ICM   R1,15,SC2NDNUM                                                   
         CHI   R1,255                                                           
         BNH   *+12                                                             
         MVI   FERN,16             NUMBER ENTERED IS TOO BIG                    
         B     EXITL                                                            
*                                                                               
         STC   R1,V_AGYID                                                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROGNAME=XXXXXXX PARAMETER                                 *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALPRG   NTR1  BASE=*,LABEL=*                                                   
         ICM   R5,15,APGMLST       JUST DO THIS ONCE                            
         BNZ   VPRG02                                                           
*                                                                               
         L     R5,VSELIST                                                       
         LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         CLI   SESYS,X'0A'         GET CONTROL SYSTEM                           
         BE    *+10                                                             
         BXLE  R5,RE,*-8                                                        
         DC    H'0'                CONTROL SYSTEM NOT FOUND                     
         ICM   R5,15,SEPGMS                                                     
         ST    R5,APGMLST                                                       
*                                                                               
VPRG02   LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         AHI   R5,6                                                             
         USING PGMLSTD,R5                                                       
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
VPRG04   EX    RF,*+8                                                           
         BE    VPRG06                                                           
         CLC   PGMNAME(0),SC2NDFLD MATCH?                                       
         BXLE  R5,RE,VPRG04                                                     
         MVI   FERN,X'04'          INVALID PROGRAM                              
         B     EXIT                                                             
*                                                                               
VPRG06   MVC   V_PRNUM,PGMNUM      SET NUMBER TO MATCH ON                       
         MVC   PROGRAM,PGMNAME     SAVE NAME                                    
         OI    FILTERS,PRGIND      SET PROGRAM OK FLAG                          
         B     EXITOK                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATES PROGNUM=XXXX PARAMETER                                    *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALPGN   NTR1  BASE=*,LABEL=*                                                   
         TM    SC2NDVAL,SCNUMQ                                                  
         BO    *+12                                                             
         MVI   FERN,12                                                          
         B     EXITL                                                            
*                                                                               
         L     R1,SC2NDNUM                                                      
         STC   R1,V_PRNUM                                                       
         CHI   R1,256              MUST BE <255                                 
         BL    *+12                                                             
         MVI   FERN,12                                                          
         B     EXITL                                                            
*                                                                               
         ICM   R5,15,APGMLST       JUST DO THIS ONCE                            
         BNZ   VPGN02                                                           
*                                                                               
         L     R5,VSELIST                                                       
         LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         CLI   SESYS,X'0A'         GET CONTROL SYSTEM                           
         BE    *+10                                                             
         BXLE  R5,RE,*-8                                                        
         DC    H'0'                CONTROL SYSTEM NOT FOUND                     
         ICM   R5,15,SEPGMS                                                     
         ST    R5,APGMLST                                                       
*                                                                               
VPGN02   LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
         CLC   V_PRNUM,PGMNUM      MATCH?                                       
         BE    VPGN04                                                           
         BXLE  R5,RE,*-10                                                       
         MVI   FERN,4              ERROR N.F.                                   
         XC    V_PRNUM,V_PRNUM                                                  
         B     EXITL                                                            
*                                                                               
VPGN04   MVC   PROGRAM,PGMNAME      GET NAME FOR LATER                          
         OI    FILTERS,PRGIND       TURN ON OK FLAG                             
         B     EXITOK                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATES SHOW ADD RECORD COMMAND                                   *         
***********************************************************************         
VALADD   NTR1  BASE=*,LABEL=*                                                   
         NI    OUTTYPE,255-ADDOUT                                               
         USING SCANBLKD,R3                                                      
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   EXITOK                                                           
         CLC   TRUE(0),SC2NDFLD                                                 
         OI    OUTTYPE,ADDOUT                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT COMMAND                                              *         
***********************************************************************         
VALINPT  NTR1  BASE=*,LABEL=*                                                   
         USING SCANBLKD,R3                                                      
         CLI   INPTYPE,0                                                        
         BE    *+12                                                             
         MVI   FERN,14                                                          
         B     EXITL                                                            
*                                                                               
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         MVI   INPTYPE,C'D'        INPUT FROM DISK (DEFAULT)                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   DISK(0),SC2NDFLD                                                 
*                                                                               
         EX    RF,*+8              INPUT FROM TAPE (OPTIONAL)                   
         BNE   VINPT02                                                          
         CLC   TAPE(0),SC2NDFLD                                                 
         MVI   INPTYPE,C'T'                                                     
         B     EXITOK                                                           
*                                                                               
VINPT02  MVI   FERN,8                                                           
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COPYTAPE COMMAND                                           *         
* NTRY: R3     = A(SCANBLK)                                           *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALCTAP  NTR1  BASE=*,LABEL=*                                                   
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         MVI   COPYTAPE,YES        COPY TAPE REQUIRED                           
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   SC2NDFLD(0),C4YES                                                
*                                                                               
         MVI   COPYTAPE,NO         COPY TAPE NOT REQUIRED                       
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   SC2NDFLD(0),C4NO                                                 
*                                                                               
         MVI   FERN,8              INVALID PARAMETER                            
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SHOW COPY RECORDS COMMAND                                  *         
***********************************************************************         
VALCOPY  NTR1  BASE=*,LABEL=*                                                   
         USING SCANBLKD,R3                                                      
         NI    OUTTYPE,255-COPYOUT                                              
         USING SCANBLKD,R3                                                      
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   EXITOK                                                           
         CLC   TRUE(0),SC2NDFLD                                                 
         OI    OUTTYPE,COPYOUT                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SHOW CHANGE RECORD COMMAND                                 *         
***********************************************************************         
VALCHNG  NTR1  BASE=*,LABEL=*                                                   
         NI    OUTTYPE,255-CHNGOUT                                              
         USING SCANBLKD,R3                                                      
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   EXITOK                                                           
         CLC   TRUE(0),SC2NDFLD                                                 
         OI    OUTTYPE,CHNGOUT                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SHOW DELETE RECORD COMMAND                                 *         
***********************************************************************         
VALDEL   NTR1  BASE=*,LABEL=*                                                   
         USING SCANBLKD,R3                                                      
         NI    OUTTYPE,255-DELOUT                                               
         USING SCANBLKD,R3                                                      
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   EXITOK                                                           
         CLC   TRUE(0),SC2NDFLD                                                 
         OI    OUTTYPE,DELOUT                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SHOW RESTORE RECORD COMMAND                                *         
***********************************************************************         
VALREST  NTR1  BASE=*,LABEL=*                                                   
         USING SCANBLKD,R3                                                      
         NI    OUTTYPE,255-RESTOUT                                              
         USING SCANBLKD,R3                                                      
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   EXITOK                                                           
         CLC   TRUE(0),SC2NDFLD                                                 
         OI    OUTTYPE,RESTOUT                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SHOW ALL RECORDS COMMAND                                   *         
***********************************************************************         
VALALL   NTR1  BASE=*,LABEL=*                                                   
         USING SCANBLKD,R3                                                      
         NI    OUTTYPE,255-ALLOUT                                               
         USING SCANBLKD,R3                                                      
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   EXITOK                                                           
         CLC   TRUE(0),SC2NDFLD                                                 
         OI    OUTTYPE,ALLOUT                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SHOW FULL DETAIL COMMAND                                   *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALFULL  NTR1  BASE=*,LABEL=*                                                   
         NI    OUTTYPE,255-FULLOUT                                              
         USING SCANBLKD,R3                                                      
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   EXITOK                                                           
         CLC   TRUE(0),SC2NDFLD                                                 
         OI    OUTTYPE,FULLOUT                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE ERASE PARAMETER                                            *         
* NTRY: R3     = A(SCANBLK)                                           *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALERAS  NTR1  BASE=*,LABEL=*                                                   
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    VALER90                                                          
*                                                                               
         BCTR  RF,0                                                             
         LA    RE,DMFLIST          LIST OF FILES TO OPEN                        
         EX    RF,*+8                                                           
         BNE   VALER20                                                          
         CLC   SC2NDFLD(0),C4YES                                                
         MVI   ERASESW,YES         ERASE RECROVEY                               
         MVI   0(RE),C'U'          SET FOR UPDATE CTRCVR                        
         B     EXITOK                                                           
*                                                                               
VALER20  EX    RF,*+8                                                           
         BNE   VALER90                                                          
         CLC   SC2NDFLD(0),C4NO                                                 
         MVI   ERASESW,NO          DO NOT ERASE RECOVERY                        
         MVI   0(RE),NO            SET FOR READ-ONLY CTRCVR (DEFAULT)           
         B     EXITOK                                                           
*                                                                               
VALER90  MVI   FERN,8              INVALID PARAMETER                            
         B     EXITL                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE PARAMETER (DEFAULT = Y)                              *         
* NTRY: R3     = A(SCANBLK)                                           *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALWRT   NTR1  BASE=*,LABEL=*                                                   
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         MVI   DODUMP,YES          WRITE TAPE                                   
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   SC2NDFLD(0),C4YES                                                
*                                                                               
         MVI   DODUMP,NO           DO NOT WRITE TAPE                            
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   SC2NDFLD(0),C4NO                                                 
*                                                                               
         MVI   FERN,8              INVALID PARAMETER                            
         B     EXITL                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FIND THE MAX BLOCK NUMBER FOR A TRACK. THIS IS SO WE CAN BUMP UP              
* BY BLOCK THEN TRACK                                                           
***********************************************************************         
         USING DTFPHD,R2                                                        
MAXBLKS  NTR1  BASE=*,LABEL=*                                                   
         L     R2,@CRCVDTF         Control recovery DTF                         
         XR    R1,R1            ZERO BLOCKS IF UNBLOCKED                        
         XR    RF,RF                                                            
         LA    RE,DEV3390                                                       
         TM    DBLKSZ,DBLK_NO   BLOCKED FILE?                                   
         BO    MAXBLKX          NO                                              
                                                                                
         USING BLKDEFD,RE                                                       
MAXBLK2  LLH   R1,BLKMAX        GET MAX BLOCK NUMBER                            
         OC    BLKHI,BLKHI                                                      
         JZ    *+2                                                              
         CLC   DBLKSZ,BLKLOW                                                    
         BNL   MAXBLKX          FOUND IT                                        
         AHI   RE,BLKLNQ                                                        
         B     MAXBLK2                                                          
                                                                                
MAXBLKX  STC   R1,MAXBLK                                                        
         J     EXITOK                                                           
         DROP  R2,RE            DTFPHD,BLKDEFD                                  
***********************************************************************         
* NUMBER OF MAX BLOCKS PER TRACK BASED ON BLOCKSIZE                             
***********************************************************************         
DEV3390  DS    0H                                                               
         DC    AL2(56664,27999,1)                                               
         DC    AL2(27998,18453,2)                                               
         DC    AL2(18452,13683,3)                                               
         DC    AL2(13682,10797,4)                                               
         DC    AL2(10796,08907,5)                                               
         DC    AL2(08906,07549,6)                                               
         DC    AL2(07548,06519,7)                                               
         DC    AL2(06518,05727,8)                                               
         DC    AL2(05726,05065,9)                                               
         DC    AL2(0)                                                           
         EJECT                                                                  
***********************************************************************         
* READ RECOVERY FILE AND DUMP TO TAPE                                 *         
* PASS INPUT TO SORT BASED ON FLAGS SET IN FILTERS                    *         
***********************************************************************         
WRTFILE  NTR1  BASE=*,LABEL=*                                                   
         MVC   TITLE+10(L'WKINP),WKINP                                          
         L     R7,AIOTEMP          R7=A(IOAREA)                                 
         USING RECDS,R7                                                         
*                                                                               
         CLI   ERASESW,YES                                                      
         BNE   WFIL01                                                           
         GOTO1 VDATAMGR,DMCBENQ,(0,ENQCTRL),(C'E',=C'CTRL')                     
*                                                                               
WFIL01   CLI   DODUMP,YES          WANT TO WRITE RECOVERY TAPE(S)?              
         BNE   WFIL04              NO                                           
*                                                                               
         OPEN  (RCVTAPE,OUTPUT)                                                 
         LTR   RF,RF               OPENED OK?                                   
         BZ    WFIL02              YES                                          
         LA    R1,MSG2             ERROR ON OPEN                                
         BRAS  RE,MSGOUT                                                        
         ABEND 911                                                              
*                                                                               
WFIL02   CLI   COPYTAPE,YES        TEST IF COPY TAPE REQUIRED                   
         BNE   WFIL04                                                           
         OPEN  (RCVCOPY,OUTPUT)                                                 
         LTR   RF,RF               OPENED OK?                                   
         BZ    WFIL04              YES                                          
         LA    R1,MSG3             ERROR ON COPY OPEN                           
         BRAS  RE,MSGOUT                                                        
         ABEND 911                                                              
*                                                                               
WFIL04   CLI   SORTFILT,SRTFNONE   WANT TO DO WITHOUT SORTING?                  
         BE    WFIL06              YES                                          
         GOTO1 VSORTER,PLIST,SORTCARD,RECCARD,0                                 
         MVI   SORTACTV,C'Y'       START SORT AND SET ACTIVE                    
*                                                                               
WFIL06   GOTO1 VDATAMGR,DMCB,(X'11',DMRSEQ),CTRCVR,DMDA,4(R7),ABUFFER           
         TM    8(R1),X'80'         END OF FILE                                  
         BZ    WFIL12                                                           
         CLI   DODUMP,YES          WANT TO WRITE RECOVERY TAPE(S)?              
         BNE   EXITOK              NO                                           
*                                                                               
         CLOSE (RCVTAPE)                                                        
         LTR   RF,RF               CLOSED OK?                                   
         BZ    WFIL08              YES                                          
         LA    R1,MSG4             ERROR ON CLOSE MESSAGE                       
         BRAS  RE,MSGOUT                                                        
         ABEND 911                                                              
*                                                                               
WFIL08   CLI   COPYTAPE,YES        TEST IF COPY TAPE REQUIRED                   
         BNE   WFIL10                                                           
         CLOSE (RCVCOPY)                                                        
         LTR   RF,RF                                                            
         BZ    WFIL10                                                           
         LA    R1,MSG5             ERROR ON COPY CLOSE                          
         BRAS  RE,MSGOUT                                                        
         ABEND 911                                                              
*                                                                               
WFIL10   LA    R1,MSG1             SET RECOVERY FILE OK MESSAGE                 
         BRAS  RE,MSGOUT                                                        
         B     EXITOK                                                           
*                                                                               
WFIL12   CLI   8(R1),0             ALL OTHER DM ERROR CONDITIONS                
         BNE   WFILERR                                                          
*                                                                               
         LH    R1,DMCB+18          GET RECORD LENGTH READ BY DM                 
         LA    R1,4(R1)            ADD 4 FOR LENGTH OF LENGTH!!                 
         XC    RECLN,RECLN                                                      
         STH   R1,RECLN            SAVE RECORD LEN AT FRONT OF RECORD           
*                                                                               
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    WFIL06                                                           
         TM    RRECTY,X'80'                                                     
         BO    WFIL06              IGNORE THE POINTER COPIES/CHANGES            
*                                                                               
         L     RF,AINPACCS         TABLE OF INPUT ACCUMULATORS                  
         USING INPACCSD,RF                                                      
WFIL14   CLC   RFILTY,INPNUM       TEST MATCH ON FILE TYPE                      
         BE    WFIL16                                                           
         AHI   RF,INPACCSL         BUMP TO NEXT ROW OF ACCUMULATORS             
         CLI   0(RF),0             TEST FOR END OF TABLE                        
         BNE   WFIL14                                                           
*                                                                               
WFIL16   XR    R1,R1                                                            
         IC    R1,RRECTY           UPDATE INPUT ACCUMULATORS                    
         BCTR  R1,0                                                             
         MHI   R1,L'INPTCPY                                                     
         LA    R1,INPTCPY(R1)                                                   
         AP    0(8,R1),PONE        INCREMENT INPUT ACCUMULATOR FOR TYPE         
         AP    INPTTOT,PONE        INCREMENT FILE TOTAL                         
         AP    INPTOTS,PONE        INCREMENT GRAND TOTAL                        
         DROP  RF                                                               
*                                                                               
WFIL18   CLI   DODUMP,YES          WANT TO WRITE RECOVERY TAPE(S)?              
         BNE   WFIL19              NO                                           
         PUT   RCVTAPE,(R7)        WRITE RECORD TO RECOVERY TAPE                
         CLI   COPYTAPE,YES        COPY TAPE REQUIRED?                          
         BNE   WFIL19              NO                                           
         PUT   RCVCOPY,(R7)                                                     
*                                                                               
WFIL19   L     R1,RECINCNT         INCREMENT SEQ NUMBER                         
         AHI   R1,1                                                             
         ST    R1,RECINCNT                                                      
         MVC   RSIN,RECINCNT       POP SEQ NUM IN RSIN FOR SORT                 
*                                                                               
WFIL20   EQU   *                                                                
*&&UK*&& CLI   RFILTY,DEMOLDQ      TEST DEMOLD/DEMNEW/DEMDIR                    
*&&UK*&& BE    WFIL06                                                           
*&&UK*&& CLI   RFILTY,DEMNEWQ      IGNORE THEM ALL                              
*&&UK*&& BE    WFIL06                                                           
*&&UK*&& CLI   RFILTY,DEMDIRQ                                                   
*&&UK*&& BE    WFIL06                                                           
*                                                                               
         TM    OUTTYPE,FULLOUT     INCLUDE AUTOMATICS IF FULL DETAILS           
         BO    WFIL26              REQUESTED                                    
         CLI   RPRG,0              PROG # 0 ARE AUTOMATICS                      
         BE    WFIL06                                                           
*                                                                               
         L     R2,ACTFADD          CTFILE RECORD                                
         CLI   RFILTY,CTFILEQ                                                   
         BE    WFIL22                                                           
         L     R2,ACTRADD          CTREQ RECORD                                 
         CLI   RFILTY,CTREQQ                                                    
         BE    WFIL22                                                           
         L     R2,ACTGADD          GENDIR/FIL RECORD                            
         CLI   RFILTY,CTREQQ                                                    
         BE    WFIL22                                                           
         L     R2,ACTUADD          UNKNOWN RECORD                               
*                                                                               
         USING ADDTABD,R2                                                       
WFIL22   CLI   ADRECNUM,255        UNKNOWN RECORD - PASS IT ON                  
         BE    WFIL26                                                           
         CLC   RFILTY,ADFTYPE                                                   
         BE    *+12                                                             
         L     R2,ACTUADD                                                       
         B     WFIL26                                                           
*                                                                               
         XR    RF,RF               LOOK FOR LIVE KEY                            
         ICM   RF,1,ADMTCHL                                                     
         BZ    WFIL26              AUTOMATIC MATCH                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   WFIL24              DOES NOT MATCH LIVE KEY                      
         CLC   ADMTCHC(0),RCVFRST                                               
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,ADPASSL        PASSIVE KEY IN RECORD?                       
         BZ    WFIL26              NO - PUT TO SORT                             
         BCTR  RF,0                                                             
*                                                                               
         MVC   WORK(1),ADPASSF     SET FILL CHARACTER                           
         MVC   WORK+1(32),WORK                                                  
*                                                                               
         XR    R1,R1                                                            
         IC    R1,ADPASSD                                                       
         LA    R1,RCVFRST(R7)      R1=A(MATCH AREA IN RECORD)                   
         EX    RF,*+8                                                           
         BE    WFIL06              PASSIVE - IGNORE IT                          
         CLC   WORK(0),0(R1)                                                    
         B     WFIL26                                                           
*                                                                               
WFIL24   AHI   R2,ADDTABLQ         NEXT IN TABLE                                
         B     WFIL22                                                           
*                                                                               
WFIL26   BRAS  RE,VALPARMS         TEST FILTER PARAMETERS FOR MATCH             
         BNE   WFIL06              EXCLUDE RECORD                               
         CLI   SORTFILT,SRTFNONE                                                
         BE    WFIL06                                                           
*                                                                               
         CLI   FLAG,0              LOOK FOR ACTIVITY DATE                       
         BNE   WFIL30                                                           
         CLI   RFILTY,X'A1'        CTFILE?                                      
         BNE   WFIL30              YES                                          
         CLI   RRECTY,2            CHANGE?                                      
         BNE   WFIL30              NO                                           
*                                                                               
         CLI   ADRECNUM,1          DESCRIPTION                                  
         BE    WFIL30                                                           
         CLI   ADRECNUM,71         FAX NUMBER                                   
         BE    WFIL30                                                           
         CLI   ADRECNUM,113        RFP GROUP HEADER                             
         BE    WFIL30                                                           
         CLI   ADRECNUM,73         PROGRAM DETAILS                              
         BE    WFIL30                                                           
         CLI   ADRECNUM,74         PROGRAM DETAILS                              
         BE    WFIL30                                                           
         CLI   ADRECNUM,75         PROGRAM DETAILS                              
         BE    WFIL30                                                           
*                                                                               
         LA    R3,RCVFRST+28       FIRST ELEMENT                                
         USING CTACTD,R3                                                        
         CLI   CTACTEL,CTACTELQ    ACTIVITY                                     
         BNE   WFIL30                                                           
         CLI   CTACTLEN,CTACTLNQ                                                
         BNE   WFIL30                                                           
         GOTO1 VDATCON,PLIST,(3,CTACTDT),(10,REPDATE),0                         
         MVI   FLAG,255                                                         
         DROP  R2,R3                                                            
*                                                                               
WFIL30   TM    RTIME,X'80'         NEW STYLE TIME?                              
         BZ    WFIL32              NO                                           
         NI    RTIME,X'7F'         SETS NEW STYLE TIME TO OLD                   
         MVI   RTIME+3,X'C0'       STYLE TIME FOR OUTPUT                        
         ICM   R1,15,RTIME                                                      
         SRL   R1,4                                                             
         STCM  R1,15,RTIME                                                      
*                                                                               
WFIL32   GOTO1 VSORTER,PLIST,PUT,(R7)                                           
*                                                                               
         TM    UPSI,X'80'          IF PARM=1 CARD PRINT KEYS & DA               
         BZ    WFIL06                                                           
         BRAS  RE,PRTKEY                                                        
         B     WFIL06                                                           
*                                                                               
WFILERR  LA    R2,PBLOCK                                                        
         MVC   0(L'RDERRM,R2),RDERRM                                            
         MVC   L'RDERRM(L'CTRCVR,R2),CTRCVR                                     
         AHI   R2,L'P                                                           
         MVC   0(L'RDERRDA,R2),RDERRDA                                          
         GOTO1 VHEXOUT,PLIST,DMDA,3(R2),4,0        HEXOUT DISK ADDRESS          
         GOTO1 (RF),(R1),PLIST,L'RDERRDA(R2),24,0  HEXOUT DMCB                  
         AHI   R2,L'P                                                           
         MVC   0(L'RDERRCN,R2),RDERRCN                                          
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R5,@CRCVDTF         Control recovery DTF                         
         USING DTFPHD,R5                                                        
         TM    DTFTYPE,DTFTBIGF    20 bit ?                                     
         BO    WFILER20                                                         
         CLC   DMDA+2(1),MAXBLK    Max blocks at 8000 blk size                  
*        CLI   DMDA+2,6            Max blocks for 16 bit file at 8000           
         BL    WFILER10                                                         
         MVI   DMDA+2,0            Set block to zero TTTT00RR                   
         LH    R3,DMDA             Bump up next track number                    
         LA    R3,1(R3)                                                         
         STH   R3,DMDA                                                          
*                                                                               
WFILER10 LLC   R1,DMDA+2           Get block number                             
         AHI   R1,1                                                             
         STC   R1,DMDA+2           Bump up next block number                    
         AP    INPTOTS,PONE        UPDATE BAD RECORD TOTALS                     
         AP    INPERRS,PONE                                                     
         B     WFIL06                                                           
*                                                                               
WFILER20 LLC   R3,DMDA+2           TB of TTTTTBRR                               
         NILL  GR3,X'0F'           Isolated B from TB                           
         CLM   R3,1,MAXBLK         Max blocks at 8000 blk size                  
         BL    WFILER30                                                         
         XR    R3,R3               Set block to zero                            
         ICM   R4,15,DMDA          TTTTTBRR                                     
         SRL   R4,12               000TTTTT                                     
         AHI   R4,1                                                             
         SLL   R4,12               TTTTT000                                     
         ST    R4,DMDA                                                          
*                                                                               
WFILER30 AHI   R3,1                R3 = Block number                            
         STC   R3,BYTE             Set or up block number                       
         OC    DMDA+2(1),BYTE                                                   
         AP    INPTOTS,PONE        UPDATE BAD RECORD TOTALS                     
         AP    INPERRS,PONE                                                     
         B     WFIL06                                                           
*                                                                               
WKINP    DC    CL28'Recovery file input key dump'                               
RDERRM   DC    CL28'Disk Error On Recovery File - '                             
RDERRDA  DC    CL17'DA=XXXXXXXX,DMCB='                                          
RDERRCN  DC    CL14'Run Continuing'                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* READS RECOVERY FILE FROM TAPE FOR SPECIALS.                         *         
***********************************************************************         
READFILE NTR1  BASE=*,LABEL=*                                                   
         MVC   TITLE+10(L'RKINP),RKINP                                          
         OPEN  (TAPEIN,INPUT)                                                   
         GOTO1 VSORTER,PLIST,SORTCARD,RECCARD,0                                 
*                                                                               
RFIL02   GET   TAPEIN                                                           
         LR    R7,R1                                                            
         USING RECDS,R7                                                         
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    RFIL02                                                           
         TM    RRECTY,X'80'        IGNORE POINTER COPIES/CHANGES                
         BO    RFIL02                                                           
*                                                                               
         L     R1,RECINCNT         INCREMENT SEQ NUMBER                         
         AHI   R1,1                                                             
         ST    R1,RECINCNT                                                      
         MVC   RSIN,RECINCNT       POP SEQ NUM IN RSIN FOR SORT                 
*                                                                               
         L     RF,AINPACCS         TABLE OF INPUT ACCUMULATORS                  
         USING INPACCSD,RF                                                      
RFIL04   CLC   RFILTY,INPNUM       TEST MATCH ON FILE TYPE                      
         BE    RFIL06                                                           
         AHI   RF,INPACCSL         BUMP TO NEXT ROW OF ACCUMULATORS             
         CLI   0(RF),0             TEST FOR END OF TABLE                        
         BNE   RFIL04                                                           
*                                                                               
RFIL06   XR    R1,R1                                                            
         IC    R1,RRECTY           UPDATE INPUT ACCUMULATORS                    
         BCTR  R1,0                                                             
         MHI   R1,L'INPTCPY                                                     
         LA    R1,INPTCPY(R1)                                                   
         AP    0(8,R1),PONE        INCREMENT INPUT ACCUMULATOR FOR TYPE         
         AP    INPTTOT,PONE        INCREMENT FILE TOTAL                         
         AP    INPTOTS,PONE        INCREMENT GRAND TOTAL                        
         DROP  RF                                                               
*                                                                               
RFIL08   EQU   *                                                                
*&&UK*&& CLI   RFILTY,DEMOLDQ      TEST DEMOLD/DEMNEW/DEMDIR                    
*&&UK*&& BE    RFIL02                                                           
*&&UK*&& CLI   RFILTY,DEMNEWQ      IGNORE THEM ALL                              
*&&UK*&& BE    RFIL02                                                           
*&&UK*&& CLI   RFILTY,DEMDIRQ                                                   
*&&UK*&& BE    RFIL02                                                           
*                                                                               
         TM    OUTTYPE,FULLOUT     INCLUDE AUTOMATICS IF FULL DETAILS           
         BO    RFIL14              REQUESTED                                    
         CLI   RPRG,0              PROG # 0 ARE AUTOMATICS                      
         BE    RFIL02                                                           
*                                                                               
         L     R2,ACTFADD          CTFILE RECORD                                
         CLI   RFILTY,CTFILEQ                                                   
         BE    RFIL10                                                           
         L     R2,ACTRADD          CTREQ RECORD                                 
         CLI   RFILTY,CTREQQ                                                    
         BE    RFIL10                                                           
         L     R2,ACTGADD          GENDIR/FIL RECORD                            
         CLI   RFILTY,CTREQQ                                                    
         BE    RFIL10                                                           
         L     R2,ACTUADD          UNKNOWN RECORD                               
         USING ADDTABD,R2                                                       
*                                                                               
RFIL10   CLI   0(R2),X'FF'         UNKNOWN RECORD - PASS IT ON                  
         BE    RFIL14                                                           
         CLC   RFILTY,ADFTYPE                                                   
         BE    *+12                                                             
         L     R2,ACTUADD                                                       
         B     RFIL14                                                           
*                                                                               
         XR    RF,RF               LOOK FOR LIVE KEY                            
         ICM   RF,1,ADMTCHL                                                     
         BZ    RFIL14              AUTOMATIC MATCH                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   RFIL12              DOES NOT MATCH LIVE KEY                      
         CLC   ADMTCHC(0),RCVFRST                                               
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,ADPASSL        PASSIVE KEY IN RECORD?                       
         BZ    RFIL14              NO - PUT TO SORT                             
         BCTR  RF,0                                                             
*                                                                               
         MVC   WORK(1),ADPASSF     SET FILL CHARACTER                           
         MVC   WORK+1(32),WORK                                                  
*                                                                               
         XR    R1,R1                                                            
         IC    R1,ADPASSD                                                       
         LA    R1,RCVFRST(R7)      R1=A(MATCH AREA IN RECORD)                   
         EX    RF,*+8                                                           
         BE    RFIL02              PASSIVE - IGNORE IT                          
         CLC   WORK(0),0(R1)                                                    
         B     RFIL14                                                           
*                                                                               
RFIL12   AHI   R2,ADDTABLQ         NEXT IN TABLE                                
         B     RFIL10                                                           
         DROP  R2                                                               
*                                                                               
RFIL14   BRAS  RE,VALPARMS         TEST FILTER PARAMETERS FOR MATCH             
         BNE   RFIL02              EXCLUDE RECORD                               
         CLI   SORTFILT,SRTFNONE                                                
         BE    RFIL02                                                           
*                                                                               
         CLI   FLAG,0              LOOK FOR ACTIVITY DATE                       
         BNE   RFIL18                                                           
         LHI   R3,28                                                            
         CLI   RFILTY,X'A1'        CTFILE?                                      
         BE    RFIL16              YES                                          
         CLI   RFILTY,X'A3'        CTREQ?                                       
         BE    RFIL16              YES - NO RECORD TO PRINT FOR NOW             
         CLI   RFILTY,X'AE'        GENDIR?                                      
         BE    RFIL16              YES - NO RECORD TO PRINT FOR NOW             
         LHI   R3,42               DISP TO DATA FOR GENFIL                      
*                                                                               
RFIL16   CLI   RFILTY,X'A1'        CTFILE?                                      
         BNE   RFIL18              YES                                          
         LA    R3,RCVFRST(R3)                                                   
         USING ACTVD,R3                                                         
         CLI   ACTVEL,X'F1'        GET AN ACTIVITY ELEMENT                      
         BNE   RFIL18                                                           
         CLI   ACTVLEN,ACTVLENQ                                                 
         BNE   RFIL18                                                           
         MVC   REPDATE,SPACES                                                   
         GOTO1 VDATCON,DMCB,(3,ACTVCHDT),(8,REPDATE),0                          
         MVI   FLAG,255                                                         
         DROP  R3                                                               
*                                                                               
RFIL18   TM    RTIME,X'80'         NEW STYLE TIME?                              
         BZ    RFIL20              NO                                           
         NI    RTIME,X'7F'         SETS NEW STYLE TIME TO OLD                   
         MVI   RTIME+3,X'C0'       STYLE TIME FOR OUTPUT                        
         ICM   R1,15,RTIME                                                      
         SRL   R1,4                                                             
         STCM  R1,15,RTIME                                                      
*                                                                               
RFIL20   GOTO1 VSORTER,PLIST,PUT,(R7)                                           
*                                                                               
         TM    UPSI,X'80'          IF PARM=1 CARD PRINT KEYS & DA               
         BZ    RFIL02                                                           
         BRAS  RE,PRTKEY                                                        
         B     RFIL02                                                           
*                                                                               
TAPEEND  CLOSE (TAPEIN)                                                         
         B     EXITOK                                                           
*                                                                               
RKINP    DC    CL28'Recovery file input key dump'                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT INPUT KEY TO PRINTER FOR DEBUGGING                           *         
* NTRY: R7 = A(RECOVERY FILE HEADER)                                  *         
***********************************************************************         
         USING RECDS,R7                                                         
PRTKEY   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,PBLOCK                                                        
         MVC   0(6,R3),=CL6'RFHDR='                                             
         GOTO1 VHEXOUT,PLIST,(R7),10(R3),RCVFRST-RECDS,0,0                      
         AHI   R3,L'P                                                           
         MVC   0(6,R3),=CL6'RFKEY='                                             
         LHI   RF,25                                                            
         CLI   RFILTY,CTFILEQ                                                   
         BE    *+8                                                              
         LHI   RF,32                                                            
         GOTO1 VHEXOUT,PLIST,RCVFRST,6(R3),(RF),0                               
         AHI   R3,L'P                                                           
         MVI   0(R3),C'.'                                                       
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT MESSAGE TO CONSOLE                                           *         
* NTRY:  R2    = A(MESSAGE)                                           *         
***********************************************************************         
MSGOUT   NTR1  BASE=*,LABEL=*                                                   
         XR    R0,R0                                                            
         LA    R2,WORK                                                          
         MVC   0(2,R2),=H'50'                                                   
         MVC   2(50,R2),0(R1)                                                   
         WTO   TEXT=((MSGHL,C),(0(R2),D),(0,E))                                 
         B     EXITOK                                                           
*                                                                               
MSGHL    DC    AL2(30)                                                          
         DC    CL30'Control Daily File Activity'                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETERS BEFORE PASSING RECORD TO SORT                   *         
* NTRY: R2 = A(ADDTAB ENTRY FOR THIS RECORD)                          *         
***********************************************************************         
         USING ADDTABD,R2                                                       
VALPARMS NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOTEMP                                                       
         USING RECDS,R3                                                         
         TM    SORTFILT,SRTFALL    SORT=ALL                                     
         BO    EXITOK                                                           
*                                                                               
         OC    V_TCODE,V_TCODE     TEST FOR TERMINAL #                          
         BZ    VFLT02                                                           
         CLC   RTRM,V_TCODE                                                     
         BNE   EXITL                                                            
*                                                                               
VFLT02   OC    V_USRID,V_USRID     TEST FOR USER ID #                           
         BZ    VFLT04                                                           
         CLC   RUSER,V_USRID                                                    
         BNE   EXITL                                                            
*                                                                               
VFLT04   OC    V_AGYID,V_AGYID     TEST FOR AGENCY ID                           
         BZ    VFLT06                                                           
         CLC   RAG,V_AGYID                                                      
         BNE   EXITL                                                            
*                                                                               
VFLT06   OC    V_RTYPE,V_RTYPE     TEST FOR RECORD NUMBER                       
         BZ    VFLT08                                                           
         CLC   V_RTYPE,ADRECNUM                                                 
         BNE   EXITL                                                            
*                                                                               
VFLT08   TM    FILTERS,STDONLY     STANDARD OUTPUT ONLY?                        
         BZ    VFLT10                                                           
         CLI   ADSTDREP,0          NZ IF WANTED AS DEFAULT                      
         BE    EXITL                                                            
*                                                                               
VFLT10   B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT OF RECOVERY RECORD INPUT TOTALS                        *         
***********************************************************************         
PRNTACCS NTR1  BASE=*,LABEL=*                                                   
PC       USING PLINED,BOXCOLS                                                   
         ZAP   PAGE,PONE                                                        
         ZAP   LINE,P99            ENSURE PAGE THROW                            
         MVC   TITLE,SPACES                                                     
         MVC   TITLE,IPTITLE                                                    
         GOTO1 VPRINTER            PRINT TITLES                                 
*                                                                               
         OI    BOXDDCTL,BOXDDLC                                                 
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         MVI   BOXREQ,C'O'                                                      
*                                                                               
         MVC   BOXCOLS,SPACES      CLEAR DOWN COLUMNS                           
         MVI   PC.PILEFT,C'L'      SET INPUT COLUMNS                            
         MVI   PC.PIC1,C'C'                                                     
         MVI   PC.PIC2,C'C'                                                     
         MVI   PC.PIC3,C'C'                                                     
         MVI   PC.PIC4,C'C'                                                     
         MVI   PC.PIRIGHT,C'R'                                                  
         GOTO1 VPRINTER                                                         
         DROP  PC                                                               
*                                                                               
         MVC   PIFILE,IPIFILE   SET UP HEADERS                                  
         MVC   PICPY,IPICPY                                                     
         MVC   PICHG,IPICHG                                                     
         MVC   PIADD,IPIADD                                                     
         MVC   PITOT,IPITOT                                                     
         GOTO1 VPRINTER            DO HEADINGS                                  
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            DRAW LINE UNDER HEADINGS                     
*                                                                               
         L     R3,AINPACCS                                                      
         USING INPACCSD,R3                                                      
PACC02   CLI   INPNUM,255          EOT?                                         
         BE    PACC04                                                           
         MVC   PIFILE,INPNAME      FILE NAME                                    
         CURED INPTCPY,(8,PICPY),0,ZERO=NOBLANK                                 
         CURED INPTCHA,(8,PICHG),0,ZERO=NOBLANK                                 
         CURED INPTADD,(8,PIADD),0,ZERO=NOBLANK                                 
         CURED INPTTOT,(8,PITOT),0,ZERO=NOBLANK                                 
         GOTO1 VPRINTER            PRINT A ROW OF ACCUMULATORS                  
         AHI   R3,INPACCSL                                                      
         B     PACC02                                                           
*                                                                               
PACC04   MVC   PIFILE,IPIERRS                                                   
         CURED (P8,INPERRS),(8,PITOT),0,ZERO=NOBLANK                            
         GOTO1 VPRINTER                                                         
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            DRAW LINE UNDER FILE COUNTS                  
*                                                                               
         MVC   PIFILE,IPITOTS      NOW OUTPUT TOTALS                            
         ZAP   WORK+00(8),PZERO    COPY ACCUMULATORS                            
         ZAP   WORK+08(8),PZERO    CHANGE ACCUMULATORS                          
         ZAP   WORK+16(8),PZERO    ADD ACCUMULATORS                             
         ZAP   WORK+24(8),PZERO    TOTALS ACCUMULATORS                          
*                                                                               
         L     R3,AINPACCS                                                      
PACC06   CLI   0(R3),X'FF'         EOT                                          
         BE    PACC08                                                           
         AP    WORK+00(8),INPTCPY  ADD UP COPY ACCUMULATORS                     
         AP    WORK+08(8),INPTCHA  ADD UP CHANGE ACCUMULATORS                   
         AP    WORK+16(8),INPTADD  ADD UP ADD ACCUMULATORS                      
         AP    WORK+24(8),INPTTOT  ADD UP TOTALS ACCUMULATORS                   
         AHI   R3,INPACCSL         NEXT FILE ROW                                
         B     PACC06                                                           
*                                                                               
PACC08   CURED (P8,WORK+00),(8,PICPY),0,ZERO=NOBLANK                            
         CURED (P8,WORK+08),(8,PICHG),0,ZERO=NOBLANK                            
         CURED (P8,WORK+16),(8,PIADD),0,ZERO=NOBLANK                            
         CURED (P8,WORK+24),(8,PITOT),0,ZERO=NOBLANK                            
         GOTO1 VPRINTER            PRINT TOTALS                                 
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER            CLOSE BOX                                    
         B     EXITOK                                                           
*                                                                               
IPTITLE  DC    CL(L'TITLE)'Recovery File Input Count'                           
IPIFILE  DC    CL(L'PIFILE)'FILENAME'                                           
IPIERRS  DC    CL(L'PIFILE)'Errors'                                             
IPITOTS  DC    CL(L'PIFILE)'Totals'                                             
IPICPY   DC    CL(L'PICPY)'  Copies'                                            
IPICHG   DC    CL(L'PICHG)' Changes'                                            
IPIADD   DC    CL(L'PIADD)'    Adds'                                            
IPITOT   DC    CL(L'PITOT)'   Total'                                            
*                                                                               
PLINED   DSECT                                                                  
PLINE    DS    0CL132                                                           
PILEFT   DS    X                                                                
         DS    X                                                                
PIFILE   DS    CL8                                                              
         DS    X                                                                
PIC1     DS    X                                                                
         DS    X                                                                
PICPY    DS    CL8                                                              
         DS    X                                                                
PIC2     DS    X                                                                
         DS    X                                                                
PICHG    DS    CL8                                                              
         DS    X                                                                
PIC3     DS    X                                                                
         DS    X                                                                
PIADD    DS    CL8                                                              
         DS    X                                                                
PIC4     DS    X                                                                
         DS    X                                                                
PITOT    DS    CL8                                                              
         DS    X                                                                
PIRIGHT  DS    X                   INPUT LINE RIGHT                             
*                                                                               
CTDFAR   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORDS - AFTER RECORDS HAVE BEEN SORTED                    *         
***********************************************************************         
PROCRECS NTR1  BASE=*,LABEL=*                                                   
         ZAP   PAGE,PONE                                                        
         MVC   TITLE,SPACES                                                     
         MVC   PRDATE,REPDATE                                                   
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         OI    BOXDDCTL,BOXDDLC                                                 
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         MVI   BOXROWS+L'BOXROWS-1,C'B' FORCES LINE AT END OF PAGE              
*                                                                               
         TM    OUTTYPE,ALLOUT      ALL RECORDS O/P?                             
         BZ    *+8                                                              
         OI    OUTTYPE,X'F8'       TURN ON ALL O/P BITS                         
*                                                                               
         MVI   LSTPNUM,255                                                      
         XC    LSTCTFRD,LSTCTFRD   CLEAR DOWN USER ID VARIABLE                  
*                                                                               
PRCR02   BRAS  RE,GETFRSRT         GET A RECORD FROM THE SORT                   
         BNE   PRCRX               FINISHED WITH SORT                           
*                                                                               
         L     R2,AIOSORT          GET A(CHANGE/ADD RECORD)                     
         USING RECDS,R2                                                         
         BRAS  RE,CTCOUNT          ADD RECORD TO TOTALS                         
         CLI   MODE,MODERES        THEN IGNORE ODD RECORDS                      
         BH    PRCR02                                                           
*                                                                               
         TM    OUTTYPE,FULLOUT     FULL RECORD DETAILS?                         
         BE    *+12                                                             
         CLI   RFILTY,GENDIRQ      DON'T O/P GENDIR AS STD                      
         BE    PRCR02                                                           
*                                                                               
         CLC   LSTPNUM,RPRG        IS IT THE SAME PROGRAM?                      
         BE    PRCR04                                                           
         BRAS  RE,CTPGN            GET NEW NAME                                 
         ZAP   LINE,P99            FORCE PAGE THROW                             
         MVC   PRPGM,LSTPNAM                                                    
         MVC   PRDATE,REPDATE                                                   
         MVC   TITLE(L'PRTOT),PRTOT                                             
*                                                                               
PRCR04   LA    RF,PAGEOUT1                                                      
         CLC   =C'PROF',LSTPNAM    VARIABLE TITLE POSITIONS HERE                
         BNE   *+8                                                              
         LA    RF,PAGEOUT                                                       
         ST    RF,AOUTFORM                                                      
*                                                                               
         CLI   MODE,MODEADD        RECORD IS AN ADD?                            
         BNE   PRCR06              NO                                           
         TM    OUTTYPE,ADDOUT      OUTPUT ADD RECORDS?                          
         BZ    PRCR02              NO                                           
         BRAS  RE,RECOUT                                                        
         B     PRCR02                                                           
*                                                                               
PRCR06   CLI   MODE,MODEDEL        RECORD IS A DELETE?                          
         BNE   PRCR08              NO                                           
         TM    OUTTYPE,DELOUT      OUTPUT DELETE RECORDS?                       
         BZ    PRCR02              NO                                           
         BRAS  RE,RECOUT                                                        
         B     PRCR02                                                           
*                                                                               
PRCR08   CLI   MODE,MODERES        RESTORE?                                     
         BNE   PRCR10              NO                                           
         TM    OUTTYPE,RESTOUT     OUTPUT RESTORE RECORDS?                      
         BZ    PRCR02              NO                                           
         BRAS  RE,RECOUT                                                        
         B     PRCR02                                                           
*                                                                               
PRCR10   TM    OUTTYPE,CHNGOUT     OUTPUT COPY/CHANGE PAIRS?                    
         BZ    PRCR02                                                           
         BRAS  RE,RECOUT                                                        
         B     PRCR02                                                           
*                                                                               
PRCRX    CLI   BOXSTAT,C'I'        INSIDE A BOX STILL?                          
         BNE   EXITOK              NO                                           
         MVI   BOXREQ,C'C'         FORCE CLOSE                                  
         GOTO1 VPRINTER                                                         
         B     EXITOK              FINISHED WITH SORT                           
         DROP  R2                                                               
*                                                                               
PRTOT    DC    CL(L'TITLE)' '                                                   
         ORG   PRTOT                                                            
         DC    C'Activity Report Program = '                                    
PRPGM    DC    CL7' '                                                           
         DC    C' - Date = '                                                    
PRDATE   DC    CL10' '                                                          
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* GET A RECORD FROM THE SORT                                          *         
***********************************************************************         
GETFRSRT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
GFS02    GOTO1 VSORTER,PLIST,GET,0                                              
         ICM   R2,15,PLIST+4       END OF FILE?                                 
         BZ    EXITL                                                            
         USING RECDS,R2                                                         
*                                                                               
         CLI   RRECTY,3            WAS RECORD AN ADD?                           
         BNE   GFS04               NO                                           
         MVI   MODE,MODEADD        SET MODE=ADD                                 
*                                                                               
         L     R0,AIOSORT          MOVE IT INTO MAIN I/O AREA                   
         LH    R1,0(R2)                                                         
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,AIOSORT          CLEAR JUST AFTER RECORD                      
         AH    RF,0(R2)                                                         
         XC    0(2,RF),0(RF)                                                    
*                                                                               
         L     R0,AIOCOPY          CLEAR COPY AREA                              
         LH    R1,0(R2)                                                         
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXITOK                                                           
*                                                                               
GFS04    CLI   RRECTY,1            WAS RECORD A COPY?                           
         BE    *+12                YES                                          
         MVI   MODE,MODEERR                                                     
         B     EXITOK              NO - ERROR                                   
*                                                                               
         L     R0,AIOCOPY          MOVE RECORD INTO COPY AREA                   
         LH    R1,0(R2)                                                         
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,AIOCOPY          CLEAR AFTER RECORD                           
         AH    RF,0(R2)                                                         
         XC    0(2,RF),0(RF)                                                    
*                                                                               
         GOTO1 VSORTER,PLIST,GET,0                                              
         ICM   R2,15,PLIST+4       END OF FILE?                                 
         BZ    EXITL                                                            
*                                                                               
         CLI   RRECTY,2            WAS RECORD A CHANGE                          
         BE    *+12                YES                                          
         MVI   MODE,MODEERR                                                     
         B     EXITOK              NO - ERROR                                   
*                                                                               
         L     R0,AIOSORT          MOVE CHANGE INTO MAIN I/O AREA               
         LH    R1,0(R2)                                                         
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,AIOSORT                                                       
         AH    RF,0(R2)                                                         
         XC    0(2,RF),0(RF)                                                    
*                                                                               
         L     R4,AIOSORT          MAKE SURE THIS PAIR REALLY CHANGED           
         L     RE,AIOCOPY                                                       
         LH    R5,0(R4)                                                         
         LH    RF,0(RE)                                                         
         CLCL  R4,RE               RECORDS ARE IDENTICAL                        
         BE    GFS02               SO IGNORE THEM                               
*                                                                               
         LHI   RF,27               DISP TO STATUS FOR CTFILE                    
         CLI   RFILTY,CTFILEQ                                                   
         BE    GFS06                                                            
         LHI   RF,32               DISP TO STATUS FOR GENDIR                    
         CLI   RFILTY,GENDIRQ                                                   
         BE    GFS06                                                            
         LHI   RF,34               DISP TO STATUS FOR GENFILE                   
         CLI   RFILTY,GENFILQ                                                   
         BE    GFS06                                                            
         MVI   MODE,255                                                         
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
GFS06    MVI   MODE,MODECHG        SET DEFAULT MODE=CHANGE                      
         AHI   RF,RCVFRST-RECDS                                                 
         STH   RF,HALF             SET DISP TO STATUS FROM START                
*                                                                               
         LH    R1,HALF                                                          
         A     R1,AIOSORT                                                       
         MVC   BYTE,0(R1)          SAVE STATUS BYTE OF CHANGE                   
         LH    R1,HALF                                                          
         A     R1,AIOCOPY                                                       
         MVC   BYTE1,0(R1)         SAVE STATUS BYTE OF COPY                     
*                                                                               
         TM    BYTE1,X'80'         COPY RECORD DELETED?                         
         BO    GFS08               YES                                          
         TM    BYTE,X'80'          CHANGE RECORD DELETED?                       
         BZ    EXITOK              NO                                           
         MVI   MODE,MODEDEL        SET MODE=DELETED                             
         B     EXITOK                                                           
*                                                                               
GFS08    TM    BYTE,X'80'          CHANGE RECORD DELETED                        
         BO    EXITOK              YES                                          
         MVI   MODE,MODERES        SET MODE=RESTORED                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD OUTPUT ROUTINE                                               *         
***********************************************************************         
RECOUT   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOSORT                                                       
         USING RECDS,R2                                                         
*                                                                               
         BRAS  RE,KEYHEAD          PRINT TITLES FOR THIS RECORD                 
*                                                                               
         L     R6,AOUTFORM                                                      
         USING OUTFORM,R6                                                       
*                                                                               
         XR    R1,R1                                                            
         IC    R1,MODE             GET THE RECORD TYPE FROM MODTAB              
         BCTR  R1,0                                                             
         MHI   R1,L'MODTAB                                                      
         LA    R1,MODTAB(R1)                                                    
*                                                                               
         LH    RF,OUTACT           DISPLACEMENT REQUIRED                        
         LA    RF,PBLOCK(RF)                                                    
         MVC   0(L'MODTAB,RF),0(R1)                                             
*                                                                               
         CLC   LSTNUM,RUSER        IS USERID SAME AS LAST TIME?                 
         BE    RCO02                                                            
         MVC   HALF,RUSER          GET AND SAVE USERID                          
         BRAS  RE,GETUSRID                                                      
         MVC   LSTCTFRD,WORK                                                    
*                                                                               
RCO02    LH    RF,OUTUID           OUTPUT USERID                                
         LA    RF,PBLOCK(RF)                                                    
         MVC   0(L'LSTNAM,RF),LSTNAM                                            
*                                                                               
         MVI   TRAILER,C'N'                                                     
         TM    RTIME,X'04'         X'40' BUT SHIFTED 4 BITS                     
         BZ    *+8                                                              
         MVI   TRAILER,C'Y'                                                     
         NI    RTIME,X'03'                                                      
*                                                                               
         CLI   TRAILER,C'Y'        TRAILER RECORDS?                             
         BE    *+14                YES - ALWAYS GET TERMINAL                    
         CLC   LSTTNUM,RTRM        IS TERMINAL NO SAME AS LAST ONE?             
         BE    *+8                                                              
         BRAS  RE,GETTRM           NO - GET IT                                  
*                                                                               
         LH    RF,OUTTRM                                                        
         LA    RF,PBLOCK(RF)                                                    
         MVC   0(L'LSTTNAM,RF),LSTTNAM                                          
*                                                                               
         ZAP   DUB,RTIME           EDIT TIME OF ACTION                          
         LH    RF,OUTTIME                                                       
         LA    RF,PBLOCK(RF)       PRINT LINE DISPLACEMENT                      
         CVB   R4,DUB                                                           
         SRDA  R4,32                                                            
         D     R4,=F'100'                                                       
*&&US*&& AHI   R5,600     Adjust for US DDS time                                
         CURED (R5),(5,(RF)),2,ZERO=NOBLANK                                     
*                                                                               
         L     R3,AADDNTRY         GET A(ENTRY)                                 
         USING ADDTABD,R3                                                       
         LHI   R0,L'ADFILENM       CHOP UP RECORD NAME FOR DISPLAY              
         LHI   RF,L'PHREC                                                       
         LH    R4,OUTNAME                                                       
         LA    R4,PBLOCK(R4)                                                    
         GOTO1 VCHOPPER,DMCB,((R0),ADFILENM),((RF),(R4)),(C'P',4)               
*                                                                               
         ICM   RF,15,ADODFLT       A(KEY) O/P ROUTINE                           
         BASR  RE,RF                                                            
*                                                                               
PH       USING PLINED,BOXCOLS                                                   
         MVC   BOXCOLS,SPACES      SET COLUMNS FOR ELEMENTS                     
         MVI   PH.PHLEFT,C'L'                                                   
         MVI   PH.PHRIGHT,C'R'                                                  
         MVI   PH.PDC1,C'C'                                                     
         MVI   PH.PDC2,C'C'                                                     
         MVI   BOXREQ,C'B'         PRINT DIVIDER                                
         GOTO1 VPRINTER                                                         
         MVC   PDELEM,IPDELEM                                                   
         MVC   PDDATA(L'IPDDATA),IPDDATA                                        
         MVC   PDACT,IPDACT                                                     
         GOTO1 (RF)                                                             
         MVI   BOXREQ,C'B'                                                      
         GOTO1 (RF)                                                             
*                                                                               
         TM    OUTTYPE,FULLOUT     FULL RECORD DETAILS REQUIRED?                
         BZ    RCO04               O/P ONLY WHAT HAS CHANGED THEN               
         ICM   RF,15,ADODFLT       A(KEY) O/P ROUTINE                           
         BASR  RE,RF               GO AND DO IT                                 
         MVI   BOXREQ,C'B'                                                      
         MVC   BOXCOLS,SPACES      CLEAR COLUMNS                                
         GOTO1 VPRINTER            PRINT CLOSE BOX AND EXIT                     
         B     EXITOK                                                           
         DROP  R6,R3                                                            
*                                                                               
RCO04    LHI   R3,42               GENFILE                                      
         CLI   RFILTY,X'A3'        CHECK FOR GEN                                
         BH    *+8                                                              
         LHI   R3,28               CTFILE                                       
         LA    R2,RCVFRST(R3)                                                   
*                                                                               
         CLI   MODE,MODEDEL        DELETE - OUTPUT ALL ELEMENTS REMOVED         
         BNE   RCO06                                                            
         ST    R2,WHRCOPY                                                       
         XC    WHRCHNG,WHRCHNG                                                  
         B     RCO14                                                            
*                                                                               
RCO06    CLI   MODE,MODERES        RESTORE - OUTPUT ALL ELEMENTS ADDED          
         BNE   RCO08                                                            
         ST    R2,WHRCHNG                                                       
         XC    WHRCOPY,WHRCOPY                                                  
         B     RCO14                                                            
*                                                                               
RCO08    CLI   MODE,MODEADD        ADD - OUTPUT ALL ELEMENTS ADDED              
         BNE   RCO10                                                            
         ST    R2,WHRCHNG                                                       
         XC    WHRCOPY,WHRCOPY                                                  
         B     RCO14                                                            
*                                                                               
RCO10    CLI   MODE,MODECPY        SPECIAL - OUTPUT COPIES ONLY                 
         BNE   RCO12                                                            
         L     R2,AIOCOPY                                                       
         LA    R2,RCVFRST(R3)      FIRST DATA OF COPY RECORD                    
         ST    R2,WHRCHNG          THEN OUTPUT AS ADDS                          
         XC    WHRCOPY,WHRCOPY                                                  
         B     RCO14                                                            
*                                                                               
RCO12    CLI   MODE,MODECHG        COPY/CHANGE PAIRS                            
         BNE   EXITOK                                                           
         ST    R2,WHRCHNG                                                       
         L     R2,AIOCOPY                                                       
         LA    R2,RCVFRST(R3)                                                   
         ST    R2,WHRCOPY                                                       
*                                                                               
RCO14    BRAS  RE,ELSINFO                                                       
         MVC   BOXCOLS,SPACES      CLEAR COLUMNS                                
         MVI   BOXREQ,C'B'         PRINT CLOSE BOX                              
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
*                                                                               
IPDELEM  DC    CL(L'PDELEM)'Element name'                                       
IPDDATA  DC    C'Element data'                                                  
IPDACT   DC    CL(L'PDACT)'Action'                                              
         EJECT                                                                  
***********************************************************************         
* OUTPUT ALL UNRESOLVED COPY/CHANGE INFO AS REQUIRED                  *         
*                                                                     *         
* NTRY:  WHRCOPY  = A(1ST ELEMENT IN COPY RECORD OR ZERO)             *         
*        WHRCHNG  = A(1ST ELEMENT IN CHANGE RECORD OR ZERO)           *         
* EXIT:                                                               *         
***********************************************************************         
ELSINFO  NTR1  BASE=*,LABEL=*                                                   
         MVI   THISELM,1           SET FOR FIRST ELEMENT                        
*                                                                               
ELS02    BRAS  RE,LSTELMS          BUILD LISTS FOR COPY/CHANGE PAIRS            
         BRAS  RE,MCHELMS          REMOVE ALL MATCHING ELEMENTS                 
*                                                                               
         L     R3,ACOPY2           CHECK IF RESOLVED EVERYTHING                 
         CLI   0(R3),X'FF'                                                      
         BNE   ELS04                                                            
         L     R3,ACHNG2                                                        
         CLI   0(R3),X'FF'                                                      
         BE    ELS06                                                            
*                                                                               
ELS04    L     R1,0(R3)                                                         
         BRAS  RE,GETELTAB                                                      
*                                                                               
         L     R4,AELMNTRY                                                      
         USING CTELEMD,R4                                                       
         MVC   DUB+0(4),ACOPY2                                                  
         MVC   DUB+4(4),ACHNG2                                                  
         GOTO1 CTRPOUT,DUB         CALL PAIR OUTPUT ROUTINE                     
*                                                                               
ELS06    XR    RF,RF                                                            
         IC    RF,THISELM                                                       
         AHI   RF,1                                                             
         CHI   RF,255                                                           
         BH    EXITOK                                                           
         STC   RF,THISELM                                                       
         B     ELS02                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF ALL MATCHING ELEMENTS FOR A RECORD                    *         
*                                                                     *         
* NTRY:  WHRCOPY  = A(1ST ELEMENT IN COPY RECORD OR ZERO)             *         
*        WHRCHNG  = A(1ST ELEMENT IN CHANGE RECORD OR ZERO)           *         
*        THISELM  = ELEMENT TO LIST                                   *         
* EXIT:  AELADDS  = ADDRESSES OF MATCHING ELEMENTS                    *         
*        ACOPY2   = A(FIRST COPY IN ELADDS - FF DELIMTS END)          *         
*        ACHNG2   = A(FIRST CHANGE IN ELADDS - FF DELIMTS END)        *         
***********************************************************************         
LSTELMS  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AELADDS                                                       
         LR    R0,R3                                                            
         LHI   R1,ELADDSL                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BUILD AREA                             
*                                                                               
         ST    R3,ACOPY2           SET A(COPY LIST)                             
         ICM   R2,15,WHRCOPY                                                    
         BZ    LELM04                                                           
*                                                                               
         XR    RF,RF                                                            
LELM02   CLI   0(R2),0             EOR?                                         
         BE    LELM04                                                           
         CLC   THISELM,0(R2)       MATCH ELEMENT CODE                           
         BNE   *+12                                                             
         ST    R2,0(R3)            SAVE A(ELEMENT)                              
         AHI   R3,4                NEXT IN LIST                                 
         ICM   RF,1,1(R2)                                                       
         BZ    LELM04                                                           
         BXH   R2,RF,LELM02                                                     
*                                                                               
LELM04   MVC   0(4,R3),EFFS        DELIMIT COPY LIST                            
         AHI   R3,4                                                             
*                                                                               
         ST    R3,ACHNG2           SET A(CHANGE LIST)                           
         ICM   R2,15,WHRCHNG                                                    
         BZ    LELM08                                                           
*                                                                               
         XR    RF,RF                                                            
LELM06   CLI   0(R2),0             EOR?                                         
         BE    LELM08                                                           
         CLC   THISELM,0(R2)       MATCH ELEMENT CODE                           
         BNE   *+12                                                             
         ST    R2,0(R3)            SAVE A(ELEMENT)                              
         AHI   R3,4                NEXT IN LIST                                 
         ICM   RF,1,1(R2)                                                       
         BZ    LELM08                                                           
         BXH   R2,RF,LELM06                                                     
*                                                                               
LELM08   MVC   0(4,R3),EFFS        DELIMIT CHANGE LIST                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* COMPARE LISTS OF COPY/CHANGE PAIRS AND REMOVE ALL THAT ARE SAME     *         
*                                                                     *         
* NTRY:  AELADDS  = ADDRESSES OF COPY/CHANGE ELEMENTS LISTED          *         
*        ACOPY2   = A(FIRST COPY IN ELADDS - FF DELIMITS END)         *         
*        ACHNG2   = A(FIRST CHANGE IN ELADDS - FF DELIMITS END)       *         
* EXIT:  AELADDS  = ADDRESSES OF NON-PAIRED ELEMENTS ONLY             *         
*        ACOPY2   = A(FIRST COPY IN ELADDS UPDATED)                   *         
*        ACHNG2   = A(FIRST CHANGE IN ELADDS UPDATED)                 *         
***********************************************************************         
MCHELMS  NTR1  BASE=*,LABEL=*                                                   
         L     R3,ACOPY2                                                        
MEL02    CLC   0(4,R3),EFFS        RESOLVED ALL POSSIBLE?                       
         BE    MEL12               YES                                          
*                                                                               
         ICM   RF,15,0(R3)         GET A(COPY)                                  
         XR    R1,R1                                                            
         IC    R1,1(RF)                                                         
         BCTR  R1,0                GET L'COPY (FOR EXECUTED COMPARE)            
*                                                                               
         L     R4,ACHNG2                                                        
MEL04    CLC   0(4,R4),EFFS                                                     
         BE    MEL10                                                            
         ICM   RE,15,0(R4)                                                      
         BZ    MEL06                                                            
         CLC   1(1,RF),1(RE)       MAKE SURE LENGTHS THE SAME                   
         BNE   MEL06                                                            
         EX    R1,MELCLC                                                        
         BE    MEL08                                                            
MEL06    AHI   R4,4                NEXT CHANGE ELEMENT                          
         B     MEL04                                                            
*                                                                               
MELCLC   CLC   0(0,RF),0(RE)                                                    
*                                                                               
MEL08    XC    0(4,R3),0(R3)       CLEAR OUT MATCHING PAIR                      
         XC    0(4,R4),0(R4)                                                    
*                                                                               
MEL10    AHI   R3,4                NEXT COPY ELEMENT                            
         B     MEL02                                                            
*                                                                               
MEL12    L     R3,ACOPY2           CLOSE UP GAPS ON COPIES                      
         L     R4,AIOTEMP          BY MOVING INTO AIOTEMP                       
*                                                                               
MEL14    CLC   0(4,R3),EFFS                                                     
         BE    MEL16                                                            
         OC    0(4,R3),0(R3)                                                    
         BZ    *+14                                                             
         MVC   0(4,R4),0(R3)                                                    
         AHI   R4,4                                                             
         AHI   R3,4                                                             
         B     MEL14                                                            
*                                                                               
MEL16    MVC   0(4,R4),EFFS                                                     
         AHI   R4,4                                                             
         L     R3,ACHNG2           CLOSE UP GAPS ON CHANGES                     
*                                                                               
MEL18    CLC   0(4,R3),EFFS                                                     
         BE    MEL20                                                            
         OC    0(4,R3),0(R3)                                                    
         BZ    *+14                                                             
         MVC   0(4,R4),0(R3)                                                    
         AHI   R4,4                                                             
         AHI   R3,4                                                             
         B     MEL18                                                            
*                                                                               
MEL20    MVC   0(4,R4),EFFS                                                     
         AHI   R4,4                                                             
         LR    RF,R4                                                            
         L     RE,AIOTEMP                                                       
         SR    RF,RE                                                            
         L     R0,AELADDS                                                       
         LHI   R1,ELADDSL                                                       
         MVCL  R0,RE               MOVE DATA BACK INTO WORK2                    
*                                                                               
         L     RF,AELADDS                                                       
         ST    RF,ACOPY2           RESET A(COPIES)                              
*                                                                               
         CLC   0(4,RF),EFFS        END OF COPIES FOUND                          
         BE    *+12                YES                                          
         AHI   RF,4                                                             
         B     *-14                                                             
*                                                                               
         AHI   RF,4                GO PAST DELIMITER                            
         ST    RF,ACHNG2           RESET A(CHANGE)                              
         B     EXITOK                                                           
***********************************************************************         
* GET ELEMENT TABLE ENTRY FOR THIS PARTICULAR RECORD                  *         
* NTRY: R1       = A(ELEMENT TO MATCH)                                *         
*       AADDNTRY = A(ADDTAB TABLE ENTRY)                              *         
* EXIT: AELMNTRY = A(ELEMENT TABLE ENTRY)                             *         
***********************************************************************         
GETELTAB NTR1  BASE=*,LABEL=*                                                   
         L     R3,ACTELEMS         R3 = A(ELEMENT ARRAY)                        
         XR    RF,RF                                                            
         IC    RF,0(R1)            GET ELEMENT                                  
         BCTR  RF,0                                                             
         MHI   RF,4                                                             
         AR    R3,RF               INDEX INTO TABLE                             
         L     R3,0(R3)                                                         
         USING CTELEMD,R3                                                       
         ST    R3,AELMNTRY         SAVE A(ELEMENT TABLE ENTRY)                  
*                                                                               
         L     R2,AADDNTRY                                                      
         USING ADDTABD,R2                                                       
         LA    RF,CTESYS           NOW GET BEST TEXT MATCH                      
*                                                                               
X        USING CTESYS,RF                                                        
GEL06    CLI   X.CTESYS,0          SYSTEM 0 MEANS MATCH ALL                     
         BE    *+14                                                             
         CLC   X.CTESYS,ADFTYPE                                                 
         BNE   GEL08                                                            
*                                                                               
         CLI   X.CTEFREC,0         RECORD 0 MEANS MATCH ALL                     
         BE    *+14                                                             
         CLC   X.CTEFREC,ADRECNUM                                               
         BNE   GEL08                                                            
         MVC   ELNAMEC,X.CTENAME                                                
         B     EXITOK                                                           
*                                                                               
GEL08    AHI   RF,L'CTESYS+L'CTEFREC+L'CTENAME                                  
         B     GEL06                                                            
         DROP  R2,R3,X                                                          
         EJECT                                                                  
***********************************************************************         
* DEFAULT STANDARD OUTPUT ROUTINE                                     *         
***********************************************************************         
         USING RECDS,R2                                                         
DEFKYOUT NTR1  BASE=*,LABEL=*      DEFAULT AT PRESENT IS TO HEXOUT KEY          
         LHI   R0,32               GEN FILES                                    
         CLI   RFILTY,X'A3'        GET LENGTH OF KEY TO HEXOUT                  
         BH    *+8                                                              
         LHI   R0,25               CTFILE                                       
*                                                                               
         L     RF,AOUTFORM                                                      
         LH    R5,OUTKEY-OUTFORM(RF)                                            
         LA    R5,PBLOCK(R5)                                                    
*                                                                               
         LA    R2,RCVFRST-RECDS(R2)                                             
         GOTO1 VHEXOUT,PLIST,(R2),(R5),(R0),0                                   
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DEFAULT DETAILED OUTPUT ROUTINE                                     *         
***********************************************************************         
         USING RECDS,R2                                                         
DETAIL   NTR1  BASE=*,LABEL=*                                                   
         CLI   RFILTY,CTREQQ       TEST CTREQ                                   
         BE    EXITOK              YES - NOTHING TO PRINT                       
         CLI   RFILTY,GENDIRQ      TEST GENDIR                                  
         BE    EXITOK              YES - NOTHING TO PRINT                       
*                                                                               
         L     R6,ACTELEMS                                                      
         LHI   R3,28               DISP TO 1ST ELEMENT FOR CTFILE               
         CLI   RFILTY,CTFILEQ      CTFILE?                                      
         BE    DET02                                                            
         LHI   R3,42               DISP TO 1ST ELEMENT FOR GENFILE              
         CLI   RFILTY,GENFILQ      TEST GENFILE                                 
         BE    DET02               YES                                          
         B     EXITL                                                            
         USING CTELEMD,R6                                                       
*                                                                               
DET02    ST    R6,FULL             SAVE A(TABLE) FOR LATER                      
*                                                                               
FOU010   GOTO1 VPRINTER                                                         
         LA    R2,28(R3,R2)        FIRST DATA ELEMENT                           
*                                                                               
FOU013   L     R6,FULL             GET A(TABLE)                                 
FOU015   CLI   CTNUM,255           ELEMENT NOT FOUND                            
         BE    FOU222                                                           
         CLC   0(1,R2),CTNUM       MATCH ON ELEMENT TYPE                        
         BE    FOU222                                                           
*>???    AHI   R6,CTELEML                                                       
         B     FOU015                                                           
*                                                                               
FOU222   MVC   PDELEM,CTENAME                                                   
         ICM   RF,15,CTRFOUT                                                    
         BASR  RE,RF               PRINT THE ELEMENT                            
         XR    R3,R3                                                            
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         CLI   0(R2),0             ANY MORE ELEMENTS?                           
         BNE   FOU013                                                           
         B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DEFAULT ELEMENT PAIR PRINTING ROUTINE                               *         
* NTRY: 0(R1)  = A(COPY ELEMENT)                                      *         
*       4(R1)  = A(CHANGE ELEMENT)                                    *         
***********************************************************************         
PAIRELM  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AELMNTRY                                                      
         USING CTELEMD,R6                                                       
         LM    R2,R3,0(R1)                                                      
         LA    R4,PBLOCK                                                        
X        USING PLINED,R4                                                        
         MVI   THISELMH,0          SET DIDN'T PRINT ELEMENT NAME YET            
*                                                                               
PAIR02   CLC   0(4,R2),EFFS        FINISHED WITH COPIES?                        
         BE    PAIR04              YES                                          
         BRAS  RE,PRELPRT                                                       
*                                                                               
         MVC   X.PDACT,DELETE                                                   
         ICM   RF,15,CTRDOUT       DEFAULT INFORMATION OUTPUT                   
         TM    OUTTYPE,FULLOUT                                                  
         BZ    *+8                                                              
         ICM   RF,15,CTRFOUT       DETAILED INFORMATION                         
*                                                                               
         LTR   RF,RF               OUTPUT ROUTINE SET?                          
         BNZ   *+8                 YES                                          
         L     RF,APRELOUT         USE DEFAULT (TO HEXOUT ELEMENT)              
*                                                                               
         L     R0,0(R2)                                                         
         GOTO1 (RF),DMCB,(R0),X.PLINED                                          
         L     R4,FULL                                                          
         AHI   R2,4                                                             
         B     PAIR02                                                           
*                                                                               
PAIR04   CLC   0(4,R3),EFFS        FINISHED WITH CHANGES?                       
         BE    PAIR06              YES                                          
         BRAS  RE,PRELPRT                                                       
*                                                                               
         MVC   X.PDACT,ADD                                                      
         ICM   RF,15,CTRDOUT       DEFAULT INFORMATION                          
         TM    OUTTYPE,FULLOUT                                                  
         BZ    *+8                                                              
         ICM   RF,15,CTRFOUT       DETAILED INFORMATION                         
*                                                                               
         LTR   RF,RF               OUTPUT ROUTINE SET?                          
         BNZ   *+8                 YES                                          
         L     RF,APRELOUT         USE DEFAULT (TO HEXOUT ELEMENT)              
*                                                                               
         L     R0,0(R3)                                                         
         GOTO1 (RF),DMCB,(R0),X.PLINED                                          
         L     R4,FULL             NEXT PRINT LINE FREE                         
         AHI   R3,4                                                             
         B     PAIR04                                                           
*                                                                               
PAIR06   CLI   THISELMH,0          DID WE DO ELEMENT NAME YET?                  
         BNE   *+8                 NO                                           
         MVI   THISELMH,1          FORCE NAME OUT                               
         BRAS  RE,PRELPRT          PRINT LINES REMAINING                        
         B     EXITOK                                                           
*                                                                               
PRELPRT  ST    RE,SAVERE                                                        
         CLI   THISELMH,255                                                     
         BE    PRELP04                                                          
         CLI   THISELMH,0                                                       
         BNE   PRELP02                                                          
         LA    RF,PBLOCK+(L'P*4)   FROM THE 3 IN CHOPPER BELOW                  
         CR    R4,RF                                                            
         BL    PRELPRTX                                                         
*                                                                               
PRELP02  LHI   R0,L'ELNAMEC        CHOP UP ELEMENT NAME FOR DISPLAY             
         LHI   RF,L'PDELEM                                                      
         LA    R4,PBLOCK                                                        
         GOTO1 VCHOPPER,DMCB,((R0),ELNAMEC),((RF),X.PDELEM),(C'P',3)            
         MVI   THISELMH,255                                                     
*                                                                               
PRELP04  BRAS  RE,PRNT                                                          
         LA    R4,PBLOCK                                                        
*                                                                               
PRELPRTX L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  X,R6                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HEXOUT AN ELEMENT ONTO PRINT LINES                       *         
* NTRY: 0(R1)      = A(ELEMENT)                                       *         
*       4(R1)      = A(1ST PRINT LINE)                                *         
* EXIT: FULL       = A(NEXT FREE PRINT LINE)                          *         
***********************************************************************         
PRELOUT  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
         XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         GOTO1 VHEXOUT,DMCB,(R2),WORK,(R0),0                                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         SLL   RF,1                RF = LENGTH OF DATA TO OUTPUT                
         LA    R1,WORK                                                          
*                                                                               
PREL02   LHI   RE,L'PDDATA         DEFAULT LENGTH TO MOVE                       
         CHI   RF,L'PDDATA                                                      
         BH    *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         MVC   X.PDDATA(0),0(R1)   MOVE TO PRINT LINE                           
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R1,RE               NEXT DATA                                    
         AHI   R3,L'P              NEXT LINE                                    
         SR    RF,RE                                                            
         BP    PREL02              STILL DATA TO PRINT OUT                      
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* END OF FILE TOTAL PRINTOUTS                                         *         
***********************************************************************         
SORTTOT  NTR1  BASE=*,LABEL=*                                                   
         ZAP   PAGE,PONE           RESET PAGE COUNT AND FORCE THROW             
         ZAP   LINE,P99                                                         
         MVC   TITLE,STITLE                                                     
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         OI    BOXDDCTL,BOXDDLC                                                 
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,C'N'                                                      
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+L'BOXROWS-1,C'B' FORCES LINE AT END OF PAGE              
         GOTO1 VPRINTER                                                         
*                                                                               
PC       USING PLINED,BOXCOLS                                                   
         MVC   BOXCOLS,SPACES      CLEAR DOWN COLUMNS                           
         MVI   PC.POLEFT,C'L'      SET INPUT COLUMNS                            
         MVI   PC.POC1,C'C'                                                     
         MVI   PC.PORIGHT,C'R'                                                  
         MVI   BOXREQ,C'O'                                                      
         GOTO1 VPRINTER            OPEN BOX                                     
*                                                                               
         MVC   POREC,IPOREC                                                     
         MVC   POACT,IPOACT                                                     
         GOTO1 VPRINTER            FIRST TITLE LINE                             
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            UNDERLINE                                    
*                                                                               
         MVI   PC.POC1,C'C'                                                     
         MVI   PC.POC2,C'C'                                                     
         MVI   PC.POC3,C'C'                                                     
         MVI   PC.POC4,C'C'                                                     
         MVC   POCHA,IPOCHA                                                     
         MVC   POADD,IPOADD                                                     
         MVC   PODEL,IPODEL                                                     
         MVC   PORES,IPORES                                                     
         GOTO1 VPRINTER            SET NEW COLUMNS                              
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            UNDERLINE                                    
         DROP  PC                                                               
*                                                                               
         L     R2,AADDTAB                                                       
         USING ADDTABD,R2                                                       
SRTT02   CLI   ADFILENM,255        END OF TABLE                                 
         BE    SRTT06                                                           
         OC    ADBUCS,ADBUCS       ANYTHING TO REPORT FOR THIS TYPE?            
         BZ    SRTT04                                                           
*                                                                               
         MVC   POREC,ADFILENM                                                   
         CURED ADBADD,(8,POADD),0,ZERO=NOBLANK                                  
         CURED ADBCHG,(8,POCHA),0,ZERO=NOBLANK                                  
         CURED ADBDEL,(8,PODEL),0,ZERO=NOBLANK                                  
         CURED ADBRES,(8,PORES),0,ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
*                                                                               
SRTT04   AHI   R2,ADDTABLQ                                                      
         B     SRTT02                                                           
         DROP  R2                                                               
*                                                                               
SRTT06   MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER            CLOSE BOX                                    
         B     EXITOK                                                           
*                                                                               
STITLE   DC    CL(L'TITLE)'Control System Sort Summary'                         
IPOREC   DC    CL(L'POREC)'Record Type'                                         
IPOACT   DC    CL(L'POACT)'Actions'                                             
IPOCHA   DC    CL(L'POCHA)'  Change'                                            
IPOADD   DC    CL(L'POADD)'     Add'                                            
IPODEL   DC    CL(L'PODEL)' Restore'                                            
IPORES   DC    CL(L'PORES)'  Delete'                                            
*                                                                               
PLINED   DSECT                                                                  
         ORG   PLINED                                                           
POLEFT   DS    C                                                                
         DS    X                                                                
POREC    DS    CL(L'ADFILENM)                                                   
         DS    X                                                                
POC1     DS    C                                                                
         DS    X                                                                
POACT    DS    0XL42                                                            
POADD    DS    CL8                                                              
         DS    X                                                                
POC2     DS    C                                                                
         DS    X                                                                
POCHA    DS    CL8                                                              
         DS    X                                                                
POC3     DS    C                                                                
         DS    X                                                                
PODEL    DS    CL8                                                              
         DS    X                                                                
POC4     DS    C                                                                
         DS    X                                                                
PORES    DS    CL8                                                              
         DS    X                                                                
PORIGHT  DS    X                                                                
         ORG                                                                    
*                                                                               
CTDFAR   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET NAME FOR PROG NUMBER                                            *         
* NTRY:  R5    = A(RECOVERY RECORD)                                   *         
***********************************************************************         
         USING RECDS,R2                                                         
CTPGN    NTR1  BASE=*,LABEL=*                                                   
         XC    LSTPROG,LSTPROG     CLEAR LAST PROGRAM                           
         MVC   LSTPNUM,RPRG        SAVE PROGRAM NUMBER                          
         CLI   RPRG,0                                                           
         BNE   CTPGN02                                                          
         MVC   LSTPNAM,WORKER                                                   
         B     EXITOK                                                           
*                                                                               
CTPGN02  ICM   R5,15,ACTSE                                                      
         BNZ   CTPGN04                                                          
*                                                                               
         L     R5,VSELIST                                                       
         LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         CLI   SESYS,X'0A'         GET CONTROL SYSTEM                           
         BE    CTPGN04                                                          
         BXLE  R5,RE,*-8                                                        
         DC    H'0'                DIE IF CONTROL NOT FOUND                     
*                                                                               
CTPGN04  ICM   R5,15,SEPGMS        R5 = A(PROGRAM TABLE)                        
         ST    R5,ACTPGMS                                                       
         LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         AHI   R5,6                                                             
         USING PGMLSTD,R5                                                       
         CLC   RPRG,PGMNUM         MATCH?                                       
         BE    VALPRNY                                                          
         BXLE  R5,RE,*-10                                                       
*                                                                               
         MVC   LSTPNAM,SPACES                                                   
         MVC   LSTPNAM+0(2),=CL2'X'''                                           
         GOTO1 VHEXOUT,DMCB,RPRG,LSTPNAM+2,1,0                                  
         MVI   LSTPNAM+4,C''''                                                  
         B     EXITOK                                                           
*                                                                               
VALPRNY  MVC   LSTPNAM,PGMNAME     GET NAME FOR PRINT                           
         MVC   LSTPNUM,PGMNUM      GET NUMBER FOR TEST                          
         B     EXITOK                                                           
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD TO ACCUMULATOR TABLES                                           *         
***********************************************************************         
CTCOUNT  NTR1  BASE=*,LABEL=*                                                   
         XC    AADDNTRY,AADDNTRY                                                
*                                                                               
         L     R2,AIOSORT                                                       
         USING RECDS,R2                                                         
         MVC   BYTE,RFILTY                                                      
         CLI   RFILTY,GENFILQ      SPECIAL FOR GENFIL - USE GENDIR              
         BNE   *+8                                                              
         MVI   BYTE,GENDIRQ                                                     
         XR    R1,R1                                                            
*                                                                               
         L     R3,AADDTAB                                                       
         USING ADDTABD,R3                                                       
         LHI   RF,ADDTABLQ                                                      
*                                                                               
CTC02    CLI   ADFTYPE,0           RECORD NOT KNOWN                             
         BE    CTC06                                                            
         CLC   ADFTYPE,BYTE        MATCH FILE TYPE?                             
         BNE   CTC04               NO                                           
*                                                                               
         IC    R1,ADMTCHL          MATCH START OF KEY?                          
         BCTR  R1,0                                                             
         EX    R1,CTCCLC                                                        
         BE    CTC06               YES                                          
*                                                                               
CTC04    BXH   R3,RF,CTC02         NEXT ENTRY IN TABLE                          
         DC    H'0'                                                             
*                                                                               
CTCCLC   CLC   ADMTCHC(0),RCVFRST                                               
*                                                                               
CTC06    CLI   MODE,MODEERR                                                     
         BE    EXITL               IGNORE BAD RECORDS                           
         IC    R1,MODE                                                          
         BCTR  R1,0                                                             
         MHI   R1,L'ADBADD         R1 = A(INDEX INTO ACCUMULATOR TABLE)         
*                                                                               
         ST    R3,AADDNTRY         SAVE A(ENTRY)                                
*                                                                               
         LA    RF,ADBUCS(R1)       UPDATE COUNT OF ENTRIES                      
         ICM   R0,15,0(RF)                                                      
         AHI   R0,1                                                             
         STCM  R0,15,0(RF)                                                      
*                                                                               
         L     R3,AADDTOTS                                                      
         LA    RF,ADBUCS(R1)       UPDATE TOTALS COUNT                          
         ICM   R0,15,0(RF)                                                      
         AHI   R0,1                                                             
         STCM  R0,15,0(RF)                                                      
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ACCESS RECORD KEY DETAIL                                            *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
ACCESSR  NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS   CTFILE KEY                                    
         USING CT5REC,R2                                                        
         L     RF,AOUTFORM                                                      
         LH    R3,OUTKEY-OUTFORM(RF)                                            
         LA    R3,PBLOCK(R3)                                                    
         MVC   0(L'PDHDR,R3),HALFID                                             
         LA    RF,L'PDHDR+1(R3)                                                 
         MVC   0(L'CT5KALPH,RF),CT5KALPH                                        
*                                                                               
         MVC   HALF,CT5KALPH       OUTPUT AGENCY NAME                           
         BRAS  RE,GETAGYID                                                      
         MVI   10(RF),C'<'                                                      
         MVC   11(10,RF),WORK+2                                                 
         AHI   RF,21                                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'>'                                                       
*                                                                               
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOCK RECORD KEY DETAIL                                              *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
LOCKRR   NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS   CTFILE KEY                                    
         USING CT8REC,R2                                                        
         L     RF,AOUTFORM                                                      
         LH    R3,OUTKEY-OUTFORM(RF)                                            
         LA    R3,PBLOCK(R3)                                                    
*                                                                               
         LR    RF,R3                                                            
         MVC   0(L'PDHDR,RF),HALFID        ALPHA ID                             
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CT8KAG,RF),CT8KAG                                            
*                                                                               
         MVC   HALF,CT8KAG         OUTPUT AGENCY NAME                           
         BRAS  RE,GETAGYID                                                      
         MVI   4(RF),C'<'                                                       
         MVC   5(10,RF),WORK+2                                                  
         AHI   RF,16                                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'>'                                                       
*                                                                               
         LA    R4,L'PHKEY/2(R3)                                                 
*                                                                               
         L     R1,VSELIST                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SESYS,CT8KSE        GET SE SYSTEM                                
         BE    LKRR02                                                           
         BXLE  R1,RE,*-10                                                       
         XR    R1,R1               SE SYSTEM NOT FOUND                          
*                                                                               
LKRR02   MVC   0(L'PDHDR,R4),HSYSNAME                                           
         AHI   R4,L'PDHDR+1                                                     
*                                                                               
         LTR   R1,R1                                                            
         BZ    LKRR03              DEIS: WAS BAD BRANCH. FIXED MAR/2015         
         MVC   0(L'SENAME,R4),SENAME                                            
*        B     LKRR04                                                           
         DROP  R1                                                               
*                                                                               
LKRR03   DS    0H                                                               
         MVC   0(L'NOTDEF,R4),NOTDEF                                            
         AHI   R4,L'NOTDEF+1                                                    
         MVC   0(4,R4),=CL4'- X'''                                              
         MVI   6(R4),C''''                                                      
         GOTO1 VHEXOUT,DMCB,CT8KSE,4(R4),1,0                                    
*                                                                               
LKRR04   AHI   R3,L'P              NEXT LINE                                    
         LR    R4,R3                                                            
         MVC   0(L'PDHDR,R4),HDATE                                              
         GOTO1 VDATCON,DMCB,(8,CT8KDATE),(13,L'PDHDR+1(R4))                     
*                                                                               
         OC    CT8KTIME,CT8KTIME   TIME STAMP?                                  
         BZ    LKRR06              NO                                           
         MVC   0(L'PDHDR,R4),HDATTIM                                            
         AHI   R4,L'PDHDR+10                                                    
         MVC   FULL(3),CT8KTIME                                                 
         XC    FULL(3),EFFS                                                     
         MVI   FULL+3,X'0C'                                                     
         UNPK  DUB,FULL                                                         
         MVC   0(2,R4),DUB+1                                                    
         MVC   3(2,R4),DUB+3                                                    
         MVC   6(2,R4),DUB+5                                                    
         MVI   2(R4),C':'                                                       
         MVI   5(R4),C':'                                                       
*                                                                               
LKRR06   LA    R4,L'PHKEY/2(R3)                                                 
         MVC   0(L'PDHDR,R4),HRECTYPE                                           
         AHI   R4,L'PDHDR+1                                                     
         MVC   0(L'CT8KTYPE,R4),CT8KTYPE                                        
         AHI   R4,L'CT8KTYPE+1                                                  
         MVC   0(4,R4),=CL4'- X'''                                              
         MVI   8(R4),C''''                                                      
         GOTO1 VHEXOUT,DMCB,CT8KTYPE,4(R4),L'CT8KTYPE                           
*                                                                               
         AHI   R3,L'P                                                           
         LR    R4,R3                                                            
         MVC   0(L'PDHDR,R4),HLOCKKEY                                           
         AHI   R4,L'PDHDR+1                                                     
         MVC   0(L'CT8KKEY,R4),CT8KKEY                                          
         GOTO1 VHEXOUT,DMCB,CT8KKEY,L'CT8KKEY+2(R4),L'CT8KKEY,0                 
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CURRENCY RECORDS KEY DETAIL                                         *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
CURRENR  NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         USING GCURD,R2                                                         
         L     RF,AOUTFORM                                                      
         LH    R3,OUTKEY-OUTFORM(RF)                                            
         LA    R3,PBLOCK(R3)                                                    
*                                                                               
         OC    GCKAGY,GCKAGY       AGENCY OVERRIDE SET?                         
         BZ    CURR02              NO                                           
         LR    RF,R3                                                            
         MVC   0(L'PDHDR,RF),HAGYID                                             
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'GCKAGY,RF),GCKAGY                                            
         AHI   R3,L'P                                                           
*                                                                               
CURR02   LR    RF,R3                                                            
         MVC   0(L'PDHDR,RF),HCURENC                                            
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'GCKCURR,RF),GCKCURR                                          
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
HCURENC  DC    CL15'Currency       '                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT TYPE KEY DETAIL                                              *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
OUTPUTR  NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         USING CTOREC,R2                                                        
         L     RF,AOUTFORM                                                      
         LH    R6,OUTKEY-OUTFORM(RF)                                            
         LA    R6,PBLOCK(R6)                                                    
*                                                                               
         LR    RF,R6               OUTPUT TYPE                                  
         MVC   0(L'PDHDR,RF),HOUTTYPE                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTOKID,RF),CTOKID                                            
         AHI   R6,L'P                                                           
*                                                                               
         LR    RF,R6               SUB NUMBER                                   
         MVC   0(L'PDHDR,RF),HSUBNUM                                            
         AHI   RF,L'PDHDR+1                                                     
         CURED CTOKSUB,(3,(RF)),0,ALIGN=LEFT,ZERO=NOBLANK                       
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LIBRARY RECORD                                                      *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
LIBRECR  NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         USING CTLREC,R2                                                        
         L     RF,AOUTFORM                                                      
         LH    R3,OUTKEY-OUTFORM(RF)                                            
         LA    R3,PBLOCK(R3)                                                    
*                                                                               
         LR    RF,R3                                                            
         MVC   0(L'PDHDR,RF),HJCLBK                                             
         CLI   CTLKTYP,C'J'       JCL BOOK                                      
         BE    LIBR02                                                           
         MVC   0(L'PDHDR,RF),HLIBBK                                             
         CLI   CTLKSCR,C' '       SUB-TYPE (SCRIPT/EXTRACT)                     
         BNH   LIBR02                                                           
         MVC   0(L'PDHDR,RF),HSCRBK                                             
         CLI   CTLKSCR,CTLKSCRQ   SCRIPT?                                       
         BE    LIBR02                                                           
         MVC   0(L'PDHDR,RF),HXTRBK                                             
         CLI   CTLKSCR,CTLKXTRQ   XTRACT?                                       
         BE    LIBR02                                                           
         MVC   0(2,RF),CTLKTYP                                                  
         MVC   2(5,RF),=CL5' Book'                                              
*                                                                               
LIBR02   AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTLKNAME,RF),CTLKNAME                                        
*                                                                               
         LA    RF,L'PH1KEY/2(R3)   LINE NUMBER                                  
         MVC   0(L'PDHDR,RF),HLINNUM                                            
         AHI   RF,L'PDHDR+1                                                     
         CURED CTLKSUB,(3,(RF)),0,ALIGN=LEFT,ZERO=NOBLANK                       
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SYSTEM LIST RECORD                                                  *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
         USING CTWREC,R2                                                        
SYSLSTR  NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         L     RF,AOUTFORM        POSITION ON PAGE                              
         LH    R6,OUTKEY-OUTFORM(RF)                                            
         LA    R6,PBLOCK(R6)                                                    
*                                                                               
         OC    CTWKAGY,CTWKAGY     ANY AGENCY ALPHA SET?                        
         BZ    SYSL02              NO                                           
         LR    RF,R6                                                            
         MVC   0(L'PDHDR,RF),HALFID                                             
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTWKAGY,RF),CTWKAGY                                          
*                                                                               
         MVC   HALF,CTWKAGY        OUTPUT AGENCY NAME                           
         BRAS  RE,GETAGYID                                                      
         MVI   10(RF),C'<'                                                      
         MVC   11(10,RF),WORK+2                                                 
         AHI   RF,21                                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'>'                                                       
*                                                                               
         AHI   R6,L'P                                                           
*                                                                               
SYSL02   LR    RF,R6                                                            
         MVC   0(L'PDHDR,RF),HLSTTYPE                                           
         AHI   RF,L'PDHDR+1                                                     
         LA    R1,CWTTBL                                                        
*                                                                               
SYSL04   CLI   0(R1),0                                                          
         BE    SYSL06                                                           
         CLC   CTWKREC,0(R1)                                                    
         BE    SYSL08                                                           
         AHI   R1,CWTTBLL                                                       
         B     SYSL04                                                           
*                                                                               
SYSL06   MVC   0(L'NOTDEF,RF),NOTDEF                                            
         AHI   RF,L'NOTDEF+1                                                    
         MVI   0(RF),C'<'                                                       
         MVC   0(L'CTWKREC,RF),CTWKREC                                          
         MVI   L'CTWKREC(RF),C'>'                                               
         B     SYSL10                                                           
*                                                                               
SYSL08   MVC   0(8,RF),1(R1)                                                    
*                                                                               
SYSL10   AHI   R6,L'P                                                           
         LR    RF,R6               LINE ID                                      
         MVC   0(L'PDHDR,RF),HLINEID                                            
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTWKID,RF),CTWKID                                            
         AHI   R6,L'P                                                           
*                                                                               
         LR    RF,R6               SUB NUMBER                                   
         MVC   0(L'PDHDR,RF),HSUBNUM                                            
         AHI   RF,L'PDHDR+1                                                     
         CURED CTWKSUB,(6,0(RF)),0,ALIGN=LEFT,ZERO=NOBLANK                      
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
CWTTBLL  EQU   16                                                               
CWTTBL   DC    C'L',CL15'Line List      '                                       
         DC    C'I',CL15'User Id List   '                                       
         DC    C'S',CL15'System List    '                                       
         DC    C'P',CL15'PTS List       '                                       
         DC    C'R',CL15'Printer List   '                                       
         DC    C'G',CL15'Id Group List  '                                       
         DC    C'A',CL15'Agency List    '                                       
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* TERMINAL RECORDS                                                    *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
TERMSRR  NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         USING CTTREC,R2                                                        
         L     RF,AOUTFORM                                                      
         LH    R6,OUTKEY-OUTFORM(RF)                                            
         LA    R6,PBLOCK(R6)                                                    
*                                                                               
         LR    R3,R6                                                            
         MVC   0(L'PDHDR,R3),HVTAML                                             
         AHI   R3,L'PDHDR+1                                                     
*                                                                               
         OC    CTTKTID(16),CTTKTID PASSIVE?                                     
         BZ    *+14                YES                                          
         MVC   0(L'CTTKTID,R3),CTTKTID                                          
         B     RRTE02                                                           
*                                                                               
         GOTO1 VHEXOUT,PLIST,CTTKTID+8,11(R3),2,0                               
         MVI   10(R3),C'<'                                                      
         MVI   15(R3),C'>'                                                      
*                                                                               
RRTE02   LA    R1,CTTDATA                                                       
         USING CTPASD,R1                                                        
         XR    RF,RF                                                            
RRTE04   CLI   CTPASEL,0          EOR                                           
         BE    RRTE10                                                           
         CLI   CTPASEL,CTPASELQ   POINTER ELEMENT                               
         BE    RRTE06                                                           
         IC    RF,CTPASLEN                                                      
         BXH   R1,RF,RRTE04                                                     
*                                                                               
RRTE06   CLI   CTPASLEN,4         PASSIVE POINTER ELEMENT                       
         BNE   RRTE08             NO                                            
         GOTO1 VHEXOUT,PLIST,CTPASDTA,11(R3),2,0                                
         MVI   10(R3),C'<'                                                      
         MVI   15(R3),C'>'                                                      
         B     RRTE10                                                           
*                                                                               
RRTE08   IC    RF,CTPASLEN        MOVE IN LUID                                  
         AHI   RF,-(CTPASDTA-CTPASD+1)                                          
         MVC   0(0,R3),CTPASDTA                                                 
         EX    RF,*-6                                                           
*                                                                               
RRTE10   OC    CTTKTID(16),CTTKTID PASSIVE?                                     
         BNZ   RRTE12              NO                                           
         MVC   20(L'PASSIVE,R3),PASSIVE                                         
         AHI   R6,L'P                                                           
         B     RRTE14                                                           
*                                                                               
RRTE12   AHI   R6,L'P                                                           
         LR    RF,R6                                                            
         OC    CTTKPASS,CTTKPASS   PASSWORD?                                    
         BZ    RRTE14              NO                                           
         MVC   0(L'PDHDR,RF),HPASSWD                                            
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTTKPASS,RF),CTTKPASS                                        
*                                                                               
RRTE14   TM    CTTSTAT,X'04'       PRINTER?                                     
         BZ    RRTE16              NO                                           
*                                                                               
         AHI   R6,L'P                                                           
         LR    RF,R6               SET AUTOMODE INFO                            
         MVC   0(L'PDHDR,RF),HPRTAUTO                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(4,RF),NO4                                                      
         TM    CTTSTAT,X'08'                                                    
         BZ    *+10                                                             
         MVC   0(4,RF),YES4                                                     
*                                                                               
RRTE16   BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PERSONAL AUTH RECORD                                                *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
PAUTHRR  NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         USING CT0REC,R2                                                        
         L     RF,AOUTFORM        POSITION ON PAGE                              
         LH    R6,OUTKEY-OUTFORM(RF)                                            
         LA    R6,PBLOCK(R6)                                                    
*                                                                               
         LR    RF,R6                                                            
         MVC   0(L'PDHDR,RF),HAGYID                                             
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CT0KAGY,RF),CT0KAGY                                          
         AHI   R6,L'P                                                           
*                                                                               
         OC    CT0KEYS(20),CT0KEYS PASSIVE?                                     
         BNZ   PAUT02              NO                                           
*                                                                               
         LR    RF,R6                                                            
         MVC   0(L'PDHDR,RF),HPRSAUTH                                           
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VHEXOUT,PLIST,CT0KNUM,(RF),L'CT0KNUM,0                           
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
PAUT02   OC    CT0KEYS(12),CT0KEYS PASSWORD?                                    
         BNZ   PAUT04              NO                                           
         LR    RF,R6                                                            
         MVC   0(L'PDHDR,RF),HPASSWD                                            
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CT0KCODE,RF),CT0KCODE                                        
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
PAUT04   LR    RF,R6                                                            
         MVC   0(L'PDHDR,RF),HOFFCODE                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CT0KOFFC,RF),CT0KOFFC                                        
         AHI   R6,L'P                                                           
*                                                                               
         CLI   CT0KFI,C' '                                                      
         BNH   PAUT06                                                           
         MVC   0(L'PDHDR,RF),HFSTINIT                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CT0KFI,RF),CT0KFI                                            
         AHI   R6,L'P                                                           
*                                                                               
PAUT06   CLI   CT0KMI,C' '                                                      
         BNH   PAUT08                                                           
         MVC   0(L'PDHDR,RF),HMIDINIT                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CT0KMI,RF),CT0KMI                                            
         AHI   R6,L'P                                                           
*                                                                               
PAUT08   MVC   0(L'PDHDR,RF),HSURNAME                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CT0KLAST,RF),CT0KLAST                                        
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ID RECORDS                                                          *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
IDRECSR  NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         USING CTIREC,R2                                                        
         L     RF,AOUTFORM                                                      
         LH    R3,OUTKEY-OUTFORM(RF)                                            
         LA    R3,PBLOCK(R3)                                                    
*                                                                               
         MVC   0(L'PDHDR,R3),HIDNAM                                             
         AHI   R3,L'PDHDR+1                                                     
*                                                                               
         OC    CTIKID(8),CTIKID    PASSIVE?                                     
         BZ    *+14                YES                                          
         MVC   0(L'CTIKID,R3),CTIKID                                            
         B     IDR02                                                            
*                                                                               
         GOTO1 VHEXOUT,PLIST,CTIKID+8,11(R3),2,0                                
         MVI   10(R3),C'<'                                                      
         MVI   15(R3),C'>'                                                      
*                                                                               
IDR02    LA    R1,CTIDATA                                                       
         USING CTDSCD,R1                                                        
         XR    RF,RF                                                            
IDR04    CLI   CTDSCEL,0          EOR                                           
         BE    IDR10                                                            
         CLI   CTDSCEL,CTDSCELQ   POINTER ELEMENT                               
         BE    IDR06                                                            
         IC    RF,CTDSCLEN                                                      
         BXH   R1,RF,IDR04                                                      
*                                                                               
IDR06    CLI   CTDSCLEN,4         PASSIVE POINTER ELEMENT                       
         BNE   IDR08              NO                                            
         GOTO1 VHEXOUT,PLIST,CTDSC,11(R3),2,0                                   
         MVI   10(R3),C'<'                                                      
         MVI   15(R3),C'>'                                                      
         B     IDR10                                                            
*                                                                               
IDR08    IC    RF,CTDSCLEN         MOVE IN ID                                   
         AHI   RF,-(CTDSC-CTDSCD+1)                                             
         MVC   0(0,R3),CTDSC                                                    
         EX    RF,*-6                                                           
*                                                                               
IDR10    OC    CTIKID(8),CTIKID    PASSIVE?                                     
         BNZ   *+10                NO                                           
         MVC   20(L'PASSIVE,R3),PASSIVE                                         
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FAX RECORDS STANDARD OUTPUT                                         *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
FAXRECSR NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         USING CTFXREC,R2                                                       
         L     RF,AOUTFORM                                                      
         LH    R3,OUTKEY-OUTFORM(RF)                                            
         LA    R3,PBLOCK(R3)                                                    
*                                                                               
         MVC   0(L'PDHDR,R3),HALFID                                             
         LA    RF,L'PDHDR+1(R3)                                                 
         MVC   0(2,RF),CTFXAGY                                                  
*                                                                               
         MVC   HALF,CTFXAGY        OUTPUT AGENCY NAME                           
         BRAS  RE,GETAGYID                                                      
         MVI   10(RF),C'<'                                                      
         MVC   11(10,RF),WORK+2                                                 
         AHI   RF,21                                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'>'                                                       
*                                                                               
         AHI   R3,L'P                                                           
         MVC   0(L'PDHDR,R3),HFAXCODE                                           
         LA    RF,L'PDHDR+1(R3)                                                 
         MVC   0(L'CTFXCODE,RF),CTFXCODE                                        
*                                                                               
         OC    CTFXSUBC,CTFXSUBC                                                
         BZ    CTFX02                                                           
*                                                                               
         AHI   R3,L'P                                                           
         MVC   0(L'PDHDR,R3),HFAXSUB                                            
         LA    RF,L'PDHDR+1(R3)                                                 
         MVC   0(L'CTFXSUBC,RF),CTFXSUBC                                        
*                                                                               
CTFX02   BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
HFAXCODE DC    CL15'Fax Code       '                                            
HFAXSUB  DC    CL15'Fax Sub Code   '                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SAVED PROGRAM RECORD KEY STANDARD OUTPUT                            *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
C1RECSRR NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         USING CT01RECD,R2                                                      
         L     RF,AOUTFORM                                                      
         LH    R6,OUTKEY-OUTFORM(RF)                                            
         LA    R6,PBLOCK(R6)                                                    
         MVC   0(L'PDHDR,R6),HALFID                                             
         LA    RF,L'PDHDR+1(R6)                                                 
         MVC   0(L'CT01AGID,RF),CT01AGID                                        
*                                                                               
         MVC   HALF,CT01AGID       OUTPUT AGENCY NAME                           
         BRAS  RE,GETAGYID                                                      
         MVI   10(RF),C'<'                                                      
         MVC   11(10,RF),WORK+2                                                 
         AHI   RF,21                                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'>'                                                       
*                                                                               
         AHI   R6,L'P                                                           
         MVC   0(L'PDHDR,R6),HPHASE                                             
         LA    RF,L'PDHDR+1(R6)                                                 
         GOTO1 VHEXOUT,DMCB,CT01SYS,0(RF),3,0                                   
         LA    RF,L'PDHDR+1(R6)                                                 
         MVI   0(RF),C'T'                                                       
*                                                                               
         AHI   R6,L'P                                                           
         MVC   0(L'PDHDR,R6),HNAME                                              
         LA    RF,L'PDHDR+1(R6)                                                 
         MVC   0(L'CT01NAME,RF),CT01NAME                                        
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROFILE RECORD KEY STANDARD OUTPUT                                  *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
PRRECSR  NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         USING CTPREC,R2                                                        
         L     RF,AOUTFORM                                                      
         LH    R6,OUTKEY-OUTFORM(RF)                                            
         LA    R6,PBLOCK(R6)                                                    
         MVC   0(L'PDHDR,R6),HSYSNAME                                           
*                                                                               
         L     R1,VSYSLST                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SYSLSTD,R1                                                       
         CLC   CTPKSYS,SYSLNUM     MATCH SYSTEM NUMBER                          
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     PPR02                                                            
*                                                                               
         LA    RF,L'PDHDR+1(R6)                                                 
         MVC   0(L'SYSLNAME,RF),SYSLNAME                                        
         B     PPR04                                                            
         DROP  R1                                                               
*                                                                               
PPR02    LA    RF,L'PDHDR+1(R6)                                                 
         MVC   0(L'CTPKSYS,RF),CTPKSYS                                          
         MVI   6(RF),C'<'                                                       
         MVC   7(L'NOTDEF,RF),NOTDEF                                            
         MVI   7+L'NOTDEF(RF),C'>'                                              
*                                                                               
PPR04    AHI   R6,L'P                                                           
         LR    RF,R6                                                            
         MVC   0(L'PDHDR,RF),HPROGRAM                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTPKPROG,RF),CTPKPROG                                        
         AHI   R6,L'P                                                           
*                                                                               
         LR    RF,R6                                                            
         MVC   0(L'PDHDR,RF),HOFFOVER                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTPKORIG,RF),CTPKORIG                                        
         OC    CTPKORIG,CTPKORIG                                                
         BNE   *+10                                                             
         MVC   0(L'DEFAULT,RF),DEFAULT                                          
         AHI   R6,L'P                                                           
*                                                                               
         LR    RF,R6                                                            
         MVC   0(L'PDHDR,RF),HMEDIA                                             
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTPKSUB,RF),CTPKSUB                                          
         CLI   CTPKSYS,C'M'                                                     
         BNE   *+12                                                             
         CLI   CTPKSUB,C'A'                                                     
         BH    PPR06                                                            
         CURED CTPKSUB,(3,0(RF)),0,ALIGN=LEFT,ZERO=NOBLANK                      
*                                                                               
PPR06    BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* USER PROFILE STANDARD OUTPUT                                        *         
* NTRY: R2     = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
USERPRS  NTR1  BASE=*,LABEL=*                                                   
         AHI   R2,RCVFRST-RECDS                                                 
         USING CTUREC,R2                                                        
         L     RF,AOUTFORM                                                      
         LH    R3,OUTKEY-OUTFORM(RF)                                            
         LA    R3,PBLOCK(R3)                                                    
*                                                                               
         OC    CTUKAGY,CTUKAGY     FIELD DESCRIPTION?                           
         BZ    USP08               NO                                           
         CLI   CTUKAGY,X'50'       USER ID RATHER THAN AGENCY?                  
         BH    USP06               NO                                           
*                                                                               
         L     R5,AIOTEMP          TRY TO FIND USER ID                          
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CTUKAGY                                                  
         GOTO1 VDATAMGR,DMCB,(X'88',DMREAD),CTFILE,(R5),(R5)                    
         TM    8(R1),X'10'         TEST IF RECORD FOUND                         
         BO    USP06                                                            
*                                                                               
         MVC   0(L'PDHDR,R3),HUSRID                                             
*                                                                               
         LA    R5,CTIDATA          GET DESCRIPTION ELEMENT                      
         USING CTDSCD,R5                                                        
         XR    RF,RF                                                            
USP02    CLI   CTDSCEL,0                                                        
         BE    USP04                                                            
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+12                                                             
         IC    RF,CTDSCLEN                                                      
         BXH   R5,RF,USP02                                                      
*                                                                               
         IC    RF,CTDSCLEN                                                      
         AHI   RF,-(CTDSC-CTDSCD+1)                                             
         MVC   L'PDHDR+1(0,R3),CTDSC                                            
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         B     USP08                                                            
*                                                                               
USP04    GOTO1 VHEXOUT,DMCB,CTUKAGY,L'PDHDR+1(R3),L'CTUKAGY,0                   
         AHI   R3,L'P                                                           
         B     USP08                                                            
         DROP  R5                                                               
*                                                                               
USP06    MVC   0(L'PDHDR,R3),HAGYID    AGENCY ID                                
         MVC   L'PDHDR+1(L'CTUKAGY,R3),CTUKAGY                                  
         AHI   R3,L'P                                                           
*                                                                               
USP08    MVC   0(L'PDHDR,R3),HPROGRAM                                           
         MVC   L'PDHDR+1(L'CTUKSYS+L'CTUKPROG,R3),CTUKSYS                       
         LA    R4,L'PHKEY/2(R3)                                                 
         MVC   0(L'PDHDR,R4),HCNTRY                                             
         MVC   L'PDHDR+1(L'NOTDEF,R4),NOTDEF                                    
*                                                                               
         L     R1,VCTRYTAB         DISPLAY COUNTRY                              
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         CLC   CTRYCODE,CTUKLANG                                                
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     USP10                                                            
         MVC   L'PDHDR+1(L'CTRYNAM,R4),CTRYNAM                                  
         DROP  R1                                                               
*                                                                               
USP10    AHI   R3,L'P                                                           
         OC    CTUKMED+3(3),CTUKMED+3                                           
         BZ    USP16                                                            
         OC    CTUKMED+4(2),CTUKMED+4                                           
         BZ    USP18                                                            
*                                                                               
         MVC   0(L'PDHDR,R3),HWRITER                                            
         MVC   L'PDHDR+1(L'CTUKNAM,R3),CTUKNAM                                  
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
USP16    MVC   0(L'PDHDR,R3),HMEDIA                                             
         MVC   L'PDHDR+1(L'CTUKMED,R3),CTUKMED                                  
         CLI   CTUKMED,0                                                        
         BNE   *+10                                                             
         MVC   L'PDHDR+1(L'DEFAULT,R3),DEFAULT                                  
*                                                                               
         LA    R4,L'PHKEY/2(R3)                                                 
         MVC   0(L'PDHDR,R4),HCLIENT                                            
         AHI   R4,L'PDHDR+1                                                     
         MVC   0(L'CTUKCLT,R4),CTUKCLT                                          
         OC    CTUKCLT,CTUKCLT                                                  
         BNZ   *+10                                                             
         MVC   0(L'DEFAULT,R4),DEFAULT                                          
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
USP18    MVC   0(L'PDHDR,R3),HACCPAK                                            
         MVC   L'PDHDR+1(L'CTUKUNT+L'CTUKLDG,R3),CTUKUNT                        
         LA    RF,L'CTUKUNT+L'CTUKLDG+L'PDHDR+1(R3)                             
         CURED CTUKACT,(8,0(RF)),0,ALIGN=LEFT,ZERO=NOBLANK                      
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
HACCPAK  DC    CL15'Accpak         '                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* X'01' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT01ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY         SAVE A(ENTRY) FOR LATER                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,1          DESCRIPTION                                  
         BE    CT01E02                                                          
         CLI   ADRECNUM,71         FAX NUMBER                                   
         BE    CT01E04                                                          
         CLI   ADRECNUM,113        RFP GROUP HEADER                             
         BE    CT01E06                                                          
         CLI   ADRECNUM,73         PROGRAM DETAILS                              
         BE    CT01E18                                                          
         CLI   ADRECNUM,74         PROGRAM DETAILS                              
         BE    CT01E18                                                          
         CLI   ADRECNUM,75         PROGRAM DETAILS                              
         BE    CT01E18                                                          
         CLI   ADRECNUM,94         SCREEN FIELD INFORMATION                     
         BE    CT01E28                                                          
         DROP  RF                                                               
*                              *** DEFAULT IS RECORD ACTIVITY ELEMENT           
         USING CTACTD,R2                                                        
         MVC   X.PDHDR,HACTON      ACTIVITY                                     
         GOTO1 VDATCON,PLIST,(3,CTACTDT),(10,X.PDINF),0                         
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CT01DSCD,R2     *** SAVED PROGRAM DESCRIPTION ELEMENT            
CT01E02  MVC   X.PDHDR,HDESC       DESCRIPTION                                  
         AHI   R3,L'PDHDR+1                                                     
*                                                                               
         XR    R0,R0               R0 = LENGTH OF DATA                          
         IC    R0,CT01DLEN                                                      
         AHI   R0,-(CT01DESC-CT01DSCD)                                          
         LHI   RF,L'PDINF          RF = WIDTH OF COLUMN                         
         GOTO1 VCHOPPER,DMCB,((R0),CT01DESC),((RF),X.PDINF),(C'P',5)            
         ICM   R0,15,8(R1)                                                      
         MHI   R0,L'P                                                           
         AR    R3,R0                                                            
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTFX1EL,R2      *** FAX NUMBER ELEMENT                           
CT01E04  MVC   X.PDHDR,HNUM        FAX NUMBER                                   
         AHI   R3,L'PDHDR+1                                                     
*                                                                               
         XR    R1,R1               R1 = LENGTH OF DATA                          
         IC    R1,CTFX1LEN                                                      
         AHI   R1,-(CTFX1NUM-CTFX1EL+1)                                         
         MVC   X.PDDATA(0),CTFX1NUM                                             
         EX    R1,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING GRPHD,R2        *** RFP HEADER                                   
CT01E06  MVC   X.PDHDR,HMINIO      MINIO KEY                                    
         GOTO1 VHEXOUT,DMCB,GRPHMKEY,X.PDINF,L'GRPHMKEY,0                       
*                                                                               
         AHI   R3,L'P                                                           
         MVC   X.PDHDR,HDESC       DESCRIPTION                                  
         MVC   X.PDINF(L'GRPHDESC),GRPHDESC                                     
*                                                                               
         AHI   R3,L'P                                                           
         MVC   X.PDHDR,HFREQ       FREQUENCY                                    
         MVC   X.PDINF+0(1),GRPHFREQ                                            
         MVC   X.PDINF+1(3),=CL3' - '                                           
         MVC   X.PDINF+4(L'NOTDEF),NOTDEF                                       
*                                                                               
         LA    RF,FREQTAB                                                       
CT01E07  CLI   0(RF),255                                                        
         BE    CT01E08                                                          
         CLC   GRPHFREQ,0(RF)                                                   
         BE    *+12                                                             
         AHI   RF,L'FREQTAB                                                     
         B     CT01E07                                                          
*                                                                               
         MVC   X.PDINF(15),1(RF)                                                
*                                                                               
CT01E08  LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HOUTTYPE                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'GRPHOTYP,RF),GRPHOTYP                                        
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HDEST DESTINATION ID                               
         AHI   RF,L'PDHDR+1                                                     
         MVC   HALF,GRPHDEST                                                    
         BRAS  RE,GETUSRID                                                      
         MVC   0(10,RF),WORK+2                                                  
*                                                                               
         AHI   R3,L'P                                                           
         MVC   X.PDHDR,HNAME       NAME                                         
         MVC   X.PDINF(L'GRPHNAME),GRPHNAME                                     
*                                                                               
         OC    GRPHXFIL,GRPHXFIL   ATTACHED XFILE GROUP KEY                     
         BZ    CT01E10                                                          
         LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HXFGKEY                                            
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VHEXOUT,DMCB,GRPHXFIL,(RF),L'GRPHXFIL,0                          
*                                                                               
CT01E10  AHI   R3,L'P                                                           
         OC    GRPHNXTR,GRPHNXTR   NEXT RUN DATE                                
         BZ    CT01E12                                                          
         MVC   X.PDHDR,HNXTRUN                                                  
         GOTO1 VDATCON,DMCB,(6,GRPHNXTR),(21,X.PDINF),0                         
*                                                                               
CT01E12  OC    GRPHLSTR,GRPHLSTR                                                
         BZ    CT01E14                                                          
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HLSTRUN                                            
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VDATCON,DMCB,(6,GRPHLSTR),(21,(RF)),0                            
*                                                                               
CT01E14  OC    GRPHEND,GRPHEND                                                  
         BZ    CT01E16                                                          
         LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HENDRUN                                            
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VDATCON,DMCB,(6,GRPHEND),(21,(RF)),0                             
*                                                                               
CT01E16  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING ACPRGD,R2                                                        
CT01E18  MVC   X.PDHDR,HPTYPE      PROGRAM TYPE                                 
         LA    RF,L'PDHDR+1(R3)                                                 
*                                                                               
         LA    R1,PTYPTAB                                                       
CT01E20  CLI   0(R1),255                                                        
         BE    CT01E22                                                          
         CLC   ACPRGTYP,0(R1)                                                   
         BE    CT01E22                                                          
         AHI   R1,L'PTYPTAB                                                     
         B     CT01E20                                                          
*                                                                               
CT01E22  MVC   X.PDINF(19),0(R1)                                                
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HTEXTNO                                            
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VHEXOUT,DMCB,ACPRGDSC,(RF),2,0                                   
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HRATYP                                             
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'NOTDEF,RF),NOTDEF                                            
         MVC   L'NOTDEF+1(2,RF),ACPRGREC                                        
*                                                                               
         LA    R1,RAIPTAB                                                       
CT01E24  CLI   0(R1),255                                                        
         BE    CT01E26                                                          
         CLC   ACPRGREC(2),0(R1)                                                
         BE    *+12                                                             
         AHI   R1,L'RAIPTAB                                                     
         B     CT01E24                                                          
         MVC   0(14,RF),2(R1)                                                   
*                                                                               
CT01E26  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING FDRELD,R2                                                        
CT01E28  MVC   X.PDHDR,HFLDDATA    NFILE FIELD RECORD                           
         XR    R0,R0                                                            
         IC    R0,FDRLN                                                         
         GOTO1 VHEXOUT,DMCB,0(R2),X.PDINF,(R0),0                                
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
FREQTAB  DS    0CL16                                                            
         DC    C'W',CL15'Weekly'                                                
         DC    C'M',CL15'Monthly'                                               
         DC    C'Q',CL15'Quarterly'                                             
         DC    C'Y',CL15'Yearly'                                                
         DC    C'N',CL15'Not regular'                                           
         DC    X'FF'                                                            
*                                                                               
RAIPTAB  DS    0CL16                                                            
         DC    C'XX',CL14'Hex,Hex'                                              
         DC    C'XA',CL14'Hex,Alpha'                                            
         DC    C'XN',CL14'Hex,Number'                                           
         DC    C'AX',CL14'Alpha,Hex'                                            
         DC    C'AN',CL14'Alpha,Number'                                         
         DC    C'AA',CL14'Alpha,Alpha'                                          
         DC    C'NX',CL14'Number,Hex'                                           
         DC    C'NA',CL14'Number,Alpha'                                         
         DC    C'NN',CL14'Number,Number'                                        
         DC    X'FF'                                                            
*                                                                               
PTYPTAB  DS    0CL20                                                            
         DC    X'01',CL19'File Maintenance'                                     
         DC    X'02',CL19'Enquiry/Summary '                                     
         DC    X'03',CL19'Request/Spool   '                                     
         DC    X'04',CL19'Input           '                                     
         DC    X'05',CL19'Info/List       '                                     
         DC    X'06',CL19'Gencon/General  '                                     
         DC    X'FF',CL19'Undefined       '                                     
*                                                                               
HENDRUN  DC    CL15'End run date   '                                            
HPTYPE   DC    CL15'Program Type   '                                            
HRATYP   DC    CL15'Rec/Act Inputs '                                            
HTEXTNO  DC    CL15'Text Number    '                                            
         DROP  X,R2                                                             
         EJECT                                                                  
***********************************************************************         
* X'02' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT02ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         USING CTDSCD,R2                                                        
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY         SAVE A(ENTRY) FOR LATER                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,1          FIELD DATA                                   
         BE    CT02E04                                                          
         CLI   ADRECNUM,71         ATTENTION NAME                               
         BE    CT02E06                                                          
         CLI   ADRECNUM,73         ACTION INFORMATION                           
         BE    CT02E10                                                          
         CLI   ADRECNUM,74         ACTION INFORMATION                           
         BE    CT02E10                                                          
         CLI   ADRECNUM,75         ACTION INFORMATION                           
         BE    CT02E10                                                          
         CLI   ADRECNUM,114        DARE INITIALS ACTIVITY                       
         BE    CT02E08                                                          
         DROP  RF                                                               
*                              *** DEFAULT IS DESCRIPTION                       
         CLI   CTDSCLEN,4          NUMERIC?                                     
         BE    CT02E02             YES                                          
         MVC   X.PDHDR,HDESC                                                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,CTDSCLEN                                                      
         AHI   RF,-(CTDSC-CTDSCD+1)                                             
         MVC   X.PDINF(0),CTDSC    MOVE IN DESCRIPTION DATA                     
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
CT02E02  MVC   X.PDHDR,HIDNUM                                                   
         GOTO1 VHEXOUT,PLIST,CTDSC,X.PDINF,2,0                                  
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CT01FLDD,R2     *** PROGRAM SAVE ELEMENT                         
CT02E04  MVC   X.PDHDR,HIDSEQ      ID/SEQUENCE NUMBER                           
         GOTO1 VHEXOUT,PLIST,CT01ID,X.PDINF,1,0                                 
         MVI   X.PDINF+3,C'/'                                                   
         GOTO1 (RF),(R1),CT01SEQ,X.PDINF+4,1,0                                  
*                                                                               
         LA    R4,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R4),HFLDDATA      FIELD DATA                           
         AHI   R4,L'PDHDR+1                                                     
*                                                                               
         XR    R0,R0                                                            
         IC    R0,CT01FLEN                                                      
         AHI   R0,-(CT01TEXT-CT01FLDD)                                          
         LA    RF,X.PDDATA+L'PDDATA                                             
         SR    RF,R4               RF = WIDTH OF COLUMN REMAINING               
*                                                                               
         GOTO1 VCHOPPER,DMCB,((R0),CT01TEXT),((RF),(R4)),(C'P',5)               
         ICM   R0,15,8(R1)                                                      
         MHI   R0,L'P                                                           
         AR    R3,R0                                                            
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTFXATT,R2      *** FAX RECORD ATTENTION NAME                    
CT02E06  MVC   X.PDHDR,HNAME                                                    
         MVC   X.PDINF(L'CTFX2ATT),CTFX2ATT                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING DASAELD,R2      *** ACTIVITY ELEMENT                             
CT02E08  MVC   X.PDHDR,HACTON      ACTIVITY                                     
         GOTO1 VHEXOUT,DMCB,(R2),X.PDINF,DASALNQ,0                              
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
CT02E10  MVC   X.PDHDR,HNUM        ACTION NUMBER                                
         GOTO1 VHEXOUT,DMCB,(R2),X.PDINF,ACACTLNQ,0                             
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         DROP  X,R2                                                             
         EJECT                                                                  
***********************************************************************         
* X'03' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
* NTRY: R2 = A(ELEMENT)                                               *         
***********************************************************************         
CT03ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY         SAVE A(ENTRY) FOR LATER                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,71         FAX RECORD                                   
         BE    CT03E04                                                          
         CLI   ADRECNUM,114        DARE ASSIST RECORD                           
         BE    CT03E06                                                          
         CLI   ADRECNUM,97         SCREEN FIELD                                 
         BE    CT03E12                                                          
         CLI   ADRECNUM,73         RECORD DESCRIPTION                           
         BE    CT03E14                                                          
         CLI   ADRECNUM,74         RECORD DESCRIPTION                           
         BE    CT03E14                                                          
         CLI   ADRECNUM,75         RECORD DESCRIPTION                           
         BE    CT03E14                                                          
         DROP  RF                                                               
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
*                                                                               
         USING CTPASD,R2       *** PASSIVE POINTER                              
         CLI   CTPASLEN,4          NUMBER?                                      
         BH    CT03E02             NO                                           
         MVC   X.PDHDR,HNUM                                                     
         GOTO1 VHEXOUT,PLIST,CTPASDTA,X.PDINF,2,0                               
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
CT03E02  MVC   X.PDHDR,HNAME                                                    
         XR    RF,RF                                                            
         IC    RF,CTPASLEN                                                      
         AHI   RF,-3                                                            
         MVC   X.PDINF(0),CTPASDTA                                              
         EX    R1,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTFXMSG,R2          FAX RECORD ATTENTION NAME                    
CT03E04  MVC   X.PDHDR,HLINED                                                   
         CURED (B1,CTFX3LIN),(3,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK              
         LA    R4,X.PDINF                                                       
         AR    R4,R0                                                            
         MVI   0(R4),C'='                                                       
*                                                                               
         XR    RF,RF                                                            
         IC    RF,CTFX3LEN                                                      
         AHI   RF,-(CTFX3MSG-CTFXMSG+1)                                         
         MVC   1(0,R4),CTFX3MSG                                                 
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING DASELD,R2                                                        
CT03E06  MVC   X.PDHDR,HINITLS                                                  
         LA    RF,X.PDINF                                                       
         LA    R1,DASINITF                                                      
         LHI   R0,DASINIT#                                                      
*                                                                               
CT03E08  CLC   0(DASINITL,R1),SPACES                                            
         BNH   CT03E10                                                          
         MVC   0(DASINITL,RF),0(R1)                                             
         MVI   DASINITL(RF),C','                                                
         AHI   RF,DASINITL+1                                                    
*                                                                               
CT03E10  AHI   R1,DASINITL                                                      
         BCT   R0,CT03E08                                                       
         BCTR  RF,0                                                             
         CLI   0(RF),C','                                                       
         BNE   *+8                                                              
         MVI   0(RF),C' '                                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING FSRELD,R2           FIELD SCREEN INFO                            
CT03E12  MVC   X.PDHDR,HSCRINF                                                  
         XR    RF,RF                                                            
         IC    RF,FSRLN                                                         
         GOTO1 VHEXOUT,PLIST,(R2),X.PDINF,(RF),0                                
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING ACACTD,R2           FIELD SCREEN INFO                            
CT03E14  MVC   X.PDHDR,HACTINF                                                  
         XR    RF,RF                                                            
         IC    RF,ACACTLN                                                       
         GOTO1 VHEXOUT,PLIST,(R2),X.PDINF,(RF),0                                
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HLINED   DC    CL15'Line Details'                                               
HINITLS  DC    CL15'Initial List'                                               
HSCRINF  DC    CL15'Screen Info '                                               
HACTINF  DC    CL15'Action Info '                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* X'04' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT04ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY         SAVE A(ENTRY) FOR LATER                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,42         JCL BOOK                                     
         BE    CT04E02                                                          
         CLI   ADRECNUM,44         LIBRARY BOOK                                 
         BE    CT04E02                                                          
         CLI   ADRECNUM,71         FAX RETURN NUMBER                            
         BE    CT04E06                                                          
         CLI   ADRECNUM,73         LANGUAGE FOR REC/ACT                         
         BE    CT04E08                                                          
         CLI   ADRECNUM,74         LANGUAGE FOR REC/ACT                         
         BE    CT04E08                                                          
         CLI   ADRECNUM,75         LANGUAGE FOR REC/ACT                         
         BE    CT04E08                                                          
         CLI   ADRECNUM,82         INPUT FIELD                                  
         BE    CT04E10                                                          
*****    CLI   ADRECNUM,93         NFILE RECORD STUFF                           
*  TO    BE    CT04E??                                                          
*        CLI   ADRECNUM,94         NFILE RECORD STUFF                           
*        BE    CT04E??                                                          
*  DO    CLI   ADRECNUM,98         NFILE RECORD STUFF                           
*****    BE    CT04E??                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTINDD,R2                                                        
CT04E02  MVC   X.PDHDR,HHIGHSEQ                                                 
         XR    R0,R0                                                            
         ICM   R0,1,CTINDHI                                                     
         CURED (R0),(3,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
         AHI   R3,L'P                                                           
         MVC   X.PDHDR,HINDNTRY                                                 
         GOTO1 VHEXOUT,DMCB,CTINDEX,WORK,L'CTINDEX,0                            
*                                                                               
         LA    RE,WORK                                                          
         LA    RF,L'PDHDR+1(R3)                                                 
         LHI   R0,L'CTINDEX*2                                                   
*                                                                               
CT04E04  LR    R1,R0                                                            
         CHI   R1,L'PDINF                                                       
         BNH   *+8                                                              
         LHI   R1,L'PDINF                                                       
         BCTR  R1,0                                                             
         MVC   X.PDINF(0),0(RE)                                                 
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
*                                                                               
         AHI   R3,L'P              NEXT PRINT LINE                              
         AR    RE,R1                                                            
         SR    R0,R1               ANY MORE DATA?                               
         BP    CT04E04             YES                                          
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTFXTEL,R2      *** FAX RETURN NUMBER                            
CT04E06  MVC   X.PDHDR,HFRETN                                                   
         XR    RF,RF                                                            
         IC    RF,CTFX4LEN                                                      
         AHI   RF,-3                                                            
         MVC   X.PDINF(0),CTFX4TEL                                              
         EX    RF,*-6                                                           
         AHI   R1,1                                                             
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING ACLNGD,R2       *** REC/ACT LANGUAGE                             
CT04E08  MVC   X.PDHDR,HFLNGA                                                   
         GOTO1 VHEXOUT,PLIST,(R2),X.PDINF,ACLNGLNQ,0                            
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING DEINPD,R2       *** INPUT FIELD                                  
CT04E10  MVC   X.PDHDR,HFINPFLD                                                 
         GOTO1 VHEXOUT,PLIST,DEINPOP,X.PDINF,1,0                                
         MVI   X.PDINF+2,C'='                                                   
         XR    RF,RF                                                            
         IC    RF,DEINPLEN                                                      
         AHI   RF,-4                                                            
         MVC   X.PDINF+3(0),DEINPTXT                                            
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HFRETN   DC    CL15'Fax Return #   '                                            
HFLNGA   DC    CL15'Language       '                                            
HFINPFLD DC    CL15'Input Field    '                                            
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'06' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT06ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY         SAVE A(ENTRY) FOR LATER                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,73         PERSON TITLE                                 
         BE    CT06E02                                                          
         CLI   ADRECNUM,74         PERSON TITLE                                 
         BE    CT06E02                                                          
         CLI   ADRECNUM,75         PERSON TITLE                                 
         BE    CT06E02                                                          
         CLI   ADFTYPE,CTFILEQ     AGENCY DEFAULT                               
         BE    CT06E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
CT06E02  GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
*                                                                               
         USING CTAGYD,R2                                                        
CT06E04  MVC   X.PDHDR,HALFID                                                   
         MVC   X.PDINF(L'CTAGYID),CTAGYID                                       
*                                                                               
         MVC   HALF,CTAGYID        OUTPUT AGENCY NAME                           
         BRAS  RE,GETAGYID                                                      
         MVI   X.PDINF+5,C'<'                                                   
         MVC   X.PDINF+6(10),WORK+2                                             
         LA    R4,X.PDINF+16                                                    
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'>'                                                       
*                                                                               
         LA    R4,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R4),HAGYLANG                                           
         AHI   R4,L'PDHDR+1                                                     
*                                                                               
         L     R1,VLANGTAB                                                      
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING LANGTABD,R1                                                      
         CLC   LANGCODE,CTAGYLNG   MATCH LANGUAGE CODE                          
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     CT06E06                                                          
*                                                                               
         MVC   0(L'LANGFUL,R4),LANGFUL                                          
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R1                                                               
*                                                                               
CT06E06  MVC   0(L'NOTDEF,R4),NOTDEF                                            
         MVI   L'NOTDEF+1(R4),C'-' UNKNOWN LANGAGE - EDIT IT OUT                
         LA    RF,L'NOTDEF+2(R4)                                                
         CURED (B1,CTAGYLNG),(3,(RF)),0,ALIGN=LEFT,ZERO=NOBLANK                 
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HAGYLANG DC    CL15'Agency Language'                                            
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'10' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT10ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,1          FILTER                                       
         BE    CT10E02                                                          
         CLI   ADRECNUM,77         EXCHANGE RATE                                
         BE    CT10E04                                                          
         CLI   ADRECNUM,78         MESSAGE                                      
         BE    CT10E06                                                          
         CLI   ADRECNUM,80         NARRATIVE                                    
         BE    CT10E08                                                          
         CLI   ADRECNUM,81         HELP HEADING                                 
         BE    CT10E10                                                          
         CLI   ADRECNUM,82         RECORD START (DRIVER)                        
         BE    CT10E12                                                          
         CLI   ADRECNUM,84         BROADCAST FILTER                             
         BE    CT10E14                                                          
         CLI   ADRECNUM,86         NIC PARAMETERS                               
         BE    CT10E22                                                          
         CLI   ADRECNUM,87         NIC PARAMETERS                               
         BE    CT10E22                                                          
         CLI   ADRECNUM,88         NIC PARAMETERS                               
         BE    CT10E22                                                          
         CLI   ADRECNUM,89         NIC PARAMETERS                               
         BE    CT10E22                                                          
         CLI   ADRECNUM,90         ROLE DESCRIPTION PARAMETERS                  
         BE    CT10E22                                                          
         CLI   ADRECNUM,91         NIC PARAMETERS                               
         BE    CT10E22                                                          
         CLI   ADRECNUM,101        NAME TEXT                                    
         BE    CT10E24                                                          
         CLI   ADRECNUM,115        SUBJECT TEXT                                 
         BE    CT10E26                                                          
         CLI   ADRECNUM,113        RFP                                          
         BE    CT10E28                                                          
         CLI   ADFTYPE,CTFILEQ     DEFAULT FOR CTFILE                           
         BE    CT10E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CT01FLTD,R2                                                      
CT10E02  MVC   X.PDHDR,HFILTER                                                  
         MVC   X.PDINF(L'CT01FILT),CT01FILT                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING GEXEL,R2        *** EXCHANGE RATE                                
CT10E04  MVC   X.PDHDR,HEXRATE                                                  
         LA    R1,GEXRATE          EDIT EXCHANGE RATE                           
         BRAS  RE,EDITNUM                                                       
         MVC   X.PDINF(12),WORK                                                 
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HRTYPE                                             
         AHI   RF,L'PDHDR+1                                                     
         TM    GEXFLAG,GEXFTPL                                                  
         BNO   *+10                                                             
         MVC   0(6,RF),=CL6'FT+N'                                               
         TM    GEXFLAG,GEXFTMI                                                  
         BNO   *+10                                                             
         MVC   0(6,RF),=CL6'FT-N'                                               
         TM    GEXFLAG,GEXFTPL+GEXFTPC                                          
         BNO   *+10                                                             
         MVC   0(6,RF),=CL6'FT+N%'                                              
         TM    GEXFLAG,GEXFTMI+GEXFTPC                                          
         BNO   *+10                                                             
         MVC   0(6,RF),=CL6'FT-N%'                                              
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING GMSGEL,R2       *** SHORT MESSAGE                                
CT10E06  MVC   X.PDHDR,HMESSAGE                                                 
         XR    R0,R0                                                            
         IC    R0,GMSGELL                                                       
         AHI   R0,-(GMSGTXT-GMSGD)                                              
         LHI   RF,L'PDINF                                                       
         LA    R3,X.PDINF                                                       
         GOTO1 VCHOPPER,DMCB,((R0),GMSGTXT),((RF),(R3)),(C'P',3)                
         ICM   R0,15,8(R1)                                                      
         MHI   R0,L'P                                                           
         AR    R3,R0                                                            
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING GNDATD,R2       *** NARRATIVE                                    
CT10E08  MVC   X.PDHDR,HSNUM                                                    
         CURED (B1,GNDATSEQ),(3,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK              
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HHDR                                               
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'GNDATHDR,RF),GNDATHDR                                        
         AHI   R3,L'P                                                           
*                                                                               
         XR    R0,R0               DATA PORTION                                 
         IC    R0,GNDATLEN                                                      
         AHI   R0,-(GNDATA-GNDATD)                                              
         LHI   RF,L'PDINF                                                       
         GOTO1 VCHOPPER,DMCB,((R0),GNDATA),((RF),X.PDINF),(C'P',3)              
         ICM   R0,15,8(R1)                                                      
         MHI   R0,L'P                                                           
         AR    R3,R0                                                            
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING HV1HEDD,R2      *** HELP (V1)                                    
CT10E10  MVC   X.PDHDR,HHDNG                                                    
         XR    R1,R1                                                            
         IC    R1,HV1HEDTL                                                      
         BCTR  R1,0                                                             
         MVC   X.PDINF(0),HV1HEDTX                                              
         EX    R1,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING DERECD,R2       *** DRIVER DICTIONARY                            
CT10E12  MVC   X.PDHDR,HRECSTMT                                                 
         XR    R1,R1                                                            
         IC    R1,DERECLEN                                                      
         BCTR  R1,0                                                             
         MVC   X.PDINF(0),DERECLAB                                              
         EX    R1,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING BRDFLTD,R2      *** BROADCAST FILTER                             
CT10E14  MVC   0(L'PDHDR,R3),HFACPAK                                            
*                                                                               
         XR    RF,RF               FACPAK=                                      
         ICM   RF,1,BRDFAPPL                                                    
         BNZ   *+14                                                             
         MVC   X.PDINF(L'ALL4),ALL4                                             
         B     CT10E16                                                          
*                                                                               
         MHI   RF,L'FACITAB                                                     
         A     RF,AFACITAB                                                      
         MVC   X.PDINF(L'FACISN4),FACISN4-FACITABD(RF)                          
*                                                                               
CT10E16  OC    BRDFSTDT,BRDFSTDT   START/END DATES                              
         BZ    CT10E18                                                          
         LA    R4,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R4),HDATE                                              
         AHI   R4,L'PDHDR+1                                                     
*                                                                               
         LA    RF,BRDFSTDT                                                      
         ST    RF,DMCB             A(DATES) -- START, AND MAYBE END TOO         
         MVI   DMCB,2              COMPRESSED INPUT TYPE                        
         CLC   BRDFSTDT,BRDFENDT   SAME START AND END DATES?                    
         BE    *+8                                                              
         OI    DMCB,X'10'          NO -- PASS BOTH DATES                        
         GOTO1 VDATCON,DMCB,,(17,0(R4)),0                                       
*                                                                               
CT10E18  OC    BRDFSTTM,BRDFSTTM   ANY TIMES IN RECORD?                         
         BZ    CT10E20                                                          
*                                                                               
         LA    R4,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,R4),HTIMERG                                            
         AHI   R4,L'PDHDR+1                                                     
*                                                                               
         XR    RE,RE                                                            
         IC    RE,BRDFSTTM         START HOUR                                   
         MHI   RE,100                                                           
         XR    RF,RF                                                            
         IC    RF,BRDFSTTM+1       START MINUTES                                
         AR    RE,RF                                                            
         STH   RE,FULL             START TIME (MILITARY)                        
*                                                                               
         XR    RE,RE                                                            
         IC    RE,BRDFENTM         END HOUR                                     
         MHI   RE,100                                                           
         XR    RF,RF                                                            
         IC    RF,BRDFENTM+1       END MINUTES                                  
         AR    RE,RF                                                            
         STH   RE,FULL+2           END TIME (MILITARY)                          
         GOTO1 VUNTIME,DMCB,FULL,0(R4)                                          
*                                                                               
CT10E20  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING GFNICD,R2                                                        
CT10E22  GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
*                                                                               
         USING DTNAMD,R2                                                        
CT10E24  MVC   X.PDHDR,HNAME                                                    
         MVC   X.PDINF(L'DTNAME),DTNAME                                         
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTDATD,R2       *** BROADCAST SUBJECT                            
CT10E26  MVC   X.PDHDR,HSUBJECT                                                 
         XR    R1,R1                                                            
         IC    R1,CTDATLEN                                                      
         AHI   R1,-(CTDATA-CTDATD+1)                                            
         MVC   X.PDINF(0),CTDATA                                                
         EX    R1,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING GRPDD,R2        *** GROUP DATE RANGE                             
CT10E28  GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
*                                                                               
HEXRATE  DC    CL15'Exchange Rate  '                                            
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'1F' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT1FELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     CONTROL FILE DEFAULT                         
         BE    CT1FE02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTIDD,R2                                                         
CT1FE02  MVC   X.PDHDR,HIDNAM                                                   
         MVC   X.PDINF(L'CTID),CTID  MOVE IN THE ID NAME                        
         OC    CTID(2),CTID                                                     
         BNZ   *+10                                                             
         MVC   X.PDINF(2),=CL2'L='                                              
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'20' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT20ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,1          OTHER DATA                                   
         BE    CT20E02                                                          
         CLI   ADRECNUM,78         HIGH MESSAGE NUMBER                          
         BE    CT20E04                                                          
         CLI   ADRECNUM,81         HELP TEXT                                    
         BE    CT20E06                                                          
         CLI   ADRECNUM,82         INPUT STATEMENT HEADER                       
         BE    CT20E08                                                          
         CLI   ADRECNUM,84         HEADING                                      
         BE    CT20E10                                                          
         CLI   ADRECNUM,113        RFP - GROUP VALUE                            
         BE    CT20E14                                                          
         CLI   ADFTYPE,CTFILEQ     CTFILE DEFAULT                               
         BE    CT20E12                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CT01OTHD,R2                                                      
CT20E02  MVC   X.PDHDR,HORGID                                                   
         CURED CT01ORIG,(7,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK                   
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HOFFCODE                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CT01OFFC,RF),CT01OFFC                                        
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HLIMACC                                            
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VHEXOUT,DMCB,CT01ACCS,(RF),L'CT01ACCS,0                          
*                                                                               
         AHI   R3,L'P                                                           
         MVC   X.PDHDR,HAUTHS                                                   
         AHI   RF,L'PDINF+1                                                     
         GOTO1 VHEXOUT,DMCB,CT01AUTH,X.PDINF,L'CT01AUTH,0                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING GMCTLD,R2       *** HIGH MESSAGE NUMBER                          
CT20E04  GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
*                                                                               
         USING HV1TXTD,R2      *** HELP TEXT MESSAGE                            
CT20E06  MVC   X.PDHDR,HSNUM                                                    
         CURED (B1,HV1TXTSQ),(3,X.PDINF),0,ZERO=NOBLANK,ALIGN=LEFT              
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HVALBITS                                           
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VHEXOUT,DMCB,HV1TXTVB,(RF),L'HV1TXTVB,0                          
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HOPTBITS                                           
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VHEXOUT,DMCB,HV1TXTOP,(RF),L'HV1TXTOP,0                          
*                                                                               
         AHI   R3,L'P                                                           
         MVC   X.PDHDR,HTEXT                                                    
         XR    RF,RF                                                            
         IC    RF,HV1TXTLN                                                      
         AHI   RF,-(HV1TXTOV+1)                                                 
         MVC   X.PDINF(0),HV1TXTTX                                              
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING DEISD,R2        *** INPUT STATEMENT LABEL                        
CT20E08  MVC   X.PDHDR,HLABEL                                                   
         MVC   X.PDINF(L'DEISLAB),DEISLAB                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING BRDHEDD,R2      *** BROADCAST HEADING                            
CT20E10  MVC   X.PDHDR,HHDNG                                                    
         MVC   X.PDINF(L'BRDHEDTX),BRDHEDTX                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTIDD,R2        *** COMPATIBLE ID                                
CT20E12  MVC   X.PDHDR,HIDNAM                                                   
         MVC   X.PDINF(L'CTID),CTID  MOVE IN THE ID NAME                        
         OC    CTID(2),CTID                                                     
         BNZ   *+10                                                             
         MVC   X.PDINF(2),=CL2'L='                                              
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING HV1TXTD,R2      *** HELP TEXT MESSAGE                            
CT20E14  GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
*                                                                               
HOPTBITS DC    CL15'Option Bits'                                                
HVALBITS DC    CL15'Validity Bits'                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* X'21' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT21ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,68         ADDRESS LINE 1                               
         BE    CT21E02                                                          
         CLI   ADRECNUM,82         OUTPUT STATEMENT LABEL                       
         BE    CT21E04                                                          
         CLI   ADFTYPE,CTFILEQ     CTFILE DEFAULT                               
         BE    CT21E06                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
CT21E02  MVC   X.PDHDR,HADRL1  *** ADDRESS LINE 1                               
         XR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         AHI   RF,-3                                                            
         MVC   X.PDINF(0),2(RF)                                                 
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING DEOSLD,R2       *** OUTPUT STATEMENT LABEL                       
CT21E04  MVC   X.PDHDR,HLABEL                                                   
         MVC   X.PDINF(L'DEOSLABL),DEOSLABL                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTSYSD,R2       *** SYSTEM ELEMENT                               
CT21E06  MVC   X.PDHDR,HSYSNAME                                                 
         XC    APGMLST,APGMLST                                                  
         XC    ASE,ASE                                                          
*                                                                               
         L     R1,VSELIST                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6                                                             
         USING SELISTD,R1                                                       
         CLC   SESYS,CTSYSSE       MATCH SE NUMBER                              
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     CT21E08                                                          
*                                                                               
         ST    R1,ASE              STORE A(THIS SYSTEM)                         
         MVC   X.PDINF(L'SENAME),SENAME                                         
         MVC   APGMLST,SEPGMS      AND A(SEPGMS)                                
         B     CT21E12                                                          
*                                                                               
CT21E08  L     R1,VSELIST                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SEOVSYS,CTSYSNUM    MATCH SE NUMBER                              
         BE    CT21E10                                                          
         BXLE  R1,RE,*-10                                                       
*                                                                               
         MVC   X.PDINF+00(L'NOTDEF),NOTDEF                                      
         MVC   X.PDINF+12(08),=CL8'CTSYSSE='                                    
         GOTO1 VHEXOUT,DMCB,CTSYSSE,X.PDINF+20,L'CTSYSSE,0                      
         MVC   X.PDINF+25(09),=CL9'CTSYSNUM='                                   
         GOTO1 VHEXOUT,DMCB,CTSYSNUM,X.PDINF+34,L'CTSYSNUM                      
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
CT21E10  MVC   APGMLST,SEPGMS      SAVE A(PGMS)                                 
         ST    R1,ASE                                                           
*                                                                               
         L     R1,VSYSLST                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SYSLSTD,R1                                                       
         CLC   CTSYSNUM,SYSLNUM    MATCH SYSTEM NUMBER                          
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         MVC   X.PDINF(L'SYSLNAME),SYSLNAME                                     
         DROP  R1                                                               
*                                                                               
CT21E12  LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HSYSPASS                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'NOTDEF,RF),NOTDEF                                            
         TM    CTSYSIND,CTSYSIYP   PASSWORD IS REQUIRED?                        
         BZ    *+10                NO                                           
         MVC   0(L'REQURD,RF),REQURD                                            
         TM    CTSYSIND,CTSYSINP   PASSWORD IS NOT REQUIRED?                    
         BZ    *+10                NO                                           
         MVC   0(L'NREQURD,RF),NREQURD                                          
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HLIMACC                                            
         AHI   RF,L'PDHDR+1                                                     
         OC    CTSYSLMT,CTSYSLMT                                                
         BNZ   *+14                                                             
         MVC   0(L'NOTDEF,RF),NOTDEF                                            
         B     CT21E18                                                          
         MVC   0(L'CTSYSLMT,RF),CTSYSLMT                                        
*&&UK                                                                           
         CLI   CTSYSNUM,4          UK MEDIA SYSTEM HAS SPECIAL CODE             
         BNE   CT21E16                                                          
         LR    R5,RF                                                            
         MVC   0(L'CTSYSLMT,R5),SPACES                                          
         LA    R6,CTSYSLMT                                                      
         LHI   R4,4                                                             
*                                                                               
CT21E14  CURED (B1,(R6)),(3,(R5)),0,ALIGN=LEFT,ZERO=NOBLANK                     
         AR    R5,R0                                                            
         MVI   0(R5),C','                                                       
         AHI   R5,1                                                             
         AHI   R6,1                                                             
         BCT   R4,CT21E16                                                       
         BCTR  R5,0                                                             
         MVI   0(R5),C' '                                                       
         B     CT21E18                                                          
*&&                                                                             
CT21E16  CLI   CTSYSNUM,14         PERSON SYSTEM HAS SPECIAL CODE               
         BNE   CT21E18             FORMAT IS N(NN)-N(NN)                        
         MVC   0(L'CTSYSLMT,RF),SPACES                                          
*                                                                               
         OC    CTSYSLMT+2(2),CTSYSLMT+2                                         
         BNZ   *+14                                                             
         MVC   0(L'NOTDEF,RF),NOTDEF                                            
         B     CT21E18                                                          
*                                                                               
         LR    R5,RF                                                            
         CURED (B1,CTSYSLMT+2),(3,0(R5)),0,ALIGN=LEFT,ZERO=NOBLANK              
         AR    R5,R0                                                            
         MVI   0(R5),C'-'                                                       
         CURED (B1,CTSYSLMT+3),(3,1(R5)),0,ALIGN=LEFT,ZERO=NOBLANK              
*                                                                               
CT21E18  AHI   R3,L'P              NEXT LINE                                    
         MVC   X.PDHDR,HPGMAUTH                                                 
         LA    R6,X.PDINF                                                       
         LA    R7,CTSYSPGM         R7=A(AUTHS)                                  
         XR    R5,R5                                                            
         IC    R5,CTSYSLEN                                                      
         CHI   R5,CTSYSL1Q         CHECK FOR ALL= VALUE ONLY                    
         BE    CT21E22                                                          
*                                                                               
         CLI   CTSYSLEN,X'18'      SPECIAL FOR LENGTH X'18'                     
         BNE   CT21E22                                                          
         MVC   0(4,R6),ALL4        SET ALL= Y, N, OR XXXX                       
         MVI   3(R6),C'='                                                       
         AHI   R6,4                                                             
*                                                                               
         LA    R7,CTSYSALL                                                      
         MVC   0(4,R6),YES4                                                     
         CLC   0(2,R7),YAUTH       YES?                                         
         BE    CT21E20                                                          
         MVC   0(4,R6),NO4         NO?                                          
         CLC   0(2,R7),NAUTH                                                    
         BE    CT21E20                                                          
         GOTO1 VHEXOUT,PLIST,0(R7),0(R6),2,0                                    
*                                                                               
CT21E20  AHI   R6,12                                                            
         MVC   0(5,R6),=C'Data='                                                
         LA    R7,CTSYSPGM                                                      
         GOTO1 VHEXOUT,PLIST,(R7),5(R6),8,0                                     
         B     CT21E32                                                          
*                                                                               
CT21E22  CHI   R5,CTSYSL1Q         STILL INDIVIDUAL AUTHS?                      
         BH    CT21E24             YES                                          
*                                                                               
         LA    R7,CTSYSALL                                                      
         MVC   PROGRAM,SPACES      SET ALL VALUE                                
         MVC   PROGRAM(4),ALL4                                                  
         MVC   0(L'PROGRAM,R6),PROGRAM                                          
         XR    R5,R5               FLAG FINAL VALUE                             
         B     CT21E26                                                          
*                                                                               
CT21E24  BRAS  RE,GETPRGN          GET NAME OF PROGRAM                          
         MVC   0(L'PROGRAM,R6),PROGRAM                                          
         AHI   R7,1                R7 = AUTH VALUE                              
*                                                                               
CT21E26  AHI   R6,L'PROGRAM-1                                                   
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C'='                                                       
*                                                                               
         MVC   2(4,R6),YES4        AUTH IS Y,N OR XXXX                          
         CLC   0(2,R7),YAUTH                                                    
         BE    CT21E28                                                          
         MVC   2(4,R6),NO4                                                      
         CLC   0(2,R7),NAUTH                                                    
         BE    CT21E28                                                          
         GOTO1 VHEXOUT,PLIST,0(R7),2(R6),2,0                                    
*                                                                               
CT21E28  LTR   R5,R5             EXIT IF ALL=VALUE JUST DONE                    
         BZ    CT21E32                                                          
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA-13                                          
         CR    R6,RF                                                            
         BNH   CT21E30                                                          
*                                                                               
         AHI   R3,L'P            LINE IS FULL                                   
         LA    R6,X.PDINF                                                       
         AHI   R7,2              GO PAST AUTH                                   
         AHI   R5,-3             ADJUST LENGTH REMAINING                        
         B     CT21E22                                                          
*                                                                               
CT21E30  AHI   R6,5              GET NEXT FREE ON LINE                          
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         AHI   R6,1                                                             
         MVI   0(R6),C','                                                       
         AHI   R6,1                                                             
*                                                                               
         AHI   R7,2              GO PAST AUTH                                   
         AHI   R5,-3             ADJUST LENGTH REMAINING                        
         B     CT21E22                                                          
*                                                                               
CT21E32  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
GETPRGN  NTR1  ,                   SUROUTINE TO EXTRACT PROGRAM NAME            
         L     R1,APGMLST          NTRY: R3 = A(PROGRAM NUMBER)                 
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6                                                             
         USING PGMLSTD,R1                                                       
         XC    PROGRAM,PROGRAM                                                  
         CLC   PGMNUM,0(R7)        MATCH ON PROGRAM NUMBER                      
         BE    GPRGN02                                                          
         BXLE  R1,RE,*-10                                                       
         GOTO1 VHEXOUT,PLIST,(R7),PROGRAM,1,0                                   
         B     EXITOK                                                           
*                                                                               
GPRGN02  MVC   PROGRAM,PGMNAME                                                  
         B     EXITOK                                                           
         DROP  R1,R2,X                                                          
*                                                                               
HADRL1   DC    CL15'Address Line 1'                                             
         EJECT                                                                  
***********************************************************************         
* X'23' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT23ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,68         ADDRESS LINE 3                               
         BE    CT23E02                                                          
         CLI   ADRECNUM,82         INPUT LENGTH                                 
         BE    CT23E04                                                          
         CLI   ADFTYPE,CTFILEQ     CTFILE DEFAULT                               
         BE    CT23E06                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
CT23E02  MVC   X.PDHDR,HADRL3  *** ADDRESS LINE 3                               
         XR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         AHI   RF,-3                                                            
         MVC   X.PDINF(0),2(RF)                                                 
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING DEILD,R2        *** INPUT LENGTH                                 
CT23E04  MVC   X.PDHDR,HINPLEN                                                  
         CURED (B1,DEILEN),(3,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK                
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTPRGD,R2       *** PROGRAM TEST ELEMENT                         
CT23E06  MVC   X.PDHDR,HTSTPHS                                                  
         MVC   X.PDINF(L'CTPRGRAM),CTPRGRAM                                     
         MVI   X.PDINF+L'CTPRGRAM,C'='                                          
         MVC   X.PDINF+L'CTPRGRAM+1(L'CTPRGTST),CTPRGTST                        
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HADRL3   DC    CL15'Address Line 3'                                             
HINPLEN  DC    CL15'Input Length'                                               
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'24' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT24ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,52         APPLID                                       
         BE    CT24E02                                                          
         CLI   ADRECNUM,68         ADDRESS LINE 4                               
         BE    CT24E04                                                          
         CLI   ADRECNUM,82         INPUT ROUTINE                                
         BE    CT24E06                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTAPLD,R2       *** APPLID                                       
CT24E02  MVC   X.PDHDR,HVTAML                                                   
         AHI   R3,L'PDHDR+1                                                     
         MVC   X.PDINF(L'CTAPLID),CTAPLID                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
CT24E04  MVC   X.PDHDR,HADRL4  *** ADDRESS LINE 4                               
         XR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         AHI   RF,-3                                                            
         MVC   X.PDINF(0),2(RF)                                                 
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING DEIRD,R2        *** INPUT ROUTINE                                
CT24E06  MVC   X.PDHDR,HINPRTN                                                  
         MVC   X.PDINF(L'DEIROUT),DEIROUT                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HADRL4   DC    CL15'Address Line 4'                                             
HINPRTN  DC    CL15'Input Routine'                                              
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'25' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT25ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,6          PHASE DESCRIPTION                            
         BE    CT25E02                                                          
         CLI   ADRECNUM,82         INPUT ARGUMENTS                              
         BE    CT25E04                                                          
         CLI   ADFTYPE,CTFILEQ     TERMINAL DEFINITION                          
         BE    CT25E06                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTPHDSCD,R2     *** PROGRAM DESCRIPTION                          
CT25E02  MVC   X.PDHDR,HDESC                                                    
         XR    RF,RF                                                            
         IC    RF,CTPHDLEN                                                      
         AHI   RF,-(CTPHDOVQ+1)                                                 
         MVC   X.PDINF(0),CTPHDDSC                                              
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING DEIAD,R2        *** INPUT ARGUMENTS                              
CT25E04  MVC   X.PDHDR,HARGMNTS                                                 
         XR    RF,RF                                                            
         IC    RF,DEIALEN                                                       
         AHI   RF,-(DEIARGS-DEIAD+1)                                            
         MVC   X.PDINF(0),DEIARGS                                               
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTTRMD,R2       *** TERMINAL DEFINITION                          
CT25E06  MVC   X.PDHDR,HDISDEV                                                  
         LA    R4,L'PDHDR+1(R3)                                                 
*                                                                               
         LA    RE,DEVTBL           DISPLAY DEVICE                               
CT25E08  CLI   0(RE),255           EOT?                                         
         BNE   *+14                NO                                           
         MVC   X.PDINF,UNKNOWN                                                  
         B     CT25E10                                                          
*                                                                               
         CLC   CTTRMDEV,8(RE)      MATCH DEVICE TYPE                            
         BE    *+12                                                             
         AHI   RE,L'DEVTBL                                                      
         B     CT25E08                                                          
         MVC   X.PDINF(8),0(RE)    SET DEVICE TYPE ON PRINT LINE                
*                                                                               
CT25E10  LA    R4,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R4),HDISTYPE                                           
         AHI   R4,L'PDHDR+1                                                     
*                                                                               
         LA    RE,TYPTBL                                                        
CT25E12  CLI   0(RE),255           EOT?                                         
         BNE   *+14                NO                                           
         MVC   0(L'UNKNOWN,R4),UNKNOWN                                          
         B     CT25E14                                                          
         CLC   CTTRMTYP,8(RE)      MATCH DEVICE TYPE                            
         BE    *+12                YES                                          
         LA    RE,L'TYPTBL(RE)                                                  
         B     CT25E12                                                          
         MVC   0(8,R4),0(RE)       SET DEVICE TYPE                              
*                                                                               
CT25E14  LA    R4,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,R4),HCNTRY                                             
         AHI   R4,L'PDHDR+1                                                     
         MVC   0(L'UNKNOWN,R4),UNKNOWN                                          
*                                                                               
         L     R1,VCTRYTAB         DISPLAY COUNTRY                              
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         CLC   CTRYCODE,CTTRMCTY                                                
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     CT25E16                                                          
*                                                                               
         MVC   0(L'UNKNOWN,R4),SPACES                                           
         MVC   0(L'CTRYSHR,R4),CTRYSHR                                          
         DROP  R1                                                               
*                                                                               
CT25E16  AHI   R3,L'P              NEXT LINE                                    
         MVC   X.PDHDR,HAGYID                                                   
         MVC   X.PDINF(L'CTTRMAGY),CTTRMAGY                                     
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HOFFCODE                                           
         AHI   RF,L'PDHDR+1                                                     
*                                                                               
         MVC   0(1,RF),CTTRMOFC                                                 
         CLI   CTTRMOFC,C'*'                                                    
         BNE   CT25E18                                                          
         TM    CTTRMFL1,X'03'      SHOW *1 DDS LEVEL                            
         BZ    CT25E18                                                          
         MVI   1(RF),C'1'                                                       
         TM    CTTRMFL1,X'02'      SHOW *2 DDS LEVEL                            
         BZ    *+8                                                              
         MVI   1(RF),C'2'                                                       
*                                                                               
CT25E18  LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HNODENM                                            
         AHI   RF,L'PDHDR+1                                                     
*                                                                               
         LA    RE,NODETBL                                                       
CT25E20  CLI   0(RE),255           EOT?                                         
         BNE   *+14                NO                                           
         MVC   0(L'UNKNOWN,RF),UNKNOWN                                          
         B     CT25E22                                                          
         CLC   CTTRMNDE,3(RE)      MATCH DEVICE TYPE                            
         BE    *+12                YES                                          
         LA    RE,L'NODETBL(RE)                                                 
         B     CT25E20                                                          
         MVC   0(3,RF),0(RE)       SET DEVICE TYPE                              
*                                                                               
CT25E22  AHI   R3,L'P                                                           
         MVC   X.PDHDR,HLINEID                                                  
         MVC   X.PDINF(L'CTTRMLNE),CTTRMLNE                                     
*                                                                               
         LA    R6,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R6),HCUDVADD                                           
         AHI   R6,L'PDHDR+1                                                     
         GOTO1 VHEXOUT,PLIST,CTTRMCU,0(R6),1                                    
         MVI   2(R6),C'/'                                                       
         GOTO1 VHEXOUT,PLIST,CTTRMCU,3(R6),1                                    
*                                                                               
         LA    R6,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,R6),HATT12                                             
         AHI   R6,L'PDHDR+1        ATTRIBUTES 1 AND 2                           
*                                                                               
         MVC   BYTE,CTTRMAT1                                                    
         BRAS  RE,DTATB            AT1=ABCDEFGH                                 
         MVI   0(R6),C'/'                                                       
         AHI   R6,1                                                             
         MVC   BYTE,CTTRMAT2                                                    
         BRAS  RE,DTATB            AT2=ABCDEFGH                                 
*                                                                               
         AHI   R3,L'P              ESCAPE SEQUENCE                              
         MVC   X.PDHDR,HESCCHR                                                  
         CURED CTTRMESC,(3,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK                   
*                                                                               
         LA    R6,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R6),HLINETYP                                           
         AHI   R6,L'PDHDR+1        LINE TYPE                                    
*                                                                               
         MVC   0(L'CTTRMLTY,R6),CTTRMLTY                                        
         CLI   CTTRMLTY,C'B'                                                    
         BNE   *+10                                                             
         MVC   0(4,R6),=CL4'BSC '                                               
         CLI   CTTRMLTY,C'L'                                                    
         BNE   *+10                                                             
         MVC   0(4,R6),=CL4'LCL '                                               
         CLI   CTTRMLTY,C'S'                                                    
         BNE   *+10                                                             
         MVC   0(4,R6),=CL4'SDLC'                                               
         CLI   CTTRMLTY,C'T'                                                    
         BNE   *+10                                                             
         MVC   0(4,R6),=CL4'TWX '                                               
*                                                                               
         LA    R6,L'PDDATA*2/3(R3)                                              
         MVC   0(L'PDHDR,R6),HLINESPD                                           
         AHI   R6,L'PDHDR+1        LINE SPEED                                   
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,CTTRMLSP                                                    
         MHI   R0,8                CONVERT FROM CPS TO BAUD                     
         CURED (R0),(8,(R6)),0,ALIGN=LEFT,ZERO=NOBLANK                          
         AR    R6,R0                                                            
         MVC   1(L'BAUD,R6),BAUD                                                
*                                                                               
         AHI   R3,L'P                                                           
         TM    CTTRMDEV,X'80'      PRINTER TYPE DEVICE?                         
         BZ    CT25E28             NO - EXIT                                    
*                                                                               
         MVC   X.PDHDR,HPRTSPD                                                  
         XR    R0,R0                                                            
         ICM   R0,3,CTTRMPSP                                                    
         BNZ   *+14                                                             
         MVC   X.PDINF(L'NOTDEF),NOTDEF                                         
         B     CT25E24                                                          
         CURED (R0),(7,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
CT25E24  LA    R6,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R6),HSPDTYPE                                           
         AHI   R6,L'PDHDR+1                                                     
         MVC   0(1,R6),CTTRMPST    PRINTER SPEED TYPE                           
         OC    CTTRMPST,CTTRMPST                                                
         BNZ   *+10                                                             
         MVC   0(L'NOTDEF,R6),NOTDEF                                            
*                                                                               
         LA    R6,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,R6),HPQNTRY                                            
         AHI   R6,L'PDHDR+1                                                     
         XR    R0,R0                                                            
         ICM   R0,1,CTTRMPRQ                                                    
         BNZ   *+14                                                             
         MVC   0(L'NOTDEF,R6),NOTDEF                                            
         B     CT25E26                                                          
         CURED (R0),(3,(R6)),0,ALIGN=LEFT                                       
*                                                                               
CT25E26  AHI   R3,L'P                                                           
         MVC   X.PDHDR,HBUFFSZ                                                  
         AHI   R6,L'PDHDR+1                                                     
*                                                                               
         XR    R0,R0               PRINTER BUFFER SIZE                          
         ICM   R0,3,CTTRMPBS                                                    
         BNZ   *+14                                                             
         MVC   X.PDINF(L'NOTDEF),NOTDEF                                         
         B     CT25E28                                                          
         CURED (R0),(5,X.PDINF),0,ALIGN=LEFT                                    
*                                                                               
CT25E28  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
DTATB    LHI   R0,8                BYTE TO ATT A/B/C/D/E/F/G/H                  
         LA    R1,DTATBITS                                                      
*                                                                               
DTATB1   MVC   BYTE1,BYTE          COPY INDICATORS                              
         NC    BYTE1,0(R1)         TEST IF FLAG SET                             
         BZ    *+14                                                             
         MVC   0(1,R6),1(R1)       MOVE IN INDICATOR                            
         AHI   R6,1                                                             
         AHI   R1,2                                                             
         BCT   R0,DTATB1                                                        
         BR    RE                                                               
*                                                                               
HATT12   DC    CL15'Attribute 1 && 2'                                           
HBUFFSZ  DC    CL15'Buffer Size    '                                            
HCUDVADD DC    CL15'CU/DV Address  '                                            
HARGMNTS DC    CL15'Input Arguments'                                            
*                                                                               
DTATBITS DC    X'80',C'A',X'40',C'B',X'20',C'C',X'10',C'D'                      
         DC    X'08',C'E',X'04',C'F',X'02',C'G',X'01',C'H'                      
*                                                                               
       ++INCLUDE CTREPLUTAB                                                     
*                                                                               
         EJECT                                                                  
***********************************************************************         
* X'26' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT26ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         INPUT OPTIONS                                
         BE    CT26E02                                                          
         CLI   ADFTYPE,CTFILEQ                                                  
         BE    CT26E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEIOD,R2        *** INPUT OPTION                                 
CT26E02  MVC   X.PDHDR,HIOPTS                                                   
         CURED (B1,DEIONUM),(3,X.PDHDR),0,ALIGN=LEFT,ZERO=NOBLANK               
         LA    R4,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R4),HVALUE                                             
         CURED (B1,DEIOVAL),(3,0(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                 
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTLUID,R2                                                        
CT26E04  MVC   X.PDHDR,HVTAML                                                   
         TM    CTLUIFLG,X'80'                                                   
         BZ    *+10                                                             
         MVC   X.PDHDR,HBTAML                                                   
         MVC   X.PDINF(L'CTLUIID),CTLUIID                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HBTAML   DC    CL15'BTAM Luid      '                                            
HIOPTS   DC    CL15'Input Option # '                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* X'27' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT27ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         INPUT COLUMN FOR RANKING                     
         BE    CT27E02                                                          
         CLI   ADFTYPE,CTFILEQ     TERMINAL INFORMATION                         
         BE    CT27E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEICD,R2        *** INPUT COLUMN                                 
CT27E02  MVC   X.PDHDR,HLABEL                                                   
         MVC   X.PDINF(L'DEICCOL),DEICCOL                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTTIND,R2                                                        
CT27E04  MVC   X.PDHDR,HSERNO                                                   
         MVC   X.PDINF(L'CTTINSER),CTTINSER                                     
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HDATECHG                                           
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VDATCON,PLIST,(3,CTTINDAT),(5,0(RF))                             
*                                                                               
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
*                                                                               
         XR    R1,R1                                                            
         IC    R1,CTTINLEN                                                      
         AHI   R1,-(CTTINVEN-CTTIND+1)                                          
         BM    EXITOK                                                           
*                                                                               
         AHI   R3,L'P                                                           
         MVC   X.PDHDR,HVENDOR                                                  
         MVC   X.PDINF(0),CTTINVEN                                              
         EX    R1,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HDATECHG DC    CL15'Date Changed   '                                            
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'29' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT29ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     DEFAULT PRINTER QUEUE                        
         BE    CT29E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTPRQD,R2       *** DEFAULT PRINTER QUEUE                        
CT29E02  CLI   CTPRQLEN,18                                                      
         BNE   CT29E04                                                          
         MVC   0(L'OLDSTYLE,R3),OLDSTYLE                                        
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
CT29E04  LR    R4,R3                                                            
         MVC   X.PDHDR,HUSRID                                                   
         MVC   HALF,CTPRQUSR       GET PQ USER ID                               
         NI    HALF,255-X'80'      TURN OFF GENERIC ID BIT                      
         BRAS  RE,GETUSRID                                                      
         MVC   X.PDINF(10),WORK+2                                               
*                                                                               
         LA    R4,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R4),HSUBID                                             
         AHI   R4,L'PDHDR+1                                                     
         MVC   0(L'CTPRQSUB,R4),CTPRQSUB                                        
*                                                                               
         LA    R4,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,R4),HCLASS                                             
         AHI   R4,L'PDHDR+1                                                     
         MVC   0(L'CTPRQCLS,R4),CTPRQCLS                                        
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
OLDSTYLE DC    CL30'Old style element - can''t be resolved'                     
HCLASS   DC    CL15'Class          '                                            
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'30' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT30ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         OUTPUT HEADER                                
         BE    CT30E02                                                          
         CLI   ADRECNUM,84         BROADCAST TEXT                               
         BE    CT30E04                                                          
         CLI   ADRECNUM,113        RFP REQUEST                                  
         BE    CT30E06                                                          
         CLI   ADFTYPE,CTFILEQ     DESTINATION DETAIL                           
         BE    CT30E10                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEOSD,R2        *** OUTPUT HEADER                                
CT30E02  MVC   X.PDHDR,HLABEL                                                   
         MVC   X.PDINF(L'DEOSLAB),DEOSLAB                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING BRDTXTD,R2      *** BROADCAST TEXT                               
CT30E04  MVC   X.PDHDR,HTEXT                                                    
         XR    R0,R0                                                            
         IC    R0,BRDTXTLN                                                      
         AHI   R0,-(BRDTXTTX-BRDTXTD)                                           
         LHI   RF,L'PDINF                                                       
         GOTO1 VCHOPPER,DMCB,((R0),BRDTXTTX),((RF),X.PDINF),(C'P',3)            
         ICM   R0,15,8(R1)                                                      
         MHI   R0,L'P                                                           
         AR    R3,R0                                                            
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING GRPRD,R2        *** RFP REQUEST                                  
CT30E06  MVC   X.PDHDR,HREQID                                                   
         MVC   X.PDINF(L'GRPRRNUM),GRPRRNUM                                     
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HSRTCODE                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'GRPRSORT,RF),GRPRSORT                                        
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HSNUM                                              
         AHI   RF,L'PDHDR+1                                                     
         CURED (B1,GRPRSEQN),(3,(RF)),0,ALIGN=LEFT,ZERO=NOBLANK                 
*                                                                               
         AHI   R3,L'P                                                           
         CLI   GRPRCRDN,GRPRCHDQ   HEADER CARD?                                 
         BNE   CT30E08             NO                                           
*                                                                               
         MVC   X.PDHDR,HSTATUS                                                  
         LA    RF,X.PDINF                                                       
         TM    GRPRSTAT,GRPRDISQ                                                
         BZ    *+14                                                             
         MVC   0(9,RF),=CL9'Disabled,'                                          
         AHI   RF,9                                                             
         TM    GRPRSTAT,GRPRSPOF                                                
         BZ    *+14                                                             
         MVC   0(12,RF),=CL12'Spoof-Style,'                                     
         AHI   RF,12                                                            
         BCTR  RF,0                                                             
         CLI   0(RF),C','                                                       
         BNE   *+12                                                             
         MVI   0(RF),C' '                                                       
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HCOLNUM                                            
         AHI   RF,L'PDHDR+1                                                     
         CURED (B1,GRPRRQST),(3,(RF)),0,ALIGN=LEFT,ZERO=NOBLANK                 
*                                                                               
         AHI   R3,L'P                                                           
         MVC   X.PDHDR,HHDR                                                     
         MVC   X.PDINF(L'GRPCARD),GRPCARD                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
CT30E08  MVC   X.PDHDR,HSTATUS                                                  
         CURED (B1,GRPRRQST),(3,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK              
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HDESC                                              
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'GRPRDSC,RF),GRPRDSC                                          
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTDSTD,R2       *** DESTINATION DETAIL                           
CT30E10  MVC   X.PDHDR,HLOGO1      LINE 1 = LOGO 1 + NAME                       
         MVC   X.PDINF(L'CTDSTLG1),CTDSTLG1                                     
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HNAME NAME                                         
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTDSTNAM,RF),CTDSTNAM                                        
*                                                                               
         AHI   R3,L'P              LINE 2 = LOGO 2 + ADDRESS1                   
         MVC   X.PDHDR,HLOGO2                                                   
         MVC   X.PDINF(L'CTDSTLG2),CTDSTLG2                                     
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HADDR                                              
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTDSTADD,RF),CTDSTADD                                        
*                                                                               
         AHI   R3,L'P              LINE 3 = POWER CODE + ADDRESS 2              
         MVC   X.PDHDR,HPOWERCD                                                 
         MVC   X.PDINF(L'CTDSTPOW),CTDSTPOW                                     
         CLI   CTDSTLEN,(CTDSTAD2-CTDSTD)                                       
         BNH   *+10                                                             
         MVC   X.PDDATA+L'PDDATA/3+L'PDHDR+1(L'CTDSTAD2),CTDSTAD2               
*                                                                               
         AHI   R3,L'P              LINE 4 = SHIPPING ROUTE + ADDRESS 3          
         MVC   X.PDHDR,HSHIPRTE                                                 
         GOTO1 VHEXOUT,PLIST,CTDSTRUT,X.PDINF,2,0                               
         CLI   CTDSTLEN,CTDSTAD3-CTDSTD                                         
         BNH   *+10                                                             
         MVC   X.PDDATA+L'PDDATA/3+L'PDHDR+1(L'CTDSTAD3),CTDSTAD3               
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HADDR    DC    CL15'Address        '                                            
HCOLNUM  DC    CL15'Column Number  '                                            
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'31' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT31ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         INPUT STATEMENT LABEL                        
         BE    CT31E02                                                          
         CLI   ADFTYPE,CTFILEQ     ATTENTION DETAIL                             
         BE    CT31E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEISLD,R2       *** INPUT STATEMENT LABEL                        
CT31E02  MVC   X.PDHDR,HLABEL                                                   
         MVC   X.PDINF(L'DEISLABL),DEISLABL                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTATTND,R2      *** ATTENTION DETAIL                             
CT31E04  MVC   X.PDHDR,HATTYPE                                                  
         MVC   X.PDINF(L'CTATTTYP),CTATTTYP                                     
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HDETAIL                                            
         AHI   RF,L'PDHDR+1                                                     
         XR    R1,R1                                                            
         IC    R1,CTATTLEN                                                      
         AHI   R1,-(CTATTDET-CTATTND+1)                                         
         MVC   0(0,RF),CTATTDET                                                 
         EX    R1,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* X'32' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT32ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         OUTPUT TYPE                                  
         BE    CT32E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEOTD,R2        *** OUTPUT TYPE                                  
CT32E02  MVC   X.PDHDR,HOUTTYPE                                                 
         MVC   X.PDINF(L'DEOTYPE),DEOTYPE                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'33' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT33ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         OUTPUT TYPE                                  
         BE    CT33E02                                                          
         CLI   ADFTYPE,CTFILEQ     AGENCY DEFINITION                            
         BE    CT33E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEOLD,R2        *** OUTPUT TYPE                                  
CT33E02  MVC   X.PDHDR,HOUTLEN                                                  
         CURED (B1,DEOLEN),(3,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK                
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
CT33E04  DS    0H                                                               
*&&UK                                                                           
         USING CTUKAD,R2                                                        
         MVC   X.PDHDR,HLINEID                                                  
         MVC   X.PDINF(L'CTUKALIN),CTUKALIN                                     
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HIPAAGY                                            
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTUKAIPA,RF),CTUKAIPA                                        
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*&&                                                                             
*&&US                                                                           
         USING CTUSAD,R2                                                        
         MVC   X.PDHDR,HDAREPID                                                 
         MVC   X.PDINF(L'CTUSADPI),CTUSADPI                                     
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HDARERC                                            
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTUSADRC,RF),CTUSADRC                                        
         LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HDAREFLG                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTUSADFL,RF),CTUSADFL                                        
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*&&                                                                             
HOUTLEN  DC    CL15'Output Length  '                                            
HDAREFLG DC    CL15'DARE Flag      '                                            
HDAREPID DC    CL15'DARE Partner Id'                                            
HDARERC  DC    CL15'DARE Route Code'                                            
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'34' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT34ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         OUTPUT ROUTINE                               
         BE    CT34E02                                                          
         CLI   ADFTYPE,CTFILEQ     VALID DESTINATION                            
         BE    CT34E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEORD,R2        *** OUTPUT ROUTINE                               
CT34E02  MVC   X.PDHDR,HROUTINE                                                 
         MVC   X.PDINF,DEOROUT                                                  
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTVALD,R2       *** VALID DESTINATION                            
CT34E04  MVC   X.PDHDR,HDSTID                                                   
         MVC   X.PDINF(L'CTVALDST),CTVALDST                                     
         OC    CTVALDST(2),CTVALDST                                             
         BNZ   *+10                                                             
         MVC   X.PDINF(2),=C'L='                                                
*                                                                               
         OC    CTVALNUM,CTVALNUM                                                
         BZ    CT34E06                                                          
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HDSTNUM                                            
         AHI   RF,L'PDHDR+1                                                     
         CURED CTVALNUM,(5,(RF)),0,ALIGN=LEFT,ZERO=NOBLANK                      
*                                                                               
CT34E06  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HROUTINE DC    CL15'Output Routine '                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* X'35' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT35ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,6          COMMENT                                      
         BE    CT35E02                                                          
         CLI   ADRECNUM,82         OUTPUT ARGUMENTS                             
         BE    CT35E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTPHCOMD,R2     *** COMMENT                                      
CT35E02  MVC   X.PDHDR,HCOMMENT                                                 
         CURED CTPHCLNM,(3,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK                   
*                                                                               
         LA    R1,X.PDINF                                                       
         AR    R1,R0                                                            
         MVI   0(R1),C'='                                                       
         XR    RF,RF                                                            
         IC    RF,CTPHCLEN                                                      
         AHI   RF,-(CTPHCOVQ+1)                                                 
         MVC   1(0,R1),CTPHCTXT                                                 
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING DEOAD,R2        *** OUTPUT ARGUMENTS                             
CT35E04  MVC   X.PDHDR,HOUTARGS                                                 
         XR    RF,RF                                                            
         IC    RF,DEOALEN                                                       
         AHI   RF,-(DEOARGS-DEOAD+1)                                            
         MVC   X.PDINF(0),DEOARGS                                               
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HOUTARGS DC    CL15'Output Arguments'                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'36' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT36ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         OUTPUT OPTION                                
         BE    CT36E02                                                          
         CLI   ADRECNUM,86         AGREEMENT EXPIRY                             
         BE    CT36E04                                                          
         CLI   ADRECNUM,87         AGREEMENT EXPIRY                             
         BE    CT36E04                                                          
         CLI   ADRECNUM,88         AGREEMENT EXPIRY                             
         BE    CT36E04                                                          
         CLI   ADRECNUM,89         AGREEMENT EXPIRY                             
         BE    CT36E04                                                          
         CLI   ADRECNUM,90         AGREEMENT EXPIRY                             
         BE    CT36E04                                                          
         CLI   ADRECNUM,91         AGREEMENT EXPIRY                             
         BE    CT36E04                                                          
         CLI   ADFTYPE,CTFILEQ     ORIGIN DETAIL - CTFILE DEFAULT               
         BE    CT36E06                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEOOD,R2        *** OUTPUT OPTION                                
CT36E02  MVC   X.PDHDR,HOOPTS                                                   
         CURED (B1,DEOONUM),(3,X.PDHDR),0,ALIGN=LEFT,ZERO=NOBLANK               
         LA    R4,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R4),HVALUE                                             
         CURED (B1,DEOOVAL),(3,0(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                 
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING GFAGRD,R2       *** AGREEMENT EXPIRY                             
CT36E04  GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
*                                                                               
         USING CTORGD,R2       *** ORIGIN DETAIL                                
CT36E06  MVC   X.PDHDR,HORGNAME                                                 
         MVC   X.PDINF(L'CTORGNAM),CTORGNAM                                     
         AHI   R3,L'P                                                           
         MVC   X.PDHDR,HORGADDR                                                 
         MVC   X.PDINF(L'CTORGADD),CTORGADD                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HOOPTS   DC    CL15'Output Option #'                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* X'3A' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT3AELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,76         CURRENCY                                     
         BE    CT3AE04                                                          
         CLI   ADRECNUM,77         CURRENCY                                     
         BE    CT3AE04                                                          
         CLI   ADFTYPE,CTFILEQ     VALID PRINTER                                
         BE    CT3AE02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTPRND,R2                                                        
CT3AE02  MVC   X.PDHDR,HPRINTER                                                 
         OC    CTPRNFLG,CTPRNFLG   PRINTER LIST?                                
         BZ    *+10                NO                                           
         MVC   X.PDHDR,HPRNTRLS                                                 
*                                                                               
         CURED CTPRNNUM,(3,X.PDINF),0,ZERO=NOBLANK,ALIGN=LEFT                   
         LA    RF,X.PDINF                                                       
         AR    RF,R0                                                            
         MVI   0(RF),C'='                                                       
         MVC   1(L'CTPRNLIN,RF),CTPRNLIN                                        
         OC    CTPRNFLG,CTPRNFLG                                                
         BNZ   *+10                                                             
         MVC   1(2,RF),=C'L='                                                   
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING GCREL,R2                                                         
CT3AE04  GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'3E' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT3EELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,86         COMMENT                                      
         BE    CT3EE02                                                          
         CLI   ADRECNUM,87         COMMENT                                      
         BE    CT3EE02                                                          
         CLI   ADRECNUM,88         COMMENT                                      
         BE    CT3EE02                                                          
         CLI   ADRECNUM,89         COMMENT                                      
         BE    CT3EE02                                                          
         CLI   ADRECNUM,90         COMMENT                                      
         BE    CT3EE02                                                          
         CLI   ADRECNUM,91         COMMENT                                      
         BE    CT3EE02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING GFCOMMD,R2                                                       
CT3EE02  MVC   X.PDHDR,HCOMMENT                                                 
         XR    RF,RF                                                            
         IC    RF,GFCOMELL                                                      
         AHI   RF,-(GFCOMFXQ+1)                                                 
         MVC   X.PDINF(0),GFCOMMNT                                              
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'40' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT40ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         HEAD STATEMENT                               
         BE    CT40E02                                                          
         CLI   ADFTYPE,CTFILEQ     DESTINATION DETAIL                           
         BE    CT40E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEHDD,R2                                                         
CT40E02  MVC   X.PDHDR,HLINNUM                                                  
         CURED DEHDLIN,(3,X.PDHDR),0,ALIGN=LEFT,ZERO=NOBLANK                    
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTDCOD,R2                                                        
CT40E04  BRAS  RE,PRFTYPE          OUTPUT PROFILE TYPE                          
         L     R3,FULL                                                          
         MVC   X.PDHDR,HDSTID                                                   
         MVC   X.PDINF(L'CTDCODE),CTDCODE                                       
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HDSTNUM                                            
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VHEXOUT,PLIST,CTDCNUM,0(RF),L'CTDCNUM,0                          
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'41' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT41ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     DESTINATION DETAIL                           
         BE    CT41E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTACOD,R2                                                        
CT41E02  BRAS  RE,PRFTYPE          OUTPUT PROFILE TYPE                          
         L     R3,FULL                                                          
         MVC   X.PDHDR,HATTYPE                                                  
         MVC   X.PDINF(L'CTACODE),CTACODE                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'42' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT42ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         CHUNK STATEMENT                              
         BE    CT42E02                                                          
         CLI   ADFTYPE,CTFILEQ     OUTPUT TYPE                                  
         BE    CT42E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DECHD,R2                                                         
CT42E02  MVC   X.PDHDR,HLABEL                                                   
         MVC   X.PDINF(L'DECHENDL),DECHENDL                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTOCOD,R2                                                        
CT42E04  BRAS  RE,PRFTYPE          OUTPUT PROFILE TYPE                          
         L     R3,FULL                                                          
         MVC   X.PDHDR,HOUTTYPE                                                 
         MVC   X.PDINF(L'CTOCODE),CTOCODE                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'43' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT43ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     OUTPUT MODE TYPE                             
         BE    CT43E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTOCOD,R2                                                        
CT43E02  BRAS  RE,PRFTYPE          OUTPUT PROFILE TYPE                          
         L     R3,FULL                                                          
         MVC   X.PDHDR,HOUTMODE                                                 
         MVC   X.PDINF(L'CTOCODE),CTOCODE                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'44' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT44ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         FIRST/LAST STATEMENT                         
         BE    CT44E02                                                          
         CLI   ADFTYPE,CTFILEQ     PRIORITY CODE                                
         BE    CT44E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEFLD,R2                                                         
CT44E02  MVC   X.PDHDR,HLABEL                                                   
         MVC   X.PDINF(L'NOTDEF),NOTDEF                                         
         CLI   DEFLIND,C'F'                                                     
         BNE   *+10                                                             
         MVC   X.PDINF(10),=CL10'First'                                         
         CLI   DEFLIND,C'L'                                                     
         BNE   *+10                                                             
         MVC   X.PDINF(10),=CL10'Last'                                          
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTPRID,R2       *** PRIORITY CODE                                
CT44E04  BRAS  RE,PRFTYPE          OUTPUT PROFILE TYPE                          
         L     R3,FULL                                                          
         MVC   X.PDHDR,HSRTCODE                                                 
         MVC   X.PDINF(L'CTPRISC),CTPRISC                                       
         OC    CTPRISC,CTPRISC                                                  
         BNZ   *+10                                                             
         MVC   X.PDINF(L'NOTDEF),NOTDEF                                         
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HSRTTYPE                                           
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'CTPRIPT,RF),CTPRIPT                                          
         OC    CTPRIPT,CTPRIPT                                                  
         BNZ   *+10                                                             
         MVC   0(L'NOTDEF,RF),NOTDEF                                            
*                                                                               
         LA    RF,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,RF),HOUTCODE                                           
         AHI   RF,L'PDHDR                                                       
         MVC   0(L'CTPRIPO,RF),CTPRIPO                                          
         OC    CTPRIPO,CTPRIPO                                                  
         BNZ   *+10                                                             
         MVC   0(L'NOTDEF,RF),NOTDEF                                            
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'45' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT45ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     READER CLASS                                 
         BE    CT45E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTRCLD,R2                                                        
CT45E02  BRAS  RE,PRFTYPE          OUTPUT PROFILE TYPE                          
         L     R3,FULL                                                          
         MVC   X.PDHDR,HRDRCLS                                                  
         MVC   X.PDINF(L'CTRCLASS),CTRCLASS                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'46' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT46ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         CONTROL BREAK INSTRUCTIONS                   
         BE    CT46E02                                                          
         CLI   ADFTYPE,CTFILEQ     SORT DATA FORMULA                            
         BE    CT46E06                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DECBD,R2                                                         
CT46E02  MVC   X.PDHDR,HINSTRCT                                                 
         MVC   X.PDINF(L'NOTDEF),NOTDEF                                         
         CLI   DECBINS,X'01'                                                    
         BNE   *+14                                                             
         MVC   X.PDINF(16),=CL16'Skip to new page'                              
         B     CT46E04                                                          
         CLI   DECBINS,X'03'                                                    
         BNE   *+14                                                             
         MVC   X.PDINF(22),=CL22'Reset page number to 1'                        
         B     CT46E04                                                          
         CLI   DECBINS,X'04'                                                    
         BNE   *+14                                                             
         MVC   X.PDINF(26),=CL26'Reset sub page number to 1'                    
         B     CT46E04                                                          
         CLI   DECBINS,X'05'                                                    
         BNE   *+14                                                             
         MVC   X.PDINF(24),=CL24'Midhead headings midpage'                      
         B     CT46E04                                                          
*                                                                               
         CLI   DECBINS,X'02'                                                    
         BNE   CT46E04                                                          
         MVC   X.PDINF(6),=CL6'Space '                                          
         CURED DECBVAL,(3,X.PDINF+6),0,ALIGN=LEFT,ZERO=NOBLANK                  
         LA    RF,X.PDINF                                                       
         AR    RF,R0                                                            
         MVC   0(5,RF),=CL5'Lines'                                              
*                                                                               
CT46E04  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTSRTD,R2       *** SORT DATA FORMULA                            
CT46E06  BRAS  RE,PRFTYPE          OUTPUT PROFILE TYPE                          
         L     R3,FULL                                                          
         MVC   X.PDHDR,HSRTFRM                                                  
         LA    R6,X.PDINF                                                       
         LA    R4,CTSRTFRM                                                      
         XR    R5,R5                                                            
         IC    R5,CTSRTLEN                                                      
         AHI   R5,-(CTSRTFRM-CTSRTD)                                            
         BP    CT46E08                                                          
         MVC   PDINF(20),=CL20'Improperly Defined'                              
         B     CT46E10                                                          
*                                                                               
CT46E08  MVI   2(R6),C','          FORMAT IS: DISP,LEN,ORDER                    
         MVI   5(R6),C','          SHOWN AS : XX,XX,X                           
         MVI   7(R6),C','                                                       
*                                                                               
         MVI   6(R6),C'A'          SET ORDER - ASCENDING OR DESCENDING          
         TM    0(R4),X'80'                                                      
         BZ    *+8                                                              
         MVI   6(R6),C'D'                                                       
         NI    0(R4),X'7F'         TURN OFF HOB SORT ORDER                      
         CURED (B1,(R4)),(2,(R6)),0,ZERO=NOBLANK                                
         CURED (B1,1(R4)),(2,3(R6)),0,ZERO=NOBLANK                              
*                                                                               
         LA    R4,2(R4)                                                         
         LA    R6,8(R6)                                                         
         AHI   R5,-2                                                            
         BP    CT46E08                                                          
         BCTR  R6,0                                                             
         CLI   0(R6),C','                                                       
         BNE   *+8                                                              
         MVI   0(R6),C' '                                                       
*                                                                               
CT46E10  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'47' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT47ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     PQ PASSWORD                                  
         BE    CT47E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTPQPD,R2       *** PQ PASSWORD                                  
CT47E02  BRAS  RE,PRFTYPE          OUTPUT PROFILE TYPE                          
         L     R3,FULL                                                          
         MVC   X.PDHDR,HPQPWD                                                   
         MVC   X.PDINF(L'CTPQPWD),CTPQPWD                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'48' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT48ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     PROCESSING INSTRUCTIONS                      
         BE    CT48E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTPRCD,R2       *** PROCESSING INSTRUCTIONS                      
CT48E02  MVC   X.PDHDR,HOPSINS                                                  
         XR    R0,R0                                                            
         IC    R0,CTPRCLEN                                                      
         AHI   R0,-(CTPRCINS-CTPRCD)                                            
         LHI   RF,L'PDINF                                                       
         GOTO1 VCHOPPER,DMCB,((R0),CTPRCINS),((RF),X.PDINF),(C'P',3)            
         ICM   R0,15,8(R1)                                                      
         MHI   R0,L'P                                                           
         AR    R3,R0                                                            
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'49' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT49ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,82         TOTALS DETAIL                                
         BE    CT49E02                                                          
         CLI   ADFTYPE,CTFILEQ     PQ RETAIN CLASS                              
         BE    CT49E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING DEDETD,R2       *** TOTALS DETAIL                                
CT49E02  MVC   X.PDHDR,HDETCNT                                                  
         GOTO1 VHEXOUT,DMCB,DEDETAIL,X.PDINF,L'DEDETAIL,0                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING CTPQCD,R2       *** RETAIN CLASS                                 
CT49E04  MVC   X.PDHDR,HRETCLS                                                  
         MVC   X.PDINF(L'CTPQCLAS),CTPQCLAS                                     
*                                                                               
         CLI   CTPQCLEN,CTPQCLA2-CTPQCD                                         
         BNH   CT49E06                                                          
         LA    RF,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,RF),HSRETCLS                                           
         AHI   RF,L'PDDATA+1                                                    
         MVC   0(L'CTPQCLA2,RF),CTPQCLA2                                        
*                                                                               
CT49E06  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HDETCNT  DC    CL15'Detail Control '                                            
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'4A' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT4AELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     PACKING DETAILS                              
         BE    CT4AE02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTPAKD,R2       *** PACKING DETAILS                              
CT4AE02  MVC   X.PDHDR,HPAKDET                                                  
         XR    R0,R0                                                            
         IC    R0,CTPAKLEN                                                      
         AHI   R0,-(CTPAKINS-CTPAKD)                                            
         LHI   RF,L'PDINF                                                       
         GOTO1 VCHOPPER,DMCB,((R0),CTPAKINS),((RF),X.PDINF),(C'P',3)            
         ICM   R0,15,8(R1)                                                      
         MHI   R0,L'P                                                           
         AR    R3,R0                                                            
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'4B' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT4BELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     REPORT SHORT DESCRIPTION                     
         BE    CT4BE02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTSDSD,R2       *** REPORT SHORT DESCRIPTION                     
CT4BE02  MVC   X.PDHDR,HDESC                                                    
         MVC   X.PDINF(L'CTSDSTXT),CTSDSTXT                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'4C' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT4CELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     SHIPPING INSTRUCTIONS                        
         BE    CT4CE02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTSHPD,R2       *** SHIPPING INSTRUCTIONS                        
CT4CE02  MVC   X.PDHDR,HSHPINS                                                  
         XR    R0,R0                                                            
         IC    R0,CTSHPLEN                                                      
         AHI   R0,-(CTSHPINS-CTSHPD)                                            
         LHI   RF,L'PDINF                                                       
         GOTO1 VCHOPPER,DMCB,((R0),CTSHPINS),((RF),X.PDINF),(C'P',3)            
         ICM   R0,15,8(R1)                                                      
         MHI   R0,L'P                                                           
         AR    R3,R0                                                            
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'4D' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT4DELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     JCL EXCEPTION                                
         BE    CT4DE02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTJCLD,R2       *** JCL EXCEPTION                                
CT4DE02  MVC   X.PDHDR,HJCLBK                                                   
         MVC   X.PDINF(L'CTJCLEX),CTJCLEX                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'4E' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT4EELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     TEST PHASE                                   
         BE    CT4EE02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTPHSD,R2       *** TEST PHASE                                   
CT4EE02  MVC   X.PDHDR,HTSTMSK                                                  
         LA    R6,X.PDINF                                                       
         MVC   0(3,R6),=CL3'01='                                                
         MVC   3(1,R6),CTPHS01                                                  
         MVI   4(R6),C','                                                       
         AHI   R6,5                                                             
         MVC   0(3,R6),=CL3'02='                                                
         MVC   3(1,R6),CTPHS02                                                  
         MVI   4(R6),C','                                                       
         AHI   R6,5                                                             
         MVC   0(3,R6),=CL3'03='                                                
         MVI   4(R6),C','                                                       
         MVC   3(1,R6),CTPHS03                                                  
         AHI   R6,5                                                             
         MVC   0(3,R6),=CL3'04='                                                
         MVC   3(1,R6),CTPHS04                                                  
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'4F' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT4FELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     REPORT FORM CODE                             
         BE    CT4FE02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTFRMD,R2       *** REPORT FORM CODE                             
CT4FE02  MVC   X.PDHDR,HFRMCD                                                   
         MVC   X.PDINF(L'CTFRMCOD),CTFRMCOD                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* PROFILE FIELD - X'70' SINGLE ELEMENT OUTPUT ROUTINE                 *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(ELEMENT)                                          *         
***********************************************************************         
CT70ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         USING CTFDD,R2                                                         
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     REPORT FORM CODE                             
         BE    CT70E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
CT70E02  MVC   X.PDHDR,HFLDNUM                                                  
         CURED CTFDNUM,(3,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK                    
*                                                                               
         CLI   CTFDLEN,CTFDDESC-CTFDD                                           
         BNH   CT70E04             NO DESCRIPTION                               
*                                                                               
         LA    R4,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R4),HDESC DESCRIPTION                                  
         AHI   R4,L'PDHDR+1                                                     
         XR    RF,RF                                                            
         IC    RF,CTFDLEN                                                       
         AHI   RF,-(CTFDDESC-CTFDD+1)                                           
         MVC   0(0,R4),CTFDDESC                                                 
         EX    RF,*-6                                                           
*                                                                               
CT70E04  AHI   R3,L'P                                                           
         MVC   X.PDHDR,HFLDTYP       FIELD TYPE                                 
         MVC   X.PDINF(L'NOTDEF),NOTDEF                                         
         CLI   CTFDTYPE,C'C'                                                    
         BNE   *+10                                                             
         MVC   X.PDINF(L'FDCHAR),FDCHAR                                         
         CLI   CTFDTYPE,C'H'                                                    
         BNE   *+10                                                             
         MVC   X.PDINF(L'FDHEX),FDHEX                                           
         CLI   CTFDTYPE,C'N'                                                    
         BNE   *+10                                                             
         MVC   X.PDINF(L'FDNUM),FDNUM                                           
*                                                                               
         LA    R4,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R4),HDDSONLY                                           
         AHI   R4,L'PDHDR+1                                                     
         MVC   0(4,R4),C4NOL                                                    
         TM    CTFDOTHR,X'80'                                                   
         BZ    *+10                                                             
         MVC   0(4,R4),C4YESL                                                   
*                                                                               
         TM    CTFDOTHR,X'40'      DEFAULT VALUE?                               
         BO    CT70E10             NO                                           
         LA    R4,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,R4),HDEFVAL                                            
         AHI   R4,L'PDHDR+1                                                     
*                                                                               
         CLI   CTFDTYPE,C'C'                                                    
         BNE   CT70E06                                                          
         MVC   0(1,R4),CTFDDEF                                                  
         B     CT70E10                                                          
*                                                                               
CT70E06  CLI   CTFDTYPE,C'N'                                                    
         BNE   CT70E08                                                          
         CURED CTFDDEF,(3,0(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                      
         B     CT70E10                                                          
*                                                                               
CT70E08  GOTO1 VHEXOUT,DMCB,CTFDDEF,0(R4),1,0                                   
*                                                                               
CT70E10  AHI   R3,L'P                                                           
         MVC   X.PDHDR,HDEFVAL                                                  
         MVC   X.PDINF(L'CTFDLIST),CTFDLIST                                     
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HFLDNUM  DC    CL15'Field Number   '                                            
HFLDTYP  DC    CL15'Field Type     '                                            
HDDSONLY DC    CL15'DDS Only       '                                            
FDCHAR   DC    CL10'Character '                                                 
FDHEX    DC    CL10'Hex       '                                                 
FDNUM    DC    CL10'Numeric   '                                                 
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'72' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CT72ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,53         USER PROFILE                                 
         BE    CT72E02                                                          
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTPVD,R2                                                         
CT72E02  MVC   X.PDHDR,HPAGNUM                                                  
         CURED CTPVPAGE,(3,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK                   
         AHI   R3,L'P                                                           
*                                                                               
         L     R5,AIOSORT          GET A(CHANGE/ADD RECORD)                     
         AHI   R5,RCVFRST-RECDS                                                 
R        USING CTUREC,R5                                                        
         L     R4,AIOTEMP          BUILD HEADER RECORD IN TEMP IO AREA          
         USING CTUREC,R4                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
         MVC   CTUKSYS,R.CTUKSYS                                                
         MVC   CTUKPROG,R.CTUKPROG                                              
         MVC   CTUKLANG,R.CTUKLANG                                              
         GOTO1 VDATAMGR,DMCB,(X'88',DMREAD),CTFILE,(R4),(R4)                    
         CLI   8(R1),0                                                          
         BE    CT72E04             RECORD FOUND OK                              
*                                                                               
         MVC   PDDATA(30),=CL30'Header record not on file'                      
         AHI   R3,L'P                                                           
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  R4,R                                                             
*                                                                               
CT72E04  GOTO1 VHELLO,PLIST,(C'G',CTFILE),(X'70',(R4)),0,0,0,0                  
         CLI   12(R1),0                                                         
         BE    CT72E06             ELEMENTS FOUND                               
*                                                                               
         MVC   PDDATA(30),=CL30'No 70 elements in header record'                
         AHI   R3,L'P                                                           
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
CT72E06  L     R4,12(R1)                                                        
         USING CTFDD,R4                                                         
         ST    R3,FULL             SAVE A(FIRST LINE)                           
         LA    R5,X.PDDATA                                                      
         USING PRFLINED,R5                                                      
         BRAS  RE,CT72HDR                                                       
         B     CT72E10                                                          
*                                                                               
CT72E08  OC    FULL,FULL           GOT SECOND COLUMN ALREADY?                   
         BZ    CT72E12             YES                                          
         L     R3,FULL             GET SECOND COLUMN OF FIRST LINE              
         LA    R5,X.PDDATA+L'PDDATA/2                                           
         BRAS  RE,CT72HDR                                                       
         XC    FULL,FULL           MAKE SURE DO THIS ONLY ONCE                  
         B     CT72E12                                                          
*                                                                               
CT72E10  CLI   CTFDEL,0                                                         
         BE    CT72E22                                                          
         CLI   CTFDEL,CTFDELQ                                                   
         BE    CT72E14                                                          
*                                                                               
CT72E12  XR    RF,RF               NEXT ELEMENT                                 
         IC    RF,CTFDLEN                                                       
         BXH   R4,RF,CT72E10                                                    
*                                                                               
CT72E14  CURED CTFDNUM,(3,PRFNUM),0,ALIGN=LEFT,ZERO=NOBLANK                     
*                                                                               
         MVC   PRFDDS,C4NOL                                                     
         TM    CTFDOTHR,X'80'                                                   
         BZ    *+10                                                             
         MVC   PRFDDS,C4YESL                                                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,CTFDLEN                                                       
         AHI   RF,-(CTFDDESC-CTFDD+1)                                           
         MVC   PRFDESC(0),CTFDDESC                                              
         EX    RF,*-6                                                           
*                                                                               
         IC    RF,CTFDNUM                                                       
         BCTR  RF,0                                                             
         LA    RF,CTPVALUE(RF)                                                  
*                                                                               
         CLI   CTFDTYPE,C'C'       DATA IS CHARACTER?                           
         BNE   CT72E16             NO                                           
         MVC   PRFVAL(1),0(RF)                                                  
         MVC   PRFDEF(1),CTFDDEF                                                
         B     CT72E20                                                          
*                                                                               
CT72E16  CLI   CTFDTYPE,C'N'       DATA IS NUMERIC?                             
         BNE   CT72E18             NO                                           
         CURED (B1,0(RF)),(3,PRFVAL),0,ALIGN=LEFT,ZERO=NOBLANK                  
         CURED (B1,CTFDDEF),(3,PRFDEF),0,ALIGN=LEFT,ZERO=NOBLANK                
         B     CT72E20                                                          
*                                                                               
CT72E18  GOTO1 VHEXOUT,DMCB,0(RF),PRFVAL,1,0                                    
         GOTO1 (RF),(R1),CTFDDEF,PRFDEF,1,0                                     
*                                                                               
CT72E20  XR    RF,RF                                                            
         IC    RF,CTFDNUM                                                       
         LHI   R0,1                                                             
         SLL   R0,16                                                            
         SRL   R0,0(RF)            R0 = BIT MASK                                
         XR    R1,R1                                                            
         ICM   R1,3,CTPVSTAT                                                    
         NR    R1,R0                                                            
         BZ    *+8                                                              
         MVI   PRFOVR,C'*'         NO OVERRIDE FLAG                             
*                                                                               
         AHI   R3,L'P                                                           
         AHI   R5,L'P                                                           
         CLI   CTFDNUM,8           FIELDS 9 - 16 IN SECOND COLUMN               
         BL    CT72E12                                                          
         B     CT72E08                                                          
*                                                                               
CT72E22  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
CT72HDR  MVC   PRFNUM,PRFNUMC      PUT OUT HEADERS AT (R3)                      
         MVC   PRFDDS,PRFDDSC                                                   
         MVC   PRFDEF,PRFDEFC                                                   
         MVC   PRFDESC(L'HDESC),HDESC                                           
         MVC   PRFVAL,PRFVALC                                                   
         AHI   R3,L'P                                                           
         AHI   R5,L'P                                                           
         MVC   PRFNUM,UNDERLN                                                   
         MVC   PRFDDS,UNDERLN                                                   
         MVC   PRFDEF,UNDERLN                                                   
         MVC   PRFDESC,UNDERLN                                                  
         MVC   PRFVAL,UNDERLN                                                   
         AHI   R3,L'P                                                           
         AHI   R5,L'P                                                           
         BR    RE                                                               
*                                                                               
PRFNUMC  DC    CL3'Num'                                                         
PRFDDSC  DC    CL3'DDS'                                                         
PRFVALC  DC    CL3'Val'                                                         
PRFDEFC  DC    CL3'Def'                                                         
*                                                                               
PRFLINED DSECT                                                                  
PRFNUM   DS    CL3                                                              
PRFOVR   DS    X                                                                
         DS    C                                                                
PRFDESC  DS    CL30                                                             
         DS    C                                                                
PRFVAL   DS    CL3                                                              
         DS    C                                                                
PRFDEF   DS    CL3                                                              
         DS    C                                                                
PRFDDS   DS    CL3                                                              
*                                                                               
CTDFAR   CSECT                                                                  
         DROP  R2,R4,R5,X                                                       
         EJECT                                                                  
***********************************************************************         
* X'A4' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CTA4ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,55         LIST                                         
         BE    CTA4E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTLSTD,R2                                                        
CTA4E02  MVC   X.PDHDR,HLSTDATA                                                 
         TM    CTLSTIND,X'80'                                                   
         BZ    *+10                                                             
         MVC   X.PDHDR+10(5),=CL5'No-Op'                                        
*                                                                               
         XR    R0,R0                                                            
         IC    R0,CTLSTLEN                                                      
         AHI   R0,-(CTLSTDTA-CTLSTD)                                            
         LHI   RF,L'PDINF                                                       
         GOTO1 VCHOPPER,DMCB,((R0),CTLSTDTA),((RF),X.PDINF),(C'P',3)            
         ICM   R0,15,8(R1)                                                      
         MHI   R0,L'P                                                           
         AR    R3,R0                                                            
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'A5' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CTA5ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,55         LIST INCLUDE                                 
         BE    CTA5E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTLIND,R2                                                        
CTA5E02  MVC   X.PDHDR,HINCLUDE                                                 
         MVC   X.PDINF(L'CTLINC),CTLINC                                         
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'B6' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CTB6ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADFTYPE,CTFILEQ     ASSOCIATED AGENCY                            
         BE    CTB6E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTAGLD,R2       *** ASSOCIATED AGENCY                            
CTB6E02  MVC   X.PDHDR,HASSAGY                                                  
         AHI   RF,L'PDHDR+1                                                     
*                                                                               
         XR    R0,R0               PICK UP ASSOCIATED AGENCIES                  
         IC    R0,CTAGLLEN                                                      
         AHI   R0,-(CTAGLAID-CTAGLD)                                            
         LA    RE,CTAGLAID                                                      
*                                                                               
CTB6E04  MVC   0(2,RF),0(RE)       COPY IN AGENCY ID                            
         AHI   R0,-2                                                            
         BNP   CTB6E06                                                          
         MVI   2(RF),C','                                                       
         AHI   RF,3                                                             
         AHI   RE,2                                                             
         B     CTB6E04                                                          
*                                                                               
CTB6E06  BCTR  RF,0                                                             
         CLI   0(RF),C','                                                       
         BNE   *+8                                                              
         MVI   0(RF),C' '                                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HASSAGY  DC    CL15'Associated Agys'                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* X'B7' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CTB7ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,70         LOCKET ENTRY                                 
         BE    CTB7E02                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTLKLD,R2       *** LOCKET ENTRY                                 
CTB7E02  MVC   X.PDHDR,HVTAML      VTAM LUID                                    
         MVC   X.PDINF(L'CTLKLUID),CTLKLUID                                     
*                                                                               
         LA    R4,X.PDDATA+L'PDDATA/3                                           
         MVC   0(L'PDHDR,R4),HREQTIME                                           
         AHI   R4,L'PDHDR+1        REQUEST TIME                                 
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,7,CTLKLT1                                                     
         SLL   R0,4                                                             
         O     R0,=X'0000000C'                                                  
         ST    R0,FULL                                                          
         CURED (P4,FULL),(8,DUB),0,ZERO=NOBLANK                                 
         MVC   0(2,R4),DUB+2                                                    
         MVC   3(2,R4),DUB+4                                                    
         MVC   6(2,R4),DUB+6                                                    
         MVI   2(R4),C':'                                                       
         MVI   5(R4),C':'                                                       
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,7,CTLKLT2                                                     
         BZ    CTB7E04                                                          
*                                                                               
         LA    R4,X.PDDATA+L'PDDATA*2/3                                         
         MVC   0(L'PDHDR,R4),HCMPTIME                                           
         AHI   R4,L'PDHDR+1        COMPLETED TIME                               
*                                                                               
         SLL   R0,4                                                             
         O     R0,=X'0000000C'                                                  
         ST    R0,FULL                                                          
         CURED (P4,FULL),(8,DUB),0,ZERO=NOBLANK                                 
         MVC   0(2,R4),DUB+2                                                    
         MVC   3(2,R4),DUB+4                                                    
         MVC   6(2,R4),DUB+6                                                    
         MVI   2(R4),C':'                                                       
         MVI   5(R4),C':'                                                       
*                                                                               
CTB7E04  AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
HCMPTIME DC    CL15'Completion Time'                                            
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'B8' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CTB8ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,28         ACTION                                       
         BE    CTB8E02                                                          
         CLI   ADFTYPE,CTFILEQ     SEC AGY ALPHA ID                             
         BE    CTB8E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING SAACTD,R2                                                        
CTB8E02  GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
*                                                                               
         USING CTSEAD,R2       *** SEC AGY ALPHA ID                             
CTB8E04  MVC   X.PDHDR,HALFID                                                   
         LA    RF,L'PDHDR+1(R3)                                                 
         MVC   X.PDINF(L'CTSEAAID),CTSEAAID                                     
*                                                                               
         MVC   HALF,CTSEAAID       OUTPUT AGENCY NAME                           
         BRAS  RE,GETAGYID                                                      
         MVI   X.PDINF+04,C'<'                                                  
         MVC   X.PDINF+05(10),WORK+2                                            
         AHI   RF,15                                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'>'                                                       
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2,X                                                             
         EJECT                                                                  
***********************************************************************         
* X'C2' ELEMENT HANDLING ROUTINES                                     *         
*                                                                     *         
* NTRY: 0(R1)   = A(ELEMENT)                                          *         
*       4(R1)   = A(PRINT LINE TO USE)                                *         
* EXIT: FULL    = A(NEXT PRINT LINE)                                  *         
***********************************************************************         
CTC2ELM  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
X        USING PLINED,R3                                                        
*                                                                               
         L     RF,AADDNTRY                                                      
         USING ADDTABD,RF                                                       
         CLI   ADRECNUM,08         PRINTER NAME DESCRIPTION                     
         BE    CTC2E02                                                          
         CLI   ADRECNUM,23         PERSON COUNT                                 
         BE    CTC2E04                                                          
*                                                                               
         GOTO1 APRELOUT,DMCB,(R2),(R3)                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
         USING CTPNDD,R2           PRINTER NAME DESCRIPTION                     
CTC2E02  MVC   X.PDHDR,HDESC                                                    
         XR    RF,RF                                                            
         IC    RF,CTPNDLEN                                                      
         AHI   RF,-CTPNDLNQ+1                                                   
         MVC   X.PDINF(0),CTPNDTXT                                              
         EX    RF,*-6                                                           
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         USING SAPCTD,R2                                                        
CTC2E04  MVC   X.PDHDR,HPRSNCNT                                                 
         CURED SAPCTVAL,(8,X.PDINF),0,ALIGN=LEFT,ZERO=NOBLANK                   
         AHI   R3,L'P                                                           
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ACTIVITY ELEMENT (X'F1' ON CTFILE)                                 *          
* NTRY: R2 = A(ELEMENT)                                              *          
**********************************************************************          
         USING ACTVD,R2                                                         
ACF1ELM  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,PBLOCK+(PDHDR-PLINED)                                         
         MVC   0(L'PDHDR,R3),HADDON                                             
         LA    RF,L'PDHDR+1(R3)                                                 
         GOTO1 VDATCON,PLIST,(3,ACTVADDT),(5,0(RF)),0                           
*                                                                               
         LA    RF,L'PDDATA/3(R3)                                                
         MVC   0(L'PDHDR,RF),HCHGON                                             
         AHI   RF,L'PDHDR+1                                                     
         GOTO1 VDATCON,PLIST,(3,ACTVCHDT),(5,0(RF)),0                           
*                                                                               
         LA    RF,L'PDDATA*2/3(R3)                                              
         MVC   0(L'PDHDR,RF),HCNTOF                                             
         AHI   RF,L'PDHDR+1                                                     
         CURED (B1,ACTVCHNM),(3,0(RF)),0,ZERO=NOBLANK,ALIGN=LEFT                
*                                                                               
         AHI   R3,L'P                                                           
         MVC   0(L'PDHDR,R3),HADDBY                                             
         LA    RF,L'PDHDR+1(R3)                                                 
         MVC   HALF,ACTVADID                                                    
         XC    HALF,EFFS                                                        
         BRAS  RE,GETUSRID                                                      
         MVC   0(10,RF),WORK+2                                                  
*                                                                               
         LA    RF,L'PDDATA/3(R3)                                                
         MVC   0(L'PDHDR,RF),HCHGBY                                             
         AHI   RF,L'PDHDR+1                                                     
         MVC   HALF,ACTVCHID                                                    
         XC    HALF,EFFS                                                        
         BRAS  RE,GETUSRID                                                      
         MVC   0(10,RF),WORK+2                                                  
*                                                                               
         OC    ACTVSCID,ACTVSCID                                                
         BZ    ACF102                                                           
         LA    RF,L'PDDATA*2/3(R3)                                              
         MVC   0(L'PDHDR,RF),HPRSNID                                            
         AHI   RF,L'PDHDR+1                                                     
         MVC   0(L'ACTVSCID,RF),ACTVSCID                                        
*                                                                               
ACF102   BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
HADDON   DC    CL15'Added on'                                                   
HADDBY   DC    CL15'Added by'                                                   
HCHGON   DC    CL15'Changed on'                                                 
HCHGBY   DC    CL15'Changed by'                                                 
HCNTOF   DC    CL15'Change Number'                                              
HPRSNID  DC    CL15'Person Id'                                                  
         EJECT                                                                  
***********************************************************************         
* OUTPUT LINE OF KEY TITLE DETAILS                                    *         
***********************************************************************         
KEYHEAD  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AOUTFORM         A(LIST OF DISPLACEMENTS FOR TITLES)          
         USING OUTFORM,R6                                                       
PH       USING PLINED,BOXCOLS                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   PH.PHLEFT,C'L'                                                   
         MVI   PH.PHRIGHT,C'R'                                                  
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+2,C'T'                                                   
         XR    RF,RF                                                            
         ICM   RF,3,MAXLINE                                                     
         BCTR  RF,0                                                             
         LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'B'                                                       
*                                                                               
         CLI   BOXSTAT,C'I'        INSIDE A BOX ALREADY?                        
         BE    KHEAD02             YES                                          
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            PRINT HEADLINES                              
*                                                                               
KHEAD02  CLC   =C'PROF',LSTPNAM    VARIABLE TITLE POSITIONS HERE                
         BNE   KHEAD04                                                          
         MVI   PH.PHC1,C'C'                                                     
         MVI   PH.PHC2,C'C'                                                     
         MVI   PH.PHC3,C'C'                                                     
         MVI   PH.PHC4,C'C'                                                     
         B     KHEAD06                                                          
*                                                                               
KHEAD04  MVI   PH.PH1C1,C'C'                                                    
         MVI   PH.PH1C2,C'C'                                                    
         MVI   PH.PH1C3,C'C'                                                    
         MVI   PH.PH1C4,C'C'                                                    
*                                                                               
KHEAD06  CLC   =C'PROF',LSTPNAM    VARIABLE TITLE POSITIONS HERE                
         BNE   KHEAD08                                                          
         MVC   PHUID,IPHUID                                                     
         MVC   PHTRM,IPHTRM                                                     
         MVC   PHREC,IPHREC                                                     
         MVC   PHKEY,IPHKEY                                                     
         MVC   PHTME,IPHTME                                                     
         GOTO1 VPRINTER            PRINT HEADLINES LINE 1                       
*                                                                               
         MVC   PHACT,IPHACT                                                     
         MVC   PHPER,IPHPER                                                     
         B     KHEAD10                                                          
*                                                                               
KHEAD08  MVC   PH1UID,IPHUID                                                    
         MVC   PH1TRM,IPHTRM                                                    
         MVC   PH1REC,IPHREC                                                    
         MVC   PH1KEY,IPHKEY                                                    
         MVC   PH1TME,IPHTME                                                    
         GOTO1 VPRINTER            PRINT HEADLINES LINE 1                       
*                                                                               
         MVC   PH1ACT,IPHACT                                                    
         MVC   PH1PER,IPHPER                                                    
*                                                                               
KHEAD10  GOTO1 VPRINTER            PRINT HEADLINES LINE 2                       
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            UNDERLINE THEM                               
         B     EXITOK                                                           
         DROP  R6                                                               
*                                                                               
IPHUID   DC    CL(L'PHUID)'User Id'                                             
IPHPER   DC    CL(L'PHPER)'Person'                                              
IPHTRM   DC    CL(L'PHTRM)'Terminal'                                            
IPHREC   DC    CL(L'PHREC)'Record Type'                                         
IPHACT   DC    CL(L'PHACT)'Action'                                              
IPHKEY   DC    CL(L'PHKEY)'Information from record key'                         
IPHTME   DC    CL(L'PHTME)'Time'                                                
*                                                                               
PLINED   DSECT                                                                  
         ORG   PLINED                                                           
PHLEFT   DS    C                                                                
         DS    X                                                                
PHUID    DS    CL10                                                             
         ORG   PHUID+1                                                          
PHACT    DS    CL8                                                              
         ORG   PHUID+L'PHUID                                                    
         DS    X                                                                
PHC1     DS    C                                                                
         DS    X                                                                
PHTRM    DS    CL8                                                              
         ORG   PHTRM+1                                                          
PHPER    DS    CL8                                                              
         DS    X                                                                
PHC2     DS    C                                                                
         DS    X                                                                
PHREC    DS    CL15                                                             
         DS    X                                                                
PHC3     DS    C                                                                
         DS    X                                                                
PHKEY    DS    CL75                                                             
         DS    X                                                                
PHC4     DS    C                                                                
         DS    X                                                                
PHTME    DS    CL6                                                              
         DS    X                                                                
*                                                                               
         ORG   PLINED                                                           
         DS    C                                                                
         DS    X                                                                
PH1REC   DS    CL15                                                             
         DS    X                                                                
PH1C1    DS    C                                                                
         DS    X                                                                
PH1UID   DS    CL10                                                             
         ORG   PH1UID+1                                                         
PH1ACT   DS    CL8                                                              
         ORG   PH1UID+L'PH1UID                                                  
         DS    X                                                                
PH1C2    DS    C                                                                
         DS    X                                                                
PH1TRM   DS    CL8                                                              
         ORG   PH1TRM+1                                                         
PH1PER   DS    CL8                                                              
         DS    X                                                                
PH1C3    DS    C                                                                
         DS    X                                                                
PH1KEY   DS    CL75                                                             
         DS    X                                                                
PH1C4    DS    C                                                                
         DS    X                                                                
PH1TME   DS    CL6                                                              
         DS    X                                                                
*                                                                               
         ORG   PLINED                                                           
         DS    C                                                                
         DS    X                                                                
PDELEM   DS    CL16                                                             
         DS    X                                                                
PDC1     DS    C                                                                
         DS    X                                                                
PDDATA   DS    0CL99                                                            
PDHDR    DS    CL15                                                             
         DS    X                                                                
PDINF    DS    CL83                                                             
         DS    X                                                                
PDC2     DS    C                                                                
         DS    X                                                                
PDACT    DS    CL6                                                              
         DS    X                                                                
*                                                                               
         ORG   PLINE+L'PLINE-1                                                  
PHRIGHT  DS    C                                                                
*                                                                               
CTDFAR   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* OUTPUT PRINT CONTROL ROUTINE                                        *         
***********************************************************************         
PRNT     NTR1  BASE=*,LABEL=*                                                   
         LHI   R1,PBLOCKL-L'P                                                   
         LA    R1,PBLOCK(R1)                                                    
*                                                                               
         LHI   R0,PBLNUMQ                                                       
PRNT02   CLC   0(L'P,R1),SPACES    LOOK FOR LAST NON-SPACE LINE                 
         BH    PRNT04                                                           
         AHI   R1,-L'P                                                          
         BCT   R0,PRNT02                                                        
         MVC   P,SPACES            IF ALL SPACES - PRINT 1 LINE ONLY            
         GOTO1 VPRINTER                                                         
         B     PRNT10                                                           
*                                                                               
PRNT04   CVD   R0,DUB                                                           
         ZAP   FULL,LINE                                                        
         AP    FULL,DUB                                                         
         CP    FULL,MAXLINE                                                     
         BNH   PRNT06                                                           
         ZAP   LINE,P99                                                         
*                                                                               
PRNT06   LA    R2,PBLOCK                                                        
         LHI   R0,PBLNUMQ                                                       
PRNT08   CLC   0(L'P,R2),SPACES                                                 
         BNH   PRNT10                                                           
         MVC   P,0(R2)                                                          
         GOTO1 VPRINTER                                                         
         AHI   R2,L'P                                                           
         BCT   R0,PRNT08                                                        
*                                                                               
PRNT10   LA    R0,PBLOCK           CLEAR PBLOCK TO SPACES                       
         LHI   R1,PBLOCKL                                                       
         LHI   RF,C' '                                                          
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ CTFILE FOR A USERID                                            *         
* NTRY: HALF    = USERID TO READ                                      *         
* EXIT: WORK    = USERID(2)/USERNAME(10)                              *         
***********************************************************************         
GETUSRID NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(12),WORK       CLEAR USERID AND NAME READ                   
         MVC   WORK(2),HALF                                                     
*                                                                               
         L     R3,AIOTEMP                                                       
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    SET UP KEY FOR USERID                        
         MVC   CTIKNUM,HALF                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R3),(R3)                            
         CLI   8(R1),0                                                          
         BNE   GUSRBAD                                                          
*                                                                               
         GOTO1 VHELLO,PLIST,(C'G',CTFILE),(2,CTIREC),0,0,0,0                    
         CLI   12(R1),0            ELEMENT FOUND?                               
         BNE   GUSRBAD                                                          
*                                                                               
         ICM   RF,15,12(R1)                                                     
         CLI   1(RF),4             LENGTH MUST BE >4                            
         BNH   GUSRBAD                                                          
         MVC   WORK+2(10),2(RF)                                                 
         B     EXITOK                                                           
*                                                                               
GUSRBAD  MVC   WORK+2(10),SPACES                                                
         MVI   WORK+2,C'#'                                                      
         CURED HALF,(6,WORK+3),0,ALIGN=LEFT,ZERO=NOBLANK                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ CTFILE FOR AN AGENCY ID                                        *         
* NTRY: HALF    = USERID TO READ                                      *         
* EXIT: WORK    = USERID(2)/USERNAME(10)                              *         
***********************************************************************         
GETAGYID NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(12),WORK       CLEAR USERID AND NAME READ                   
         MVC   WORK(2),HALF                                                     
*                                                                               
         L     R3,AIOTEMP                                                       
         USING CT5REC,R3                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    SET UP KEY FOR USERID                        
         MVC   CT5KALPH,HALF                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R3),(R3)                            
         CLI   8(R1),0                                                          
         BNE   GALFBAD                                                          
*                                                                               
         GOTO1 VHELLO,PLIST,(C'G',CTFILE),(2,CT5REC),0,0,0,0                    
         CLI   12(R1),0            ELEMENT FOUND?                               
         BNE   GALFBAD                                                          
*                                                                               
         L     RF,12(R1)                                                        
         MVC   HALF,2(RF)                                                       
         BRAS  RE,GETUSRID                                                      
         B     EXITOK                                                           
*                                                                               
GALFBAD  MVC   WORK+2(10),SPACES                                                
         MVC   WORK+2(L'NOTDEF),NOTDEF                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ CTFILE FOR A TERMINAL ID                                       *         
***********************************************************************         
GETTRM   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOSORT                                                       
         USING RECDS,R2                                                         
         XC    LSTCTFRT,LSTCTFRT   CLEAR LAST READ                              
         MVC   LSTTNUM,RTRM                                                     
         MVC   LSTTNAM,=10C'?'                                                  
*                                                                               
         CLI   TRAILER,C'Y'        TRAILER RECORDS?                             
         BNE   GTRM02              NO                                           
         LH    RF,0(R2)                                                         
         AR    RF,R2                                                            
         BCTR  RF,0                                                             
         XR    R0,R0                                                            
         IC    R0,0(RF)                                                         
         AHI   RF,1                                                             
         SR    RF,R0                                                            
         USING RECVEXT,RF                                                       
         MVC   LSTTNAM,RLUID                                                    
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
GTRM02   L     R3,AIOTEMP                                                       
         USING CTTREC,R3                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   23(2,R3),RTRM                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R3),(R3)                            
         CLI   8(R1),0                                                          
         BE    GTRM04                                                           
         MVC   LSTTNAM,SPACES                                                   
         CURED RTRM,(6,LSTTNAM),0,ALIGN=LEFT,ZERO=NOBLANK                       
         B     EXITOK                                                           
*                                                                               
GTRM04   GOTO1 VHELLO,PLIST,(C'G',CTFILE),(3,(R3)),0,0,0,0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,12(R1)                                                        
         MVC   LSTTNAM,2(RF)                                                    
         CLC   LSTNAM(2),=C'AA'    CHARACTERS?                                  
         BNL   EXITOK                                                           
         MVC   LSTNAM,=CL8'Automate'                                            
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR MESSAGE                                      *         
***********************************************************************         
ERRMSG   NTR1  BASE=*,LABEL=*                                                   
         CLI   FERN,0                                                           
         BE    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FERN                                                          
         BCTR  RF,0                                                             
         MHI   RF,EMSGL                                                         
         A     RF,AERRTAB          RF = A(ERROR MESSAGE)                        
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(L'ERRHDR),ERRHDR                                               
         MVC   P+L'ERRHDR(EMSGL),0(RF)                                          
         GOTO1 VPRINTER                                                         
         MVI   FERN,0                                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET UPSI PARAMETERS                                                 *         
***********************************************************************         
GETUPSI  NTR1  BASE=*,LABEL=*                                                   
         L     R1,ACMRG                                                         
         L     R1,0(R1)                                                         
         XR    R2,R2                                                            
         ICM   R2,3,0(R1)                                                       
         BZ    EXITOK                                                           
*                                                                               
         AHI   R1,2                                                             
         LA    RF,UPSITAB                                                       
GUP02    CLI   0(R1),C'0'                                                       
         BE    GUP04                                                            
         CLI   0(R1),C'1'                                                       
         BNE   EXITOK                                                           
         OC    UPSI,0(RF)                                                       
*                                                                               
GUP04    AHI   R1,1                                                             
         AHI   RF,1                                                             
         BCT   R2,GUP02                                                         
         B     EXITOK                                                           
*                                                                               
UPSITAB  DC    X'8040201008040201'                                              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT NUMBERS IN  99999.99999 FORMAT                    *         
* NTRY: R1     = A(NUMBER)                                            *         
***********************************************************************         
EDITNUM  NTR1  BASE=*,LABEL=*                                                   
         XC    DUB,DUB                                                          
         MVC   DUB+2(6),0(R1)                                                   
         MVC   WORK(12),=X'4020202021204B2020202020'                            
         MVC   WORK+6(1),CURSEP                                                 
         ED    WORK(12),DUB+2                                                   
         LA    R1,WORK+11                                                       
EDITN1   LA    RF,WORK                                                          
         CR    R1,RF                                                            
         BNH   EDITN2                                                           
         CLC   0(1,R1),CURSEP      SEPARATOR                                    
         BE    EDITN2                                                           
         CLI   0(R1),C'0'                                                       
         BNE   EDITN3                                                           
         MVI   0(R1),C' '                                                       
         BCT   R1,EDITN1                                                        
EDITN2   MVI   1(R1),C'0'                                                       
         LA    R1,1(R1)                                                         
EDITN3   LA    R0,12                                                            
         CLI   WORK,C' '                                                        
         BNE   *+20                                                             
         MVC   WORK(11),WORK+1                                                  
         MVI   WORK+11,C' '                                                     
         BCTR  R1,0                                                             
         BCT   R0,*-20                                                          
EDNMX    B     EXITOK                                                           
*                                                                               
CURSEP   DC    C'.'                                                             
         EJECT                                                                  
***********************************************************************         
* PROFILE ELEMENTS (40-4F) OUTPUT PROFILE TYPE INFORMATION            *         
* NTRY: R2     = A(ELEMENT)                                           *         
*       R3     = A(PRINT LINE)                                        *         
* EXIT: FULL   = A(FIRST CLEAR LINE)                                  *         
***********************************************************************         
         USING CTDCOD,R2                                                        
X        USING PLINED,R3                                                        
PRFTYPE  NTR1  BASE=*,LABEL=*                                                   
         MVC   X.PDHDR,HPRFTYPE                                                 
*                                                                               
         CLI   CTDCOTYP,C'S'       SACRED                                       
         BNE   *+14                                                             
         MVC   0(25,RF),=CL25'Sacred (No Overrides)'                            
         B     PFT06                                                            
*                                                                               
         CLI   CTDCOTYP,C'P'       PERMANENT                                    
         BNE   *+14                                                             
         MVC   0(25,RF),=CL25'Permanent'                                        
         B     PFT06                                                            
*                                                                               
         CLI   CTDCOTYP,C'D'       DAY OF WEEK                                  
         BNE   PFT02                                                            
         MVC   0(18,RF),=CL18'Day of week - For '                               
         MVC   18(L'CTDCODTA,RF),CTDCODTA                                       
         B     PFT06                                                            
*                                                                               
PFT02    CLI   CTDCOTYP,C'T'       TEMPORARY                                    
         BNE   PFT04                                                            
         MVC   0(18,RF),=CL18'Temporary - Until '                               
         GOTO1 VDATCON,PLIST,(3,CTDCODTA),(5,18(RF)),0                          
         B     PFT06                                                            
*                                                                               
PFT04    MVC   0(L'NOTDEF,RF),NOTDEF                                            
         B     PFT06                                                            
*                                                                               
PFT06    AHI   R3,L'P              NEXT LINE                                    
         ST    R3,FULL                                                          
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* COMMONLY ADDRESSIBLE ROUTINES - ADDRESSED OFF R?                   *          
**********************************************************************          
COMMON   DC    CL8'*COMMON*'                                                    
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD                                                        
         XBASE                                                                  
         EJECT                                                                  
**********************************************************************          
* CONSTANTS AND EQUATES                                              *          
**********************************************************************          
ELDATAD  EQU   32                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
C4YESL   DC    CL4'Yes '                                                        
C4NOL    DC    CL4'No  '                                                        
C4YES    DC    CL4'YES '                                                        
C4NO     DC    CL4'NO  '                                                        
EFFS     DC    8X'FF'                                                           
UNDERLN  DC    32C'-'                                                           
*                                                                               
CTFILEQ  EQU   X'A1'                                                            
CTREQQ   EQU   X'A3'                                                            
GENDIRQ  EQU   X'AE'                                                            
GENFILQ  EQU   X'AF'                                                            
*&&UK                                                                           
DEMOLDQ  EQU   X'A8'                                                            
DEMNEWQ  EQU   X'A9'                                                            
DEMDIRQ  EQU   X'AA'                                                            
*&&                                                                             
*&&US                                                                           
CTUSER   EQU   X'AD'                                                            
*&&                                                                             
SEFILEQ  EQU   X'EE'                                                            
*                                                                               
FF       EQU   255                                                              
IOL      EQU   2144                LENGTH OF AN IO AREA                         
*                                                                               
@CRCVDTF DC    A(0)                A(CONTROL FILE DTF)                          
*                                                                               
AADDTAB  DC    A(ADDTAB)           A(ADDTAB)                                    
ACTFADD  DC    A(CTFADD)           A(CTFILE PORTION OF ADDTAB)                  
ACTRADD  DC    A(CTRADD)           A(CTREQ PORTION OF ADDTAB)                   
ACTGADD  DC    A(CTGADD)           A(GENDIR/FIL PORTION OF ADDTAB)              
ACTUADD  DC    A(CTUADD)           A(UNKNOWN RECORD TYPES)                      
*                                                                               
AADDTOTS DC    A(ADDTOTS)                                                       
*                                                                               
ACTELEMS DC    A(CTELEMS)          A(ELEMENTS IN CONTROL FILE)                  
*                                                                               
ACARDTAB DC    A(CARDTAB)                                                       
AINPACCS DC    A(INPACCS)                                                       
AINPERRS DC    A(INPERRS)                                                       
AINPTOTS DC    A(INPTOTS)                                                       
ABUFFER  DC    A(BUFFER)                                                        
AIOSORT  DC    A(IO1)                                                           
AIOCOPY  DC    A(IO2)                                                           
AIOTEMP  DC    A(IO3)                                                           
ATYPTBL  DC    A(TYPTBL)                                                        
AERRTAB  DC    A(ERRTAB)                                                        
*                                                                               
VCARDS   DC    V(CARDS)                                                         
VCHOPPER DC    V(CHOPPER)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VCTRYTAB DC    V(CTRYTAB)                                                       
VDATAMGR DC    V(DATAMGR)                                                       
VDATCON  DC    V(DATCON)                                                        
VDMOD000 DC    V(DMOD000)                                                       
VDADDS   DC    V(DADDS)                                                         
VHELLO   DC    V(HELLO)                                                         
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VLANGTAB DC    V(LANGTAB)                                                       
VLOGIO   DC    V(LOGIO)                                                         
VPRINTER DC    V(PRINTER)                                                       
VSCANNER DC    V(SCANNER)                                                       
VSELIST  DC    V(SELIST)                                                        
VSORTER  DC    V(SORTER)                                                        
VBOXAREA DC    V(BOXAREA)                                                       
CUREDIT  DC    V(CUREDIT)                                                       
VSYSLST  DC    A(SYSLST)                                                        
VUNTIME  DC    V(UNTIMEC)                                                       
AFACITAB DC    A(FACIDTAB)                                                      
APRELOUT DC    A(PRELOUT)                                                       
*                                                                               
DMFLIST  DC    C'NCTRCVR NCTFILE X'                                             
DMSYS    DC    CL8'CONTROL '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
DMDTF    DC    CL8'DTFAD   '                                                    
GETREC   DC    CL8'GETREC  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
CTRCVR   DC    CL8'CTRCVR  '                                                    
TRUE     DC    CL8'TRUE    '                                                    
TAPE     DC    CL4'TAPE'                                                        
DISK     DC    CL4'DISK'                                                        
FALSE    DC    CL8'FALSE   '                                                    
DMOPEN   DC    CL8'OPEN'                                                        
DMCLOSE  DC    CL8'DMCLSE'                                                      
PUT      DC    CL4'PUT '                                                        
GET      DC    CL4'GET '                                                        
END      DC    CL4'END '                                                        
ENQCTRL  DC    CL8'ENQCTL'                                                      
*                                                                               
ERASESW  DC    AL1(NO)             ERASE FILE DEFAULT = N                       
DODUMP   DC    AL1(YES)            WRITE RECOVERY TAPE DEFAULT = Y              
COPYTAPE DC    AL1(NO)             COPY OF RECOVERY TAPE REQUIRED               
*                                                                               
ERRHDR   DC    C'*** ERROR - '                                                  
*                                                                               
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
PTWO     DC    P'2'                                                             
PTHREE   DC    P'3'                                                             
PFOUR    DC    P'4'                                                             
PFIVE    DC    P'5'                                                             
P99      DC    P'99'                                                            
*                                                                               
DMCBENQ  DS    6F                                                               
*                                                                               
ERSPARS  DC    A(0)                PARAM LIST TO ERASE FILE                     
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(ERSPAR6)                                                       
ERSPAR6  DC    A(0)                                                             
*                                                                               
DEF_SORT DC    CL80'SORT FIELDS=(22,1,A,25,2,A,7,2,A,29,25,A,9,4,A),FOR*        
               MAT=BI,WORK=1'                                                   
*                                                                               
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(2100,,,,) '                              
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=27648,LRECL=8200,BUFNO=2                                 
                                                                                
RCVCOPY  DCB   DDNAME=RCVCOPY,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=27648,LRECL=8200,BUFNO=2                                 
                                                                                
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GL),RECFM=VB,             *        
               BLKSIZE=0,LRECL=8200,BUFNO=2,EODAD=TAPEEND                       
*                                                                               
MTHTAB   DC    C'JanFebMarAprMayJunJulAugSepOctNovDec'                          
*                                                                               
MODTAB   DS    0CL8                                                             
         DC    CL8'Add'                                                         
         DC    CL8'Change'                                                      
         DC    CL8'Delete'                                                      
         DC    CL8'Restore'                                                     
         DC    CL8'From'                                                        
*                                                                               
RCVKEY   DC    C'Recovery Key = '                                               
                                                                                
***********************************************************************         
*THIS IS THE TABLE WHICH DEFINES THE POSITION ON THE PAGE OF THE HEADER         
*INFORMATION WHICH COMES OUT FOR EACH RECORD                                    
*FORMAT: PROG/USERID/TERMINAL/ACTION/TIME/KEY/RECORD NAME                       
***********************************************************************         
                                                                                
         DS    0H                                                               
PAGEOUT  DC    AL2(PHPER-PLINED+L'P)     OUTPRG                                 
         DC    AL2(PHUID-PLINED)         OUTUID                                 
         DC    AL2(PHTRM-PLINED)         OUTTRM                                 
         DC    AL2(PHACT-PLINED+L'P)     OUTACT - ON LINE 2                     
         DC    AL2(PHTME-PLINED)         OUTTIME                                
         DC    AL2(PHKEY-PLINED)         OUTKEY                                 
         DC    AL2(PHREC-PLINED)         OUTNAME                                
*                                                                               
PAGEOUT1 DC    AL2(PH1PER-PLINED+L'P)    OUTPRG                                 
         DC    AL2(PH1UID-PLINED)        OUTUID                                 
         DC    AL2(PH1TRM-PLINED)        OUTTRM                                 
         DC    AL2(PH1ACT-PLINED+L'P)    OUTACT                                 
         DC    AL2(PH1TME-PLINED)        OUTTIME                                
         DC    AL2(PH1KEY-PLINED)        OUTKEY                                 
         DC    AL2(PH1REC-PLINED)        OUTNAME                                
*                                                                               
MSG1     DC    CL50'Recovery file tape created ok'                              
MSG2     DC    CL50'Error opening recovery tape - Please Check'                 
MSG3     DC    CL50'Error opening recovery copy - Please Check'                 
MSG4     DC    CL50'Error closing recovery tape - Please Check'                 
MSG5     DC    CL50'Error closing recovery copy - Please Check'                 
*                                                                               
YAUTH    DC    X'000F'             Used for 21 Element Validation               
NAUTH    DC    X'0000'             "                                            
XAUTH    DC    X'FFFF'             "                                            
*                                                                               
BAUD     DC    CL04'Baud'                                                       
YES4     DC    CL04'Yes '                                                       
ALL4     DC    CL04'All '                                                       
NO4      DC    CL04'No  '                                                       
UNKNOWN  DC    CL08'Unknown'                                                    
PASSIVE  DC    CL08'Passive'                                                    
DEFAULT  DC    CL08'Default'                                                    
NOTDEF   DC    CL09'Undefined'                                                  
REQURD   DC    CL09'Required'                                                   
NREQURD  DC    CL12'Not Required'                                               
WORKER   DC    CL7'Worker'                                                      
ADD      DC    CL7'Add'                                                         
DELETE   DC    CL7'Delete'                                                      
*                                                                               
HACTON   DC    CL15'Activity on    '                                            
HAGYID   DC    CL15'Agency Id      '                                            
HALFID   DC    CL15'Alpha Id       '                                            
HATTYPE  DC    CL15'Attention Type '                                            
HAUTHS   DC    CL15'Authorisation  '                                            
HCLIENT  DC    CL15'Client         '                                            
HCOMMENT DC    CL15'Comment '                                                   
HCNTRY   DC    CL15'Country        '                                            
HDATE    DC    CL15'Date           '                                            
HDATTIM  DC    CL15'Date / Time    '                                            
HDEFVAL  DC    CL15'Default value  '                                            
HDESC    DC    CL15'Description    '                                            
HDSTID   DC    CL15'Destination Id '                                            
HDEST    DC    CL15'Destination    '                                            
HDSTNUM  DC    CL15'Destination Num'                                            
HDETAIL  DC    CL15'Detail         '                                            
HDISTYPE DC    CL15'Display Type   '                                            
HDISDEV  DC    CL15'Display Device '                                            
HESCCHR  DC    CL15'Escape Char    '                                            
HFACPAK  DC    CL15'Facpak         '                                            
HFLDDATA DC    CL15'Field Data     '                                            
HFLDID   DC    CL15'Field Id       '                                            
HFILTER  DC    CL15'Filter         '                                            
HFSTINIT DC    CL15'First Initial  '                                            
HFRMCD   DC    CL15'Form Code      '                                            
HFREQ    DC    CL15'Frequency      '                                            
HHDR     DC    CL15'Header         '                                            
HHDNG    DC    CL15'Heading        '                                            
HHIGHSEQ DC    CL15'High Sequence #'                                            
HIDNAM   DC    CL15'Id Name        '                                            
HIDNUM   DC    CL15'Id Number      '                                            
HIDSEQ   DC    CL15'Id/Sequence #  '                                            
HINCLUDE DC    CL15'Include        '                                            
HINDNTRY DC    CL15'Index Entries  '                                            
HINSTRCT DC    CL15'Instruction    '                                            
HIPAAGY  DC    CL15'IPA Agency Id  '                                            
HJCLBK   DC    CL15'JCL Book       '                                            
HLABEL   DC    CL15'Label          '                                            
HLSTRUN  DC    CL15'Last run date  '                                            
HLIBBK   DC    CL15'Library Book   '                                            
HLIMACC  DC    CL15'Limit Access   '                                            
HLINEID  DC    CL15'Line Id        '                                            
HLINNUM  DC    CL15'Line Number'                                                
HLINESPD DC    CL15'Line Speed     '                                            
HLINETYP DC    CL15'Line Type      '                                            
HLSTDATA DC    CL15'List Data      '                                            
HLSTTYPE DC    CL15'List Type      '                                            
HLOCKKEY DC    CL15'Lock Key       '                                            
HLOGO1   DC    CL15'Logo 1         '                                            
HLOGO2   DC    CL15'Logo 2         '                                            
HMEDIA   DC    CL15'Media          '                                            
HMESSAGE DC    CL15'Message        '                                            
HMIDINIT DC    CL15'Middle Initial '                                            
HMINIO   DC    CL15'Minio Key      '                                            
HNAME    DC    CL15'Name           '                                            
HNXTRUN  DC    CL15'Next run date'                                              
HNODENM  DC    CL15'Node name      '                                            
HNUM     DC    CL15'Number         '                                            
HDATA    DC    CL15'Data           '                                            
HOFFCODE DC    CL15'Office Code    '                                            
HOFFOVER DC    CL15'Office Override'                                            
HOPSINS  DC    CL15'Ops Instruction'                                            
HORGADDR DC    CL15'Origin Address '                                            
HORGID   DC    CL15'Origin Id #    '                                            
HORGNAME DC    CL15'Origin Name    '                                            
HOUTCODE DC    CL15'Output Code    '                                            
HOUTMODE DC    CL15'Output Mode    '                                            
HOUTTYPE DC    CL15'Output Type    '                                            
HPAKDET  DC    CL15'Packing details'                                            
HPASSWD  DC    CL15'Password       '                                            
HPAGNUM  DC    CL15'Page Number    '                                            
HPRSNCNT DC    CL15'Person Count   '                                            
HPRSAUTH DC    CL15'Person Auth #  '                                            
HPHASE   DC    CL15'Phase          '                                            
HPQNTRY  DC    CL15'PQ Entry Number'                                            
HPQPWD   DC    CL15'PQ Password    '                                            
HPOWERCD DC    CL15'Power Code     '                                            
HPRINTER DC    CL15'Printer        '                                            
HPRTAUTO DC    CL15'Print Automode '                                            
HPRNTRLS DC    CL15'Printer List   '                                            
HPRTSPD  DC    CL15'Printer Speed  '                                            
HPRFDATA DC    CL15'Profile Data   '                                            
HPRFTYPE DC    CL15'Profile Type   '                                            
HPROGRAM DC    CL15'Program        '                                            
HPGMAUTH DC    CL15'Program Auths  '                                            
HRTYPE   DC    CL15'Rate Type      '                                            
HRDRCLS  DC    CL15'Reader Class   '                                            
HRECSTMT DC    CL15'Record Statemnt'                                            
HRECTYPE DC    CL15'Record Type    '                                            
HREQID   DC    CL15'Request Id     '                                            
HREQTIME DC    CL15'Request Time   '                                            
HRETCLS  DC    CL15'Retain Class   '                                            
HSCRBK   DC    CL15'Script Source  '                                            
HSERNO   DC    CL15'Serial Number  '                                            
HSNUM    DC    CL15'Sequence Number'                                            
HSHPINS  DC    CL15'Ship Instructns'                                            
HSHIPRTE DC    CL15'Shipping Route '                                            
HSRETCLS DC    CL15'Soon Retain Cls'                                            
HSRTCODE DC    CL15'Sort Code      '                                            
HSRTFRM  DC    CL15'Sort Formula   '                                            
HSRTTYPE DC    CL15'Sort Type      '                                            
HSPDTYPE DC    CL15'Speed Type     '                                            
HSTATUS  DC    CL15'Status         '                                            
HSUBID   DC    CL15'Sub Id         '                                            
HSUBJECT DC    CL15'Subject        '                                            
HSUBNUM  DC    CL15'Sub Number     '                                            
HSURNAME DC    CL15'Surname        '                                            
HSYSNAME DC    CL15'System Name    '                                            
HSYSPASS DC    CL15'System Password'                                            
HSYSPROG DC    CL15'System/Program '                                            
HTSTPHS  DC    CL15'Test Phase     '                                            
HTSTMSK  DC    CL15'Test Phase Mask'                                            
HTEXT    DC    CL15'Text           '                                            
HTIMERG  DC    CL15'Time Range     '                                            
HUSRID   DC    CL15'User Id        '                                            
HVALUE   DC    CL15'Value          '                                            
HVENDOR  DC    CL15'Vendor         '                                            
HVTAML   DC    CL15'VTAM LUID      '                                            
HWRITER  DC    CL15'Writer         '                                            
HXTRBK   DC    CL15'Xtract Book    '                                            
HXFGKEY  DC    CL15'XFILE Group key'                                            
HSTATBIT DC    CL15'16 Status bits '                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLE OF INPUT ACCUMULATORS                                        *          
**********************************************************************          
INPERRS  DC    PL8'0'              INPUT ERROR COUNT                            
INPTOTS  DC    PL8'0'              INPUT TOTALS COUNT                           
*                                                                               
INPACCS  DC    AL1(CTFILEQ),CL7'CTFILE ',4PL8'0'                                
         DC    AL1(CTREQQ),CL7'CTREQ  ',4PL8'0'                                 
*&&UK*&& DC    X'A8',CL7'DEMOLD ',4PL8'0'                                       
*&&UK*&& DC    X'A9',CL7'DEMNEW ',4PL8'0'                                       
*&&UK*&& DC    X'AA',CL7'DEMDIR ',4PL8'0'                                       
*&&US*&& DC    X'AD',CL7'CTUSER ',4PL8'0'                                       
         DC    AL1(GENDIRQ),CL7'GENDIR ',4PL8'0'                                
         DC    AL1(GENFILQ),CL7'GENFIL ',4PL8'0'                                
         DC    X'00',CL7'UNKNOWN',4PL8'0'                                       
         DC    X'FF'                                                            
*                                                                               
       ++INCLUDE FACIDTAB                                                       
*                                                                               
INPACCSD DSECT                                                                  
INPNUM   DS    XL1                 DMFILES INTERNAL NUMBER FOR FILE             
INPNAME  DS    CL7                 ADFILENM                                     
INPTCPY  DS    PL8                                                              
INPTCHA  DS    PL8                                                              
INPTADD  DS    PL8                                                              
INPTTOT  DS    PL8                                                              
INPACCSL EQU   *-INPACCSD                                                       
*                                                                               
CTDFAR   CSECT                                                                  
         EJECT                                                                  
**********************************************************************          
* SYSLST                                                             *          
**********************************************************************          
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         EJECT                                                                  
**********************************************************************          
* ERROR MESSAGES                                                     *          
**********************************************************************          
EMSGL    EQU   50                                                               
ERRTAB   DS    0D                                                               
ERR01    DC    CL(EMSGL)'Terminal Number Does Not Exist'                        
ERR02    DC    CL(EMSGL)'LUID Does Not Exist'                                   
ERR03    DC    CL(EMSGL)'User ID Does Not Exist'                                
ERR04    DC    CL(EMSGL)'Program Does Not Exist'                                
ERR05    DC    CL(EMSGL)'Invalid Record Type'                                   
ERR06    DC    CL(EMSGL)'Invalid Agency ID'                                     
ERR07    DC    CL(EMSGL)'Invalid Record Parameter'                              
ERR08    DC    CL(EMSGL)'Invalid Parameter Card'                                
ERR09    DC    CL(EMSGL)'Invalid Sort Card'                                     
ERR10    DC    CL(EMSGL)'Parameter Card Not Found'                              
ERR11    DC    CL(EMSGL)'User Name Not Found'                                   
ERR12    DC    CL(EMSGL)'Program Number is Not Valid'                           
ERR13    DC    CL(EMSGL)'Unknown Parameter Card'                                
ERR14    DC    CL(EMSGL)'Parameter has already been set'                        
ERR15    DC    CL(EMSGL)'User-Id must be a number'                              
ERR16    DC    CL(EMSGL)'Number entered is too big'                             
ERR17    DC    CL(EMSGL)'Terminal Number must be numeric'                       
ERR18    DC    CL(EMSGL)'Agency binary value must be numeric'                   
         EJECT                                                                  
**********************************************************************          
* INPUT CARD TABLE                                                   *          
**********************************************************************          
CARDTAB  DC    CL8'ADDOUT  ',AL1(2,6,0,0),AL4(VALADD)                           
         DC    CL8'AGYBIN  ',AL1(2,6,0,0),AL4(VALAGY)                           
         DC    CL8'ALLOUT  ',AL1(2,6,0,0),AL4(VALALL)                           
         DC    CL8'CHNGOUT ',AL1(2,7,0,0),AL4(VALCHNG)                          
         DC    CL8'COPYOUT ',AL1(5,7,0,0),AL4(VALCOPY)                          
         DC    CL8'COPYTAPE',AL1(5,8,0,0),AL4(VALCTAP)                          
         DC    CL8'DELOUT  ',AL1(3,6,0,0),AL4(VALDEL)                           
         DC    CL8'DETAIL  ',AL1(3,6,0,0),AL4(VALFULL)                          
         DC    CL8'ERASE   ',AL1(2,5,0,0),AL4(VALERAS)                          
         DC    CL8'WRITE   ',AL1(2,5,0,0),AL4(VALWRT)                           
         DC    CL8'INPUT   ',AL1(2,5,0,0),AL4(VALINPT)                          
         DC    CL8'LUID    ',AL1(2,4,0,0),AL4(VALUID)                           
         DC    CL8'PROGNUM ',AL1(6,7,0,0),AL4(VALPGN)                           
         DC    CL8'PROGNAME',AL1(6,8,0,0),AL4(VALPRG)                           
         DC    CL8'RESTOUT ',AL1(3,7,0,0),AL4(VALREST)                          
         DC    CL8'SORT    ',AL1(2,4,0,0),AL4(VALSORT)                          
         DC    CL8'TERMINAL',AL1(2,8,0,0),AL4(VALTRM)                           
         DC    CL8'USERID  ',AL1(5,6,0,0),AL4(VALID)                            
         DC    CL8'USERNAME',AL1(5,8,0,0),AL4(VALNID)                           
         DC    X'FF'                                                            
*                                                                               
CARDTABD DSECT ,                   MATCH INPUT CARD PARAMETERS                  
CARDTXT  DS    CL8                                                              
CMIN     DS    X                                                                
CMAX     DS    X                                                                
         DS    XL2                                                              
CARDVAL  DS    AL4                                                              
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CTDFAR   CSECT                                                                  
         EJECT                                                                  
**********************************************************************          
* RECORD TOTALS TABLE                                                *          
**********************************************************************          
ADDTAB   DS    0D                                                               
CTFADD   DC    CL32'Saved Program                   '                           
         DC    AL1(01,CTFILEQ,00,00,00,00),AL1(01),XL3'01FFFF'                  
         DC    AL4(0,0,0,0),AL4(C1RECSRR,DETAIL)                                
*                                                                               
         DC    CL32'User Records (DRIVER)           '                           
         DC    AL1(02,CTFILEQ,00,00,00,00),AL1(01),XL3'02FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Date Schemes                    '                           
         DC    AL1(03,CTFILEQ,00,00,00,00),AL1(01),XL3'03FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'EDR Publication Name            '                           
         DC    AL1(04,CTFILEQ,00,00,00,00),AL1(02),XL3'0401FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'EDR Publication Linkage         '                           
         DC    AL1(05,CTFILEQ,00,00,00,00),AL1(02),XL3'0400FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Phase                           '                           
         DC    AL1(06,CTFILEQ,00,00,00,00),AL1(02),XL3'0501FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'ADDS Directory                  '                           
         DC    AL1(07,CTFILEQ,00,00,00,00),AL1(02),XL3'0504FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'ADDS Station                    '                           
         DC    AL1(08,CTFILEQ,00,00,00,00),AL1(02),XL3'0506FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'EASYLINK                        '                           
         DC    AL1(09,CTFILEQ,00,00,00,00),AL1(02),XL3'0507FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Dare EDICT Information          '                           
         DC    AL1(10,CTFILEQ,00,00,00,00),AL1(02),XL3'0508FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Zenith Client                   '                           
         DC    AL1(11,CTFILEQ,00,00,00,00),AL1(02),XL3'0509FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Zenith Payee Rep                '                           
         DC    AL1(12,CTFILEQ,00,00,00,00),AL1(02),XL3'050AFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Office                          '                           
         DC    AL1(13,CTFILEQ,00,00,00,00),AL1(02),XL3'050BFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Batch Type                      '                           
         DC    AL1(14,CTFILEQ,00,00,00,00),AL1(01),XL3'06FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Facpak Program Version Control  '                           
         DC    AL1(15,CTFILEQ,00,00,00,00),AL1(02),XL3'07C6FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Stereo Program Version Control  '                           
         DC    AL1(16,CTFILEQ,00,00,00,00),AL1(02),XL3'07E2FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Printer Name                    '                           
         DC    AL1(17,CTFILEQ,00,00,00,00),AL1(01),XL3'08FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'TWX Addressee                   '                           
         DC    AL1(18,CTFILEQ,00,00,00,00),AL1(01),XL3'C1FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Demo (Latest Book)              '                           
         DC    AL1(19,CTFILEQ,00,00,00,00),AL1(01),XL3'C2FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Comment (Book)                  '                           
         DC    AL1(20,CTFILEQ,00,00,00,00),AL1(01),XL3'C3FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Demo Name                       '                           
         DC    AL1(21,CTFILEQ,00,00,00,00),AL1(01),XL3'C4FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Error Message                   '                           
         DC    AL1(22,CTFILEQ,00,00,00,00),AL1(01),XL3'C5FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Office Security                 '                           
         DC    AL1(23,CTFILEQ,00,00,00,01),AL1(02),XL3'C602FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Department Security             '                           
         DC    AL1(24,CTFILEQ,00,00,00,01),AL1(02),XL3'C603FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Person Security             '                               
         DC    AL1(25,CTFILEQ,00,00,00,01),AL1(02),XL3'C604FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Person Access Group Security'                               
         DC    AL1(26,CTFILEQ,00,00,00,01),AL1(02),XL3'C605FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Security - Program              '                           
         DC    AL1(27,CTFILEQ,00,00,00,01),AL1(02),XL3'C610FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Action Security                 '                           
         DC    AL1(28,CTFILEQ,00,00,00,01),AL1(02),XL3'C611FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Record Security                 '                           
         DC    AL1(29,CTFILEQ,00,00,00,01),AL1(02),XL3'C612FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Field Security                  '                           
         DC    AL1(30,CTFILEQ,00,00,00,01),AL1(02),XL3'C613FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Options Security                '                           
         DC    AL1(31,CTFILEQ,00,00,00,01),AL1(02),XL3'C614FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Access Security                 '                           
         DC    AL1(32,CTFILEQ,00,00,00,01),AL1(02),XL3'C615FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Field Control Security          '                           
         DC    AL1(33,CTFILEQ,00,00,00,01),AL1(02),XL3'C616FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Option Control Security         '                           
         DC    AL1(34,CTFILEQ,00,00,00,01),AL1(02),XL3'C617FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Limit Access Group Security '                               
         DC    AL1(35,CTFILEQ,00,00,00,01),AL1(02),XL3'C618FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Limit Access List Security  '                               
         DC    AL1(36,CTFILEQ,00,00,00,01),AL1(02),XL3'C619FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Timesheet Approver Group    '                               
         DC    AL1(37,CTFILEQ,00,00,00,01),AL1(02),XL3'C620FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'File Control                '                               
         DC    AL1(38,CTFILEQ,00,00,00,00),AL1(01),XL3'C6FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Request Group               '                               
         DC    AL1(39,CTFILEQ,00,00,00,00),AL1(01),XL3'C7FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'HUT (Network)               '                               
         DC    AL1(40,CTFILEQ,00,00,00,00),AL1(01),XL3'C8FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Identification              '                               
         DC    AL1(41,CTFILEQ,15,08,00,01),AL1(01),XL3'C9FFFF'                  
         DC    AL4(0,0,0,0),AL4(IDRECSR,DETAIL)                                 
*                                                                               
         DC    CL32'JCL Book                    '                               
         DC    AL1(42,CTFILEQ,00,00,00,00),AL1(01),XL3'D1FFFF'                  
         DC    AL4(0,0,0,0),AL4(LIBRECR,DETAIL)                                 
*                                                                               
         DC    CL32'Authorisation               '                               
         DC    AL1(43,CTFILEQ,00,00,00,01),AL1(01),XL3'D2FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Library Book                '                               
         DC    AL1(44,CTFILEQ,00,00,00,00),AL1(01),XL3'D3FFFF'                  
         DC    AL4(0,0,0,0),AL4(LIBRECR,DETAIL)                                 
*                                                                               
         DC    CL32'Screen Field Map                '                           
         DC    AL1(45,CTFILEQ,00,00,00,00),AL1(01),XL3'D4FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Demo Code                       '                           
         DC    AL1(46,CTFILEQ,00,00,00,00),AL1(01),XL3'D5FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Output Type                     '                           
         DC    AL1(47,CTFILEQ,00,00,00,01),AL1(01),XL3'D6FFFF'                  
         DC    AL4(0,0,0,0),AL4(OUTPUTR,DETAIL)                                 
*                                                                               
         DC    CL32'Profile                         '                           
         DC    AL1(48,CTFILEQ,00,00,00,01),AL1(01),XL3'D7FFFF'                  
         DC    AL4(0,0,0,0),AL4(PRRECSR,DETAIL)                                 
*                                                                               
         DC    CL32'Demo Adjustment                 '                           
         DC    AL1(49,CTFILEQ,00,00,00,00),AL1(01),XL3'D8FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Demo Agency Control             '                           
         DC    AL1(50,CTFILEQ,00,00,00,00),AL1(01),XL3'D9FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Station Call Letter Change List '                           
         DC    AL1(51,CTFILEQ,00,00,00,00),AL1(01),XL3'E2FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Terminal                        '                           
         DC    AL1(52,CTFILEQ,07,16,00,01),AL1(01),XL3'E3FFFF'                  
         DC    AL4(0,0,0,0),AL4(TERMSRR,DETAIL)                                 
*                                                                               
         DC    CL32'User Profile                    '                           
         DC    AL1(53,CTFILEQ,00,00,00,01),AL1(01),XL3'E4FFFF'                  
         DC    AL4(0,0,0,0),AL4(USERPRS,DETAIL)                                 
*                                                                               
         DC    CL32'Value                           '                           
         DC    AL1(54,CTFILEQ,00,00,00,00),AL1(01),XL3'E5FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'System List                     '                           
         DC    AL1(55,CTFILEQ,00,00,00,01),AL1(01),XL3'E6FFFF'                  
         DC    AL4(0,0,0,0),AL4(SYSLSTR,DETAIL)                                 
*                                                                               
         DC    CL32'CPP Extract Rules               '                           
         DC    AL1(56,CTFILEQ,00,00,00,00),AL1(01),XL3'E7FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'CPP Projection Formula          '                           
         DC    AL1(57,CTFILEQ,00,00,00,00),AL1(01),XL3'E8FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Interface Tape                  '                           
         DC    AL1(58,CTFILEQ,00,00,00,00),AL1(01),XL3'E9FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Personal Authorisation          '                           
         DC    AL1(59,CTFILEQ,00,00,00,01),AL1(01),XL3'F0FFFF'                  
         DC    AL4(0,0,0,0),AL4(PAUTHRR,DETAIL)                                 
*                                                                               
         DC    CL32'Country Name                    '                           
         DC    AL1(60,CTFILEQ,00,00,00,00),AL1(01),XL3'F1FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Country Code                    '                           
         DC    AL1(61,CTFILEQ,00,00,00,00),AL1(01),XL3'F2FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Facpak Table - Application      '                           
         DC    AL1(62,CTFILEQ,00,00,00,00),AL1(02),XL3'F3C1FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'IP Subnet ID                    '                           
         DC    AL1(116,CTFILEQ,00,00,00,00),AL1(02),XL3'F3C9FF'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Facpak Table - DDS Dept Security'                           
         DC    AL1(63,CTFILEQ,00,00,00,00),AL1(02),XL3'F4C4FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Facpak Table - Easilink Station '                           
         DC    AL1(64,CTFILEQ,00,00,00,00),AL1(02),XL3'F4C5FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Facpak Table - Program          '                           
         DC    AL1(65,CTFILEQ,00,00,00,00),AL1(02),XL3'F4D7FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Facpak Table - System SE        '                           
         DC    AL1(66,CTFILEQ,00,00,00,00),AL1(02),XL3'F4E2FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Braodcast Message               '                           
         DC    AL1(115,CTFILEQ,00,00,00,01),AL1(01),XL3'F4FFFF'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'System Access                   '                           
         DC    AL1(67,CTFILEQ,00,00,00,01),AL1(01),XL3'F5FFFF'                  
         DC    AL4(0,0,0,0),AL4(ACCESSR,DETAIL)                                 
*                                                                               
         DC    CL32'Traffic TWX                     '                           
         DC    AL1(68,CTFILEQ,00,00,00,00),AL1(01),XL3'F6FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Script Object                   '                           
         DC    AL1(69,CTFILEQ,00,00,00,00),AL1(01),XL3'F7FFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Lock Records                    '                           
         DC    AL1(70,CTFILEQ,00,00,00,00),AL1(01),XL3'F8FFFF'                  
         DC    AL4(0,0,0,0),AL4(LOCKRR,DETAIL)                                  
*                                                                               
         DC    CL32'Fax Records                     '                           
         DC    AL1(71,CTFILEQ,00,00,00,00),AL1(01),XL3'F9FFFF'                  
         DC    AL4(0,0,0,0),AL4(FAXRECSR,DETAIL)                                
*                                                                               
CTRADD   DC    CL32'Request File Record (CTREQ)     '                           
         DC    AL1(72,CTREQQ,00,00,00,00),AL1(01),XL3'FFFFFF'                   
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
CTGADD   DC    CL32'Access Control Record           '                           
         DC    AL1(73,GENDIRQ,00,00,00,00),AL1(03),XL3'00000A'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Department                      '                           
         DC    AL1(74,GENDIRQ,00,00,00,00),AL1(03),XL3'00000B'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Public IDs                      '                           
         DC    AL1(75,GENDIRQ,00,00,00,00),AL1(03),XL3'00000F'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Currency                        '                           
         DC    AL1(76,GENDIRQ,00,00,00,01),AL1(03),XL3'0000C3'                  
         DC    AL4(0,0,0,0),AL4(CURRENR,DETAIL)                                 
*                                                                               
         DC    CL32'Exchange Rate                   '                           
         DC    AL1(77,GENDIRQ,00,00,00,00),AL1(03),XL3'0000C5'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Message                         '                           
         DC    AL1(78,GENDIRQ,00,00,00,00),AL1(03),XL3'0000D4'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Spare                           '                           
         DC    AL1(79,GENDIRQ,00,00,00,00),AL1(03),XL3'FFFFFF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Narrative                       '                           
         DC    AL1(80,GENDIRQ,00,00,00,00),AL1(03),XL3'0000D5'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Help                            '                           
         DC    AL1(81,GENDIRQ,00,00,00,00),AL1(02),XL3'0002FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Driver Dictionary               '                           
         DC    AL1(82,GENDIRQ,00,00,00,00),AL1(02),XL3'0003FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'TV/Data Panel                   '                           
         DC    AL1(83,GENDIRQ,00,00,00,00),AL1(02),XL3'0007FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Broadcast                       '                           
         DC    AL1(84,GENDIRQ,00,00,00,00),AL1(02),XL3'0009FF'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'XFILE Group Pointer             '                           
         DC    AL1(85,GENDIRQ,00,00,00,00),AL1(02),XL3'000012'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Fees - Channel Configuration    '                           
         DC    AL1(86,GENDIRQ,00,00,00,00),AL1(03),XL3'00C6C3'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Fees - New Channel Configuration'                           
         DC    AL1(87,GENDIRQ,00,00,00,00),AL1(03),XL3'00C603'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Fees - National Insurance       '                           
         DC    AL1(88,GENDIRQ,00,00,00,00),AL1(03),XL3'00C6D5'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Fees - Payment Parameters       '                           
         DC    AL1(89,GENDIRQ,00,00,00,00),AL1(03),XL3'00C6D7'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Fees - Role Descriptions        '                           
         DC    AL1(90,GENDIRQ,00,00,00,00),AL1(03),XL3'00C6D9'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Fees - TVR Bands                '                           
         DC    AL1(91,GENDIRQ,00,00,00,00),AL1(03),XL3'00C6E3'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'NFILE Action Definition         '                           
         DC    AL1(92,GENDIRQ,00,00,00,00),AL1(03),XL3'00E7C1'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'NFILE Download Definition       '                           
         DC    AL1(93,GENDIRQ,00,00,00,00),AL1(03),XL3'00E7C4'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'NFILE Field Definition          '                           
         DC    AL1(94,GENDIRQ,00,00,00,00),AL1(03),XL3'00E7C6'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'NFILE PFKey Definition          '                           
         DC    AL1(95,GENDIRQ,00,00,00,00),AL1(03),XL3'00E7D7'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'NFILE Record Definition         '                           
         DC    AL1(96,GENDIRQ,00,00,00,00),AL1(03),XL3'00E7D9'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'NFILE Screen Definition         '                           
         DC    AL1(97,GENDIRQ,00,00,00,00),AL1(03),XL3'00E7E2'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'NFILE Default Column Definition  '                          
         DC    AL1(98,GENDIRQ,00,00,00,00),AL1(03),XL3'00E7E7'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'PC Billing Data Source          '                           
         DC    AL1(99,GENDIRQ,00,00,00,00),AL1(03),XL3'00C1C4'                  
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'PC Billing Keyword List         '                           
         DC    AL1(100,GENDIRQ,00,00,00,00),AL1(03),XL3'00C1D2'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Scroller Datatype               '                           
         DC    AL1(101,GENDIRQ,00,00,00,00),AL1(02),XL3'000DFF'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'EXTRACT - Agency                '                           
         DC    AL1(102,GENDIRQ,00,00,00,00),AL1(03),XL3'000010'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'EXTRACT - ESS (Server) Control  '                           
         DC    AL1(103,GENDIRQ,00,00,00,00),AL1(03),XL3'000020'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'EXTRACT - Extract File Defn.     '                          
         DC    AL1(104,GENDIRQ,00,00,00,00),AL1(03),XL3'000030'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'EXTRACT - Recovery System Log    '                          
         DC    AL1(105,GENDIRQ,00,00,00,00),AL1(03),XL3'000040'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'ESS Id                          '                           
         DC    AL1(106,GENDIRQ,00,00,00,00),AL1(03),XL3'000050'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'PQ Report Tranformation Control  '                          
         DC    AL1(107,GENDIRQ,00,00,00,00),AL1(03),XL3'000060'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Equate Name                      '                          
         DC    AL1(108,GENDIRQ,00,00,00,00),AL1(03),XL3'000094'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Report Spec                      '                          
         DC    AL1(109,GENDIRQ,00,00,00,00),AL1(02),XL3'0070FF'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Staff                            '                          
         DC    AL1(110,GENDIRQ,00,00,00,00),AL1(02),XL3'0051FF'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Staff - Passive                  '                          
         DC    AL1(111,GENDIRQ,00,00,00,00),AL1(02),XL3'0055FF'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'ADV / Agency                     '                          
         DC    AL1(111,GENDIRQ,00,00,00,00),AL1(02),XL3'0060FF'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Agency Routing                   '                          
         DC    AL1(112,GENDIRQ,00,00,00,00),AL1(02),XL3'005BFF'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Dare Rep Table                   '                          
         DC    AL1(113,GENDIRQ,00,00,00,00),AL1(02),XL3'005CFF'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'RFP                              '                          
         DC    AL1(113,GENDIRQ,00,00,00,00),AL1(02),XL3'002FFF'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
         DC    CL32'Dare Assist Initials             '                          
         DC    AL1(114,GENDIRQ,00,00,00,00),AL1(03),XL3'0000C4'                 
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
CTUADD   DC    CL32'Unknown record types             '                          
         DC    AL1(255,00,00,00,00,00),AL1(00),XL3'FFFFFF'                      
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
ADDTOTS  DC    CL32'---FILE TOTALS-------------------'                          
         DC    AL1(000,00,00,00,00,00),AL1(00),XL3'FFFFFF'                      
         DC    AL4(0,0,0,0),AL4(DEFKYOUT,DETAIL)                                
*                                                                               
ADDTABX  DC    32X'FF'                                                          
*                                                                               
ADDTABD  DSECT                                                                  
ADFILENM DS    XL32                RECORD NAME                                  
ADRECNUM DS    XL1                 RECORD ID NUMBER (INTERNAL)                  
ADFTYPE  DS    XL1                 FILE TYPE (FROM RCVRHDR)                     
ADPASSD  DS    AL1                 IF PASSIVE - DISPLACEMENT IN KEY             
ADPASSL  DS    AL1                              LENGTH TO CHECK                 
ADPASSF  DS    AL1                 PASSIVE FILL TYPE (DEFAULT 0)                
ADSTDREP DS    AL1                 REQUIRED FOR STANDARD REPORT IF <>0          
*                                                                               
ADMTCHL  DS    XL1                 LENGTH OF IDENTIFIER FOR THIS KEY            
ADMTCHC  DS    XL3                 IDENTIFIER FOR THIS KEY (FF FILLED)          
*                                                                               
ADBUCS   DS    0XL16               TOTALS FOR INPUT                             
ADBADD   DS    AL4                 ADD                                          
ADBCHG   DS    AL4                 CHANGE                                       
ADBDEL   DS    AL4                 DELETE                                       
ADBRES   DS    AL4                 RESTORE                                      
*                                                                               
ADODFLT  DS    AL4                 DEFAULT OUTPUT ROUTINE                       
ADODETL  DS    AL4                 DETAILED OUTPUT ROUTINE                      
ADDTABLQ EQU   *-ADDTABD                                                        
*                                                                               
CTDFAR   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ELEMENT ARRAY (SAVES SEQUENTIAL SEARCH)                             *         
***********************************************************************         
CTELEMS  DC    A(CT01,CT02,CT03,CT04,CT05,CT06,CT07,CT08)                       
         DC    A(CT09,CT0A,CT0B,CT0C,CT0D,CT0E,CT0F,CT10)                       
         DC    A(CT11,CT12,CT13,CT14,CT15,CT16,CT17,CT18)                       
         DC    A(CT19,CT1A,CT1B,CT1C,CT1D,CT1E,CT1F,CT20)                       
         DC    A(CT21,CT22,CT23,CT24,CT25,CT26,CT27,CT28)                       
         DC    A(CT29,CT2A,CT2B,CT2C,CT2D,CT2E,CT2F,CT30)                       
         DC    A(CT31,CT32,CT33,CT34,CT35,CT36,CT37,CT38)                       
         DC    A(CT39,CT3A,CT3B,CT3C,CT3D,CT3E,CT3F,CT40)                       
         DC    A(CT41,CT42,CT43,CT44,CT45,CT46,CT47,CT48)                       
         DC    A(CT49,CT4A,CT4B,CT4C,CT4D,CT4E,CT4F,CT50)                       
         DC    A(CT51,CT52,CT53,CT54,CT55,CT56,CT57,CT58)                       
         DC    A(CT59,CT5A,CT5B,CT5C,CT5D,CT5E,CT5F,CT60)                       
         DC    A(CT61,CT62,CT63,CT64,CT65,CT66,CT67,CT68)                       
         DC    A(CT69,CT6A,CT6B,CT6C,CT6D,CT6E,CT6F,CT70)                       
         DC    A(CT71,CT72,CT73,CT74,CT75,CT76,CT77,CT78)                       
         DC    A(CT79,CT7A,CT7B,CT7C,CT7D,CT7E,CT7F,CT80)                       
         DC    A(CT81,CT82,CT83,CT84,CT85,CT86,CT87,CT88)                       
         DC    A(CT89,CT8A,CT8B,CT8C,CT8D,CT8E,CT8F,CT90)                       
         DC    A(CT91,CT92,CT93,CT94,CT95,CT96,CT97,CT98)                       
         DC    A(CT99,CT9A,CT9B,CT9C,CT9D,CT9E,CT9F,CTA0)                       
         DC    A(CTA1,CTA2,CTA3,CTA4,CTA5,CTA6,CTA7,CTA8)                       
         DC    A(CTA9,CTAA,CTAB,CTAC,CTAD,CTAE,CTAF,CTB0)                       
         DC    A(CTB1,CTB2,CTB3,CTB4,CTB5,CTB6,CTB7,CTB8)                       
         DC    A(CTB9,CTBA,CTBB,CTBC,CTBD,CTBE,CTBF,CTC0)                       
         DC    A(CTC1,CTC2,CTC3,CTC4,CTC5,CTC6,CTC7,CTC8)                       
         DC    A(CTC9,CTCA,CTCB,CTCC,CTCD,CTCE,CTCF,CTD0)                       
         DC    A(CTD1,CTD2,CTD3,CTD4,CTD5,CTD6,CTD7,CTD8)                       
         DC    A(CTD9,CTDA,CTDB,CTDC,CTDD,CTDE,CTDF,CTE0)                       
         DC    A(CTE1,CTE2,CTE3,CTE4,CTE5,CTE6,CTE7,CTE8)                       
         DC    A(CTE9,CTEA,CTEB,CTEC,CTED,CTEE,CTEF,CTF0)                       
         DC    A(CTF1,CTF2,CTF3,CTF4,CTF5,CTF6,CTF7,CTF8)                       
         DC    A(CTF9,CTFA,CTFB,CTFC,CTFD,CTFE,CTFF,0000)                       
         EJECT                                                                  
***********************************************************************         
* ELEMENT DESCRIPTOR TABLE (COVERED BY CTELEMD)                       *         
***********************************************************************         
CT01     DC    AL2(CT02-CT01),X'01',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT01ELM,CT01ELM,0)                                   
         DC    AL1(000,001),CL30'Description                   '                
         DC    AL1(000,071),CL30'Fax Number                    '                
         DC    AL1(000,073),CL30'Program Details               '                
         DC    AL1(000,074),CL30'Program Details               '                
         DC    AL1(000,075),CL30'Program Details               '                
         DC    AL1(000,094),CL30'Screen Field Information      '                
         DC    AL1(000,113),CL30'RFP Group Header              '                
         DC    AL1(000,000),CL30'Activity                      '                
*                                                                               
CT02     DC    AL2(CT03-CT02),X'02',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT02ELM,CT02ELM,0)                                   
         DC    AL1(000,001),CL30'Field Data                    '                
         DC    AL1(000,071),CL30'Attention Name                '                
         DC    AL1(000,073),CL30'Action Information            '                
         DC    AL1(000,074),CL30'Action Information            '                
         DC    AL1(000,075),CL30'Action Information            '                
         DC    AL1(000,114),CL30'Initials Activity Information '                
         DC    AL1(000,000),CL30'Description                   '                
*                                                                               
CT03     DC    AL2(CT04-CT03),X'03',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT03ELM,CT03ELM,0)                                   
         DC    AL1(000,071),CL30'Fax Message Line              '                
         DC    AL1(000,073),CL30'Record Description            '                
         DC    AL1(000,074),CL30'Record Description            '                
         DC    AL1(000,075),CL30'Record Description            '                
         DC    AL1(000,092),CL30'Record Information            '                
         DC    AL1(000,097),CL30'Screen Field Position         '                
         DC    AL1(000,114),CL30'Assistant Initials            '                
         DC    AL1(000,000),CL30'Passive Pointer               '                
*                                                                               
CT04     DC    AL2(CT05-CT04),X'04',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT04ELM,CT04ELM,0)                                   
         DC    AL1(000,042),CL30'Index                         '                
         DC    AL1(000,044),CL30'Index                         '                
         DC    AL1(000,071),CL30'Fax Return Number             '                
         DC    AL1(000,073),CL30'Language for Record/Action    '                
         DC    AL1(000,074),CL30'Language for Record/Action    '                
         DC    AL1(000,075),CL30'Language for Record/Action    '                
         DC    AL1(000,082),CL30'Input Field                   '                
         DC    AL1(000,093),CL30'Download Column List          '                
         DC    AL1(000,094),CL30'Filter Field Information      '                
         DC    AL1(000,098),CL30'Default Column List           '                
         DC    AL1(000,000),CL30'Unknown 04 Element            '                
*                                                                               
CT05     DC    AL2(CT06-CT05),X'05',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,073),CL30'Person Name                   '                
         DC    AL1(000,074),CL30'Person Name                   '                
         DC    AL1(000,075),CL30'Person Name                   '                
         DC    AL1(000,084),CL30'High Message Number           '                
         DC    AL1(000,096),CL30'Record Definition             '                
         DC    AL1(000,023),CL30'Limit Access List             '                
         DC    AL1(000,024),CL30'Limit Access List             '                
         DC    AL1(000,025),CL30'Limit Access List             '                
         DC    AL1(000,026),CL30'Limit Access List             '                
         DC    AL1(000,027),CL30'Limit Access List             '                
         DC    AL1(000,028),CL30'Limit Access List             '                
         DC    AL1(000,029),CL30'Limit Access List             '                
         DC    AL1(000,030),CL30'Limit Access List             '                
         DC    AL1(000,031),CL30'Limit Access List             '                
         DC    AL1(000,032),CL30'Limit Access List             '                
         DC    AL1(000,033),CL30'Limit Access List             '                
         DC    AL1(000,034),CL30'Limit Access List             '                
         DC    AL1(000,035),CL30'Limit Access List             '                
         DC    AL1(000,036),CL30'Limit Access List             '                
         DC    AL1(000,037),CL30'Limit Access List             '                
         DC    AL1(000,038),CL30'Limit Access List             '                
         DC    AL1(000,082),CL30'Sub-entry                     '                
         DC    AL1(000,092),CL30'Action definition             '                
         DC    AL1(CTFILEQ,000),CL30'System Element                '            
         DC    AL1(000,000),CL30'Unknown 05 Element            '                
*                                                                               
CT06     DC    AL2(CT07-CT06),X'06',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT06ELM,CT06ELM,0)                                   
         DC    AL1(000,073),CL30'Person Title                  '                
         DC    AL1(000,074),CL30'Person Title                  '                
         DC    AL1(000,075),CL30'Person Title                  '                
         DC    AL1(CTFILEQ,0),CL30'Agency Alpha Id                 '            
         DC    AL1(000,000),CL30'Unknown 06 Element            '                
*                                                                               
CT07     DC    AL2(CT08-CT07),X'07',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT10ELM,CT10ELM,0)                                   
         DC    AL1(000,073),CL30'Record/Action combo           '                
         DC    AL1(000,074),CL30'Record/Action combo           '                
         DC    AL1(000,075),CL30'Record/Action combo           '                
         DC    AL1(000,095),CL30'PFKey definition              '                
         DC    AL1(CTFILEQ,0),CL30'User Id Options                '             
         DC    AL1(000,000),CL30'Unknown 07 Element            '                
*                                                                               
CT08     DC    AL2(CT09-CT08),X'08',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,073),CL30'Limit access                  '                
         DC    AL1(000,074),CL30'Limit access                  '                
         DC    AL1(000,075),CL30'Limit access                  '                
         DC    AL1(000,082),CL30'Attribute                     '                
         DC    AL1(000,086),CL30'VAT Percentage                '                
         DC    AL1(000,087),CL30'VAT Percentage                '                
         DC    AL1(000,088),CL30'VAT Percentage                '                
         DC    AL1(000,089),CL30'VAT Percentage                '                
         DC    AL1(000,090),CL30'VAT Percentage                '                
         DC    AL1(000,091),CL30'VAT Percentage                '                
         DC    AL1(000,097),CL30'Shadow screen pointer         '                
         DC    AL1(000,000),CL30'Unknown 08 Element            '                
*                                                                               
CT09     DC    AL2(CT0A-CT09),X'09',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,073),CL30'Department                    '                
         DC    AL1(000,074),CL30'Department                    '                
         DC    AL1(000,075),CL30'Department                    '                
         DC    AL1(000,097),CL30'Page Name                     '                
         DC    AL1(000,000),CL30'Unknown 09 Element            '                
*                                                                               
CT0A     DC    AL2(CT0B-CT0A),X'0A',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 0A Element            '                
*                                                                               
CT0B     DC    AL2(CT0C-CT0B),X'0B',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 0B Element            '                
*                                                                               
CT0C     DC    AL2(CT0D-CT0C),X'0C',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 0C Element            '                
*                                                                               
CT0D     DC    AL2(CT0E-CT0D),X'0D',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 0D Element            '                
*                                                                               
CT0E     DC    AL2(CT0F-CT0E),X'0E',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 0E Element            '                
*                                                                               
CT0F     DC    AL2(CT10-CT0F),X'0F',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 0F Element            '                
*                                                                               
CT10     DC    AL2(CT11-CT10),X'10',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT10ELM,CT10ELM,0)                                   
         DC    AL1(000,001),CL30'Filter                       '                 
         DC    AL1(000,077),CL30'Exchange Rate                '                 
         DC    AL1(000,078),CL30'Short Message                '                 
         DC    AL1(000,080),CL30'Narrative Data               '                 
         DC    AL1(000,081),CL30'Heading                      '                 
         DC    AL1(000,082),CL30'Record Start                 '                 
         DC    AL1(000,084),CL30'Filter                       '                 
         DC    AL1(000,086),CL30'NIC (EES/ERS) Parameters     '                 
         DC    AL1(000,087),CL30'NIC (EES/ERS) Parameters     '                 
         DC    AL1(000,088),CL30'NIC (EES/ERS) Parameters     '                 
         DC    AL1(000,089),CL30'NIC (EES/ERS) Parameters     '                 
         DC    AL1(000,091),CL30'NIC (EES/ERS) Parameters     '                 
         DC    AL1(000,090),CL30'Role Description Parameter   '                 
         DC    AL1(000,101),CL30'Name text (Row Display)      '                 
         DC    AL1(000,113),CL30'Date range                   '                 
         DC    AL1(000,115),CL30'Subject Text                 '                 
         DC    AL1(CTFILEQ,000),CL30'Data                         '             
         DC    AL1(000,000),CL30'Unknown 10 Element            '                
*                                                                               
CT11     DC    AL2(CT12-CT11),X'11',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,077),CL30'Exchange Rate as Input       '                 
         DC    AL1(000,078),CL30'Sub-Language Message Number  '                 
         DC    AL1(000,115),CL30'Text Line 1                  '                 
         DC    AL1(CTFILEQ,000),CL30'Include                      '             
         DC    AL1(000,000),CL30'Unknown 11 Element            '                
*                                                                               
CT12     DC    AL2(CT13-CT12),X'12',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,078),CL30'Full Text Message            '                 
         DC    AL1(000,082),CL30'Data Start                   '                 
         DC    AL1(000,086),CL30'Channel configuration        '                 
         DC    AL1(000,087),CL30'Channel configuration        '                 
         DC    AL1(000,088),CL30'Channel configuration        '                 
         DC    AL1(000,089),CL30'Channel configuration        '                 
         DC    AL1(000,090),CL30'Channel configuration        '                 
         DC    AL1(000,091),CL30'Channel configuration        '                 
         DC    AL1(000,115),CL30'Text Line 2                  '                 
         DC    AL1(000,000),CL30'Unknown 12 Element           '                 
*                                                                               
CT13     DC    AL2(CT14-CT13),X'13',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,115),CL30'Text Line 3                  '                 
         DC    AL1(000,000),CL30'Unknown 13 Element           '                 
*                                                                               
CT14     DC    AL2(CT15-CT14),X'14',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,078),CL30'Message Help Pointer         '                 
         DC    AL1(000,082),CL30'Set/Unset                    '                 
         DC    AL1(000,115),CL30'Text Line 4                  '                 
         DC    AL1(000,000),CL30'Unknown 14 Element           '                 
*                                                                               
CT15     DC    AL2(CT16-CT15),X'15',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,006),CL30'Reload List                  '                 
         DC    AL1(000,062),CL30'Subnet Mask                  '                 
         DC    AL1(000,081),CL30'Status                       '                 
         DC    AL1(000,000),CL30'Unknown 15 Element           '                 
*                                                                               
CT16     DC    AL2(CT17-CT16),X'16',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,078),CL30'Program Sub-Id + Max Length  '                 
         DC    AL1(000,086),CL30'Pay Period                   '                 
         DC    AL1(000,087),CL30'Pay Period                   '                 
         DC    AL1(000,088),CL30'Pay Period                   '                 
         DC    AL1(000,089),CL30'Pay Period                   '                 
         DC    AL1(000,090),CL30'Pay Period                   '                 
         DC    AL1(000,091),CL30'Pay Period                   '                 
         DC    AL1(000,000),CL30'Unknown 16 Element           '                 
*                                                                               
CT17     DC    AL2(CT18-CT17),X'17',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 17 Element           '                 
*                                                                               
CT18     DC    AL2(CT19-CT18),X'18',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,078),CL30'Equate symbol + Misc Info    '                 
         DC    AL1(000,086),CL30'Pay Pecentage                '                 
         DC    AL1(000,087),CL30'Pay Pecentage                '                 
         DC    AL1(000,088),CL30'Pay Pecentage                '                 
         DC    AL1(000,089),CL30'Pay Pecentage                '                 
         DC    AL1(000,090),CL30'Pay Pecentage                '                 
         DC    AL1(000,091),CL30'Pay Pecentage                '                 
         DC    AL1(000,000),CL30'Unknown 18 Element           '                 
*                                                                               
CT19     DC    AL2(CT1A-CT19),X'19',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 19 Element           '                 
*                                                                               
CT1A     DC    AL2(CT1B-CT1A),X'1A',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 1A Element           '                 
*                                                                               
CT1B     DC    AL2(CT1C-CT1B),X'1B',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 1B Element           '                 
*                                                                               
CT1C     DC    AL2(CT1D-CT1C),X'1C',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 1C Element           '                 
*                                                                               
CT1D     DC    AL2(CT1E-CT1D),X'1D',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 1D Element           '                 
*                                                                               
CT1E     DC    AL2(CT1F-CT1E),X'1E',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 1E Element           '                 
*                                                                               
CT1F     DC    AL2(CT20-CT1F),X'1F',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT1FELM,CT1FELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Principal Id                 '             
         DC    AL1(000,000),CL30'Unknown 1F Element           '                 
*                                                                               
CT20     DC    AL2(CT21-CT20),X'20',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT20ELM,CT20ELM,0,0)                                 
         DC    AL1(000,001),CL30'Other Data                   '                 
         DC    AL1(000,078),CL30'High Message Number          '                 
         DC    AL1(000,081),CL30'Text                         '                 
         DC    AL1(000,082),CL30'Input Statement Header       '                 
         DC    AL1(000,084),CL30'Heading                      '                 
         DC    AL1(000,113),CL30'Group Symbolic Value         '                 
         DC    AL1(CTFILEQ,000),CL30'Compatible Id                '             
         DC    AL1(000,000),CL30'Unknown 20 Element           '                 
*                                                                               
CT21     DC    AL2(CT22-CT21),X'21',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT21ELM,CT21ELM,0)                                   
         DC    AL1(000,068),CL30'Address Line 1               '                 
         DC    AL1(000,082),CL30'Output Statement Label       '                 
         DC    AL1(CTFILEQ,000),CL30'System Authorisation         '             
         DC    AL1(000,000),CL30'Unknown 21 Element           '                 
*                                                                               
CT22     DC    AL2(CT23-CT22),X'22',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,068),CL30'Address Line 2               '                 
         DC    AL1(000,082),CL30'Input Type                   '                 
         DC    AL1(CTFILEQ,000),CL30'Program Information          '             
         DC    AL1(000,000),CL30'Unknown 22 Element           '                 
*                                                                               
CT23     DC    AL2(CT24-CT23),X'23',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT23ELM,CT23ELM,0)                                   
         DC    AL1(000,068),CL30'Address Line 3               '                 
         DC    AL1(000,082),CL30'Input Length                 '                 
         DC    AL1(CTFILEQ,000),CL30'Program Test                 '             
         DC    AL1(000,000),CL30'Unknown 23 Element           '                 
*                                                                               
CT24     DC    AL2(CT25-CT24),X'24',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT24ELM,CT24ELM,0)                                   
         DC    AL1(000,052),CL30'Application Id               '                 
         DC    AL1(000,068),CL30'Address Line 4               '                 
         DC    AL1(000,082),CL30'Input Routine                '                 
         DC    AL1(000,000),CL30'Unknown 24 Element           '                 
*                                                                               
CT25     DC    AL2(CT26-CT25),X'25',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT25ELM,CT25ELM,0)                                   
         DC    AL1(000,006),CL30'Description                  '                 
         DC    AL1(000,082),CL30'Input Arguments              '                 
         DC    AL1(CTFILEQ,000),CL30'Terminal Definition          '             
         DC    AL1(000,000),CL30'Unknown 25 Element           '                 
*                                                                               
CT26     DC    AL2(CT27-CT26),X'26',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT26ELM,CT26ELM,0)                                   
         DC    AL1(000,082),CL30'Input Options                '                 
         DC    AL1(CTFILEQ,000),CL30'VTAM Luid                    '             
         DC    AL1(000,000),CL30'Unknown 26 Element           '                 
*                                                                               
CT27     DC    AL2(CT28-CT27),X'27',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT27ELM,CT27ELM,0)                                   
         DC    AL1(000,082),CL30'Input Column (for ranking)   '                 
         DC    AL1(CTFILEQ,000),CL30'Terminal Information         '             
         DC    AL1(000,000),CL30'Unknown 27 Element           '                 
*                                                                               
CT28     DC    AL2(CT29-CT28),X'28',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(CTFILEQ,000),CL30'Printer Id                   '             
         DC    AL1(000,000),CL30'Unknown 28 Element           '                 
*                                                                               
CT29     DC    AL2(CT2A-CT29),X'29',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT29ELM,CT29ELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Default Printer Queue        '             
         DC    AL1(000,000),CL30'Unknown 29 Element           '                 
*                                                                               
CT2A     DC    AL2(CT2B-CT2A),X'2A',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 2A Element           '                 
*                                                                               
CT2B     DC    AL2(CT2C-CT2B),X'2B',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 2B Element           '                 
*                                                                               
CT2C     DC    AL2(CT2D-CT2C),X'2C',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 2C Element           '                 
*                                                                               
CT2D     DC    AL2(CT2E-CT2D),X'2D',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 2D Element           '                 
*                                                                               
CT2E     DC    AL2(CT2F-CT2E),X'2E',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 2E Element           '                 
*                                                                               
CT2F     DC    AL2(CT30-CT2F),X'2F',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 2F Element           '                 
*                                                                               
CT30     DC    AL2(CT31-CT30),X'30',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT30ELM,CT30ELM,0)                                   
         DC    AL1(000,082),CL30'Output Header                '                 
         DC    AL1(000,084),CL30'Broadcast Text               '                 
         DC    AL1(000,113),CL30'Request Information          '                 
         DC    AL1(CTFILEQ,000),CL30'Destination detail           '             
         DC    AL1(000,000),CL30'Unknown 30 Element           '                 
*                                                                               
CT31     DC    AL2(CT32-CT31),X'31',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT31ELM,CT31ELM,0)                                   
         DC    AL1(000,082),CL30'Input Statement Label        '                 
         DC    AL1(CTFILEQ,000),CL30'Attention Detail             '             
         DC    AL1(000,000),CL30'Unknown 31 Element           '                 
*                                                                               
CT32     DC    AL2(CT33-CT32),X'32',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT32ELM,CT32ELM,0)                                   
         DC    AL1(000,082),CL30'Output Statement Label       '                 
         DC    AL1(000,000),CL30'Unknown 32 Element           '                 
*                                                                               
CT33     DC    AL2(CT34-CT33),X'33',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT33ELM,CT33ELM,0)                                   
         DC    AL1(000,082),CL30'Output Length                '                 
*&&UK*&& DC    AL1(CTFILEQ,000),CL30'Agency Definition            '             
*&&US*&& DC    AL1(CTFILEQ,000),CL30'Agency Extra Information     '             
         DC    AL1(000,000),CL30'Unknown 33 Element           '                 
*                                                                               
CT34     DC    AL2(CT35-CT34),X'34',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT34ELM,CT34ELM,0)                                   
         DC    AL1(000,082),CL30'Output Routine               '                 
         DC    AL1(CTFILEQ,000),CL30'Valid Destination            '             
         DC    AL1(000,000),CL30'Unknown 34 Element           '                 
*                                                                               
CT35     DC    AL2(CT36-CT35),X'35',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT35ELM,CT35ELM,0)                                   
         DC    AL1(000,006),CL30'Comment                      '                 
         DC    AL1(000,082),CL30'Output Argument              '                 
         DC    AL1(000,000),CL30'Unknown 35 Element           '                 
*                                                                               
CT36     DC    AL2(CT37-CT36),X'36',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT36ELM,CT36ELM,0)                                   
         DC    AL1(000,082),CL30'Output Option                '                 
         DC    AL1(000,086),CL30'Agreement Expiry             '                 
         DC    AL1(000,087),CL30'Agreement Expiry             '                 
         DC    AL1(000,088),CL30'Agreement Expiry             '                 
         DC    AL1(000,089),CL30'Agreement Expiry             '                 
         DC    AL1(000,090),CL30'Agreement Expiry             '                 
         DC    AL1(000,091),CL30'Agreement Expiry             '                 
         DC    AL1(CTFILEQ,000),CL30'Origin Detail                '             
         DC    AL1(000,000),CL30'Unknown 36 Element           '                 
*                                                                               
CT37     DC    AL2(CT38-CT37),X'37',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 37 Element           '                 
*                                                                               
CT38     DC    AL2(CT39-CT38),X'38',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,086),CL30'Band Percentage              '                 
         DC    AL1(000,087),CL30'Band Percentage              '                 
         DC    AL1(000,088),CL30'Band Percentage              '                 
         DC    AL1(000,089),CL30'Band Percentage              '                 
         DC    AL1(000,090),CL30'Band Percentage              '                 
         DC    AL1(000,091),CL30'Band Percentage              '                 
         DC    AL1(CTFILEQ,000),CL30'Output Detail                '             
         DC    AL1(000,000),CL30'Unknown 38 Element           '                 
*                                                                               
CT39     DC    AL2(CT3A-CT39),X'39',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 39 Element           '                 
*                                                                               
CT3A     DC    AL2(CT3B-CT3A),X'3A',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT3AELM,CT3AELM,0)                                   
         DC    AL1(000,076),CL30'Currency                     '                 
         DC    AL1(000,077),CL30'Currency                     '                 
         DC    AL1(CTFILEQ,000),CL30'Valid Printer                '             
         DC    AL1(000,000),CL30'Unknown 3A Element           '                 
*                                                                               
CT3B     DC    AL2(CT3C-CT3B),X'3B',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 3B Element           '                 
*                                                                               
CT3C     DC    AL2(CT3D-CT3C),X'3C',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 3C Element           '                 
*                                                                               
CT3D     DC    AL2(CT3E-CT3D),X'3D',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 3D Element           '                 
*                                                                               
CT3E     DC    AL2(CT3F-CT3E),X'3E',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT3EELM,CT3EELM,0)                                   
         DC    AL1(000,086),CL30'Comment                      '                 
         DC    AL1(000,087),CL30'Comment                      '                 
         DC    AL1(000,088),CL30'Comment                      '                 
         DC    AL1(000,089),CL30'Comment                      '                 
         DC    AL1(000,090),CL30'Comment                      '                 
         DC    AL1(000,091),CL30'Comment                      '                 
         DC    AL1(000,000),CL30'Unknown 3E Element           '                 
*                                                                               
CT3F     DC    AL2(CT40-CT3F),X'3F',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 3F Element           '                 
*                                                                               
CT40     DC    AL2(CT41-CT40),X'40',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT40ELM,CT40ELM,0)                                   
         DC    AL1(000,082),CL30'Head Statement               '                 
         DC    AL1(CTFILEQ,000),CL30'Destination Code             '             
         DC    AL1(000,000),CL30'Unknown 40 Element           '                 
*                                                                               
CT41     DC    AL2(CT42-CT41),X'41',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT41ELM,CT41ELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Attention Type               '             
         DC    AL1(000,000),CL30'Unknown 41 Element           '                 
*                                                                               
CT42     DC    AL2(CT43-CT42),X'42',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT42ELM,CT42ELM,0)                                   
         DC    AL1(000,082),CL30'Chunk Statement              '                 
         DC    AL1(CTFILEQ,000),CL30'Output Type Code             '             
         DC    AL1(000,000),CL30'Unknown 42 Element           '                 
*                                                                               
CT43     DC    AL2(CT44-CT43),X'43',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT43ELM,CT43ELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Output Mode Type             '             
         DC    AL1(000,000),CL30'Unknown 43 Element           '                 
*                                                                               
CT44     DC    AL2(CT45-CT44),X'44',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT44ELM,CT44ELM,0)                                   
         DC    AL1(000,082),CL30'First/Last Statement         '                 
         DC    AL1(CTFILEQ,000),CL30'Priority Code                '             
         DC    AL1(000,000),CL30'Unknown 44 Element           '                 
*                                                                               
CT45     DC    AL2(CT46-CT45),X'45',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT45ELM,CT45ELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Reader Class                 '             
         DC    AL1(000,000),CL30'Unknown 45 Element           '                 
*                                                                               
CT46     DC    AL2(CT47-CT46),X'46',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT46ELM,CT46ELM,0)                                   
         DC    AL1(000,082),CL30'Control Break Instructions   '                 
         DC    AL1(CTFILEQ,000),CL30'SORT Data Formula            '             
         DC    AL1(000,000),CL30'Unknown 46 Element           '                 
*                                                                               
CT47     DC    AL2(CT48-CT47),X'47',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT47ELM,CT47ELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Print Queue Password         '             
         DC    AL1(000,000),CL30'Unknown 47 Element           '                 
*                                                                               
CT48     DC    AL2(CT49-CT48),X'48',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT48ELM,CT48ELM,0)                                   
         DC    AL1(000,082),CL30'Total Statement              '                 
         DC    AL1(CTFILEQ,082),CL30'Processing Instructions      '             
         DC    AL1(000,000),CL30'Unknown 48 Element           '                 
*                                                                               
CT49     DC    AL2(CT4A-CT49),X'49',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT49ELM,CT49ELM,0)                                   
         DC    AL1(000,082),CL30'Total Detail                 '                 
         DC    AL1(CTFILEQ,000),CL30'Print Queue Retain Class     '             
         DC    AL1(000,000),CL30'Unknown 49 Element           '                 
*                                                                               
CT4A     DC    AL2(CT4B-CT4A),X'4A',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT4AELM,CT4AELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Packing Instructions         '             
         DC    AL1(000,000),CL30'Unknown 4A Element           '                 
*                                                                               
CT4B     DC    AL2(CT4C-CT4B),X'4B',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT4BELM,CT4BELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Report Short Description     '             
         DC    AL1(000,000),CL30'Unknown 4B Element           '                 
*                                                                               
CT4C     DC    AL2(CT4D-CT4C),X'4C',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT4CELM,CT4CELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Shipping Instructions        '             
         DC    AL1(000,000),CL30'Unknown 4C Element           '                 
*                                                                               
CT4D     DC    AL2(CT4E-CT4D),X'4D',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT4DELM,CT4DELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'JCL Exception                '             
         DC    AL1(000,000),CL30'Unknown 4D Element           '                 
*                                                                               
CT4E     DC    AL2(CT4F-CT4E),X'4E',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT4EELM,CT4EELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Test Phase                   '             
         DC    AL1(000,000),CL30'Unknown 4E Element           '                 
*                                                                               
CT4F     DC    AL2(CT50-CT4F),X'4F',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT4FELM,CT4FELM,0)                                   
         DC    AL1(CTFILEQ,000),CL30'Report Form Code             '             
         DC    AL1(000,000),CL30'Unknown 4F Element           '                 
*                                                                               
CT50     DC    AL2(CT51-CT50),X'50',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'Start Computation Expression '                 
         DC    AL1(000,101),CL30'Name Text                    '                 
         DC    AL1(000,106),CL30'ID Definition                '                 
         DC    AL1(CTFILEQ,000),CL30'SQL Transform Formula        '             
         DC    AL1(000,000),CL30'Unknown 50 Element           '                 
*                                                                               
CT51     DC    AL2(CT52-CT51),X'51',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,106),CL30'Associated LU                '                 
         DC    AL1(000,000),CL30'Unknown 51 Element           '                 
*                                                                               
CT52     DC    AL2(CT53-CT52),X'52',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'Computation Formula          '                 
         DC    AL1(000,106),CL30'Contact Person               '                 
         DC    AL1(000,000),CL30'Unknown 52 Element           '                 
*                                                                               
CT53     DC    AL2(CT54-CT53),X'53',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 53 Element           '                 
*                                                                               
CT54     DC    AL2(CT55-CT54),X'54',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 54 Element           '                 
*                                                                               
CT55     DC    AL2(CT56-CT55),X'55',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 55 Element           '                 
*                                                                               
CT56     DC    AL2(CT57-CT56),X'56',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(CTFILEQ,000),CL30'Timing                       '             
         DC    AL1(000,000),CL30'Unknown 56 Element           '                 
*                                                                               
CT57     DC    AL2(CT58-CT57),X'57',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 57 Element           '                 
*                                                                               
CT58     DC    AL2(CT59-CT58),X'58',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,101),CL30'Name Text (Column Display)   '                 
         DC    AL1(000,000),CL30'Unknown 58 Element           '                 
*                                                                               
CT59     DC    AL2(CT5A-CT59),X'59',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 59 Element           '                 
*                                                                               
CT5A     DC    AL2(CT5B-CT5A),X'5A',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 5A Element           '                 
*                                                                               
CT5B     DC    AL2(CT5C-CT5B),X'5B',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 5B Element           '                 
*                                                                               
CT5C     DC    AL2(CT5D-CT5C),X'5C',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 5C Element           '                 
*                                                                               
CT5D     DC    AL2(CT5E-CT5D),X'5D',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 5D Element           '                 
*                                                                               
CT5E     DC    AL2(CT5F-CT5E),X'5E',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 5E Element           '                 
*                                                                               
CT5F     DC    AL2(CT60-CT5F),X'5F',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 5F Element           '                 
*                                                                               
CT60     DC    AL2(CT61-CT60),X'60',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'Condition                    '                 
         DC    AL1(000,000),CL30'Unknown 60 Element           '                 
*                                                                               
CT61     DC    AL2(CT62-CT61),X'61',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 61 Element           '                 
*                                                                               
CT62     DC    AL2(CT63-CT62),X'62',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'Sub-Condition                '                 
         DC    AL1(000,000),CL30'Unknown 62 Element           '                 
*                                                                               
CT63     DC    AL2(CT64-CT63),X'63',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 63 Element           '                 
*                                                                               
CT64     DC    AL2(CT65-CT64),X'64',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'Max/Min Condition            '                 
         DC    AL1(000,000),CL30'Unknown 64 Element           '                 
*                                                                               
CT65     DC    AL2(CT66-CT65),X'65',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 65 Element           '                 
*                                                                               
CT66     DC    AL2(CT67-CT66),X'66',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 66 Element           '                 
*                                                                               
CT67     DC    AL2(CT68-CT67),X'67',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 67 Element           '                 
*                                                                               
CT68     DC    AL2(CT69-CT68),X'68',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 68 Element           '                 
*                                                                               
CT69     DC    AL2(CT6A-CT69),X'69',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 69 Element           '                 
*                                                                               
CT6A     DC    AL2(CT6B-CT6A),X'6A',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 6A Element           '                 
*                                                                               
CT6B     DC    AL2(CT6C-CT6B),X'6B',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 6B Element           '                 
*                                                                               
CT6C     DC    AL2(CT6D-CT6C),X'6C',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 6C Element           '                 
*                                                                               
CT6D     DC    AL2(CT6E-CT6D),X'6D',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 6D Element           '                 
*                                                                               
CT6E     DC    AL2(CT6F-CT6E),X'6E',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 6E Element           '                 
*                                                                               
CT6F     DC    AL2(CT70-CT6F),X'6F',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 6F Element           '                 
*                                                                               
CT70     DC    AL2(CT71-CT70),X'70',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT70ELM,CT70ELM,0)                                   
         DC    AL1(000,053),CL30'Profile Field Definition     '                 
         DC    AL1(000,000),CL30'Unknown 70 Element           '                 
*                                                                               
CT71     DC    AL2(CT72-CT71),X'71',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,053),CL30'Profile Post Edit Validation '                 
         DC    AL1(000,000),CL30'Unknown 71 Element           '                 
*                                                                               
CT72     DC    AL2(CT73-CT72),X'72',XL5'0000000000'                             
         DC    AL4(PAIRELM,CT72ELM,CT72ELM,0)                                   
         DC    AL1(000,053),CL30'Profile Value                '                 
         DC    AL1(000,000),CL30'Unknown 72 Element           '                 
*                                                                               
CT73     DC    AL2(CT74-CT73),X'73',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 73 Element           '                 
*                                                                               
CT74     DC    AL2(CT75-CT74),X'74',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(CTFILEQ,000),CL30'Demo Extract                 '             
         DC    AL1(000,000),CL30'Unknown 74 Element           '                 
*                                                                               
CT75     DC    AL2(CT76-CT75),X'75',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 75 Element           '                 
*                                                                               
CT76     DC    AL2(CT77-CT76),X'76',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(CTFILEQ,000),CL30'Program Type Equivalent      '             
         DC    AL1(000,000),CL30'Unknown 76 Element           '                 
*                                                                               
CT77     DC    AL2(CT78-CT77),X'77',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 77 Element           '                 
*                                                                               
CT78     DC    AL2(CT79-CT78),X'78',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(CTFILEQ,000),CL30'CPP Projection Factor        '             
         DC    AL1(000,000),CL30'Unknown 78 Element           '                 
*                                                                               
CT79     DC    AL2(CT7A-CT79),X'79',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 79 Element           '                 
*                                                                               
CT7A     DC    AL2(CT7B-CT7A),X'7A',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 7A Element           '                 
*                                                                               
CT7B     DC    AL2(CT7C-CT7B),X'7B',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 7B Element           '                 
*                                                                               
CT7C     DC    AL2(CT7D-CT7C),X'7C',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 7C Element           '                 
*                                                                               
CT7D     DC    AL2(CT7E-CT7D),X'7D',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 7D Element           '                 
*                                                                               
CT7E     DC    AL2(CT7F-CT7E),X'7E',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 7E Element           '                 
*                                                                               
CT7F     DC    AL2(CT80-CT7F),X'7F',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 7F Element           '                 
*                                                                               
CT80     DC    AL2(CT81-CT80),X'80',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'Dictionary Name              '                 
         DC    AL1(000,000),CL30'Unknown 80 Element           '                 
*                                                                               
CT81     DC    AL2(CT82-CT81),X'81',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'Entry Name                   '                 
         DC    AL1(000,000),CL30'Unknown 81 Element           '                 
*                                                                               
CT82     DC    AL2(CT83-CT82),X'82',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'System Routine               '                 
         DC    AL1(000,000),CL30'Unknown 82 Element           '                 
*                                                                               
CT83     DC    AL2(CT84-CT83),X'83',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'System Arguments             '                 
         DC    AL1(000,000),CL30'Unknown 83 Element           '                 
*                                                                               
CT84     DC    AL2(CT85-CT84),X'84',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'IF Condition Label           '                 
         DC    AL1(000,000),CL30'Unknown 84 Element           '                 
*                                                                               
CT85     DC    AL2(CT86-CT85),X'85',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'NOT Condition Label          '                 
         DC    AL1(000,000),CL30'Unknown 85 Element           '                 
*                                                                               
CT86     DC    AL2(CT87-CT86),X'86',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'Output Print Position        '                 
         DC    AL1(000,000),CL30'Unknown 86 Element           '                 
*                                                                               
CT87     DC    AL2(CT88-CT87),X'87',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,082),CL30'Output Literal               '                 
         DC    AL1(000,000),CL30'Unknown 87 Element           '                 
*                                                                               
CT88     DC    AL2(CT89-CT88),X'88',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 88 Element           '                 
*                                                                               
CT89     DC    AL2(CT8A-CT89),X'89',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 89 Element           '                 
*                                                                               
CT8A     DC    AL2(CT8B-CT8A),X'8A',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 8A Element           '                 
*                                                                               
CT8B     DC    AL2(CT8C-CT8B),X'8B',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 8B Element           '                 
*                                                                               
CT8C     DC    AL2(CT8D-CT8C),X'8C',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 8C Element           '                 
*                                                                               
CT8D     DC    AL2(CT8E-CT8D),X'8D',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 8D Element           '                 
*                                                                               
CT8E     DC    AL2(CT8F-CT8E),X'8E',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 8E Element           '                 
*                                                                               
CT8F     DC    AL2(CT90-CT8F),X'8F',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 8F Element           '                 
*                                                                               
CT90     DC    AL2(CT91-CT90),X'90',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(CTFILEQ,000),CL30'Weekly HUT                   '             
         DC    AL1(000,000),CL30'Unknown 90 Element           '                 
*                                                                               
CT91     DC    AL2(CT92-CT91),X'91',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 91 Element           '                 
*                                                                               
CT92     DC    AL2(CT93-CT92),X'92',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,043),CL30'Authorisation                '                 
         DC    AL1(000,000),CL30'Unknown 92 Element           '                 
*                                                                               
CT93     DC    AL2(CT94-CT93),X'93',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 93 Element           '                 
*                                                                               
CT94     DC    AL2(CT95-CT94),X'94',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 94 Element           '                 
*                                                                               
CT95     DC    AL2(CT96-CT95),X'95',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 95 Element           '                 
*                                                                               
CT96     DC    AL2(CT97-CT96),X'96',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 96 Element           '                 
*                                                                               
CT97     DC    AL2(CT98-CT97),X'97',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 97 Element           '                 
*                                                                               
CT98     DC    AL2(CT99-CT98),X'98',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 98 Element           '                 
*                                                                               
CT99     DC    AL2(CT9A-CT99),X'99',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 99 Element           '                 
*                                                                               
CT9A     DC    AL2(CT9B-CT9A),X'9A',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 9A Element           '                 
*                                                                               
CT9B     DC    AL2(CT9C-CT9B),X'9B',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 9B Element           '                 
*                                                                               
CT9C     DC    AL2(CT9D-CT9C),X'9C',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 9C Element           '                 
*                                                                               
CT9D     DC    AL2(CT9E-CT9D),X'9D',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 9D Element           '                 
*                                                                               
CT9E     DC    AL2(CT9F-CT9E),X'9E',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 9E Element           '                 
*                                                                               
CT9F     DC    AL2(CTA0-CT9F),X'9F',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown 9F Element           '                 
*                                                                               
CTA0     DC    AL2(CTA1-CTA0),X'A0',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,054),CL30'Value Name                   '                 
         DC    AL1(000,000),CL30'Unknown A0 Element           '                 
*                                                                               
CTA1     DC    AL2(CTA2-CTA1),X'A1',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,054),CL30'Byte Value                   '                 
         DC    AL1(000,000),CL30'Unknown A1 Element           '                 
*                                                                               
CTA2     DC    AL2(CTA3-CTA2),X'A2',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown A2 Element           '                 
*                                                                               
CTA3     DC    AL2(CTA4-CTA3),X'A3',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown A3 Element           '                 
*                                                                               
CTA4     DC    AL2(CTA5-CTA4),X'A4',XL5'0000000000'                             
         DC    AL4(PAIRELM,CTA4ELM,CTA4ELM,0)                                   
         DC    AL1(000,055),CL30'List                         '                 
         DC    AL1(000,000),CL30'Unknown A4 Element           '                 
*                                                                               
CTA5     DC    AL2(CTA6-CTA5),X'A5',XL5'0000000000'                             
         DC    AL4(PAIRELM,CTA5ELM,CTA5ELM,0)                                   
         DC    AL1(000,055),CL30'List Include                 '                 
         DC    AL1(000,000),CL30'Unknown A5 Element           '                 
*                                                                               
CTA6     DC    AL2(CTA7-CTA6),X'A6',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown A6 Element           '                 
*                                                                               
CTA7     DC    AL2(CTA8-CTA7),X'A7',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown A7 Element           '                 
*                                                                               
CTA8     DC    AL2(CTA9-CTA8),X'A8',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown A8 Element           '                 
*                                                                               
CTA9     DC    AL2(CTAA-CTA9),X'A9',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown A9 Element           '                 
*                                                                               
CTAA     DC    AL2(CTAB-CTAA),X'AA',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown AA Element           '                 
*                                                                               
CTAB     DC    AL2(CTAC-CTAB),X'AB',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown AB Element           '                 
*                                                                               
CTAC     DC    AL2(CTAD-CTAC),X'AC',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown AC Element           '                 
*                                                                               
CTAD     DC    AL2(CTAE-CTAD),X'AD',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown AD Element           '                 
*                                                                               
CTAE     DC    AL2(CTAF-CTAE),X'AE',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown AE Element           '                 
*                                                                               
CTAF     DC    AL2(CTB0-CTAF),X'AF',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown AF Element           '                 
*                                                                               
CTB0     DC    AL2(CTB1-CTB0),X'B0',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown B0 Element           '                 
*                                                                               
CTB1     DC    AL2(CTB2-CTB1),X'B1',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,045),CL30'Field Map                    '                 
         DC    AL1(000,000),CL30'Unknown B1 Element           '                 
*                                                                               
CTB2     DC    AL2(CTB3-CTB2),X'B2',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,058),CL30'Interface                    '                 
         DC    AL1(000,000),CL30'Unknown B2 Element           '                 
*                                                                               
CTB3     DC    AL2(CTB4-CTB3),X'B3',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Rate                         '                 
*                                                                               
CTB4     DC    AL2(CTB5-CTB4),X'B4',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,067),CL30'Agency Group Details         '                 
         DC    AL1(000,000),CL30'Unknown B4 Element           '                 
*                                                                               
CTB5     DC    AL2(CTB6-CTB5),X'B5',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(CTFILEQ,000),CL30'Date Scheme Description      '             
         DC    AL1(000,000),CL30'Unknown B5 Element           '                 
*                                                                               
CTB6     DC    AL2(CTB7-CTB6),X'B6',XL5'0000000000'                             
         DC    AL4(PAIRELM,CTB6ELM,CTB6ELM,0)                                   
         DC    AL1(CTFILEQ,070),CL30'Associated Agency            '             
         DC    AL1(000,000),CL30'Unknown B6 Element           '                 
*                                                                               
CTB7     DC    AL2(CTB8-CTB7),X'B7',XL5'0000000000'                             
         DC    AL4(PAIRELM,CTB7ELM,CTB7ELM,0)                                   
         DC    AL1(000,070),CL30'Locket Entry                 '                 
         DC    AL1(CTFILEQ,000),CL30'Program (Security)           '             
         DC    AL1(000,000),CL30'Unknown B7 Element           '                 
*                                                                               
CTB8     DC    AL2(CTB9-CTB8),X'B8',XL5'0000000000'                             
         DC    AL4(PAIRELM,CTB8ELM,CTB8ELM,0)                                   
         DC    AL1(000,028),CL30'Action                       '                 
         DC    AL1(CTFILEQ,000),CL30'Security Agency Alpha Id     '             
         DC    AL1(000,000),CL30'Unknown B8 Element           '                 
*                                                                               
CTB9     DC    AL2(CTBA-CTB9),X'B9',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,029),CL30'Record                       '                 
         DC    AL1(CTFILEQ,000),CL30'Agency Access Detail         '             
         DC    AL1(000,000),CL30'Unknown B9 Element           '                 
*                                                                               
CTBA     DC    AL2(CTBB-CTBA),X'BA',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,015),CL30'Program Version Control      '                 
         DC    AL1(000,016),CL30'Program Version Control      '                 
         DC    AL1(000,029),CL30'Record/Action Combination    '                 
         DC    AL1(000,000),CL30'Unknown BA Element           '                 
*                                                                               
CTBB     DC    AL2(CTBC-CTBB),X'BB',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'Field                        '                 
         DC    AL1(000,024),CL30'Field                        '                 
         DC    AL1(000,025),CL30'Field                        '                 
         DC    AL1(000,026),CL30'Field                        '                 
         DC    AL1(000,027),CL30'Field                        '                 
         DC    AL1(000,028),CL30'Field                        '                 
         DC    AL1(000,029),CL30'Field                        '                 
         DC    AL1(000,030),CL30'Field                        '                 
         DC    AL1(000,031),CL30'Field                        '                 
         DC    AL1(000,032),CL30'Field                        '                 
         DC    AL1(000,033),CL30'Field                        '                 
         DC    AL1(000,034),CL30'Field                        '                 
         DC    AL1(000,035),CL30'Field                        '                 
         DC    AL1(000,036),CL30'Field                        '                 
         DC    AL1(000,037),CL30'Field                        '                 
         DC    AL1(CTFILEQ,000),CL30'Adv Timeout                  '             
         DC    AL1(000,000),CL30'Unknown BB Element           '                 
*                                                                               
CTBC     DC    AL2(CTBD-CTBC),X'BC',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'Option                       '                 
         DC    AL1(000,024),CL30'Option                       '                 
         DC    AL1(000,025),CL30'Option                       '                 
         DC    AL1(000,026),CL30'Option                       '                 
         DC    AL1(000,027),CL30'Option                       '                 
         DC    AL1(000,028),CL30'Option                       '                 
         DC    AL1(000,029),CL30'Option                       '                 
         DC    AL1(000,030),CL30'Option                       '                 
         DC    AL1(000,031),CL30'Option                       '                 
         DC    AL1(000,032),CL30'Option                       '                 
         DC    AL1(000,033),CL30'Option                       '                 
         DC    AL1(000,034),CL30'Option                       '                 
         DC    AL1(000,035),CL30'Option                       '                 
         DC    AL1(000,036),CL30'Option                       '                 
         DC    AL1(000,037),CL30'Option                       '                 
         DC    AL1(000,067),CL30'UK Media Agency              '                 
         DC    AL1(000,000),CL30'Unknown BC Element           '                 
*                                                                               
CTBD     DC    AL2(CTBE-CTBD),X'BD',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,033),CL30'Field Control Write/Read     '                 
         DC    AL1(000,113),CL30'Principal User Id   '                          
         DC    AL1(000,000),CL30'Unknown BD Element           '                 
         DC    CL32'Field Control Write / Read      '                           
         DC    X'BD',XL3'000000',AL4(00000000,00000000,PAIRELM)                 
*                                                                               
CTBE     DC    AL2(CTBF-CTBE),X'BE',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,033),CL30'Field Control Read           '                 
         DC    AL1(000,113),CL30'Security Principal User Id   '                 
         DC    AL1(000,000),CL30'Unknown BE Element           '                 
*                                                                               
CTBF     DC    AL2(CTC0-CTBF),X'BF',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,034),CL30'Options Control              '                 
         DC    AL1(CTFILEQ,034),CL30'Lotus Notes Admin User Id    '             
         DC    AL1(000,000),CL30'Unknown BF Element           '                 
*                                                                               
CTC0     DC    AL2(CTC1-CTC0),X'C0',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,008),CL30'Printer Name LUID            '                 
         DC    AL1(000,023),CL30'Office                       '                 
         DC    AL1(000,000),CL30'Unknown C0 Element           '                 
*                                                                               
CTC1     DC    AL2(CTC2-CTC1),X'C1',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,008),CL30'Printer Name Terminal X-ref  '                 
         DC    AL1(000,024),CL30'Department                   '                 
         DC    AL1(000,000),CL30'Unknown C1 Element           '                 
*                                                                               
CTC2     DC    AL2(CTC3-CTC2),X'C2',XL5'0000000000'                             
         DC    AL4(PAIRELM,CTC2ELM,CTC2ELM,0)                                   
         DC    AL1(000,008),CL30'Printer Name Description     '                 
         DC    AL1(000,023),CL30'Person Count                 '                 
         DC    AL1(000,024),CL30'Person Count                 '                 
         DC    AL1(000,025),CL30'Person Count                 '                 
         DC    AL1(000,026),CL30'Person Count                 '                 
         DC    AL1(000,027),CL30'Person Count                 '                 
         DC    AL1(000,028),CL30'Person Count                 '                 
         DC    AL1(000,029),CL30'Person Count                 '                 
         DC    AL1(000,030),CL30'Person Count                 '                 
         DC    AL1(000,031),CL30'Person Count                 '                 
         DC    AL1(000,032),CL30'Person Count                 '                 
         DC    AL1(000,033),CL30'Person Count                 '                 
         DC    AL1(000,034),CL30'Person Count                 '                 
         DC    AL1(000,035),CL30'Person Count                 '                 
         DC    AL1(000,036),CL30'Person Count                 '                 
         DC    AL1(000,037),CL30'Person Count                 '                 
         DC    AL1(000,000),CL30'Unknown C2 Element           '                 
*                                                                               
CTC3     DC    AL2(CTC4-CTC3),X'C3',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,008),CL30'Printer Name Default User Id '                 
         DC    AL1(000,059),CL30'Personal Id                  '                 
         DC    AL1(000,000),CL30'Unknown C3 Element           '                 
*                                                                               
CTC4     DC    AL2(CTC5-CTC4),X'C4',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,025),CL30'Personal Password            '                 
         DC    AL1(CTFILEQ,000),CL30'AFP User Id Pointer          '             
         DC    AL1(000,000),CL30'Unknown C4 Element           '                 
*                                                                               
CTC5     DC    AL2(CTC6-CTC5),X'C5',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,025),CL30'Person Name                  '                 
         DC    AL1(000,000),CL30'Unknown C5 Element           '                 
*                                                                               
CTC6     DC    AL2(CTC7-CTC6),X'C6',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,025),CL30'Personnel Details            '                 
         DC    AL1(000,000),CL30'Unknown C6 Element           '                 
*                                                                               
CTC7     DC    AL2(CTC8-CTC7),X'C7',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,025),CL30'Person Address               '                 
         DC    AL1(000,000),CL30'Unknown C7 Element           '                 
*                                                                               
CTC8     DC    AL2(CTC9-CTC8),X'C8',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,025),CL30'Person Access Group Code     '                 
         DC    AL1(000,000),CL30'Unknown C8 Element           '                 
*                                                                               
CTC9     DC    AL2(CTCA-CTC9),X'C9',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,026),CL30'Person Access Group Name     '                 
         DC    AL1(000,000),CL30'Unknown C9 Element           '                 
*                                                                               
CTCA     DC    AL2(CTCB-CTCA),X'CA',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,059),CL30'Password Effective Date      '                 
         DC    AL1(000,000),CL30'Unknown CA Element           '                 
*                                                                               
CTCB     DC    AL2(CTCC-CTCB),X'CB',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'Limit Access Group Name      '                 
         DC    AL1(000,000),CL30'Unknown CB Element           '                 
*                                                                               
CTCC     DC    AL2(CTCD-CTCC),X'CC',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'Limit Access System          '                 
         DC    AL1(000,000),CL30'Unknown CC Element           '                 
*                                                                               
CTCD     DC    AL2(CTCE-CTCD),X'CD',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,025),CL30'Person Limit Access Grp Code '                 
         DC    AL1(000,000),CL30'Unknown CD Element           '                 
*                                                                               
CTCE     DC    AL2(CTCF-CTCE),X'CE',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,026),CL30'Comment/Information Line     '                 
         DC    AL1(000,000),CL30'Unknown CE Element           '                 
*                                                                               
CTCF     DC    AL2(CTD0-CTCF),X'CF',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'Manager Id                   '                 
         DC    AL1(000,024),CL30'Manager Id                   '                 
         DC    AL1(000,025),CL30'Manager Id                   '                 
         DC    AL1(000,026),CL30'Manager Id                   '                 
         DC    AL1(000,027),CL30'Manager Id                   '                 
         DC    AL1(000,028),CL30'Manager Id                   '                 
         DC    AL1(000,029),CL30'Manager Id                   '                 
         DC    AL1(000,030),CL30'Manager Id                   '                 
         DC    AL1(000,031),CL30'Manager Id                   '                 
         DC    AL1(000,032),CL30'Manager Id                   '                 
         DC    AL1(000,033),CL30'Manager Id                   '                 
         DC    AL1(000,034),CL30'Manager Id                   '                 
         DC    AL1(000,035),CL30'Manager Id                   '                 
         DC    AL1(000,036),CL30'Manager Id                   '                 
         DC    AL1(000,037),CL30'Manager Id                   '                 
         DC    AL1(000,000),CL30'Unknown CF Element           '                 
*                                                                               
CTD0     DC    AL2(CTD1-CTD0),X'D0',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,014),CL30'List of Unit/Ledgers         '                 
         DC    AL1(000,023),CL30'Old Person Personal Id       '                 
         DC    AL1(000,024),CL30'Old Person Personal Id       '                 
         DC    AL1(000,025),CL30'Old Person Personal Id       '                 
         DC    AL1(000,026),CL30'Old Person Personal Id       '                 
         DC    AL1(000,027),CL30'Old Person Personal Id       '                 
         DC    AL1(000,028),CL30'Old Person Personal Id       '                 
         DC    AL1(000,029),CL30'Old Person Personal Id       '                 
         DC    AL1(000,030),CL30'Old Person Personal Id       '                 
         DC    AL1(000,031),CL30'Old Person Personal Id       '                 
         DC    AL1(000,032),CL30'Old Person Personal Id       '                 
         DC    AL1(000,033),CL30'Old Person Personal Id       '                 
         DC    AL1(000,034),CL30'Old Person Personal Id       '                 
         DC    AL1(000,035),CL30'Old Person Personal Id       '                 
         DC    AL1(000,036),CL30'Old Person Personal Id       '                 
         DC    AL1(000,037),CL30'Old Person Personal Id       '                 
         DC    AL1(000,000),CL30'Unknown D0 Element           '                 
*                                                                               
CTD1     DC    AL2(CTD2-CTD1),X'D1',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'1R Person Account Code       '                 
         DC    AL1(000,024),CL30'1R Person Account Code       '                 
         DC    AL1(000,025),CL30'1R Person Account Code       '                 
         DC    AL1(000,026),CL30'1R Person Account Code       '                 
         DC    AL1(000,027),CL30'1R Person Account Code       '                 
         DC    AL1(000,028),CL30'1R Person Account Code       '                 
         DC    AL1(000,029),CL30'1R Person Account Code       '                 
         DC    AL1(000,030),CL30'1R Person Account Code       '                 
         DC    AL1(000,031),CL30'1R Person Account Code       '                 
         DC    AL1(000,032),CL30'1R Person Account Code       '                 
         DC    AL1(000,033),CL30'1R Person Account Code       '                 
         DC    AL1(000,034),CL30'1R Person Account Code       '                 
         DC    AL1(000,035),CL30'1R Person Account Code       '                 
         DC    AL1(000,036),CL30'1R Person Account Code       '                 
         DC    AL1(000,037),CL30'1R Person Account Code       '                 
         DC    AL1(000,000),CL30'Unknown D1 Element           '                 
*                                                                               
CTD2     DC    AL2(CTD3-CTD2),X'D2',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'Password Activity            '                 
         DC    AL1(000,024),CL30'Password Activity            '                 
         DC    AL1(000,025),CL30'Password Activity            '                 
         DC    AL1(000,026),CL30'Password Activity            '                 
         DC    AL1(000,027),CL30'Password Activity            '                 
         DC    AL1(000,028),CL30'Password Activity            '                 
         DC    AL1(000,029),CL30'Password Activity            '                 
         DC    AL1(000,030),CL30'Password Activity            '                 
         DC    AL1(000,031),CL30'Password Activity            '                 
         DC    AL1(000,032),CL30'Password Activity            '                 
         DC    AL1(000,033),CL30'Password Activity            '                 
         DC    AL1(000,034),CL30'Password Activity            '                 
         DC    AL1(000,035),CL30'Password Activity            '                 
         DC    AL1(000,036),CL30'Password Activity            '                 
         DC    AL1(000,037),CL30'Password Activity            '                 
         DC    AL1(000,000),CL30'Unknown D2 Element           '                 
*                                                                               
CTD3     DC    AL2(CTD4-CTD3),X'D3',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,024),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,025),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,026),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,027),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,028),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,029),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,030),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,031),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,032),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,033),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,034),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,035),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,036),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,037),CL30'Dictionary Substitution Ref #'                 
         DC    AL1(000,000),CL30'Unknown D3 Element           '                 
*                                                                               
CTD4     DC    AL2(CTD5-CTD4),X'D4',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown D4 Element           '                 
*                                                                               
CTD5     DC    AL2(CTD6-CTD5),X'D5',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown D5 Element           '                 
*                                                                               
CTD6     DC    AL2(CTD7-CTD6),X'D6',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown D6 Element           '                 
*                                                                               
CTD7     DC    AL2(CTD8-CTD7),X'D7',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown D7 Element           '                 
*                                                                               
CTD8     DC    AL2(CTD9-CTD8),X'D8',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown D8 Element           '                 
*                                                                               
CTD9     DC    AL2(CTDA-CTD9),X'D9',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown D9 Element           '                 
*                                                                               
CTDA     DC    AL2(CTDB-CTDA),X'DA',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown DA Element           '                 
*                                                                               
CTDB     DC    AL2(CTDC-CTDB),X'DB',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown DB Element           '                 
*                                                                               
CTDC     DC    AL2(CTDD-CTDC),X'DC',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown DC Element           '                 
*                                                                               
CTDD     DC    AL2(CTDE-CTDD),X'DD',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,025),CL30'DDS Person Attributes        '                 
         DC    AL1(000,000),CL30'Unknown DD Element           '                 
*                                                                               
CTDE     DC    AL2(CTDF-CTDE),X'DE',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,025),CL30'DDS Agency List              '                 
         DC    AL1(000,000),CL30'Unknown DE Element           '                 
*                                                                               
CTDF     DC    AL2(CTE0-CTDF),X'DF',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,037),CL30'Timesheet Approver Group     '                 
         DC    AL1(000,000),CL30'Unknown DF Element           '                 
*                                                                               
CTE0     DC    AL2(CTE1-CTE0),X'E0',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,024),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,025),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,026),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,027),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,028),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,029),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,030),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,031),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,032),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,033),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,034),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,035),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,036),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,037),CL30'Timesheet Approver Group Code'                 
         DC    AL1(000,000),CL30'Unknown E0 Element           '                 
*                                                                               
CTE1     DC    AL2(CTE2-CTE1),X'E1',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,069),CL30'Script Object Code           '                 
         DC    AL1(000,000),CL30'Unknown E1 Element           '                 
*                                                                               
CTE2     DC    AL2(CTE3-CTE2),X'E2',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,062),CL30'Application Access           '                 
         DC    AL1(000,000),CL30'Unknown E2 Element           '                 
*                                                                               
CTE3     DC    AL2(CTE4-CTE3),X'E3',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(CTFILEQ,000),CL30'DDS Department Level Security'             
         DC    AL1(000,000),CL30'Unknown E3 Element           '                 
*                                                                               
CTE4     DC    AL2(CTE5-CTE4),X'E4',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'Password History             '                 
         DC    AL1(000,024),CL30'Password History             '                 
         DC    AL1(000,025),CL30'Password History             '                 
         DC    AL1(000,026),CL30'Password History             '                 
         DC    AL1(000,027),CL30'Password History             '                 
         DC    AL1(000,028),CL30'Password History             '                 
         DC    AL1(000,029),CL30'Password History             '                 
         DC    AL1(000,030),CL30'Password History             '                 
         DC    AL1(000,031),CL30'Password History             '                 
         DC    AL1(000,032),CL30'Password History             '                 
         DC    AL1(000,033),CL30'Password History             '                 
         DC    AL1(000,034),CL30'Password History             '                 
         DC    AL1(000,035),CL30'Password History             '                 
         DC    AL1(000,036),CL30'Password History             '                 
         DC    AL1(000,037),CL30'Password History             '                 
         DC    AL1(000,069),CL30'Script Book Name             '                 
         DC    AL1(000,000),CL30'Unknown E4 Element           '                 
*                                                                               
CTE5     DC    AL2(CTE6-CTE5),X'E5',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,023),CL30'Person E-mail Id             '                 
         DC    AL1(000,024),CL30'Person E-mail Id             '                 
         DC    AL1(000,025),CL30'Person E-mail Id             '                 
         DC    AL1(000,026),CL30'Person E-mail Id             '                 
         DC    AL1(000,027),CL30'Person E-mail Id             '                 
         DC    AL1(000,028),CL30'Person E-mail Id             '                 
         DC    AL1(000,029),CL30'Person E-mail Id             '                 
         DC    AL1(000,030),CL30'Person E-mail Id             '                 
         DC    AL1(000,031),CL30'Person E-mail Id             '                 
         DC    AL1(000,032),CL30'Person E-mail Id             '                 
         DC    AL1(000,033),CL30'Person E-mail Id             '                 
         DC    AL1(000,034),CL30'Person E-mail Id             '                 
         DC    AL1(000,035),CL30'Person E-mail Id             '                 
         DC    AL1(000,036),CL30'Person E-mail Id             '                 
         DC    AL1(000,037),CL30'Person E-mail Id             '                 
         DC    AL1(000,000),CL30'Unknown E5 Element           '                 
*                                                                               
CTE6     DC    AL2(CTE7-CTE6),X'E6',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown E6 Element           '                 
*                                                                               
CTE7     DC    AL2(CTE8-CTE7),X'E7',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown E7 Element           '                 
*                                                                               
CTE8     DC    AL2(CTE9-CTE8),X'E8',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown E8 Element           '                 
*                                                                               
CTE9     DC    AL2(CTEA-CTE9),X'E9',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown E9 Element           '                 
*                                                                               
CTEA     DC    AL2(CTEB-CTEA),X'EA',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown EA Element           '                 
*                                                                               
CTEB     DC    AL2(CTEC-CTEB),X'EB',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown EB Element           '                 
*                                                                               
CTEC     DC    AL2(CTED-CTEC),X'EC',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown EC Element           '                 
*                                                                               
CTED     DC    AL2(CTEE-CTED),X'ED',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown ED Element           '                 
*                                                                               
CTEE     DC    AL2(CTEF-CTEE),X'EE',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown EE Element           '                 
*                                                                               
CTEF     DC    AL2(CTF0-CTEF),X'EF',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown EF Element           '                 
*                                                                               
CTF0     DC    AL2(CTF1-CTF0),X'F0',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown F0 Element           '                 
*                                                                               
CTF1     DC    AL2(CTF2-CTF1),X'F1',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Activity                     '                 
*                                                                               
CTF2     DC    AL2(CTF3-CTF2),X'F2',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown F2 Element           '                 
*                                                                               
CTF3     DC    AL2(CTF4-CTF3),X'F3',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown F3 Element           '                 
*                                                                               
CTF4     DC    AL2(CTF5-CTF4),X'F4',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown F4 Element           '                 
*                                                                               
CTF5     DC    AL2(CTF6-CTF5),X'F5',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown F5 Element           '                 
*                                                                               
CTF6     DC    AL2(CTF7-CTF6),X'F6',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown F6 Element           '                 
*                                                                               
CTF7     DC    AL2(CTF8-CTF7),X'F7',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown F7 Element           '                 
*                                                                               
CTF8     DC    AL2(CTF9-CTF8),X'F8',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown F8 Element           '                 
*                                                                               
CTF9     DC    AL2(CTFA-CTF9),X'F9',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown F9 Element           '                 
*                                                                               
CTFA     DC    AL2(CTFB-CTFA),X'FA',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown FA Element           '                 
*                                                                               
CTFB     DC    AL2(CTFC-CTFB),X'FB',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown FB Element           '                 
*                                                                               
CTFC     DC    AL2(CTFD-CTFC),X'FC',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown FC Element           '                 
*                                                                               
CTFD     DC    AL2(CTFE-CTFD),X'FD',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown FD Element           '                 
*                                                                               
CTFE     DC    AL2(CTFF-CTFE),X'FE',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown FE Element           '                 
*                                                                               
CTFF     DC    AL2(CTXX-CTFF),X'FF',XL5'0000000000'                             
         DC    AL4(PAIRELM,0,0,0)                                               
         DC    AL1(000,000),CL30'Unknown FF Element            '                
*                                                                               
CTXX     DC    X'FFFF'                                                          
         EJECT                                                                  
**********************************************************************          
* DSECT COVERING CTELEM TABLE                                        *          
**********************************************************************          
CTELEMD  DSECT                                                                  
CTEDSP   DS    AL2                 DISPLACEMENT TO NEXT ELEMENT                 
CTNUM    DS    X                   ELEMENT NUMBER                               
CTSPR    DS    XL5                 N/D                                          
CTRPOUT  DS    AL4                 PAIRED ELEMENT INFORMATION O/P               
CTRDOUT  DS    AL4                 SINGLE ELEMENT INFO O/P                      
CTRFOUT  DS    AL4                 SINGLE ELEMENT DETAILED INFO O/P             
         DS    AL4                 N/D                                          
CTFELEML EQU   *-CTELEMD           FIXED PORTION LENGTH                         
*                                                                               
CTESYS   DS    X                   SYSTEM                                       
CTEFREC  DS    X                   RECORD NUMBER                                
CTENAME  DS    CL30                ELEMENT NAME                                 
*                                                                               
CTDFAR   CSECT                                                                  
         EJECT                                                                  
**********************************************************************          
* NON-ADDRESSIBLE STORAGE AREAS                                      *          
**********************************************************************          
         DS    0D                                                               
         DC    CL16'******UTL******'                                            
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    AL1(10)             Control system                               
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL16'******SSB******'                                            
SSB      DC    (SSOLNQ)X'00'                                                    
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'                                                            
         ORG   SSB+(SSOSTAT2-SSBD)                                              
         DC    AL1(SSOSRSQF)       RECOVER OFFLINE TO SEQUENTIAL FILE           
         ORG   SSB+(SSODATE-SSBD)                                               
         DC    CL8' '                                                           
         ORG                                                                    
                                                                                
         DS    0D                                                               
IO1      DS    (IOL)X                                                           
IO2      DS    (IOL)X                                                           
IO3      DS    (IOL)X                                                           
*                                                                               
BUFFER   DS    60000C                                                           
         EJECT                                                                  
**********************************************************************          
* DSECTS                                                             *          
**********************************************************************          
*FORMAT: PROG/USERID/TERMINAL/ACTION/TIME/KEY/RECORD NAME                       
OUTFORM  DSECT                                                                  
OUTPRG   DS    H                                                                
OUTUID   DS    H                                                                
OUTTRM   DS    H                                                                
OUTACT   DS    H                                                                
OUTTIME  DS    H                                                                
OUTKEY   DS    H                                                                
OUTNAME  DS    H                                                                
*                                                                               
BLKDEFD  DSECT                                                                  
BLKHI    DS    H                                                                
BLKLOW   DS    H                                                                
BLKMAX   DS    H                                                                
BLKLNQ   EQU   *-BLKDEFD                                                        
*                                                                               
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
APGMLST  DS    A                                                                
AADDNTRY DS    A                                                                
AELMNTRY DS    A                                                                
AELCOPYS DS    A                                                                
AELCHNGS DS    A                                                                
AELADDS  DS    A                                                                
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
RECINCNT DS    F                   CURRENT TOTAL NO. OF RECORDS READ            
DMDA     DS    F                   CURRENT DM DISK ADDRESS                      
DMCB     DS    XL24                DM PARAMETER LIST                            
DMWORK   DS    XL72                DM WORK AREA                                 
PLIST    DS    XL24                GENERAL PARAMETER LIST                       
ACMRG    DS    F                                                                
*                                                                               
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
TRAILER  DS    X                                                                
MAXBLK   DS    X                                                                
*                                                                               
JOB      DS    CL8                                                              
JOBNUM   DS    CL8                                                              
*                                                                               
PARMCNT  DS    X                   NO OF I/P PARAMETERS                         
PARMCNTQ EQU   20                  MAX NUMBER OF I/P PARAMETERS                 
SCANTAB  DS    (PARMCNTQ)CL(SCBLKLQ)                                            
*                                                                               
LSTCTFRD DS    0XL12               COVERS LAST USERID READ (CTFILE)             
LSTNUM   DS    XL2                 USERID NO.                                   
LSTNAM   DS    XL10                ASSOCIATED NAME                              
*                                                                               
LSTCTFRT DS    0XL10               COVERS LAST TERMINAL NO. READ (CTF)          
LSTTNUM  DS    XL2                 TERMINAL NO.                                 
LSTTNAM  DS    XL8                 LUID                                         
*                                                                               
LSTPROG  DS    0XL8                COVERS LAST PROGRAM READ (FATABOFF)          
LSTPNUM  DS    X                   PROGRAM NUMBER                               
LSTPNAM  DS    XL7                 PROGRAM NAME                                 
*                                                                               
SFLAG    DS    X                                                                
UPSI     DS    X                   MVS PARM= CARD VALUES                        
FILTERS  DS    X                   FIXED BYTE OF PARAMETERS SET                 
VAL_BYTE DS    X                   RESETTABLE BYTE FOR MATCHES                  
*                                                                               
TRMIND   EQU   X'80'               TERMINAL ID NO.         VALIDATED            
LUIND    EQU   X'40'               LUID                    VALIDATED            
USRIND   EQU   X'20'               USER ID NO.             VALIDATED            
PRGIND   EQU   X'10'               PROGRAM NUMBER          VALIDATED            
AGYIND   EQU   X'08'               AGENCY ID (BIN<255)     N.VALIDATED          
RTYPIND  EQU   X'04'               RECORD TYPE FROM RECTAB VALIDATED            
STDONLY  EQU   X'02'               STANDARD RECORDS ONLY                        
NOSORT   EQU   X'01'               NO RECORDS REQUIRED                          
*                                                                               
INPTYPE  DS    X                   INPUT MEDIUM (TAPE OR DISK)                  
*                                                                               
OUTTYPE  DS    X                   OUTPUT TYPES TO BE DISPLAYED                 
ADDOUT   EQU   X'80'               ADD RECORDS SHOWN                            
COPYOUT  EQU   X'40'               COPY RECORDS SHOWN                           
CHNGOUT  EQU   X'20'               CHANGE RECORDS SHOWN                         
DELOUT   EQU   X'10'               DELETE RECORDS SHOWN                         
RESTOUT  EQU   X'08'               RESTORE RECORDS SHOWN                        
ALLOUT   EQU   X'04'               ALL RECORDS TO BE DISPLAYED                  
FULLOUT  EQU   X'02'               FULL INFORMATION ON RECORDS                  
*                                                                               
SORTACTV DS    X                                                                
SORTFILT DS    X                   RECORDS TO PASS TO SORTER                    
SRTFALL  EQU   C'A'                ALL RECORDS  (IGNORE FILTERS)                
SRTFNONE EQU   C'N'                NO RECORDS   (SET ALL INVALID)               
SRTFSTD  EQU   C'S'                STANDARD SET (USE FILTERS)                   
*                                                                               
FERN     DS    X                   CONTAINS CURRENT ERROR NO. OR 0              
*                                                                               
THISELM  DS    X                                                                
THISELMH DS    X                                                                
*                                                                               
V_TCODE  DS    H                   TERMINAL CODE VALUE IF SET                   
V_USRID  DS    H                   USER ID VALUE IF SET                         
V_RTYPE  DS    X                   RECORD TYPE IF SET                           
V_AGYID  DS    X                   AGENCY ID IF SET                             
V_PRNUM  DS    X                   PROGRAM NO. IF SET                           
*                                                                               
PROGRAM  DS    CL7                 PROGRAM NAME IF VALID                        
REPDATE  DS    CL10                                                             
*                                                                               
MODE     DS    X                   ADD/DELETE/RESTORE/CHANGE TYPE               
MODEADD  EQU   1                   RECORD ADDED                                 
MODECHG  EQU   2                   RECORD CHANGED                               
MODEDEL  EQU   3                   RECORD DELETED                               
MODERES  EQU   4                   RECORD RESTORED                              
MODECPY  EQU   5                   SPECIAL                                      
MODEERR  EQU   255                 ERROR                                        
*                                                                               
SORTCARD DS    CL80                INPUT CARD FOR SORT ROUTINE                  
CARDIO   DS    CL80                                                             
IND_FLAG DS    XL1                 INDICATOR FLAG                               
*                                                                               
ACOPY1   DS    A                  A(FIRST UNRESOLVED COPY ELEMENT)              
ACOPY2   DS    A                  A(CURRENT COPY ELEMENT)                       
ACHNG1   DS    A                  A(FIRST UNRESOLVED CHANGE ELEMENT)            
ACHNG2   DS    A                  A(CURRENT CHANGE ELEMENT)                     
WHRCOPY  DS    A                  A(CURRENT POS IN ELCOPYS)                     
WHRCHNG  DS    A                  A(CURRENT POS IN ELCHNGS)                     
NOW1     DS    A                  WHERE IN RECORD CURRENT FOR PRINT             
WAS1     DS    A                  WHERE IN RECORD COPY FOR PRINT                
ELADD    DS    A                                                                
AOUTFORM DS    A                  ADDRESS OF CURRENT TITLE DISPLACEMENT         
RELO     DS    A                  RELOCATION FACTOR (IF NOT S/ALONE)            
ASE      DS    A                  A(SELIST ENTRY)                               
ACTSE    DS    A                  A(CONTROL SELIST ENTRY)                       
ACTPGMS  DS    A                  A(CONTROL PROGRAM TABLE)                      
*                                                                               
COPYNUM1 DS    X                  CURRENT NO. OF UNRESOLVED COPY ELMNTS         
CHNGNUM1 DS    X                  CURRENT NO. OF UNRESOLVED CHNG ELMNTS         
ADDNUM1  DS    X                  TOTAL OF UNRESOLVED ADDS IN EL.               
ADDNUM2  DS    X                  TOTAL ADDS RESOLVED THIS TIME                 
DELNUM1  DS    X                  TOTAL OF UNRESOLVED DELS IN EL.               
DELNUM2  DS    X                  TOTAL DELS RESOLVED THIS TIME                 
FLAG     DS    X                                                                
*                                                                               
ELNAMEC  DS    CL30               SAVED ELEMENT NAME                            
*                                                                               
         DS    0D                                                               
WORK     DS    CL512                                                            
PBLNUMQ  EQU   24                                                               
PBLOCK   DS    (PBLNUMQ)CL132                                                   
PBLOCKL  EQU   *-PBLOCK                                                         
*                                                                               
ELCOPYS  DS    XL(256*4)         ELEMENT COUNT FOR COPY - KEEP TOGETHER         
ELCOPYSL EQU   *-ELCOPYS                                                        
ELCHNGS  DS    XL(256*4)         ELEMENT COUNT FOR SORT                         
ELCHNGSL EQU   *-ELCHNGS                                                        
ELADDS   DS    XL(256*4)                                                        
ELADDSL  EQU   *-ELADDS                                                         
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DMRCVRHDR                                                           *         
***********************************************************************         
RECDS    DSECT                                                                  
RECLN    DS    CL4                                                              
       ++INCLUDE DMRCVRHDR                                                      
RCVFRST  DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* GEGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEGENFILE                                                      
         PRINT ON                                                               
* GEGENSPEC                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEGENSPEC                                                      
         PRINT ON                                                               
* GEGENESS                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENESS                                                       
         PRINT ON                                                               
* CTGENAPASS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENAPASS                                                     
         PRINT ON                                                               
* CTGENASTAF                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENASTAF                                                     
         PRINT ON                                                               
* CTGENAGRD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENAGRD                                                      
         PRINT ON                                                               
* CTGENADVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENADVD                                                      
         PRINT ON                                                               
* CTGENDTBD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENDTBD                                                      
         PRINT ON                                                               
* CTGENRFP                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENRFP                                                       
         PRINT ON                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
* DDACTIVD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* FAPGMLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DMRCVREXT                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMRCVREXT                                                      
         PRINT ON                                                               
*DMSPACED                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
         EJECT                                                                  
*DMDSYSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMDSYSHDR                                                      
         PRINT ON                                                               
*DMDTFPH                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
* FACTRY                                                                        
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
         PRINT ON                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* FASSBOFF                                                                      
SSBD     DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
SSOLNQ   EQU   *-SSBD                                                           
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* CVT                                                                           
*        CVT     DSECT=YES                                                      
* TIOT                                                                          
*        IEFTIOT1                                                               
* IHASDWA                                                                       
*        IHASDWA GR32=YES                                                       
* IKJTCB                                                                        
         IKJTCB  LIST=YES                                                       
* IHAPSA                                                                        
         IHAPSA  LIST=YES                                                       
* IHAASCB                                                                       
         IHAASCB LIST=YES                                                       
* IHASTCB                                                                       
         IHASTCB LIST=YES                                                       
* IHAASSB                                                                       
         IHAASSB LIST=YES                                                       
* IHAJSAB                                                                       
         IAZJSAB LIST=YES                                                       
* IHALDA                                                                        
         IHALDA                                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008CTDFAR    07/09/19'                                      
         END                                                                    
