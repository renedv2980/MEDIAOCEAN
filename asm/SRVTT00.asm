*          DATA SET SRVTT00    AT LEVEL 017 AS OF 06/12/02                      
*PHASE T15B00A                                                                  
         SPACE 1                                                                
***********************************************************************         
* THIS SERVICE REQUEST PROVIDES VTAM TRACE SERVICES                   *         
*                                                                     *         
* P1 = +   P2 = LUNAME/ALL/'ME' TURNS TRACE ON                        *         
* P1 = -   P2 =                 TURNS TRACE OFF                       *         
* P1 = L   P2 =            LIST BUFFER DATA FROM LAST DISK ADDR       *         
*          P2 = P          LIST PRINTERS IN BUFFER                    *         
*          P2 = S          LIST SHUTTLES IN BUFFER                    *         
*          P2 = LUNAME     LIST BUFFER DATA FILTERED ON LUNAME        *         
*                          (CAN SPEC ANY # OF CHARS FOR LUNAME)       *         
*          P2 = R=NNNN     LIST BUFFER DATA FILTERED ON RCFD          *         
*                          (CAN SPEC ANY # OF CHARS FOR NNNN)         *         
*          P2 = S=NNNNNNNN LIST BUFFER DATA FILTERED ON SENSE         *         
*                          (CAN SPEC ANY # OF CHARS FOR NNNNNNNN)     *         
*          P3 = NNNNNNNN   START LIST FROM THIS D/A                   *         
* P1 = X                   START LIST AT TOP & CLR TWAB               *         
*          P2 = SEE P1 = L ALL P2'S FROM P1=L ARE VALID AND WORK SAME *         
* P1 = C                   TEST ALL TCID'S (UTL) VS. VTAMCID (LN CNTL)*         
*                          AND REPORT DISCREPANCIES                   *         
*          P2 = LUNAME     TEST CIDS FILTERED ON LUNAME (OR PART)     *         
*                                                                     *         
*                                                                     *         
* DATA BUFFER RESIDES IN FALCMV AND CAN BE OBTAINED WITH              *         
* VTBUFFAD COMMAND CODE.                                              *         
*                                                                     *         
* REGISTER USAGE:                                                     *         
*      R1:  WORK                                                      *         
*      R2:  POINTER TO FLDHDRD, SRVLSTD IN LIST, SRVLINED IN DISP     *         
*           DON'T FUCK WITH R2 IN LIST WITHOUT CAREFUL CONSIDERATION  *         
*           DO A FIND ON '*** HARD ***'                               *         
*      R3:  POINTER TO SRVTTFFD                                       *         
*      R4:  WORK, POINTER TO VTTRCD IN LIST & DISP                    *         
*           POINTER TO UTLD IN CID                                    *         
*      R5:  2ND BASE                                                  *         
*      R6:  WORK, POINTER TO VTTSVD IN LIST & DISP                    *         
*      R7:  WORK, POINTER TO IFGRPL IN LIST & DISP                    *         
*      R8:  POINTER TO TIA                                            *         
*      R9:  POINTER TO SRPARMD                                        *         
*      RA:  POINTER TO SYSFACD                                        *         
*      RB:  BASE                                                      *         
*      RC:  POINTER TO WORKING STORAGE                                *         
***********************************************************************         
         TITLE '$VTT - VTAM TRACE/DISPLAY ROUTINES'                             
         PRINT NOGEN                                                            
VTT      CSECT                                                                  
         NMOD1 WRKX-WRKD,*$VTT**,R5,CLEAR=YES,RR=R4                             
         USING WRKD,RC                                                          
         ST    R4,RELO                                                          
         ST    RD,BASERD                                                        
*                                                                               
         LR    R9,R1                                                            
         USING SRPARMD,R9          R9=A(S/R PARAM LIST)                         
*                                                                               
         L     RA,SRPARM1                                                       
         USING SYSFACD,RA          RA=A(SYS FAC LIST)                           
*                                                                               
         L     R8,SRPARM2                                                       
         USING SRSD,R8             R8=A(TIA - USED FOR SR SAVE PAGE)            
*                                                                               
         L     R3,SRPARM6                                                       
         USING T15BFFD,R3          R3=A(TWA)                                    
*                                                                               
         L     R4,SRPARM4                                                       
         ST    R4,VCOMFACS                                                      
*                                                                               
         NI    SRVIDH+6,X'BF'                                                   
         XC    SRVMSG,SRVMSG                                                    
*                                                                               
         L     R4,SRPARM3                                                       
         USING UTLD,R4                                                          
         MVC   TRMNUM,TNUM                                                      
         DROP  R4                                                               
*                                                                               
         L     R4,VSSB                                                          
         USING SSBD,R4                                                          
         MVC   RECLEN,SSBTWAL                                                   
         CLI   SSBVTID,C' '                                                     
         BNH   ERR8                                                             
         DROP  R4                                                               
*                                                                               
         L     RE,VADRBUFF         ADRFILE DATA IN ADRBUFF HEADER               
         SH    RE,=H'16'                                                        
         MVC   RECSBLK,0(RE)                                                    
         MVC   RECSIZE,2(RE)                                                    
         LH    RF,RECSBLK                                                       
         MH    RF,RECSIZE                                                       
         STH   RF,BLKSIZE          VTAM TRACE BUFF SAME SIZE AS ADRBUFF         
         SH    RF,=H'10'           ADJUST FOR OVERHEAD                          
         SR    RE,RE                                                            
         LA    R0,VTTRCLEN         GET LEN OF VTAM TRACE RECORDS                
         DR    RE,R0                                                            
         STC   RF,TRCRBLK          NUMBER OF TRACE RECORDS PER BLOCK            
         LA    RF,1(RF)                                                         
         STC   RF,TRCRBLK1                                                      
*                                                                               
         LA    R2,SRVS1H           S1 = SELECT FIELD 1                          
*                                                                               
VALP2    CLI   8(R2),C'S'          WAS FIELD SELECTED?                          
         BE    DISP                YES? DISPLAY                                 
         LH    R1,LIN              INCREMENT LINE                               
         LA    R1,1(R1)                                                         
         STH   R1,LIN                                                           
*                                                                               
         MVI   8(R2),C' '          BLANK OUT SELECT FIELD                       
         ZIC   R0,0(R2)            POINT AT LIST FIELD                          
         AR    R2,R0                                                            
         IC    R0,0(R2)            POINT AT NEXT SELECT FIELD                   
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN?                               
         BNE   VALP2               NO, CHECK FOR DATA                           
         EJECT                                                                  
         LA    R2,SRVP1H           P1 = COMMAND ID                              
         USING FLDHDRD,R2                                                       
*                                                                               
         CLI   8(R2),C'+'          TURN LOG ON                                  
         BE    SWLOG                                                            
         CLI   8(R2),C'-'          TURN LOG OFF                                 
         BE    SWLOG                                                            
         CLI   8(R2),C'L'          LIST MULTIPLE TRANSACTIONS                   
         BE    LIST                                                             
         CLI   8(R2),C'C'          TEST CID'S                                   
         BE    CID                                                              
         CLI   8(R2),C'X'          CLEAR TWA11                                  
         BE    CLRTWAB                                                          
         CLI   FLDILEN,0           NO COMMAND?  USE P1 = X                      
         BE    CLRTWAB                                                          
         B     ERR1                                                             
         EJECT                                                                  
***********************************************************************         
* CLEAR TWAB (ONLY CLEARS FIRST 4 BYTES)                              *         
***********************************************************************         
         SPACE                                                                  
CLRTWAB  DS    0H                                                               
         XC    SR$VTT(4),SR$VTT                                                 
         LA    R2,SRPAGENO         WRITE BACK S/R SAVE DATA                     
         SLL   R2,32-8                                                          
         ICM   R2,3,TRMNUM                                                      
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',=C'DMWRT'),=C'TEMPSTR',(R2),(R8)                
         XC    SRVP1,SRVP1                                                      
         MVI   SRVP1,C'L'                                                       
         OI    SRVP1H+6,X'80'                                                   
         B     LIST                                                             
         EJECT                                                                  
***********************************************************************         
* TEST TCID = VTAM CID                                                *         
***********************************************************************         
         SPACE                                                                  
CID      DS    0H                                                               
*                                                                               
         MVI   FLTLEN,0            0 LEN FILT INDICATES NO FILTER               
         LA    R2,SRVP2H                                                        
         USING FLDHDRD,R2                                                       
         CLI   FLDILEN,0           WAS THERE INPUT?                             
         BE    CID5                NO? CONTINUE                                 
*                                                                               
* FILTER LIST ON LUNAME OR PART                                                 
*                                                                               
         MVC   FLTLEN,FLDILEN      SAVE OFF INPUT SIZE                          
         MVC   FILTER(8),FLDDATA   SAVE OFF FILTER                              
*                                                                               
CID5     BRAS  RE,ON31                                                          
         L     R4,VUTL                                                          
         LH    R6,0(R4)                                                         
         L     R7,2(R4)                                                         
         LA    R4,6(R4)                                                         
         USING UTLD,R4                                                          
         LA    R2,SRVL1H                                                        
*                                                                               
CID10    OC    TCID,TCID           NO PROBLEM IF TCID NULL                      
         BZ    CID20                                                            
         GOTO1 VLCM,DMCB,VTGETCID,(R4)                                          
         MVC   VTCID,DMCB+8        SAVE OFF VTAM TCID                           
         MVC   DUB(2),DMCB+12      SAVE OFF ERROR CODE                          
         CLC   DUB(2),=H'7'        NO LONGER CONNECTED?                         
         BE    CID20                                                            
         CLC   VTCID,TCID          NO PROBLEM IF EQUAL                          
         BE    CID20                                                            
         CLI   FLTLEN,0            ARE WE FILTERING ON LUID (OR PART)?          
         BE    CID15               NO FILTER                                    
         ZIC   R1,FLTLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+12                                                          
         BNE   CID20               ONE WE DON'T WANT                            
         B     *+10                ONE WE DO                                    
         CLC   FILTER(0),TSYM                                                   
CID15    LR    R3,R2               OUTPUT DATA TO SCRN                          
         LA    R3,8(R3)                                                         
         MVC   0(8,R3),TSYM        DISPLAY LUID                                 
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         LA    R3,9(R3)                                                         
         GOTO1 (RF),DMCB,TCID,(R3),4,=C'TOG'         TCID                       
         LA    R3,9(R3)                                                         
         GOTO1 (RF),DMCB,VTCID,(R3),4,=C'TOG'        VTAM CID                   
         LA    R3,9(R3)                                                         
         GOTO1 (RF),DMCB,DUB,(R3),2,=C'TOG'          ERROR CODE                 
         DROP  R4                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               PT AT NEXT SEL FIELD                         
         IC    R0,0(R2)                                                         
         AR    R2,R0               PT AT NEXT LST FIELD                         
         CLI   0(R2),0                                                          
         BE    CIDX                STOP WHEN NO MORE ROOM ON SCRN               
CID20    BXLE  R4,R6,CID10                                                      
CIDX     BRAS  RE,OFF31                                                         
         LA    R2,SRVP1H                                                        
         B     SETEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* TURN TRACE ON OR OFF - 'ALL', 'ME', OR AN LUNAME                    *         
***********************************************************************         
         SPACE 1                                                                
SWLOG    XC    TRMUTL,TRMUTL       CLEAR UTL ENTRY ADDRESS                      
         LA    R2,SRVP2H                                                        
         CLI   FLDILEN,3                                                        
         BNE   SL2                                                              
         CLC   FLDDATA(3),=C'ALL'                                               
         BE    SLX                                                              
         B     ERR2                                                             
*                                                                               
SL2      CLI   FLDILEN,2                                                        
         BNE   SL4                                                              
         CLC   FLDDATA(2),=C'ME'                                                
         BNE   SL4                                                              
         MVC   TRMUTL,SRQAUTL                                                   
         B     SLX                                                              
*                                                                               
SL4      L     RF,VCOMFACS                                                      
         L     RF,CTERMVAL-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2)       VALIDATE TERM NUM OR TERMINAL ID            
         ICM   R4,15,4(R1)                                                      
         BZ    ERR2                                                             
         ST    R4,TRMUTL                                                        
*                                                                               
SLX      MVI   ANDMASK,0                                                        
         MVI   ORMASK,TST4TRC                                                   
         CLI   SRVP1,C'+'                                                       
         BE    *+12                                                             
         MVI   ANDMASK,X'FF'-TST4TRC                                            
         MVI   ORMASK,0                                                         
*                                                                               
         BRAS  RE,ON31                                                          
         ICM   R4,15,TRMUTL        GET UTL ENTRY ADDRESS                        
         USING UTLD,R4                                                          
         BZ    SLX10               = 0 MEANS ALL TERMINALS                      
         NC    TSTAT4,ANDMASK                                                   
         OC    TSTAT4,ORMASK                                                    
         MVC   SRVMSG(11),=C'AYE, KEPTIN'                                       
         LA    R2,SRVIDH           POSITION CURSOR                              
         BRAS  RE,OFF31                                                         
         B     SETEXIT                                                          
         DROP  R4                                                               
*                                                                               
SLX10    BRAS  RE,ON31                                                          
         L     R4,VUTL                                                          
         LH    R6,0(R4)                                                         
         L     R7,2(R4)                                                         
         LA    R4,6(R4)                                                         
         USING UTLD,R4                                                          
*                                                                               
SLX12    NC    TSTAT4,ANDMASK                                                   
         OC    TSTAT4,ORMASK                                                    
         BXLE  R4,R6,SLX12                                                      
         BRAS  RE,OFF31                                                         
         MVC   SRVMSG(7),=C'WORKING'                                            
         LA    R2,SRVIDH           POSITION CURSOR                              
         B     SETEXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LIST TRANSACTIONS (FILTERED OR NOT)                                 *         
***********************************************************************         
         SPACE                                                                  
LIST     DS    0H                                                               
         MVI   SFLAG,0                                                          
         MVI   RFLAG,0                                                          
*                                                                               
         LA    R6,SRVTTSV+4                                                     
         USING VTTSVD,R6                                                        
*                                                                               
         BAS   RE,RDTWAB           READ TWA 11                                  
*                                                                               
         LA    R2,SRVP3H                                                        
         CLI   5(R2),0             TEST D/A INPUT                               
         BE    L20                                                              
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CHEXIN-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,8(R2),BUFFDA,8                                         
*                                                                               
L20      BAS   RE,GETBUFF                                                       
         BNE   ERR6                                                             
*                                                                               
         MVI   FLTLEN,0            0 LEN FILT INDICATES NO FILTER               
         LA    R2,SRVP2H                                                        
         USING FLDHDRD,R2                                                       
         CLI   FLDILEN,0           WAS THERE INPUT?                             
         BE    L100                NO? CONTINUE                                 
*                                                                               
* FILTER LIST ON LUNAME OR PART                                                 
*                                                                               
         MVC   FLTLEN,FLDILEN      SAVE OFF INPUT SIZE                          
         MVC   FILTER(8),FLDDATA   SAVE OFF FILTER                              
*                                                                               
L100     CLC   SRVTTSV(4),=C'$VTT' HAVE WE ALREADY LISTED A SCREEN?             
         BE    L110                YES, CONTINUE                                
*                                                                               
         CLC   SRVTTSV(4),=C'$VTD' DID WE JUST DO A DISP?                       
         BNE   VTT110              NO? START AT END                             
         LA    R6,SRVTTSV+4        PT AT SAVE AREA                              
         BAS   RE,VALDA                                                         
         BAS   RE,GETBUFF                                                       
         BNE   ERR6                                                             
         B     VTT109                                                           
*                                                                               
L110     DS    0H                                                               
         LA    R6,SVEND            LAST DISPLAY LINE IN TWAB                    
         LA    R1,VTTSVEND-VTTSVDA LENGTH OF ENTRY - 34                         
         SR    R6,R1               LAST DA IN TWAB                              
         BAS   RE,VALDA                                                         
*                                                                               
         BAS   RE,GETBUFF                                                       
         BNE   ERR6                                                             
*                                                                               
* NEED TO BACK UP TO PREVIOUS RECORD *                                          
*                                                                               
VTT103   L     R4,BUFFPTR          GET BUFFER POINTER                           
         SH    R4,=AL2(VTTRCLEN)                                                
         ST    R4,BUFFPTR                                                       
         ZIC   R0,BUFFDA+3                                                      
         BCTR  R0,0                                                             
         STC   R0,BUFFDA+3                                                      
         LTR   R0,R0                                                            
         BNZ   VTT109                                                           
*                                                                               
VTT104   LA    R4,BUFFER                                                        
         OC    5(4,R4),5(R4)       TEST FOR BACKWARD POINTER                    
         BZ    VTT106              NO                                           
         MVC   BUFFDA,5(R4)        MOVE BACKWARD POINTER (TTTTBB)               
         MVC   BUFFDA+3(1),TRCRBLK SET FOR LAST REC IN BLOCK                    
         B     VTT108                                                           
*                                                                               
VTT106   DS    0H                  NO POINTER, TRY PREVIOUS REC                 
         OC    BUFFDA(3),BUFFDA    TEST HAVE DISK ADDRESS                       
         BZ    VTT107                                                           
         BAS   RE,BACKUP                                                        
         MVC   BUFFDA+3(1),TRCRBLK SET FOR ACTUAL LAST IN BLOCK                 
         B     VTT108                                                           
*                                                                               
VTT107   L     RE,VADRFILE                                                      
         MVC   BUFFDA(3),DNEXT-DTFPHD(RE)                                       
         MVI   BUFFDA+3,0                                                       
         OC    BUFFDA,BUFFDA                                                    
         BZ    ERR6                                                             
*                                                                               
VTT108   BAS   RE,GETBUFF          READ THIS BUFFER                             
         BNE   ERR6                                                             
*                                                                               
* LIST ALL FROM END BACKWARD                                                    
*                                                                               
VTT109   LA    R6,SRVTTSV+4        START ADDR OF SAVE AREA                      
*                                                                               
         DROP  R2                                                               
VTT110   DS    0H                                                               
         MVI   SRVS1H+8,C' '       POINT TO FIRST SELECT FIELD                  
         LA    R2,SRVL1H           PT @DA FIELD                                 
         USING SRVLSTD,R2                                                       
*                                                                               
VTT150   DS    0H                  R2 PTS@ DA FLDHDR                            
         L     R4,BUFFPTR                                                       
         USING VTTRCD,R4                                                        
*                                                                               
* IF '*** BAD CID' PUT '*' IN SEL FIELD...                                      
*                                                                               
         L     R4,BUFFPTR                                                       
         USING VTTRCD,R4                                                        
         CLC   VTTRCDTA(11),=C'*** BAD CID'                                     
         BNE   VTT151                                                           
         BCTR  R2,0                *** HARD ***                                 
         MVI   0(R2),C'*'          *** HARD ***                                 
         LA    R2,1(R2)            *** HARD ***                                 
*                                                                               
* FILL IN DISK ADDR                                                             
*                                                                               
VTT151   L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,BUFFDA,LSTDA,4,=C'TOG'                                 
*                                                                               
         OC    BUFFDA(3),BUFFDA    TEST DISPLAY OF CORE/DISK                    
         BNZ   VTT152              DISK                                         
         MVC   LSTDA+8(2),LSTDA+6  MOVE RECORD NUM TO RIGHT                     
         MVC   LSTDA(8),=C'COREBUFF'                                            
*                                                                               
* FILL IN LUNAME                                                                
*                                                                               
VTT152   MVC   VTTSVDA(10),LSTDA   SAVE CORE/DISK ADDR                          
         MVC   LSTLU,VTTRCLU       LUNAME                                       
         MVC   VTTSVLU(8),LSTLU    SAVE LUNAME                                  
         CLI   FLTLEN,0            IS FILTER ACTIVE?                            
         BE    VTT154              NO? LIST ALL                                 
*                                                                               
         CLI   FLTLEN,1            IS FILTER 1 CHAR?                            
         BNE   VTT152B             NO, NORMAL FILTER                            
*                                                                               
         CLI   FILTER,C'P'         LIST PRINTERS?                               
         BNE   VTT152A             NO, CHECK SHUTTLE FILTER                     
         CLI   LSTLU+7,C'P'        IS THIS A PRINTER?                           
         BE    VTT154              YES? CONTINUE                                
         B     NOMATCH                                                          
*                                                                               
VTT152A  DS    0H                                                               
         CLI   FILTER,C'S'         LIST SHUTTLES?                               
         BNE   VTT152B             NO, NORMAL FILTER                            
         CLI   LSTLU+7,C'S'        IS THIS A SHUTTLE?                           
         BE    VTT154              YES? CONTINUE                                
         B     NOMATCH                                                          
*                                                                               
VTT152B  DS    0H                                                               
         CLC   FILTER(2),=C'R='    FILTER ON RCFD?                              
         BNE   *+12                                                             
         MVI   RFLAG,1                                                          
         B     VTT154                                                           
*                                                                               
         CLC   FILTER(2),=C'S='    FILTER ON SENSE?                             
         BNE   *+12                                                             
         MVI   SFLAG,1                                                          
         B     VTT154                                                           
*                                                                               
         ZIC   R1,FLTLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,VTT152X                                                       
         BE    VTT154                                                           
         B     NOMATCH                                                          
*                                                                               
VTT152X  CLC   LSTLU(0),FILTER     EXECUTED COMPARE                             
*                                                                               
* FILL IN DATE                                                                  
*                                                                               
VTT154   DS    0H                                                               
         L     RF,VCOMFACS         DATE AND TIME                                
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(6,VTTRCDAT),(8,LSTDATE)                               
         MVC   VTTSVDT(8),LSTDATE  SAVE DATE                                    
*                                                                               
* FILL IN TIME                                                                  
*                                                                               
         ICM   R0,15,VTTRCTIM      PICK UP HHMMSSTF                             
         SRL   R0,4                DROP HUNDREDTHS                              
         XC    DUB,DUB                                                          
         ST    R0,DUB+4                                                         
         OI    DUB+7,X'0F'         MAKE TENTHS INTO SIGN BITS                   
         MVC   WORK(10),=X'402120204B20204B2020'                                
         ED    WORK(10),DUB+4                                                   
         MVC   LSTTIME,WORK+2                                                   
         MVC   VTTSVTM(8),LSTTIME  SAVE TIME                                    
*                                                                               
         LA    R7,VTTRCRPL         POINT TO RPL                                 
         USING IFGRPL,R7                                                        
*                                                                               
* FILL IN CMD (I.E.: SEND, RECEIVE)                                             
*                                                                               
         ZIC   RF,RPLREQ           GET OP CODE                                  
         N     RF,=X'0000007F'     DROP INTERNAL TRACE FLAG                     
         SLL   RF,3                X 8                                          
         L     RE,=A(VTAMCMDS)                                                  
         A     RE,RELO                                                          
         AR    RE,RF                                                            
         MVC   LSTCMD,0(RE)                                                     
         TM    RPLREQ,X'80'        TEST INTERNAL TRACE RECORD                   
         BZ    *+16                                                             
         MVC   LSTCMD(2),=C'DD'                                                 
         MVC   LSTCMD+2(6),0(RE)                                                
*                                                                               
         CLI   RPLREQ,0            TEST DDS INFO MESSAGE                        
         BNE   *+10                                                             
         MVC   LSTCMD,VTTRCDTA     MOVE START OF MSG TO LINE                    
*                                                                               
         CLC   =C'BAD CHECK',VTTRCDTA   TEST SPECIAL ERROR                      
         BNE   *+10                                                             
         MVC   LSTCMD,VTTRCDTA     MOVE START OF MSG TO LINE                    
*                                                                               
* FILL IN RCFD                                                                  
*                                                                               
         MVC   LSTRCFD(5),=C'RCFD='                                             
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,RPLRTNCD,LSTRCFD+5,2,=C'TOG'                           
*                                                                               
         CLI   RFLAG,1                                                          
         BNE   VTT155                                                           
         ZIC   R1,FLTLEN                                                        
         SH    R1,=H'3'                                                         
         EX    R1,VTT154X                                                       
         BE    VTT155                                                           
         B     NOMATCH                                                          
*                                                                               
VTT154X  CLC   LSTRCFD+5(0),FILTER+2   EXECUTED COMPARE                         
*                                                                               
* FILL IN SENSE                                                                 
*                                                                               
VTT155   DS    0H                                                               
         MVC   LSTSENSE(6),=C'SENSE='                                           
         GOTO1 (RF),DMCB,RPLSSEI,LSTSENSE+6,4                                   
*                                                                               
         CLI   SFLAG,1                                                          
         BNE   VTT155A                                                          
         ZIC   R1,FLTLEN                                                        
         SH    R1,=H'3'                                                         
         EX    R1,VTT155X                                                       
         BE    VTT155A                                                          
         B     NOMATCH                                                          
*                                                                               
VTT155X  CLC   LSTSENSE+6(0),FILTER+2  EXECUTED COMPARE                         
*                                                                               
VTT155A  DS    0H                                                               
         LA    R6,VTTSVEND-VTTSVDA(R6)                                          
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               PT @NEXT SEL FIELD                           
         CLI   0(R2),0             TEST MORE DISPLAY ROOM                       
         BE    VTT160              NO - DONE                                    
         MVI   8(R2),C' '          BLANK OUT SELECT FIELD                       
         ZIC   R0,0(R2)            PT @NEXT DA FIELD                            
         AR    R2,R0                                                            
*                                                                               
* NEED TO BACK UP TO PREVIOUS RECORD *                                          
*                                                                               
NOMATCH  DS    0H                                                               
         BCTR  R2,0                *** HARD ***                                 
         MVI   0(R2),C' '          *** HARD ***                                 
         LA    R2,1(R2)            *** HARD ***                                 
*                                                                               
         L     R4,BUFFPTR          GET BUFFER POINTER                           
         SH    R4,=AL2(VTTRCLEN)                                                
         ST    R4,BUFFPTR                                                       
*                                                                               
         ZIC   R0,BUFFDA+3                                                      
         BCTR  R0,0                                                             
         STC   R0,BUFFDA+3                                                      
         LTR   R0,R0                                                            
         BNZ   VTT150                                                           
*                                                                               
         DROP  R4                                                               
         LA    R4,BUFFER                                                        
         OC    5(4,R4),5(R4)       TEST FOR BACKWARD POINTER                    
         BZ    VTT156              NO                                           
         MVC   BUFFDA,5(R4)        MOVE BACKWARD POINTER (TTTTBB)               
         MVC   BUFFDA+3(1),TRCRBLK SET FOR LAST REC IN BLOCK                    
         B     VTT158                                                           
*                                                                               
VTT156   DS    0H                  NO POINTER, TRY PREVIOUS REC                 
         OC    BUFFDA(3),BUFFDA    TEST HAVE DISK ADDRESS                       
         BZ    VTT157                                                           
         BAS   RE,BACKUP                                                        
         MVC   BUFFDA+3(1),TRCRBLK SET FOR ACTUAL LAST IN BLOCK                 
         B     VTT158                                                           
*                                                                               
VTT157   L     RE,VADRFILE                                                      
         MVC   BUFFDA(3),DNEXT-DTFPHD(RE)                                       
         MVI   BUFFDA+3,0                                                       
         OC    BUFFDA,BUFFDA                                                    
         BZ    ERR6                                                             
*                                                                               
VTT158   BAS   RE,GETBUFF          READ THIS BUFFER                             
         BNE   ERR6                                                             
         B     VTT150                                                           
*                                                                               
* NO MORE DISPLAY ROOM                                                          
*                                                                               
VTT160   DS    0H                                                               
*                                                                               
VTT164   BAS   RE,WRTTWAB                                                       
         MVC   SRVMSG(27),=C'HERE IT IS - ENTER FOR MORE'                       
         MVI   SRVP1H+8,C'L'                                                    
         LA    R2,SRVS1H                                                        
         B     SETEXIT                                                          
*                                                                               
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISK ADDRESS OF BUFFER                                     *         
***********************************************************************         
         SPACE 1                                                                
VALDA    NTR1                                                                   
         XC    BUFFDA,BUFFDA       CLEAR D/A                                    
*                                                                               
         CLC   =C'COREBUFF',0(R6)                                               
         BE    VALDA10                                                          
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CHEXIN-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,(R6),BUFFDA,8                                          
         B     VALDA12                                                          
         SPACE 1                                                                
*                                                                               
* THIS CODE VALIDATES COREBUFFNN INPUT WHERE NN IS REC NUM                      
*                                                                               
         SPACE 1                                                                
VALDA10  DS    0H                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CHEXIN-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,8(R6),BUFFDA+3,2                                       
*                                                                               
VALDA12  OC    12(4,R1),12(R1)                                                  
         BZ    ERR4                                                             
         CLC   BUFFDA+3(1),TRCRBLK1 BEYOND MAX RECORD NUM +1                    
         BH    ERR3                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GETBUFF - READ A TRACE BUFFER FROM ADRFILE                          *         
***********************************************************************         
         SPACE 1                                                                
GETBUFF  NTR1                                                                   
         OC    BUFFDA(3),BUFFDA    TEST REC ON DISK OR IN CORE                  
         BNZ   GETBUFF2            DISK                                         
         SPACE 1                                                                
***********************************************************************         
*  GET ADDRESS OF CORERES LCM BUFFER AND MOVE TO MY BUFFER            *         
***********************************************************************         
         SPACE 1                                                                
         GOTO1 VLCM,DMCB,VTBUFFAD                                               
         L     R0,4(R1)            FROM ADDRESS AND LENGTH                      
         LH    R1,BLKSIZE                                                       
         LA    RE,BUFFER           TO ADDRESS AND LENGTH                        
         LR    RF,R1                                                            
         MVCL  (RE),(R0)                                                        
         B     GETBUFF4                                                         
*                                                                               
GETBUFF2 DS    0H                                                               
         L     R7,VADRFILE         READ ADRFILE RECORD                          
         MVC   P1,VRDID            INITIALISE DADDS PLIST FOR READS             
         LA    RE,BUFFER                                                        
         ST    RE,P2                                                            
         XC    P3,P3                                                            
         MVC   P4,VADRFILE                                                      
         LA    RE,P6                                                            
         ST    RE,P5                                                            
         MVC   P6(3),BUFFDA                                                     
         MVI   P6+3,0                                                           
*                                                                               
         ZIC   R1,IOCNT                                                         
         LA    R1,1(R1)                                                         
         STC   R1,IOCNT                                                         
         CLI   IOCNT,200                                                        
         BH    ERR7                                                             
         GOTO1 VDADDS,P1                                                        
         OC    P3(2),P3                                                         
         BNZ   ERR5                                                             
*                                                                               
         CLC   =C'*VTAM',BUFFER    IS IT A VTAM TRACE RECORD                    
         BE    GETBUFF4            YES - CONTINUE                               
*                                                                               
         BAS   RE,BACKUP                                                        
         B     GETBUFF2                                                         
         EJECT                                                                  
GETBUFF4 DS    0H                                                               
         CLI   BUFFDA+3,0                                                       
         BNE   *+10                                                             
         MVC   BUFFDA+3(1),TRCRBLK                                              
         LA    R4,BUFFER+10        POINT TO FIRST TRACE RECORD                  
         ZIC   R7,BUFFDA+3         GET RECORD NUMBER IN BUFFER                  
         BCTR  R7,0                                                             
         MH    R7,=AL2(VTTRCLEN)                                                
         AR    R4,R7                                                            
         ST    R4,BUFFPTR          SET BUFFER POINTER                           
*                                                                               
         OC    BUFFDA(3),BUFFDA    TEST CORERES REQ                             
         BNZ   EQXIT               NO - BUFFER MUST BE COMPLETE                 
         CLI   0(R4),0             TEST LUNAME PRESENT                          
         BNE   EQXIT               YES - DONE                                   
*                                                                               
         ZIC   RE,BUFFDA+3         BACK UP TO PREVIOUS                          
         BCTR  RE,0                                                             
         STC   RE,BUFFDA+3                                                      
         LTR   RE,RE                                                            
         BNZ   GETBUFF4                                                         
         SPACE 1                                                                
***********************************************************************         
* NO DATA IN CORERES BUFFER, BACK UP TO PREVIOUS RECORD               *         
***********************************************************************         
         SPACE 1                                                                
         OC    BUFFER+5(4),BUFFER+5  TEST CHAINED TO PREVIOUS                   
         BZ    GETBUFF6            NO                                           
         MVC   BUFFDA(4),BUFFER+5  MOVE ADDRESS OF PREVIOUS                     
         B     GETBUFF2            AND CONTINUE                                 
*                                                                               
GETBUFF6 L     RE,VADRFILE         GET DTF ADDRESS                              
         MVC   BUFFDA(3),DNEXT-DTFPHD(RE)                                       
         MVI   BUFFDA+3,0                                                       
         OC    BUFFDA,BUFFDA                                                    
         BZ    ERR6                                                             
         B     GETBUFF2            AND SEARCH DISK FILE                         
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE BACKS UP TO PREVIOUS RECORD OR PREVIOUS TRACK            *         
***********************************************************************         
         SPACE 1                                                                
BACKUP   NTR1                                                                   
         MVC   BUFFDA+3(1),TRCRBLK SET FOR LAST REC IN NEW BLOCK                
         SR    RE,RE                                                            
         ICM   RE,1,BUFFDA+2       GET BLOCK NUMBER                             
         BZ    BACKUP2                                                          
         BCTR  RE,0                DECREMENT                                    
         STC   RE,BUFFDA+2         SET NEW VALUE                                
         LTR   RE,RE               MAKE SURE NOT REC 0                          
         BNZ   EXIT                READ AGAIN                                   
*                                                                               
BACKUP2  SR    RE,RE                                                            
         ICM   RE,3,BUFFDA         GET TRACK NUMBER                             
         BZ    ERR6                                                             
         BCTR  RE,0                DECREMENT                                    
         STCM  RE,3,BUFFDA                                                      
         LTR   RE,RE               TEST REACHED BOF                             
         BZ    ERR6                YES - NO MORE                                
*                                                                               
         LH    R0,BLKSIZE          FIND ADRFILE RECS/TRACK                      
         GOTO1 VDADDS,P1,VDARPT,,(R0),VADRFILE                                  
         MVC   BUFFDA+2(1),P3+3    SET HIGH REC PREVIOUS TRACK                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
         SPACE                                                                  
DISP     DS    0H                                                               
         BAS   RE,SVDISP                                                        
         MVI   8(R2),C' '          CLEAR SELECT FIELD                           
         BAS   RE,RDTWAB                                                        
*                                                                               
         LA    R6,SRVTTSV                                                       
         LA    R6,4(R6)            SKIP OVER $VTT                               
         USING VTTSVD,R6                                                        
*                                                                               
         LA    R1,VTTSVEND-VTTSVDA    LENGTH OF REC                             
         MH    R1,LIN              HOW FAR INTO REC                             
         AR    R6,R1               PT R6@REC WANTED                             
         BAS   RE,VALDA                                                         
*                                                                               
D100     BAS   RE,GETBUFF                                                       
         BNE   ERR6                                                             
*                                                                               
         L     R4,BUFFPTR                                                       
         USING VTTRCD,R4                                                        
*                                                                               
         LA    R2,SRVL1H           POINT TO FIRST DISPLAY LINE                  
         USING SRVLINED,R2                                                      
*                                                                               
         MVC   8(8,R2),VTTRCLU     LUNAME ON LINE 1                             
         CLC   VTTSVLU,VTTRCLU     IS THIS THE LUNAME WANTED?                   
         BE    D110                                                             
         BAS   RE,BACKBUFF         NO? GO BACK 1 RECORD                         
         B     D100                                                             
D110     ZIC   R0,0(R2)            PT @ NXT SEL FLD                             
         AR    R2,R0                                                            
         IC    R0,0(R2)            PT@ NXTDAT FLD                               
         AR    R2,R0                                                            
*                                                                               
         L     RF,VCOMFACS         DATE ON LINE 2                               
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(6,VTTRCDAT),(8,SRVLDATE)                              
         CLC   VTTSVDT,SRVLDATE    IS THIS RIGHT REC?                           
         BE    D120                                                             
         BAS   RE,BACKBUFF         NO, BACK UP 1 REC                            
         B     D100                                                             
*                                                                               
D120     ZIC   R0,0(R2)            TIME ON LINE 3                               
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         ICM   R0,15,VTTRCTIM      PICK UP HHMMSSTF                             
         SRL   R0,4                DROP HUNDREDTHS                              
         XC    DUB,DUB                                                          
         ST    R0,DUB+4                                                         
         OI    DUB+7,X'0F'         MAKE TENTHS INTO SIGN BITS                   
         MVC   WORK(10),=X'402120204B20204B2020'                                
         ED    WORK(10),DUB+4                                                   
         MVC   SRVLTIME,WORK+2                                                  
         CLC   VTTSVTM,SRVLTIME                                                 
         BE    D130                                                             
         BAS   RE,BACKBUFF                                                      
         B     D100                                                             
*                                                                               
D130     ZIC   R0,0(R2)            SKIP 2 LINES                                 
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    R7,VTTRCRPL         POINT TO RPL                                 
         USING IFGRPL,R7                                                        
*                                                                               
         ZIC   RF,RPLREQ           GET OP CODE                                  
         N     RF,=X'0000007F'     DROP INTERNAL TRACE FLAG                     
         SLL   RF,3                X 8                                          
         L     RE,=A(VTAMCMDS)                                                  
         A     RE,RELO                                                          
         AR    RE,RF                                                            
         MVC   SRVLCMD,0(RE)                                                    
         TM    RPLREQ,X'80'        TEST INTERNAL TRACE                          
         BZ    *+16                                                             
         MVC   SRVLCMD(2),=C'DD'                                                
         MVC   SRVLCMD+2(6),0(RE)                                               
*                                                                               
         ZIC   R0,0(R2)            NEXT LINE                                    
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(7,R2),=C'RCFDBK='                                              
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,RPLRTNCD,SRVLRTCD,1,=C'TOG'                            
         GOTO1 (RF),(R1),RPLFDB2,SRVLFDB2                                       
*                                                                               
         ZIC   R0,0(R2)            NEXT LINE                                    
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(7,R2),=C'SENSE ='                                              
         GOTO1 (RF),(R1),RPLSSEI,SRVLSNS,4                                      
         DROP  R7                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE RPL                                                     *         
***********************************************************************         
         SPACE 1                                                                
         LA    R2,SRVL1H                                                        
         ST    R5,HOLD                                                          
         LA    R5,7                SET LINE COUNTER                             
         SR    R6,R6               CLEAR DSPL REG                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'VTTRCRPL),VTTRCRPL   MOVE TO 'WORK'                       
         LA    R7,WORK                                                          
*                                                                               
VTT112   DS    0H                                                               
         GOTO1 FORMAT,DMCB,(R7),SRVLHEX,(R6)                                    
         LA    R7,16(R7)           ADVANCE RPL POINTER                          
         LA    R6,16(R6)           ADVANCE DSPL REG                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT LINE                           
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT LINE                           
         BCT   R5,VTT112                                                        
*                                                                               
         L     R5,HOLD                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               SKIP A LINE                                  
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(10,R2),=C'** DATA **'                                          
*                                                                               
         LA    R7,VTTRCDTA         POINT TO INPUT/OUTPUT DATA                   
         ST    R5,HOLD                                                          
         LA    R5,4                SET LINE COUNTER                             
         SR    R6,R6               CLEAR DSPL REG                               
*                                                                               
VTT114   DS    0H                                                               
         GOTO1 FORMAT,DMCB,(R7),SRVLHEX,(R6)                                    
         MVC   SRVLEBC,0(R7)       MOVE EBCDIC OUTPUT                           
         LA    R7,16(R7)                                                        
         LA    R6,16(R6)                                                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R5,VTT114                                                        
         L     R5,HOLD                                                          
*                                                                               
VTT116   DS    0H                                                               
         MVC   SRVMSG(15),=C'OK - HERE IT IS'                                   
         LA    R2,SRVP3H           POSITION CURSOR                              
         B     SETEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* BACKBUFF: COREBUFF NOT SAME, FIND WANTED RECORD                     *         
***********************************************************************         
         SPACE                                                                  
BACKBUFF NTR1                                                                   
         L     R4,BUFFPTR          GET BUFFER POINTER                           
         SH    R4,=AL2(VTTRCLEN)                                                
         ST    R4,BUFFPTR                                                       
         ZIC   R0,BUFFDA+3                                                      
         BCTR  R0,0                                                             
         STC   R0,BUFFDA+3                                                      
         LTR   R0,R0                                                            
         BNZ   BB108                                                            
*                                                                               
         LA    R4,BUFFER                                                        
         OC    5(4,R4),5(R4)       TEST FOR BACKWARD POINTER                    
         BZ    BB106               NO                                           
         MVC   BUFFDA,5(R4)        MOVE BACKWARD POINTER (TTTTBB)               
         MVC   BUFFDA+3(1),TRCRBLK SET FOR LAST REC IN BLOCK                    
         B     BB108                                                            
*                                                                               
BB106    DS    0H                  NO POINTER, TRY PREVIOUS REC                 
         OC    BUFFDA(3),BUFFDA    TEST HAVE DISK ADDRESS                       
         BZ    BB107                                                            
         BAS   RE,BACKUP                                                        
         MVC   BUFFDA+3(1),TRCRBLK SET FOR ACTUAL LAST IN BLOCK                 
         B     BB108                                                            
*                                                                               
BB107    L     RE,VADRFILE                                                      
         MVC   BUFFDA(3),DNEXT-DTFPHD(RE)                                       
         MVI   BUFFDA+3,0                                                       
         OC    BUFFDA,BUFFDA                                                    
         BZ    ERR6                                                             
BB108    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FORMAT 16 BYTES OF DATA TO OUTPUT AREA                              *         
* R1 POINTS TO  INPUT ADDRESS                                         *         
*               OUTPUT ADDRESS                                        *         
*               DSPL VALUE                                            *         
*                                                                     *         
* OUTPUT FORMAT IS DD XXXXXXXX ... XXXXXXXX  CCCCCC...CC              *         
***********************************************************************         
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         LM    R4,R6,0(R1)                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
* FORMAT DSPL                                                                   
         STC   R6,HALF                                                          
         GOTO1 (RF),(R1),HALF,(R5),1,=C'TOG'                                    
         LA    R5,3(R5)            BUMP OUTPUT POINTER                          
*                                                                               
         LA    R0,4                SET COUNTER                                  
*                                                                               
FORMAT2  DS    0H                                                               
         GOTO1 (RF),(R1),(R4),(R5),4                                            
         LA    R4,4(R4)            BUMP INPUT POINTER                           
         LA    R5,9(R5)            BUMP OUTPUT POINTER                          
         BCT   R0,FORMAT2                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ TWAB *                                                         *         
***********************************************************************         
         SPACE                                                                  
RDTWAB   NTR1                                                                   
         LA    R2,SRPAGENO         READ IN S/R SAVE TWA                         
         SLL   R2,32-8                                                          
         ICM   R2,3,TRMNUM                                                      
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 (RF),DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(R2),(R8)               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,SR$VTT           'FROM' ADDRESS                               
         LA    R1,L'SRVTTSV        'FROM' LENGTH                                
         LA    RE,SRVTTSV          'TO' ADDRESS                                 
         LR    RF,R1               'TO' LENGTH = 'FROM' LENGTH                  
         MVCL  (RE),(R0)                                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE TWAB                                                          *         
***********************************************************************         
         SPACE                                                                  
WRTTWAB  NTR1                                                                   
         MVC   SRVTTSV(4),=C'$VTT'                                              
         LA    R0,SRVTTSV          'FROM' ADDRESS                               
         LA    R1,L'SRVTTSV        'FROM' LENGTH                                
         LA    RE,SR$VTT           'TO' ADDRESS                                 
         LR    RF,R1               'TO' LENGTH = 'FROM' LENGTH                  
         MVCL  (RE),(R0)                                                        
         LA    R2,SRPAGENO         WRITE BACK S/R SAVE DATA                     
         SLL   R2,32-8                                                          
         ICM   R2,3,TRMNUM                                                      
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',=C'DMWRT'),=C'TEMPSTR',(R2),(R8)                
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* SAVE DISP *                                                         *         
***********************************************************************         
         SPACE                                                                  
SVDISP   NTR1                                                                   
         LA    R2,SRPAGENO         READ IN S/R SAVE TWA                         
         SLL   R2,32-8                                                          
         ICM   R2,3,TRMNUM                                                      
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 (RF),DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(R2),(R8)               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,SR$VTT           'FROM' ADDRESS                               
         LA    R1,L'SRVTTSV        'FROM' LENGTH                                
         LA    RE,SRVTTSV          'TO' ADDRESS                                 
         LR    RF,R1               'TO' LENGTH = 'FROM' LENGTH                  
         MVCL  (RE),(R0)                                                        
*                                                                               
         MVC   SRVTTSV(4),=C'$VTD'                                              
         LA    R0,SRVTTSV          'FROM' ADDRESS                               
         LA    R1,L'SRVTTSV        'FROM' LENGTH                                
         LA    RE,SR$VTT           'TO' ADDRESS                                 
         LR    RF,R1               'TO' LENGTH = 'FROM' LENGTH                  
         MVCL  (RE),(R0)                                                        
         LA    R2,SRPAGENO         WRITE BACK S/R SAVE DATA                     
         SLL   R2,32-8                                                          
         ICM   R2,3,TRMNUM                                                      
         L     RF,VCOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',=C'DMWRT'),=C'TEMPSTR',(R2),(R8)                
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
SETEXIT  DS    0H                                                               
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R2),X'40'         POSN CURSOR ON REQUIRED FIELD                
         B     EXIT                                                             
         SPACE 2                                                                
ERR1     MVC   SRVMSG+12(11),=C'+/-/L/D/P/X'                                    
         B     ERRX                                                             
ERR2     MVC   SRVMSG+12(10),=C'BAD LUNAME'                                     
         B     ERRX                                                             
ERR3     MVC   SRVMSG+12(24),=C'PATHETIC RECORD NUMBER !'                       
         B     ERRX                                                             
ERR4     MVC   SRVMSG+12(16),=C'BAD DISK ADDRESS'                               
         B     ERRX                                                             
ERR5     MVC   SRVMSG+12(22),=C'CAN''T READ THIS RECORD'                        
         B     ERRX                                                             
ERR6     BAS   RE,WRTTWAB                                                       
         LA    R2,SRVS1H                                                        
         MVC   SRVMSG+12(15),=C'NO MORE ENTRIES'                                
         B     ERRX                                                             
ERR7     DS    0H                                                               
* SAVE OFF D/A IN LAST ENTRY IN TABLE TO FAKE OUT FOR RE-ENTRY                  
         LA    R2,SVEND            LAST DISPLAY LINE IN TWA                     
         LA    R1,VTTSVEND-VTTSVDA LENGTH OF ENTRY = 34                         
         SR    R2,R1                                                            
         MVC   0(10,R2),VTTSVDA                                                 
         BAS   RE,WRTTWAB                                                       
*                                                                               
         LA    R2,SRVS1H                                                        
         MVC   SRVMSG+12(36),=C'TOO MANY I/O OPS - ENTER TO CONTINUE'           
         B     ERRX                                                             
ERR8     MVC   SRVMSG+12(17),=C'SRVC IS VTAM ONLY'                              
         LA    R2,SRVIDH                                                        
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     MVC   SRVMSG(12),=C'***ERROR*** '                                      
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R2),X'40'         POSN CURSOR ON INVALID FIELD                 
         L     RD,BASERD                                                        
         B     EXIT                                                             
         SPACE 2                                                                
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
VTAMCMDS DS    0D                                                               
         DC    CL8'????????'       00                                           
         DC    CL8'????????'       01                                           
         DC    CL8'CHECK   '       02                                           
         DC    CL8'????????'       03                                           
         DC    CL8'ENDREQ  '       04                                           
         DC    CL8'????????'       05                                           
         DC    CL8'VERIFY  '       06                                           
         DC    CL8'IMPORT  '       07                                           
         DC    CL8'DTAPRFMT'       08                                           
         DC    CL8'IX PRFMT'       09                                           
         DC    CL8'FORCE IO'       0A                                           
         DC    CL8'????????'       0B                                           
         DC    CL8'????????'       0C                                           
         DC    CL8'????????'       0D                                           
         DC    CL8'????????'       0E                                           
         DC    CL8'????????'       0F                                           
         DC    CL8'????????'       10                                           
         DC    CL8'WRITE   '       11                                           
         DC    CL8'RESET   '       12                                           
         DC    CL8'DO      '       13                                           
         DC    CL8'VER REFR'       14                                           
         DC    CL8'SETLOGON'       15                                           
         DC    CL8'SIMLOGON'       16                                           
         DC    CL8'OPNDST  '       17                                           
         DC    CL8'????????'       18                                           
         DC    CL8'CHANGE  '       19                                           
         DC    CL8'INQUIRE '       1A                                           
         DC    CL8'INTRPRT '       1B                                           
         DC    CL8'????????'       1C                                           
         DC    CL8'READ    '       1D                                           
         DC    CL8'SOLICIT '       1E                                           
         DC    CL8'CLSDST  '       1F                                           
         DC    CL8'????????'       20                                           
         DC    CL8'CLOSEACB'       21                                           
         DC    CL8'SEND    '       22                                           
         DC    CL8'RECEIVE '       23                                           
         DC    CL8'RESETSR '       24                                           
         DC    CL8'SESSIONC'       25                                           
         DC    CL8'????????'       26                                           
         DC    CL8'SENDCMD '       27                                           
         DC    CL8'RCVCMD  '       28                                           
         DC    CL8'REQSESS '       29                                           
         DC    CL8'OPNSEC  '       2A                                           
         DC    CL8'CLSSEC  '       2B                                           
         DC    CL8'TRMSESS '       2C                                           
         DC    CL8'????????'       2D                                           
         DC    CL8'????????'       2E                                           
         DC    CL8'????????'       2F                                           
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
BASERD   DS    A                                                                
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
BUFFDA   DS    F                                                                
BUFFPTR  DS    F                                                                
*                                                                               
RELO     DS    A                                                                
VCOMFACS DS    A                                                                
TRMUTL   DS    F                                                                
VTCID    DS    F                                                                
HOLD     DS    F                                                                
TRMNUM   DS    H                                                                
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
RECSBLK  DS    H                   ADRFILE RECORDS PER BLOCK                    
RECSIZE  DS    H                   ADRFILE RECORD LENGTH                        
BLKSIZE  DS    H                   ADRFILE BLOCK SIZE                           
TRCRBLK  DS    X                   TRACE RECORDS PER ADRFILE BLOCK              
TRCRBLK1 DS    X                   TRCRBLK+!                                    
LIN      DS    H                   INDICATES LINE SELECTED                      
RFLAG    DS    X                   FILTER FLAG                                  
SFLAG    DS    X                   FILTER FLAG                                  
DUMMYH   DS    X'1000000080080000'                                              
DUMMY    DS    CL8                                                              
ANDMASK  DS    C                                                                
ORMASK   DS    C                                                                
IOCNT    DS    X                   COUNTER OF I/O OPERATIONS                    
FILTER   DS    CL8                 LUNAME OR PART                               
FLTLEN   DS    X                   LENGTH OF FILTER                             
SRVTTSV  DS    CL548               544 FOR SCRN DATA + 4 FOR $VTT               
SVEND    EQU   *                                                                
*                                                                               
WORK     DS    CL128                                                            
*                                                                               
BUFFER   DS    6400C                                                            
BUFFERX  EQU   *                                                                
*                                                                               
*                                                                               
WRKX     DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE VTTRCBUFF                                                      
         EJECT                                                                  
***********************************************************************         
* DSECT FOR DISPLAY LINE DATA                                         *         
***********************************************************************         
         SPACE 1                                                                
SRVLINED DSECT                                                                  
         DS    CL8                 FLDHDR                                       
*                                                                               
SRVLDATE DS    CL8                 MMMDD/YY                                     
*                                                                               
         ORG   SRVLDATE                                                         
SRVLTIME DS    CL8                 12.34.56                                     
*                                                                               
         ORG   SRVLDATE                                                         
SRVLCMD  DS    CL8                                                              
*                                                                               
         ORG   SRVLDATE                                                         
         DS    CL8                 'RCFDBK= '                                   
SRVLRTCD DS    CL2                                                              
         DS    CL1                                                              
SRVLFDB2 DS    CL2                                                              
*                                                                               
         ORG   SRVLDATE                                                         
         DS    CL8                 'SENSE = '                                   
SRVLSNS  DS    CL8                                                              
*                                                                               
         ORG   SRVLDATE+20                                                      
SRVLHEX  DS    CL34                                                             
         DS    CL6                                                              
SRVLEBC  DS    CL16                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT FOR LIST DISPLAY LINE AREA                                    *         
***********************************************************************         
         SPACE 1                                                                
SRVLSTD  DSECT                                                                  
         DS    CL8                 HEADER                                       
LSTDA    DS    CL10                DISK ADDRESS                                 
         DS    CL1                                                              
LSTLU    DS    CL8                                                              
         DS    CL1                                                              
LSTDATE  DS    CL8                                                              
         DS    CL1                                                              
LSTTIME  DS    CL8                                                              
         DS    CL1                                                              
LSTCMD   DS    CL8                                                              
         DS    CL1                                                              
LSTRCFD  DS    CL9                                                              
         DS    CL1                                                              
LSTSENSE DS    CL14                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER DATA SAVED IN TWAB (TWA 11)                          *         
***********************************************************************         
         SPACE 1                                                                
VTTSVD   DSECT                                                                  
VTTSVDA  DS    CL10                DISK ADDRESS                                 
VTTSVLU  DS    CL8                 LU NAME                                      
VTTSVDT  DS    CL8                 DATE                                         
VTTSVTM  DS    CL8                 TIME                                         
VTTSVEND EQU   *                                                                
         EJECT                                                                  
*DDFLDHDR                                                                       
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
*SRVTTFFD                                                                       
       ++INCLUDE SRVTTFFD                                                       
         EJECT                                                                  
         PRINT NOGEN                                                            
         IFGRPL AM=VTAM                                                         
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SRVTT00   06/12/02'                                      
         END                                                                    
