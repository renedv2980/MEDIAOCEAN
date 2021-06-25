*          DATA SET WKFDEF     AT LEVEL 001 AS OF 04/03/12                      
*PHASE WKFDEFA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE SCANNER                                                                
*                                                                               
*&&      SET   NOP=N                                                            
*                                                                               
         TITLE 'WKFDEF - WRKF FILE DEFINITION FIX'                              
         PRINT NOGEN                                                            
WKFDEF   CSECT                                                                  
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         NBASE 0,WKFDEF,R9,R8,WORK=A(WKWORK)                                    
*                                                                               
         LARL  RA,GLOBAL                                                        
         USING GLOBAL,RA                                                        
*                                                                               
         ST    R1,ACOMRG           SAVE MVS SUPV INFO                           
         L     R1,0(R1)                                                         
         LH    R2,0(R1)                                                         
         LTR   R2,R2               R2=L'PARM DATA                               
         BZ    WKM030                                                           
         CHI   R2,8                                                             
         BNH   *+8                                                              
         LA    R2,8                                                             
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         LA    RF,UPSITAB                                                       
WKM010   CLI   0(R1),C'0'                                                       
         BE    WKM020                                                           
         CLI   0(R1),C'1'                                                       
         BNE   WKM030                                                           
         OC    UPSIVAL,0(RF)                                                    
WKM020   LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,WKM010                                                        
*                                                                               
WKM030   MVC   UPSIINP,UPSIVAL     SAVE UPSI                                    
         XC    ALOADPT,ALOADPT     CLEAR EXTERNAL LOAD POINT ADDR               
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,DUB),(10,DATEIPL)                             
         GOTO1 (RF),(R1),(4,DATEIPL),(0,DATEYMD)                                
         GOTO1 (RF),(R1),(0,DATEYMD),(2,DATECPR)                                
*                                                                               
         TBIN  SECS                R1 IS TIME IN SECS                           
         SR    R0,R0                                                            
         D     R0,=F'60'           R1 IS TIME IN MINS                           
         SR    R0,R0                                                            
         D     R0,=F'10'           R1 IS TIME IN 10 MIN INCREMENTS              
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,TIMEINC                                                       
*                                                                               
         L     RC,=V(CPRINT)       RC IS PRINTER CONTROL REGISTER               
         USING DPRINT,RC                                                        
*                                                                               
         L     R1,=A(CIREC)        INITIALISE WRKF FILE BUFFERS                 
         ST    R1,ACIREC                                                        
         L     R1,=A(CXREC)                                                     
         ST    R1,ACXREC                                                        
         XC    CIDATA,CIDATA                                                    
         LA    RE,L'W_INDEX                                                     
         STH   RE,CINDXLN                                                       
         MVC   WRKFID,WRKFIL                                                    
                                                                                
*======================================================================         
* READ A SET OF INPUT PARAMETER CARDS                                           
*======================================================================         
GETPARM  CLI   FRSTTIME,C'X'       WAS LAST SET TERMINATED WITH /* CARD         
         BE    EOJ                 YES EOJ                                      
         BH    *+16                                                             
         ZAP   LINE,=P'99'                                                      
         MVC   TITLE+29(16),SPACES                                              
         L     RF,=A(INFO2)        PARAMETER CARDS                              
         BAS   RE,PINFO                                                         
         L     RF,=A(INFO0)        UNDERLINE                                    
         BAS   RE,PINFO                                                         
*                                                                               
         BRAS  RE,VALPARM                                                       
         CLI   ERRNUM,0                                                         
         BNE   GPERR               ERROR FOUND IN CARD                          
*                                                                               
         CLI   EXTID,C'C'          CSC WRKF FILES                               
         BE    GP040                                                            
         CLI   EXTID,C'Q'          FQA WRKF FILES                               
         BE    GP040                                                            
*&&US*&& CLI   EXTID,C'R'          REP WRKF FILES                               
*&&US*&& BE    GP040                                                            
         CLI   EXTID,C'T'          TST WRKF FILES                               
         BE    GP040                                                            
*                                                                               
         XC    DUB,DUB             GET DATASPACE HEADER FROM TABS               
         MVC   DUB(4),=AL4(DTWRKR)                                              
         MVI   DUB,X'20'           SET ENQUIRE                                  
         GOTO1 =V(LOCKSPC),DUB                                                  
*                                                                               
         L     RF,4(R1)            SAVE DSPACE HEADER                           
         USING DMSPACED,RF                                                      
         NC    DSPTFRST,=XL4'3FFFFFFF'                                          
         ICM   R2,15,DSPTFRST                                                   
         L     RE,=A(SSB)                                                       
         LAM   AR2,AR2,SSOTBLET-SSOOFF(RE)                                      
         SAC   512                                                              
         CLI   0(R2),0             CHECK WKBUFF ACTIVE                          
         BE    GP020               NO                                           
         SAC   0                                                                
         LAM   AR0,ARF,=16F'0'     CANT DO ANYTHING WITH WKBUFF ACTIVE          
*                                                                               
         WTO   'WKBUFF STILL ACTIVE - CANNOT USE WRKFM PROGRAM'                 
         ABEND 911,DUMP                                                         
         DROP  RF                                                               
*                                                                               
GP020    SAC   0                   WKBUFF HAS BEEN STOPPED                      
         LAM   AR0,ARF,=16F'0'                                                  
*                                                                               
GP040    EQU   *                                                                
*                                                                               
         LA    R4,PARMTBL          CHECK FOR REQUIRED & OPTIONAL PARMS          
GP050    TM    2(R4),X'80'                                                      
         BZ    GP060                                                            
         CLI   0(R4),0             WAS REQUIRED PARM INPUT                      
         BNE   GP070               YES                                          
         LA    R1,4(R4)            NO- ERROR                                    
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,1                                                         
         B     GPERR                                                            
GP060    CLI   0(R4),0             WAS OPTIONAL PARM INPUT                      
         BNE   GP070               YES                                          
         MVC   0(1,R4),1(R4)       NO- SET DEFAULT VALUE                        
GP070    LA    R4,L'PARMTBL(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   GP050                                                            
*                                                                               
         CLI   WRKFMAX,1           SET DEFAULT WKID IF ONLY ONE FILE            
         BNE   GP090                                                            
         MVI   WKID,1              SET DEFAULT INTERNAL WRKF FILE NUM           
         MVI   WRKFINP,1           SET ONE WRKF FILE INPUT                      
         OI    WRKFINP+1,X'01'     SET REFERENCED BY WKID=U PARM                
*                                                                               
GP090    CLI   MODE,1              INIT - MUST DEFINE WRKF ID                   
         BNE   GP100                                                            
         CLI   WRKFINP,1           MUST BE SINGLE FILE ONLY                     
         BE    GETPARMX                                                         
         MVI   ERRNUM,5                                                         
         B     GPERR                                                            
*                                                                               
GP100    DC    H'0'                                                             
*                                                                               
GPERR    L     R1,ERRNUM           POINT TO ERROR INFO WORD                     
*                                                                               
GPERR1   CLI   ERRNUM,1            HIGH ORDER BYTE HAS ERR NUM                  
         BNE   GPERR2                                                           
         L     RF,=A(ERRMSG1)      MISSING PARAM                                
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+26(8),0(R1)                                                 
         B     GPERRA                                                           
*                                                                               
GPERR2   CLI   ERRNUM,2                                                         
         BNE   GPERR3                                                           
         L     RF,=A(ERRMSG2)      INVALID SYNTAX                               
         MVC   WORK(60),0(RF)                                                   
         B     GPERRA                                                           
*                                                                               
GPERR3   CLI   ERRNUM,3                                                         
         BNE   GPERR4                                                           
         L     RF,=A(ERRMSG3)      INVALID PARAM                                
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+26(8),0(R1)                                                 
         B     GPERRA                                                           
*                                                                               
GPERR4   CLI   ERRNUM,4                                                         
         BNE   GPERR5                                                           
         L     RF,=A(ERRMSG4)      INVALID VALUE FOR PARAM                      
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+36(8),0(R1)                                                 
         B     GPERRA                                                           
*                                                                               
GPERR5   CLI   ERRNUM,5                                                         
         BNE   GPERR6                                                           
         L     RF,=A(ERRMSG5)      MUST SPECIFY A SINGLE WRKF ONLY              
         MVC   WORK(60),0(RF)                                                   
         B     GPERRA                                                           
*                                                                               
GPERR6   DC    H'0'                DIE IF UNKNOWN ERROR                         
*                                                                               
GPERRA   GOTO1 =V(PRINTER)         DISPLAY AND PRINT ERROR MESSAGE              
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PUTMSGP                                                       
         GOTO1 =V(PRINTER)                                                      
         CLI   FRSTTIME,C'X'       FLUSH AND PRINT REMAINING CARDS              
         BE    GPERRX                                                           
*                                                                               
GPERRB   GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'                                                   
         BE    GPERRX                                                           
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         B     GPERRB                                                           
*                                                                               
GPERRX   SR    RF,RF               EXIT WITH ERRNUM AS RETURN CODE              
         IC    RF,ERRNUM                                                        
         XBASE RC=(RF)                                                          
*                                                                               
GETPARMX B     OPEN                                                             
*                                                                               
EOJ      XBASE                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
                                                                                
*======================================================================         
* INITIALISE AND OPEN WRKF FILES AND TAPES FOR THIS MODE                        
*======================================================================         
OPEN     BRAS  RE,OPNWK            OPEN ALL WRKF FILES REFERENCED               
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         L     RF,=A(INFO3)        ACTION MESSAGES                              
         BAS   RE,PINFO                                                         
         L     RF,=A(INFO0)        UNDERLINE                                    
         BAS   RE,PINFO                                                         
         MVC   DUB1,WRKFIL         SET WRKF FILE IN TITLE                       
         MVC   DUB2,DUB1                                                        
         CLI   WRKFINP,1           TEST ONLY ONE WRKF FILE REFERENCED           
         BNE   *+16                                                             
         MVC   DUB1(7),FILEIX                                                   
         MVC   DUB2(7),FILEID                                                   
         MVC   TITLE+29(5),DUB1                                                 
         CLC   DUB1(7),DUB2                                                     
         BE    *+14                                                             
         MVI   TITLE+34,C'='                                                    
         MVC   TITLE+35(7),DUB2                                                 
*                                                                               
         CLI   MODE,1              GO TO ROUTINE FOR MODE                       
         BE    INITWK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALIZE THE WORKER FILE                                                    
***********************************************************************         
INITWK   BRAS  RE,IWK              GO TO INITIALISE ROUTINE                     
*                                                                               
         CLI   ERRNUM,1            TEST FOR GOOD INITIALISE                     
         BL    INITWK2             YES                                          
         BH    *+12                                                             
         L     RF,=A(ERRMSGA)      ERROR END OF FILE                            
         B     *+8                                                              
         L     RF,=A(ERRMSGB)      ERROR DISK WRITE                             
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+6(5),WRKFID                                                 
         BAS   RE,PUTMSGP                                                       
         DC    H'0'                DIE IF FAIL TO INITIALISE                    
*                                                                               
INITWK2  L     RF,=A(INFO4)        INITIALISED WRKFX                            
         MVC   12(5,RF),FILEIX                                                  
         CLC   FILEIX(7),FILEID    TEST IF WRKF FILE WAS RENAMED                
         BE    INITWKX                                                          
         MVI   17(RF),C'='                                                      
         MVC   18(7,RF),FILEID                                                  
*                                                                               
INITWKX  BAS   RE,PINFO                                                         
         GOTO1 =V(PRINTER)                                                      
         BRAS  RE,WKOUT            PRINT ATTRIBUTES OF NEW WRKFIL               
         B     GETPARM                                                          
         EJECT                                                                  
                                                                                
**********************************************************************          
* GET/PUT MESSAGE TO OPERATOR AND OPTIONALLY TO PRINTER                         
* MESSAGE IS IN WORK AND REPLY IS RETURNED IN OPERANS                           
**********************************************************************          
PUTMSG   MVI   DMCB+16,X'01'       SET PUT FLAG                                 
         B     PAGM1                                                            
PUTMSGP  MVI   DMCB+16,X'81'       SET PUT AND PRINT FLAG                       
         B     PAGM1                                                            
GETMSG   MVI   DMCB+16,X'02'       SET GET FLAG                                 
         B     PAGM1                                                            
PAGMSG   MVI   DMCB+16,X'03'       SET PUT AND GET FLAGS                        
         B     PAGM1                                                            
*                                                                               
PAGM1    ST    RE,SAVERE                                                        
         TM    DMCB+16,X'01'       PUT MSG IN OPERMSG TO CONSOLE                
         BZ    PAGM2                                                            
         GOTO1 =V(LOGIO),DMCB,1,(60,WORK)                                       
         TM    DMCB+16,X'80'       AND SEE IF PRINTER ALSO                      
         BZ    PAGM2                                                            
         MVC   P(60),WORK                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PAGM2    TM    DMCB+16,X'02'       GET REPLY INTO OPERANS                       
         BZ    PAGMX                                                            
         MVC   OPERANS,SPACES                                                   
         GOTO1 =V(LOGIO),DMCB,0,(8,OPERANS)                                     
PAGMX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT INFO MSG AT RF                                                          
***********************************************************************         
PINFO    ST    RE,SAVERE                                                        
         MVC   P(60),0(RF)                                                      
         GOTO1 =V(PRINTER)                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* DROP EXTRA BASE REGS                                                          
***********************************************************************         
         DROP  R8,R9                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ/PRINT/VALIDATE A SET OF PARAMETER CARDS                                  
***********************************************************************         
VALPARM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,MODE+4           CLEAR ERRNUM AND SET A(PARMTBL NTRY)         
         ST    R1,ERRNUM                                                        
         CLI   FRSTTIME,C'Y'       READ FIRST CARD                              
         BNE   VPARM2                                                           
         MVI   FRSTTIME,C'N'                                                    
*                                                                               
VPARM1   GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(80),CARD                                                       
         CLI   CARD,C'*'           IGNORE COMMENT CARDS                         
         BE    VPARM1P                                                          
*                                                                               
VPARM1A  CLC   CARD(5),=CL8'DATE=' DATE=DD/MM/YY TO SET SYSTEM DATE             
         BNE   VPARM1B                                                          
         GOTO1 =V(DATVAL),DMCB,(0,CARD+5),DUB                                   
         OC    0(4,R1),0(R1)                                                    
         BNZ   VPARM1A1                                                         
         LA    R1,=CL8'DATE='                                                   
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     VPARMX                                                           
*                                                                               
VPARM1A1 MVC   DATEIPL,CARD+5      OVERRIDE SYSTEM IPL DATE                     
         GOTO1 =V(DATCON),DMCB,(4,DATEIPL),(0,DATEYMD)                          
         GOTO1 (RF),(R1),(0,DATEYMD),(2,DATECPR)                                
         L     RF,=A(SSB)                                                       
         MVI   5(RF),X'80'         SET OFFLINE PASS OF DATE IN V(SSB)           
         MVC   8(6,RF),DATEYMD     PASS DATE AS C'YYMMDD'                       
         B     VPARM1P                                                          
*                                                                               
VPARM1B  CLC   CARD(6),=CL8'DDSIO=' DDSIO=XXXXXXXX TO SET WHICH DMDMGR          
         BNE   VPARM1C                                                          
         L     RF,=V(DDSIO)        OVERRIDE DATAMGR LOAD MODULE NAME            
         MVC   0(8,RF),CARD+6                                                   
         B     VPARM1P                                                          
*                                                                               
VPARM1C  CLC   CARD(7),=CL8'DSPACE=' DSPACE=X TO SET THE DATA SPACE             
         BNE   VPARM1X                                                          
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         B     VPARM1P                                                          
*                                                                               
VPARM1P  GOTO1 =V(PRINTER)                                                      
         B     VPARM1                                                           
*                                                                               
VPARM1X  GOTO1 VDATAMGR,DMCB,=C'OPEN',=C'SER',=C'NCTFILE X'                     
         L     RF,ACIREC                                                        
         XC    NXUSRINF,NXUSRINF                                                
         GOTO1 VDATAMGR,DMCB,=C'GLIST',WRKFIL,INDEX,,(RF)                       
         ICM   RE,15,NXUSRINF                                                   
         ICM   RF,15,NXUSRINF+4                                                 
         ST    RE,AWRKFLST         SAVE A(WRKF FILE LIST)                       
         ST    RF,AWRKFXPE         SAVE A(WRKF INDEX PAGE/ENTRY LIST)           
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         XC    0(2,RF),0(RF)       SET TO START AT PAGE ONE                     
         MVC   WRKFMAX,0(RE)       SAVE NUMBER OF WRKF FILES IN LIST            
         CLI   WRKFMAX,16                                                       
         BNH   *+6                                                              
         DC    H'0'                MAX OF 16 FILES THIS VERSION                 
         LA    RE,8(RE)                                                         
         LA    RF,FILEIX+8                                                      
         SR    R1,R1                                                            
VPARM1X1 CLI   0(RE),0             TEST END OF WRKF LIST                        
         BE    VPARM1X2                                                         
         ICM   R1,7,5(RE)          GET A(DTF)                                   
         MVC   0(7,RF),22(R1)      SAVE ORIGIONAL DTF FILE ID                   
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         B     VPARM1X1                                                         
*                                                                               
VPARM1X2 L     RE,=V(DMISGENQ)     SAVE A(ENQ/DEQ ROUTINE)                      
         ST    RE,FIWENQ                                                        
*                                                                               
VPARM2   CLC   CARD(2),MODE+4      FIRST CARD OF SET MUST BE MODE CARD          
         BE    *+12                                                             
         MVI   ERRNUM,1                                                         
         B     VPARMX                                                           
         LA    R1,PARMTBL          INITIALIZE VALUES                            
         CLI   0(R1),X'FF'                                                      
         BE    VPARM4                                                           
         MVI   0(R1),0                                                          
         LA    R1,L'PARMTBL(R1)                                                 
         B     *-16                                                             
*                                                                               
VPARM4   MVC   FILEIX(8),=CL8' '   RESET FILE DTF NAMES                         
         MVC   FILEID,FILEIX                                                    
         XC    WRKFINP,WRKFINP     RESET FILE INPUT LIST                        
         LA    R1,REPTAB                                                        
         CLC   0(4,R1),=32X'FF'                                                 
         BE    *+18                                                             
         XC    0(L'REPTAB,R1),0(R1)                                             
         LA    R1,L'REPTAB(R1)                                                  
         B     *-20                                                             
         LA    R1,REPTAB                                                        
         LA    R0,L'REPTAB                                                      
         SR    R1,R0                                                            
         ST    R1,AREPTAB                                                       
         B     VPARM8                                                           
*                                                                               
VPARM6   GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLI   CARD,C'*'                                                        
         BE    VPARMP              IGNORE COMMENT CARDS                         
         CLC   CARD(2),=C'/*'                                                   
         BNE   *+12                                                             
         MVI   FRSTTIME,C'X'                                                    
         B     VPARMX                                                           
         CLC   CARD(2),MODE+4                                                   
         BE    VPARMX                                                           
*                                                                               
VPARM8   MVC   P(80),CARD                                                       
         MVC   CARD+72(8),SPACES                                                
         L     R2,ACIREC                                                        
         GOTO1 =V(SCANNER),DMCB,(C'C',CARD),(R2)                                
         CLI   4(R1),0                                                          
         BNE   *+12                                                             
         MVI   ERRNUM,2            INVALID SYNTAX                               
         B     VPARMX                                                           
         SR    R0,R0                                                            
         IC    R0,4(R1)            R0=NUM OF PARMS ON CARD                      
*                                                                               
VPARMA   LA    R1,12(R2)           POINT TO KEYWORD NAME                        
         ST    R1,ERRNUM                                                        
         CLI   0(R2),2             MUST BE 2 TO 8 CHRS LONG                     
         BNL   *+12                                                             
VPARMB   MVI   ERRNUM,3                                                         
         B     VPARMX                                                           
         CLI   0(R2),8                                                          
         BH    VPARMB                                                           
         LA    R4,PARMTBL          SEARCH FOR KEYWORD IN TABLE                  
         SR    RF,RF                                                            
         IC    RF,0(R2)            MATCH ON 2 THRU N CHRS                       
         BCTR  RF,0                                                             
VPARMC   EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),4(R4)                                                   
         BE    VPARMD                                                           
         LA    R4,L'PARMTBL(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BE    VPARMB                                                           
         B     VPARMC                                                           
*                                                                               
VPARMD   CLC   1(1,R2),3(R4)       KEYWORD VALUE MUST BE N THRU 8 CHRS          
         BNL   *+12                                                             
VPARME   MVI   ERRNUM,4                                                         
         B     VPARMX                                                           
         CLI   1(R2),8                                                          
         BH    VPARME                                                           
         SR    RE,RE                                                            
         IC    RE,1(R2)                                                         
         BCTR  RE,0                                                             
         L     RF,12(R4)                                                        
         TM    2(R4),X'40'         KEYWORD VALUE IN LIST                        
         BO    VPARMF              YES                                          
         TM    2(R4),X'20'         KEYWORD VALUE BY ROUTINE                     
         BO    VPARMG              YES                                          
         MVC   0(8,RF),22(R2)      NO- SAVE VALUE                               
         MVI   0(R4),YES                                                        
         B     VPARMN                                                           
*                                                                               
VPARMF   CLI   0(RF),X'FF'         SEARCH VALUE LIST                            
         BE    VPARME                                                           
         EX    RE,*+8              MATCH ON N THRU 7 CHRS                       
         B     *+10                                                             
         CLC   1(0,RF),22(R2)                                                   
         BE    *+12                                                             
         LA    RF,8(RF)                                                         
         B     VPARMF                                                           
         MVC   0(1,R4),0(RF)       SAVE VALUE IN PARMTBL                        
         B     VPARMN                                                           
*                                                                               
VPARMG   BASR  RE,RF               GO TO ROUTINE WITH R2=A(ENTRY)               
         CLI   ERRNUM,0                                                         
         BNE   VPARMX                                                           
*                                                                               
VPARMN   LA    R2,32(R2)           BUMP TO NEXT PARM                            
         BCT   R0,VPARMA                                                        
*                                                                               
VPARMP   GOTO1 =V(PRINTER)         PRINT CARD                                   
         B     VPARM6              GO GET NEXT PARM CARD                        
*                                                                               
VPARMX   J     XIT                                                              
                                                                                
*======================================================================         
* NUMBER OF CONTROL INTERVALS                                                   
*======================================================================         
VNCIS    MVI   0(R4),1                                                          
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VNCISERR                                                         
         L     RF,8(R2)                                                         
         C     RF,=F'65000'        MAXIMUM VALUE                                
         BH    VNCISERR                                                         
         C     RF,=F'10'           MINIMUM VALUE                                
         BL    VNCISERR                                                         
         CLI   12(R2),C'O'                                                      
         BE    *+12                                                             
         STCM  RF,3,CICITOT                                                     
         B     *+8                                                              
         STCM  RF,3,CJCITOT                                                     
         B     *+8                                                              
VNCISERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
*======================================================================         
* TRACKS PER CONTROL INTERVAL                                                   
*======================================================================         
VTRKS    MVI   0(R4),1                                                          
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VTRKSERR                                                         
         L     RF,8(R2)                                                         
         C     RF,=F'60'           MAXIMUM VALUE                                
         BH    VTRKSERR                                                         
         C     RF,=F'1'            MINIMUM VALUE                                
         BL    VTRKSERR                                                         
         CLI   12(R2),C'O'                                                      
         BE    *+12                                                             
         STH   RF,CITRKS                                                        
         B     *+8                                                              
         STH   RF,CJTRKS                                                        
         B     *+8                                                              
VTRKSERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
*======================================================================         
* BLOCK SIZE IN BYTES                                                           
*======================================================================         
VBLKS    MVI   BLKSIZE,1                                                        
         CLI   MODE,1                                                           
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VBLKSERR                                                         
         L     RF,8(R2)                                                         
         C     RF,=F'6000'         OLD VALUE                                    
         BE    VBLKS1                                                           
         C     RF,=F'13680'        NEW VALUE                                    
         BE    VBLKS1                                                           
         B     VBLKSERR                                                         
VBLKS1   STH   RF,CIBLKLN                                                       
         B     *+8                                                              
VBLKSERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
                                                                                
*======================================================================         
* WRKF FILE DTF/DD OVERRIDE NAME                                                
*======================================================================         
VFILE    NTR1                                                                   
*                                                                               
         CLI   1(R2),5                                                          
         BL    VFILEERR                                                         
         CLI   1(R2),7                                                          
         BH    VFILEERR                                                         
         IC    RF,FILE             BUMP NUMBER OF FILE NAMES INPUT              
         LA    RF,1(RF)                                                         
         STC   RF,FILE                                                          
         MVC   DUB,22(R2)                                                       
         L     RF,AWRKFLST         POINT TO WRKF FILE LIST                      
         CLI   WRKFMAX,1                                                        
         BNE   VFILE1                                                           
         LA    RF,8(RF)            NO RULES IF ONLY ONE FILE                    
         B     VFILE3                                                           
VFILE1   LA    RF,8(RF)                                                         
         CLI   0(RF),0             TEST END OF TABLE                            
         BE    VFILEERR                                                         
         CLC   1(1,RF),DUB+4       FIFTH CHR MUST MATCH FILE ID CHR             
         BNE   VFILE1                                                           
VFILE3   SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
VFILE4   LA    RF,WRKFINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'10'                                                      
         BO    VFILEERR                                                         
         OI    0(RF),X'10'         SET REFERENCED VIA FILE=XXXXX                
         SLL   R1,3                                                             
         LA    RF,FILEID(R1)                                                    
         MVC   0(7,RF),DUB         SAVE INPUT OVERRIDE IN FILEID LIST           
         B     *+8                                                              
VFILEERR MVI   ERRNUM,4                                                         
         J     VPARMX                                                           
                                                                                
*======================================================================         
* WRKF FILE ID = LIST OF WRKF ID CHRS                                           
*======================================================================         
VWKID    NTR1                                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R2)                                                       
         BZ    VWKIDERR            R0=NUMBER OF FILES                           
         LA    R5,22(R2)           R5=A(NEXT WRKF FILE CHR)                     
VWKID1   L     RF,AWRKFLST         POINT TO LIST OF WRKF FILES                  
         LA    RF,8(RF)                                                         
VWKID2   CLI   0(RF),0             TEST END OF TABLE                            
         BE    VWKIDERR                                                         
         CLC   1(1,RF),0(R5)       TEST INPUT CHR WITH ALPHA ID CHR             
         BE    VWKID3                                                           
         LA    RF,8(RF)                                                         
         B     VWKID2                                                           
VWKID3   MVC   WKID(1),0(RF)       SET WKID TO INTERNAL WRKF FILE NUM           
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    RF,WRKFINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'0F'                                                      
         BNZ   VWKID4                                                           
         IC    R1,WRKFINP          BUMP NUM OF WRKF FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,WRKFINP                                                       
VWKID4   TM    0(RF),X'01'         TEST DUPLICATE                               
         BO    VWKIDERR                                                         
         OI    0(RF),X'01'         SET REFERENCED BY WKID INPUT                 
         LA    R5,1(R5)                                                         
         BCT   R0,VWKID1           BACK FOR NEXT WRKF FILE ID CHR               
         B     *+8                                                              
VWKIDERR MVI   ERRNUM,4                                                         
         J     VPARMX                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* OPEN WRKF FILE(S) REFERENCED BY INPUT PARAMS                                  
***********************************************************************         
OPNWK    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,WRKFINP          POINT TO LIST OF INPUT WRKF IDS              
         ST    R1,AWRKFINP                                                      
*                                                                               
OPNWK1   L     R1,AWRKFINP         BUMP TO NEXT FILE IN LIST                    
         LA    R1,1(R1)                                                         
         ST    R1,AWRKFINP                                                      
         CLI   0(R1),X'FF'         TEST END OF LIST                             
         BE    OPNWK6                                                           
         TM    0(R1),X'0F'         TEST IF FILE REFERENCED                      
         BZ    OPNWK1                                                           
         LA    R0,WRKFINP                                                       
         SR    R1,R0               R1=INTERNAL WRKF FILE NUM                    
         SLL   R1,3                                                             
         L     RF,AWRKFLST         INDEX INRO WRKF FILE LIST                    
         AR    RF,R1                                                            
         MVC   WRKFID,WRKFIL                                                    
         MVC   WRKFID+4(1),1(RF)   SET WRKF FILE ID FOR DATAMGR                 
         MVC   WRKFINT,0(RF)       SET WRKF FILE INTERNAL NUM                   
         MVC   WRKFEXT,4(RF)       SET WRKF FILE EXTERNAL NUM                   
         MVC   WRKFDTF+1(3),5(RF)  SET WRKF FILE A(DTF)                         
         LA    RF,FILEIX(R1)                                                    
         MVC   FILEIX(8),0(RF)     SET ORIGINAL DTF NAME                        
         LA    RF,FILEID(R1)                                                    
         MVC   FILEID(8),0(RF)     SET OVERRIDE DTF NAME                        
*                                                                               
OPNWK2   L     R2,WRKFDTF          R2=A(WRKFIL DTF)                             
         USING DTFPHD,R2                                                        
         MVI   DUB,C'N'                                                         
         MVC   DUB+1(7),FILEID                                                  
         MVI   DUB+8,C'X'                                                       
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         MVC   P2,ACXREC                                                        
         MVC   P4,WRKFDTF                                                       
         LA    RE,P6                                                            
         ST    RE,P5                                                            
         MVC   P6,=X'00010100'                                                  
*                                                                               
         TM    DTFOPEN,X'20'       TEST IF ALREADY OPEN                         
         BZ    OPNWK3              NO                                           
         CLC   DTFFID,FILEID       TEST IF SAME FILE ID                         
         BE    OPNWK4              YES                                          
         MVC   P1,=A(DACLOSE)                                                   
         GOTO1 VDATAMGR,P0,DADDS                                                
*                                                                               
OPNWK3   MVC   DTFFID,FILEID                                                    
         GOTO1 VDATAMGR,DMCB,=C'OPEN',=C'SER',DUB                               
*                                                                               
OPNWK4   MVC   DNEXT,=X'00010000'                                               
         MVC   P1,=A(RDID)                                                      
         GOTO1 VDATAMGR,P0,DADDS   READ FIRST RECORD                            
         DROP  R2                                                               
*                                                                               
         CLI   MODE,1              EXIT IF MODE IS INITIALISE                   
         BE    OPNWKX                                                           
*                                                                               
OPNWK5   L     R2,ACXREC           READ FIRST INDEX RECORD                      
         MVC   CXADDR,=X'00010100'                                              
         GOTO1 VDATAMGR,DMCB,DMREAD,WRKFID,CXADDR,(R2)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,WRKFDTF          SET CIDATA STORED INFRONT OF DTF             
         AHI   RE,-40                                                           
         MVC   0(20,RE),0(R2)      EXTRACT PART 1 INDEX DATA                    
         TM    0(RE),X'80'                                                      
         BZ    *+14                                                             
         MVC   20(20,RE),L'W_INDEX(R2) EXTRACT PART 2 INDEX DATA                
         NI    0(RE),X'7F'                                                      
         MVC   14(1,RE),WRKFINT    SET INTERNAL WRKF FILE NUM                   
         MVC   15(5,RE),WRKFID     SET ALPHA WRKF FILE NAME                     
         MVC   CIDATA,0(RE)                                                     
         L     RE,WRKFDTF          SET F/L REC LEN IN WRKFIL DTF                
         LA    RE,52(RE)                                                        
         MVC   0(2,RE),CIBLKLN                                                  
         OI    0(RE),X'80'                                                      
         B     OPNWK1              BACK FOR NEXT WRKF FILE                      
*                                                                               
OPNWK6   XC    CXPAGE,CXPAGE       SET FIRST INDEX ENTRY                        
         LH    R5,CICINDX                                                       
         BCTR  R5,0                                                             
         STH   R5,CXENTRY                                                       
*                                                                               
OPNWKX   J     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALISE A WRKF FILE                                                        
***********************************************************************         
IWK      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ERRNUM,0                                                         
*                                                                               
         CLI   ENTRYS,0            SET DEFAULT VALUES IF NOT INPUT              
         BNE   *+10                                                             
         MVC   CICITOT,=H'100'     DEFAULT NUM OF ENTRYS                        
         CLI   CISIZE,0                                                         
         BNE   *+10                                                             
         MVC   CITRKS,=H'1'        DEFAULT TRACKS PER CI                        
         CLI   BLKSIZE,0                                                        
         BNE   *+10                                                             
         MVC   CIBLKLN,=H'13680'   DEFAULT BLOCK SIZE                           
         CLI   OENTRYS,0                                                        
         BNE   *+16                                                             
         MVC   CJCITOT(14),=X'000000007FFF7FFF000000010100'                     
         XC    CJCITOT+14(6),CJCITOT+14                                         
         MVI   CIRSNF,1            SET RSN IS REL POSN IN INDEX                 
         TM    DSPNDX,X'02'                                                     
         BZ    *+8                                                              
         OI    CIRSNF,X'02'        SET INDEX RECS IN DATA SPACE                 
*                                                                               
         MVC   FIWRES,WRKFID                                                    
         TM    LOCK,YES                                                         
         BZ    IWK010                                                           
         SAM31                                                                  
         BRAS  RE,FIRFLOCK         LOCK WORKER FILE                             
         SAM24                                                                  
*                                                                               
IWK010   SR    RE,RE               SET BLOCK SIZE MOD ENTRY SIZE                
         LH    RF,CIBLKLN                                                       
         LA    R0,L'W_INDEX                                                     
         DR    RE,R0                                                            
         MR    RE,R0                                                            
         STH   RF,CIBLKLN                                                       
*                                                                               
         XC    P1(24),P1           CALC BLOCKS PER TRACK                        
         MVC   P1,=A(DARPT)                                                     
         MVC   P3+2(2),CIBLKLN                                                  
         MVC   P4,WRKFDTF                                                       
         GOTO1 VDATAMGR,P0,DADDS                                                
         LH    RF,P3+2                                                          
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                DIE IF BLOCK TOO BIG FOR TRACK               
         STH   RF,CIHIREC                                                       
         MH    RF,CITRKS                                                        
         ST    RF,FULL             FULL=RECORDS PER CI                          
*                                                                               
         SR    RE,RE               CALC ENTRYS PER RECORD                       
         LH    RF,CIBLKLN                                                       
         LA    R0,L'W_INDEX                                                     
         DR    RE,R0                                                            
         STH   RF,CIENTRYS                                                      
*                                                                               
         LH    R0,CIENTRYS         OPTIMISE PART 1 NUMBER OF CI'S               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         DR    RE,R0                                                            
         CHI   RE,10                                                            
         BNL   *+12                                                             
         MR    RE,R0                                                            
         BCTR  RF,0                                                             
         STCM  RF,3,CICITOT                                                     
         SR    RE,RE               OPTIMISE PART 2 NUMBER OF CI'S               
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         OC    CJCITOT,CJCITOT                                                  
         BZ    IWK050                                                           
         LA    RF,1(RF)                                                         
         AH    RF,CJCITOT                                                       
         DR    RE,R0                                                            
         CHI   RE,10                                                            
         BNL   IWK030                                                           
         MR    RE,R0                                                            
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         ICM   RE,3,CICITOT                                                     
         SR    RF,RE                                                            
         STH   RF,CJCITOT                                                       
*                                                                               
IWK030   SR    RE,RE               CALC PAGE/ENTRY OF PART 2 INDEX              
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         LA    RF,1(RF)                                                         
         DR    RE,R0                                                            
         STH   RF,CJPAGE                                                        
         STH   RE,CJENTRY                                                       
         SR    RE,RE               CALC DISK ADDR OF PART 2 INDEX               
         LH    R0,CIHIREC                                                       
         DR    RE,R0                                                            
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         STH   RF,CJNDXADR                                                      
         STC   RE,CJNDXADR+2                                                    
         MVI   CJNDXADR+3,0                                                     
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,CICITOT        CALC DISK ADDR OF PART 2 CI                  
         MH    RE,CITRKS                                                        
         LA    RE,1(RE)                                                         
         STCM  RE,3,CJSTTRK                                                     
*                                                                               
IWK050   LH    R0,CIENTRYS         CALC NUM OF INDEX PAGES                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,CICITOT                                                     
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LA    RF,1(RF)                                                         
         AH    RF,CJCITOT                                                       
         DR    RE,R0                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         STH   RF,CIPAGES                                                       
*                                                                               
         SR    RE,RE               CALC NUM OF CI'S TO HOLD INDEX               
         L     R0,FULL                                                          
         DR    RE,R0                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         CH    RF,CINDXMIN         CHECK FOR MINIMUM                            
         BNL   *+8                                                              
         LH    RF,CINDXMIN                                                      
         STH   RF,CICINDX                                                       
         MH    RF,CITRKS                                                        
         LA    RF,1(RF)                                                         
         STH   RF,CIFDTRK          SET TRACK NUM OF FIRST DATA CI               
         MVC   CFWFXID,EXTID       SET WRKF EXTERNAL ID (T/A)                   
*                                                                               
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         MVC   P1,=A(WTCKD)                                                     
         MVC   P2,ACXREC                                                        
         MVC   P3+2(2),CIBLKLN                                                  
         MVC   P4,WRKFDTF                                                       
         LA    R0,P6                                                            
         ST    R0,P5                                                            
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,CICITOT        R3=NUM OF ACTIVE INDEX ENTRYS                
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LA    R3,1(R3)                                                         
         AH    R3,CJCITOT                                                       
         LH    R4,CICINDX                                                       
         MH    R4,CITRKS                                                        
         MH    R4,CIHIREC          R4=NUM OF INDEX RECS                         
         XC    FULL,FULL           SET DUB TO PAGE/ENTRY OF END PART 1          
         MVC   DUB(2),=X'7FFF'                                                  
         OC    CJCITOT,CJCITOT                                                  
         BZ    IWK060                                                           
         MVC   DUB(2),CJPAGE                                                    
         LH    RF,CJENTRY                                                       
         AHI   RF,-1                                                            
         STH   RF,DUB+2                                                         
         BNM   IWK060                                                           
         LH    RF,CJPAGE                                                        
         BCTR  RF,0                                                             
         STH   RF,DUB                                                           
         LH    RF,CIENTRYS                                                      
         BCTR  RF,0                                                             
         STH   RF,DUB+2                                                         
*                                                                               
IWK060   L     R5,ACXREC           WRITE INDEX RECORDS LOOP                     
         LH    R6,CIENTRYS         R6=NUM OF 00 ENTRYS                          
         SR    R7,R7               R7=NUM OF FF ENTRYS                          
         CR    R3,R6                                                            
         BL    *+10                                                             
         SR    R3,R6               FULL INDEX PAGE                              
         B     IWK070                                                           
         LTR   R3,R3                                                            
         BNZ   *+14                                                             
         SR    R6,R6               EMPTY INDEX PAGE                             
         LH    R7,CIENTRYS                                                      
         B     IWK070                                                           
         LR    R6,R3               PARTIAL INDEX PAGE                           
         LH    R7,CIENTRYS                                                      
         SR    R7,R3                                                            
         SR    R3,R3                                                            
*                                                                               
IWK070   LTR   R6,R6                                                            
         BZ    IWK080                                                           
         XC    0(L'W_INDEX,R5),0(R5)                                            
         LA    R5,L'W_INDEX(R5)                                                 
         BCT   R6,*-10                                                          
IWK080   LTR   R7,R7                                                            
         BZ    IWK090                                                           
         MVC   0(L'W_INDEX,R5),=32X'FF'                                         
         LA    R5,L'W_INDEX(R5)                                                 
         BCT   R7,*-10                                                          
*                                                                               
IWK090   OC    FULL(2),FULL        SET FIRST PAGE DATA                          
         BNZ   IWK100                                                           
         L     R5,ACXREC                                                        
         MVC   0(20,R5),CIDATA                                                  
         MVC   L'W_INDEX(20,R5),CIDATA+20                                       
         OC    CJCITOT,CJCITOT     SET PART 2 INDEX PRESENT                     
         BZ    IWK110                                                           
         OI    0(R5),X'80'                                                      
IWK100   CLC   FULL(2),DUB         SET END OF PART 1 INDEX                      
         BNE   IWK110                                                           
         LH    R5,DUB+2                                                         
         MH    R5,CINDXLN                                                       
         A     R5,ACXREC                                                        
         MVC   0(L'W_INDEX,R5),=32X'FF'                                         
*                                                                               
IWK110   GOTO1 VDATAMGR,P0,DADDS                                                
         OC    P3(2),P3                                                         
         BNZ   IWK120                                                           
         LH    RE,FULL                                                          
         LA    RE,1(RE)                                                         
         STH   RE,FULL                                                          
         BCT   R4,IWK060                                                        
         B     IWKX                                                             
*                                                                               
IWK120   MVI   ERRNUM,1            SET END OF FILE                              
         TM    P3+1,X'04'                                                       
         BO    *+8                                                              
         MVI   ERRNUM,2            SET DISK ERROR                               
*                                                                               
IWKX     TM    LOCK,YES                                                         
         JZ    XIT                                                              
         SAM31                                                                  
         BRAS  RE,FIRFUNLK         UNLOCK WORKER FILE                           
         SAM24                                                                  
         J     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO PRINT CONTROL INTERVAL DATA FOR WRKF FILE                          
***********************************************************************         
WKOUT    NTR1  BASE=*,LABEL=*                                                   
         L     R7,=A(WKOUTA)                                                    
*                                                                               
         LH    R0,CIBLKLN                                                       
         BAS   R5,WKOUT2+4         RECORD LENGTH                                
         LH    R0,CIHIREC                                                       
         BAS   R5,WKOUT2           RECS PER TRACK                               
         LH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         BAS   R5,WKOUT2           TRKS FOR INDEX                               
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         BAS   R5,WKOUT2           TRKS FOR PART1                               
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         MH    R0,CJTRKS                                                        
         BAS   R5,WKOUT2           TRKS FOR PART2                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LH    R0,CITRKS                                                        
         BAS   R5,WKOUT2           TRKS/INDEX CI                                
         LH    R0,CITRKS                                                        
         BAS   R5,WKOUT2           TRKS/PART1 CI                                
         LH    R0,CJTRKS                                                        
         BAS   R5,WKOUT2           TRKS/PART2 CI                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LH    R0,CICINDX                                                       
         BAS   R5,WKOUT2           NUM OF INDEX CIS                             
         SR    R0,R0                                                            
         ICM   R0,3,CICITOT                                                     
         SH    R0,CICINDX                                                       
         BAS   R5,WKOUT2           NUM OF PART1 CIS                             
         SR    R0,R0                                                            
         ICM   R0,3,CJCITOT                                                     
         BAS   R5,WKOUT2           NUM OF PART2 CIS                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LH    R0,CIENTRYS                                                      
         BAS   R5,WKOUT2           INDEX ENTRYS/REC                             
         LH    R0,CIPAGES                                                       
         BAS   R5,WKOUT2           INDEX TOTAL RECS                             
         LH    R0,CIPAGES                                                       
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+22                                                             
         LH    R0,CJPAGE                                                        
         OC    CJENTRY,CJENTRY                                                  
         BZ    *+8                                                              
         AHI   R0,-1                                                            
         BAS   R5,WKOUT2           INDEX PART1 RECS                             
         SR    R0,R0                                                            
         OC    CJCITOT,CJCITOT                                                  
         BZ    *+12                                                             
         LH    R0,CIPAGES                                                       
         SH    R0,CJPAGE                                                        
         BAS   R5,WKOUT2           INDEX PART2 RECS                             
         B     WKOUTX                                                           
*                                                                               
WKOUT2   LA    R7,16(R7)           BUMP TO NEXT ALPHA                           
         MVC   P(16),0(R7)                                                      
         EDIT  (R0),(5,P+19),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
         BR    R5                                                               
*                                                                               
WKOUTX   J     XIT                                                              
*                                                                               
WKOUTA   DC    CL16'RECORD LENGTH'                                              
         DC    CL16'RECS PER TRACK'                                             
         DC    CL16'TRKS FOR INDEX'                                             
         DC    CL16'TRKS FOR PART1'                                             
         DC    CL16'TRKS FOR PART2'                                             
         DC    CL16'TRKS/INDEX CI'                                              
         DC    CL16'TRKS/PART1 CI'                                              
         DC    CL16'TRKS/PART2 CI'                                              
         DC    CL16'NUM OF INDEX CIS'                                           
         DC    CL16'NUM OF PART1 CIS'                                           
         DC    CL16'NUM OF PART2 CIS'                                           
         DC    CL16'INDEX ENTRYS/REC'                                           
         DC    CL16'INDEX TOTAL RECS'                                           
         DC    CL16'INDEX PART1 RECS'                                           
         DC    CL16'INDEX PART2 RECS'                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* SHARED MEMORY FILE INDEX ROUTINES                                             
***********************************************************************         
       ++INCLUDE SHFIR                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* GLOBAL PROGRAM CONSTANTS AND VARIABLES                                        
***********************************************************************         
GLOBAL   DS    0F                                                               
*                                                                               
*======================================================================         
* PARAMETER TABLE - LIST OF KEYWORDS AND VALUES                                 
*                                                                               
* XL1    PARM VALUE                                                             
* XL1    PARM DEFAULT VALUE                                                     
* XL1    PARM FLAGS X'80'=REQUIRED,X'40'=LIST,X'20'=ROUT,X'01'=SINGLE           
* XL1    PARM MIN LEN                                                           
* CL8    PARM KEYWORD NAME                                                      
* AL4    PARM VALUE LIST                                                        
*======================================================================         
         DS    0F                                                               
PARMTBL  DS    0CL16                                                            
MODE     DC    X'0000C004',C'MODE    ',A(MODEL)                                 
MSG      DC    X'00024001',C'MSG     ',A(MSGL)                                  
WARN     DC    X'00024001',C'WARNINGS',A(WARNL)                                 
WKID     DC    X'00002001',C'WKID    ',A(VWKID)                                 
WRITE    DC    X'00024001',C'WRITE   ',A(WRITEL)                                
LOCK     DC    X'00024001',C'LOCK    ',A(LOCKL)                                 
ENTRYS   DC    X'00002001',C'ENTRYS  ',A(VNCIS)                                 
CISIZE   DC    X'00002001',C'CISIZE  ',A(VTRKS)                                 
BLKSIZE  DC    X'00002004',C'BLKSIZE ',A(VBLKS)                                 
OENTRYS  DC    X'00002001',C'OENTRYS ',A(VNCIS)                                 
OCISIZE  DC    X'00002001',C'OCISIZE ',A(VTRKS)                                 
DSPNDX   DC    X'00014001',C'DSPNDX  ',A(DSPNDXL)                               
FILE     DC    X'00002005',C'FILEID  ',A(VFILE)                                 
         DC    X'00002005',C'DAFILE  ',A(VFILE)                                 
EXTID    DC    X'00004001',C'EXTID   ',A(EXTIDL)                                
PARMTBLX DC    X'FFFF'                                                          
*                                                                               
LOADNAME DC    CL8' '                                                           
*                                                                               
FILEID   DC    CL144' ',X'FF'      MAX 0F 16 WRKF FILES                         
FILEIX   DC    CL144' ',X'FF'                                                   
WRKFINP  DC    XL17'00',X'FF'                                                   
         EJECT                                                                  
                                                                                
*======================================================================         
* PROGRAM AREA STORAGE                                                          
*======================================================================         
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
PACKED   DS    PL6                                                              
*                                                                               
ERRNUM   DS    F                                                                
ACOMRG   DS    A                                                                
ALOADPT  DS    A                                                                
AQBNEXT  DS    A                                                                
SAVERE   DS    A                                                                
*                                                                               
MAXSEQ   DC    F'65000'            MAXIMUM SEQUENCE NUM FOR USER ID             
FRSTTIME DC    C'Y'                                                             
UPSIVAL  DC    X'00'                                                            
UPSIINP  DC    X'00'                                                            
*                                                                               
TODAY4   DS    0CL8                                                             
DATEIPL  DC    CL8' '              FORMAT-4 C'DD/MM/YY'                         
TODAY0   DS    0CL6                                                             
DATEYMD  DC    CL8' '              FORMAT-0 C'YYMMDD'                           
TODAY2   DS    0XL2                                                             
DATECPR  DC    XL2'00'             FORMAT-2 B'YYYYYYYMMMMDDDDD'                 
TIMEINC  DC    XL1'00'                                                          
         DC    XL1'00'                                                          
*                                                                               
P0       DS    F                                                                
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
PARAM0   DS    F                                                                
PARAM1   DS    F                                                                
PARAM2   DS    F                                                                
PARAM3   DS    F                                                                
PARAM4   DS    F                                                                
PARAM5   DS    F                                                                
PARAM6   DS    F                                                                
*                                                                               
PARM     DS    6F                                                               
DMCB     DS    6F                                                               
*                                                                               
         DS    0F                                                               
INDEX    DS    0XL66               INDEX ENTRY                                  
NXSRCID  DS    XL2                                                              
NXSYSPG  DS    CL3                                                              
NXSUBPG  DS    CL1                                                              
NXDAY    DS    CL1                                                              
NXCLASS  DS    CL1                                                              
NXFILENO DS    XL2                                                              
NXTYPE   DS    XL1                                                              
NXATTB   DS    XL1                                                              
NXSTAT   DS    XL1                                                              
NXSEQ    DS    XL1                                                              
NXAGES   DS    XL1                                                              
NXAGELD  DS    XL2                                                              
NXUDATA  DS    XL2                                                              
NXAGERD  DS    XL2                                                              
NXAGERT  DS    XL1                                                              
NXAGELT  DS    XL2                                                              
*                                                                               
         DS    XL2                                                              
NXINFO   DS    XL2                                                              
NXFILNOX DS    XL2                                                              
NXCIADDR DS    XL2                                                              
NXFLAG   DS    X                                                                
         DS    X                                                                
NXUSRINF DS    CL8                                                              
         DS    CL24                                                             
*                                                                               
SAVE     DS    CL256                                                            
WORK     DS    CL256                                                            
OPERANS  DS    CL8                                                              
CARD     DS    CL80                                                             
*                                                                               
RDID     EQU   01                                                               
WTCKD    EQU   05                                                               
DACLOSE  EQU   15                                                               
DARPT    EQU   16                                                               
VDATAMGR DC    V(DATAMGR)                                                       
*                                                                               
DADDS    DC    C'DADDS   '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMPRINT  DC    C'DMPRINT '                                                      
DMWRT    DC    C'DMWRT   '                                                      
WRKFIL   DC    C'WRKFIL  '                                                      
SEQ      DC    C'SEQ     '                                                      
ADD      DC    C'ADD     '                                                      
*                                                                               
AWRKFXPE DC    A(0)                                                             
AWRKFLST DC    A(0)                                                             
WRKFMAX  DC    AL1(0)                                                           
         DC    AL1(0)                                                           
WRKFINT  DC    AL1(0)                                                           
WRKFEXT  DC    AL1(0)                                                           
WRKFID   DC    CL8' '                                                           
WRKFDTF  DC    A(0)                                                             
*                                                                               
FFS      DC    8X'FF'                                                           
*                                                                               
ACIREC   DS    A                                                                
ACXREC   DS    A                                                                
AREPTAB  DS    A                                                                
AWRKFINP DS    A                                                                
*                                                                               
CINDXMIN DC    H'2'                                                             
*                                                                               
UPSITAB  DC    X'8040201008040201'                                              
*                                                                               
       ++INCLUDE DMWRKFW                                                        
       ++INCLUDE SHFIW                                                          
*                                                                               
SOFLAB   DS    0CL8                                                             
         DC    C'*SOFSOF*'                                                      
EOFLAB   DS    0CL8                                                             
         DC    C'*EOFEOF*'                                                      
*                                                                               
DOTS     DC    16C'.'                                                           
ZEROS    DC    16C'0'                                                           
*                                                                               
PLXTRN   DC    A(CIREC),A(0),A(0),A(0)                                          
         DC    V(PRINTER),V(CPRINT)                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
         DC    8X'FF',C'*REPTAB*',8X'FF',C'*REPTAB*'                            
REPTAB   DS    50XL32                                                           
         DC    8X'FF'                                                           
*                                                                               
         DS    0D                                                               
         DC    C'CICICICI'                                                      
CIREC    DS    14336C                                                           
*                                                                               
         DS    0D                                                               
         DC    C'CXCXCXCX'                                                      
CXREC    DS    14336C                                                           
*                                                                               
         DS    0D                                                               
         DC    C'WKWKWKWK'                                                      
WKWORK   DS    4000D                                                            
*                                                                               
CTREC    DS    4096C                                                            
PARMCARD DS    CL80                                                             
*                                                                               
INFO0    DC    CL60'---------------'                                            
INFO1    DC    CL60'WORKER FILE MAINTENANCE'                                    
INFO2    DC    CL60'PARAMETER CARDS'                                            
INFO3    DC    CL60'ACTION MESSAGES'                                            
INFO4    DC    CL60'INITIALISED XXXXX'                                          
*                                                                               
ERRMSG1  DC    CL60'ERROR MISSING PARAMETER - '                                 
ERRMSG2  DC    CL60'ERROR INVALID PARAMETER CARD SYNTAX'                        
ERRMSG3  DC    CL60'ERROR INVALID PARAMETER - '                                 
ERRMSG4  DC    CL60'ERROR INVALID VALUE FOR PARAMETER - '                       
ERRMSG5  DC    CL60'ERROR MUST SPECIFY A SINGLE WRKF ONLY'                      
*                                                                               
ERRMSGA  DC    CL60'ERROR WRKFL DISK END OF FILE'                               
ERRMSGB  DC    CL60'ERROR WRKFL DISK WRITE ERROR'                               
ERRMSGC  DC    CL60'ERROR WRKFL DISK READ ERROR'                                
ERRMSGD  DC    CL60'ERROR WRKFL INVALID CI DATA'                                
                                                                                
*======================================================================         
* LISTS OF PARAMETER VALUES AND EQUATES                                         
*======================================================================         
NO       EQU   X'01'                                                            
YES      EQU   X'02'                                                            
*                                                                               
MODEL    DC    X'01',CL7'INIT'                                                  
         DC    X'02',CL7'PRINT'                                                 
         DC    X'03',CL7'REPORT'                                                
         DC    X'04',CL7'COPY'                                                  
MODELX   DC    X'FF'                                                            
*                                                                               
MSGL     DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
MSGLX    DC    X'FF'                                                            
*                                                                               
WARNL    DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
WARNLX   DC    X'FF'                                                            
*                                                                               
WRITEL   DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
WRITELX  DC    X'FF'                                                            
*                                                                               
LOCKL    DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
LOCKLX   DC    X'FF'                                                            
*                                                                               
EXTIDL   DC    C'A',CL7'ADV'                                                    
         DC    C'C',CL7'CSC'                                                    
         DC    C'Q',CL7'FQA'                                                    
*&&US*&& DC    C'R',CL7'REP'                                                    
         DC    C'T',CL7'TST'                                                    
EXTIDLX  DC    X'FF'                                                            
*                                                                               
DSPNDXL  DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
DSPNDXLX DC    X'FF'                                                            
*                                                                               
         DS    0F                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',1024X'00'                          
UTL      DC    F'0',X'01',XL3'00',XL252'00'                                     
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* SUMMARY REPORT LINE                                                           
***********************************************************************         
SMRYLD   DSECT                                                                  
*                                                                               
SMRYL    DS    0CL165                                                           
SLFRSTC  DS    CL1                 FIRST COLUMN FOR BOX                         
SLUSER   DS    CL06                                                             
         DS    CL1                                                              
SLRTOTL  DS    CL12                                                             
         DS    CL1                                                              
SLRECS   DS    CL12                                                             
         DS    CL1                                                              
SLNCI    DS    CL12                                                             
SLMIDL1  DS    CL1                                                              
         DS    CL1                                                              
SLRLIVE  DS    CL12                                                             
         DS    CL1                                                              
SLRDEAD  DS    CL12                                                             
         DS    CL1                                                              
SLRERROR DS    CL12                                                             
SLMIDL2  DS    CL1                                                              
         DS    CL1                                                              
SLRXPRD  DS    CL12                                                             
         DS    CL1                                                              
SLRSEMI  DS    CL12                                                             
         DS    CL1                                                              
SLRRETN  DS    CL12                                                             
SLLASTC  DS    CL1                 LAST COLUMN FOR BOX                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
* IHAASCB                                                                       
         PRINT OFF                                                              
         IHAASCB                                                                
         PRINT ON                                                               
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*DMWRKFD                                                                        
       ++INCLUDE DMWRKFD                                                        
         EJECT                                                                  
*SHFID                                                                          
       ++INCLUDE SHFID                                                          
         EJECT                                                                  
*FATABSDEQU                                                                     
       ++INCLUDE FATABSDEQU                                                     
         EJECT                                                                  
*DMSPACED                                                                       
       ++INCLUDE DMSPACED                                                       
         EJECT                                                                  
*DMWRKRD                                                                        
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
*DMWRKRK                                                                        
       ++INCLUDE DMWRKFK                                                        
         EJECT                                                                  
*DMWRKFL                                                                        
       ++INCLUDE DMWRKFL                                                        
         EJECT                                                                  
*DDDPRINTL                                                                      
       ++INCLUDE DDDPRINTL                                                      
         EJECT                                                                  
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001WKFDEF    04/03/12'                                      
         END                                                                    
