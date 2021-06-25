*          DATA SET DMPRTQXFR  AT LEVEL 019 AS OF 11/16/20                      
*PHASE PRTQXFRA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE SCANNER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE STXITER                                                                
*INCLUDE TIMBER                                                                 
         TITLE 'PRTQXFR - MVS DATA FILE TO/FROM PRTQUE'                         
         PRINT NOGEN                                                            
PRTQXFR  CSECT                                                                  
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,PRTQXFR,RA,R9,R8,WORK=A(PQWORK)                                
         ST    R1,ACOMRG           SAVE MVS SUPV INFO                           
**********************************************************************          
* FIRST PASS OF CARDS                                                           
**********************************************************************          
INIT     GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         JE    *+2                                                              
         CLI   C,C'*'                                                           
         BE    INIT                                                             
         CLC   C(6),=C'DDSIO='                                                  
         BE    INIT10                                                           
         CLC   C(7),=C'DSPACE='                                                 
         BE    INIT12                                                           
         B     INIT20              CONTINUE                                     
*                                                                               
INIT10   L     RE,=V(DDSIO)                                                     
         MVC   0(8,RE),C+6                                                      
         B     INIT                                                             
*                                                                               
         USING SSBD,RE                                                          
INIT12   L     RE,=V(SSB)                                                       
         MVC   SSODSPAC,C+7                                                     
         B     INIT                                                             
*                                                                               
INIT20   L     RE,=A(SSB)          SET DSPACE=A AS DEFAULT                      
         CLI   SSODSPAC,C' '                                                    
         BH    INIT40                                                           
         DROP  RE                                                               
*                                                                               
         LA    R5,DUB              SCAN THE OLD WAY FOR NOW                     
         EXTRACT (5),FIELDS=TIOT                                                
         L     R5,DUB                                                           
         LA    R5,24(R5)                                                        
         SR    R6,R6                                                            
*                                                                               
         L     RE,=A(SSB)          SET DSPACE=                                  
         USING SSBD,RE                                                          
INIT22   CLI   0(R5),0             TEST FOR END OF TIOT TABLE                   
         JNE   INIT24                                                           
         WTO   '<666> **WARNING** MISSING DSPACE=, DEFAULT IS A'                
         B     INIT40                                                           
*                                                                               
INIT24   CLC   =C'DSPACE',4(R5)    //DSPACEX DD DUMMY                           
         BNE   *+10                                                             
         MVC   SSODSPAC,10(R5)                                                  
         CLC   =C'ADVPQDD0',4(R5)  TEST DDNAME=ADVPQDD0                         
         BNE   *+8                                                              
         MVI   SSODSPAC,C'A'                                                    
         CLC   =C'CSCPQDD0',4(R5)  TEST DDNAME=CSCPQDD0                         
         BNE   *+8                                                              
         MVI   SSODSPAC,C'C'                                                    
         CLC   =C'FQAPQDD0',4(R5)  TEST DDNAME=FQAPQDD0                         
         BNE   *+8                                                              
         MVI   SSODSPAC,C'Q'                                                    
         CLC   =C'REPPQDD0',4(R5)  TEST DDNAME=REPPQDD0                         
         BNE   *+8                                                              
         MVI   SSODSPAC,C'R'                                                    
         CLC   =C'TSTPQDD0',4(R5)  TEST DDNAME=TSTPQDD0                         
         BNE   *+8                                                              
         MVI   SSODSPAC,C'T'                                                    
         CLI   SSODSPAC,C' '       DID IT GET SET?                              
         BH    INIT40              YES                                          
         IC    R6,0(,R5)           GET LENGTH OF THIS ENTRY                     
         AR    R5,R6                                                            
         B     INIT22                                                           
         DROP  RE                                                               
*                                                                               
INIT40   L     R1,ACOMRG           LOAD MVS SUPV INFO                           
         L     R1,0(R1)                                                         
         LH    R2,0(R1)                                                         
         LTR   R2,R2               R2=L'PARM DATA                               
         BZ    INIT46                                                           
         CHI   R2,8                                                             
         BNH   *+8                                                              
         LA    R2,8                                                             
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         LA    RF,UPSITAB                                                       
INIT42   CLI   0(R1),C'0'                                                       
         BE    INIT44                                                           
         CLI   0(R1),C'1'                                                       
         BNE   INIT46                                                           
         OC    UPSIVAL,0(RF)                                                    
INIT44   LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,INIT42                                                        
*                                                                               
INIT46   MVC   UPSIINP,UPSIVAL     SAVE UPSI                                    
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
INIT50   XC    ALOADPT,ALOADPT     CLEAR EXTERNAL LOAD POINT ADDR               
         MVI   XFLAG,0                                                          
         XC    EXCB(24),EXCB       INITIALISE EXTERNAL PARM LIST                
         L     RE,=V(PRINTER)                                                   
         ST    RE,EXCB+16                                                       
         L     RE,=V(CPRINT)                                                    
         ST    RE,EXCB+20                                                       
         MVI   COPYERR,C'N'                                                     
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
         MVC   COLSMAX(5),=AL1(110,001,000,031,100)                             
         MVI   CHRULT,C' '                                                      
         L     RF,=A(INFO1)        MVS DATA FILE TO PRINT QUEUE                 
         MVC   TITLE(28),0(RF)                                                  
         L     RE,=V(BOXAREA)                                                   
         USING BOXD,RE                                                          
         MVC   BOXWIDTH,=F'110'                                                 
*                                                                               
         L     R1,=A(CIREC)        INITIALISE PRTQ FILE BUFFERS                 
         ST    R1,ACIREC                                                        
         L     R1,=A(CXREC)                                                     
         ST    R1,ACXREC                                                        
         XC    CIDATA,CIDATA                                                    
*                                                                               
         LA    RE,L'PQINDEX                                                     
         STH   RE,CINDXLN                                                       
         MVC   PRTQID,PRTQUE                                                    
         EJECT                                                                  
***********************************************************************         
* READ A SET OF INPUT PARAMETER CARDS                                           
***********************************************************************         
GETPARM  CLI   FRSTTIME,C'X'       WAS LAST SET TERMINATED WITH /* CARD         
         BE    EOJ                 YES EOJ                                      
         BH    *+10                                                             
         ZAP   LINE,=P'99'                                                      
         L     RF,=A(INFO2)        PARAMETER CARDS                              
         BAS   RE,PINFO                                                         
         L     RF,=A(INFO0)        UNDERLINE                                    
         BAS   RE,PINFO                                                         
*                                                                               
GP1      LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         LR    R0,RC               POINT TO THIS CSECT'S CPRINT                 
         L     RF,=A(VALPARM)                                                   
         BASR  RE,RF                                                            
         CLI   ERRNUM,0                                                         
         BNE   GPERR               ERROR FOUND IN CARD                          
*                                                                               
         LA    R4,PARMTBL          CHECK FOR REQUIRED & OPTIONAL PARMS          
GP2      TM    2(R4),X'80'                                                      
         BZ    GP2A                                                             
         CLI   0(R4),0             WAS REQUIRED PARM INPUT                      
         BNE   GP2B                YES                                          
         LA    R1,4(R4)            NO- ERROR                                    
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,1                                                         
         B     GPERR                                                            
GP2A     CLI   0(R4),0             WAS OPTIONAL PARM INPUT                      
         BNE   GP2B                YES                                          
         MVC   0(1,R4),1(R4)       NO- SET DEFAULT VALUE                        
GP2B     LA    R4,L'PARMTBL(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   GP2                                                              
*                                                                               
GP3      CLI   PQID,0              SET DEFAULT PQID IF ONLY ONE FILE            
         BNE   GP4                                                              
         CLI   PRTQMAX,1                                                        
         BNE   GP4                                                              
         MVI   PQID,1              SET DEFAULT INTERNAL PRTQ FILE NUM           
         MVI   PRTQINP,1           SET ONE PRTQ FILE INPUT                      
         OI    PRTQINP+1,X'01'     SET REFERENCED BY PQID=U PARM                
*                                                                               
GP4      CLI   FILE,0              TEST IF ANY FILE RENAMES VIA FILE=           
         BE    GP4X                                                             
         CLC   FILE(1),PRTQINP     NUM RENAMED >= NUM REFERENCED                
         BNL   GP4B                                                             
GP4A     LA    R1,FILE+4           INVALID FILE=                                
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GP4B     LA    R1,PRTQINP+1        POINT TO LIST OF INPUT FILES REF             
GP4C     TM    0(R1),X'0F'         TEST IF REFERENCED                           
         BZ    *+12                NO                                           
         TM    0(R1),X'10'         YES MUST BE RENAMED VIA FILE=                
         BZ    GP4A                                                             
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   GP4C                                                             
GP4X     EQU   *                                                                
*                                                                               
GP5      CLI   MODE,COPY           COPY - SET DEFAULT OUTPUT=                   
         BNE   GP5X                                                             
         MVI   REPLMODE,0                                                       
         CLI   OUTPUT,0                                                         
         BNE   GP5X                                                             
         TM    INPUT,MVS                                                        
         BZ    *+12                                                             
         MVI   OUTPUT,PRTQ                                                      
         B     *+8                                                              
         MVI   OUTPUT,MVS                                                       
GP5X     EQU   *                                                                
*                                                                               
GP6      CLI   OUTPUT,0            OUTPUT ONLY FOR MODE=COPY OR REPLACE         
         BE    GP6B                                                             
         CLI   MODE,COPY                                                        
         BE    GP6B                                                             
         CLI   MODE,REPLACE                                                     
         BE    GP6B                                                             
GP6A     LA    R1,OUTPUT+4         INVALID OUTPUT=                              
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GP6B     TM    INPUT,PRTQ          CANT HAVE INPUT=OUTPUT=PRTQ                  
         BZ    *+12                                                             
         TM    OUTPUT,PRTQ                                                      
         BO    GP6A                                                             
GP6C     TM    INPUT,MVS           CANT HAVE INPUT=OUTPUT=MVS                   
         BZ    *+12                                                             
         TM    OUTPUT,MVS                                                       
         BO    GP6A                                                             
*                                                                               
GP7      TM    USER,X'02'          MUST HAVE SPECIFIC USER ID                   
         BO    GP7X                                                             
         LA    R1,USER+4           INVALID USER=                                
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GP7X     EQU   *                                                                
*                                                                               
GP8      TM    REPORT,X'01'        MUST HAVE SPECIFIC REPORT SUBID              
         BO    GP8B                                                             
GP8A     LA    R1,REPORT+4         INVALID REPORT=                              
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GP8B     TM    INPUT,PRTQ          IF INPUT=PRTQ MUST HAVE XXX,NNNN             
         BZ    GP8C                                                             
         TM    REPORT,X'08'        TEST SPECIFIC SEQ NUM INPUT                  
         BZ    GP8A                                                             
         B     GP8X                                                             
GP8C     CLI   MODE,REPLACE        INPUT=MVS/REPLACE MUST HAVE XXX,NNNN         
         BNE   GP8D                                                             
         TM    REPORT,X'08'        TEST SPECIFIC SEQ NUM INPUT                  
         BZ    GP8A                                                             
         B     GP8X                                                             
GP8D     TM    REPORT,X'08'        INPUT=MVS MUST JUST HAVE REPORT=XXX          
         BNZ   GP8A                                                             
GP8X     OC    ALOADPT,ALOADPT     SET LOAD TO NOP IF NOT USED                  
         BNZ   *+12                                                             
         LA    R1,ANOP                                                          
         ST    R1,ALOADPT                                                       
*                                                                               
GPM9     CLI   MODE,REPLACE        REPLACE - INPUT=MVS,OUTPUT=PRTQ              
         BNE   GPM10                                                            
         CLI   INPUT,0             INPUT DEFAULTS TO MVS                        
         BNE   *+8                                                              
         MVI   INPUT,MVS                                                        
         CLI   OUTPUT,0            OUTPUT DEFAULTS TO PRTQ                      
         BNE   *+8                                                              
         MVI   OUTPUT,PRTQ                                                      
GPM9A    TM    INPUT,MVS           INPUT MUST BE MVS                            
         BNZ   GPM9B                                                            
         LA    R1,INPUT+4          INVALID INPUT=                               
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GPM9B    TM    OUTPUT,PRTQ         OUTPUT MUST BE PRTQ                          
         BNZ   GPM9C                                                            
         LA    R1,OUTPUT+4         INVALID OUTPUT=                              
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     GPERR                                                            
GPM9C    MVI   MODE,COPY           SET MODE=COPY WITH REPLACE OPTION            
         MVI   REPLMODE,1                                                       
*                                                                               
GPM10    B     OPEN                ALL INPUT PARAMS ARE CONSISTANT              
*                                                                               
ANOP     BR    RE                                                               
*                                                                               
GPERR    L     R1,ERRNUM           POINT TO ERROR INFO WORD                     
*                                                                               
GPERR1   CLI   ERRNUM,1            HIGH ORDER BYTE HAS ERR NUM                  
         BNE   GPERR2                                                           
         L     RF,=A(ERRMSG1)      MISSING PARAM                                
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+32(8),0(R1)                                                 
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
         MVC   WORK+32(8),0(R1)                                                 
         B     GPERRA                                                           
*                                                                               
GPERR4   CLI   ERRNUM,4                                                         
         BNE   GPERR5                                                           
         L     RF,=A(ERRMSG4)      INVALID VALUE FOR PARAM                      
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+42(8),0(R1)                                                 
         B     GPERRA                                                           
*                                                                               
GPERR5   CLI   ERRNUM,5                                                         
         BNE   GPERR6                                                           
         L     RF,=A(ERRMSG5)      CAN'T FIND EXTERNAL LOAD=                    
         MVC   WORK(60),0(RF)                                                   
         MVC   WORK+42(8),0(R1)                                                 
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
GPERRB   GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    GPERRX                                                           
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         B     GPERRB                                                           
*                                                                               
GPERRX   SR    RF,RF               EXIT WITH ERRNUM AS RETURN CODE              
         IC    RF,ERRNUM                                                        
         XBASE RC=(RF)                                                          
*                                                                               
EOJ      SR    RF,RF               EXIT WITH RETCODE=8 IF COPY ERROR            
         CLI   COPYERR,C'Y'                                                     
         BNE   *+8                                                              
         LA    RF,8                                                             
         XBASE RC=(RF)                                                          
*                                                                               
GETPARMX EQU   *                                                                
         EJECT                                                                  
* INITIALISE AND OPEN PRTQ FILES AND MVS FILES FOR THIS MODE                    
*                                                                               
OPEN     LA    R1,PARMTBL          POINT TO THIS CSECT'S STORAGE                
         L     RF,=A(OPNPQ)                                                     
         BASR  RE,RF               OPEN ALL PRTQ FILES REFERENCED               
*                                                                               
OPEN2    TM    INPUT,MVS           OPEN INPUT MVS IF SPECIFIED                  
         BZ    OPEN3                                                            
         BAS   RE,OPNIN                                                         
         L     R1,=A(MVSIN)                                                     
         OC    FILETYP,FILETYP     TEST IF TYPE=..... INPUT                     
         BNZ   OPEN2A                                                           
         MVC   FILETYP,=C'FC132'   SET DEFAULT                                  
         MVI   RELINET,QLLTFL+QLLTCC                                            
         MVI   RELINEW,132                                                      
         B     OPEN2D                                                           
*                                                                               
OPEN2A   CLI   FILETYP,C'*'        SET TO F/L OR V/L FROM MVS FILE              
         BNE   OPEN2AX                                                          
         MVI   FILETYP,C'V'                                                     
         TM    36(R1),X'80'        TEST F/L RECORDS                             
         BZ    OPEN2AX                                                          
         MVI   FILETYP,C'F'                                                     
OPEN2AX  CLI   FILETYP,C'F'                                                     
         BNE   *+8                                                              
         OI    RELINET,QLLTFL      SET F/L RECORDS IN PQ REPORT                 
*                                                                               
OPEN2B   CLI   FILETYP+1,C'*'      SET CC CHR FROM INPUT                        
         BNE   OPEN2BX                                                          
         MVI   FILETYP+1,C'N'                                                   
         TM    36(R1),X'02'                                                     
         BZ    OPEN2BX                                                          
         MVI   FILETYP+1,C'C'                                                   
OPEN2BX  CLI   FILETYP+1,C'C'                                                   
         BNE   *+8                                                              
         OI    RELINET,QLLTCC      SET CC CHR IN PQ REPORT                      
*                                                                               
OPEN2C   CLI   RELINEW,0           TEST IF LINE WIDTH SPECIFIED                 
         BNE   OPEN2D                                                           
         MVI   RELINEW,99          SET A DEFAULT FOR V/L                        
         CLI   FILETYP,C'F'                                                     
         BNE   OPEN2D                                                           
         LH    RF,82(R1)           GET RECORD LENGTH                            
         CH    RF,=H'255'                                                       
         BNH   OPEN2C1                                                          
         MVC   P(40),=CL40'WARNING - MAX F/L RECORD IS 255 BYTES'               
         GOTO1 =V(PRINTER)                                                      
         LH    RF,=H'255'                                                       
OPEN2C1  STC   RF,RELINEW          SET PQ F/L RECORD TO SAME AS INPUT           
*                                                                               
OPEN2D   EQU   *                                                                
*                                                                               
OPEN3    TM    OUTPUT,MVS          OPEN OUTPUT MVS IF SPECIFIED                 
         BZ    OPEN4                                                            
         BAS   RE,OPNOUT                                                        
*                                                                               
OPEN4    GOTO1 =V(PRINTER)                                                      
         L     RF,=A(INFO3)        ACTION MESSAGES                              
         BAS   RE,PINFO                                                         
         L     RF,=A(INFO0)        UNDERLINE                                    
         BAS   RE,PINFO                                                         
         MVC   DUB1,PRTQUE         SET PRTQ FILE IN TITLE                       
         MVC   DUB2,DUB1                                                        
         CLI   PRTQINP,1           TEST ONLY ONE PRTQ FILE REFERENCED           
         BNE   *+16                                                             
         MVC   DUB1(7),FILEIX                                                   
         MVC   DUB2(7),FILEID                                                   
         MVC   TITLE+29(5),DUB1                                                 
         CLC   DUB1(7),DUB2                                                     
         BE    *+14                                                             
         MVI   TITLE+34,C'='                                                    
         MVC   TITLE+35(7),DUB2                                                 
*                                                                               
OPEN5    CLI   MODE,PRNT           GO TO ROUTINE FOR MODE                       
         BE    PRTPQ                                                            
         CLI   MODE,COPY                                                        
         BE    CPYPQ                                                            
         DC    H'0'                                                             
         EJECT                                                                  
**********************************************************************          
* COPY REPORTS FROM PRTQ/MVS TO MVS/PRTQ                             *          
**********************************************************************          
CPYPQ    GOTO1 ALOADPT,EXCB,0      INIT CALL FOR EXTERNAL                       
*                                                                               
         XC    COPYCTRS,COPYCTRS   CLEAR COPY COUNTERS                          
         XC    INDEX,INDEX                                                      
         LA    RE,PRTQINP          SET NEXT LOCN IN PRTQ INPUT LIST             
         ST    RE,APRTQINP                                                      
         L     R2,AQH              R2=A(RECORD LENGTH HEADER)                   
         L     R3,AQ                                                            
         USING PQPLD,R3            R3=A(REPORT HEADER PRINT LINE)               
         L     R5,ACIREC                                                        
         USING PQRECD,R5           R5=A(CIREC FOR DISK HDR & DATA)              
*                                                                               
CPYPQ0   TM    INPUT,PRTQ          BUMP TO NEXT INPUT PRTQ FILE                 
         BZ    CPYPQ0A                                                          
         L     RE,APRTQINP                                                      
         LA    RE,1(RE)                                                         
         ST    RE,APRTQINP                                                      
         CLI   0(RE),X'FF'         TEST IF END OF INPUT LIST                    
         BE    CPYPQA                                                           
         TM    0(RE),X'0F'         TEST IF PRTQ FILE REFERENCED                 
         BZ    CPYPQ0                                                           
         LA    R0,PRTQINP                                                       
         SR    RE,R0               RE=INTERNAL PRTQ FILE NUM                    
         SLL   RE,3                                                             
         L     RF,APRTQLST         INDEX INTO PRTQ FILE LIST                    
         AR    RF,RE                                                            
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),1(RF)   SET PRTQ FILE ID FOR DATAMGR                 
         MVC   PRTQINT,0(RF)                                                    
         MVC   PRTQEXT,4(RF)                                                    
         MVC   PRTQDTF+1(3),5(RF)                                               
         LA    RF,FILEIX(RE)       SET ORIGINAL DTF NAME                        
         MVC   FILEIX(8),0(RF)                                                  
         LA    RF,FILEID(RE)       SET OVERRIDE DTF NAME                        
         MVC   FILEID(8),0(RF)                                                  
         LA    RF,COPYCTRS(RE)     SET COPY CTRS INDEX FOR PRTQ FILE            
         ST    RF,COPYINDX                                                      
         XC    INDEX,INDEX                                                      
*                                                                               
CPYPQ0A  BAS   RE,GETREPT          GET NEXT REPORT HEADER (USING CXREC)         
         BNZ   CPYPQ0B                                                          
         TM    INPUT,MVS           END OF INPUT MVS FILE                        
         BO    CPYPQA                                                           
         B     CPYPQ0                                                           
*                                                                               
CPYPQ0B  L     RF,TOTLREAD         BUMP TOTAL REPORTS READ                      
         LA    RF,1(RF)                                                         
         ST    RF,TOTLREAD                                                      
         TM    INPUT,MVS           DONT FILTER IF INPUT IS MVS FILE             
         BO    CPYPQ1                                                           
         L     RE,COPYINDX         BUMP THIS PRTQ FILE REPORTS READ             
         L     RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,0(RE)                                                         
*                                                                               
CPYPQ0C  MVC   DUB(2),QLSRCID                                                   
         BAS   RE,TSTUSR           FILTER ON USER ID                            
         BNE   CPYPQ0A                                                          
         MVC   DUB(1),QLCLASS                                                   
         BAS   RE,TSTCLASS         FILTER ON CLASS                              
         BNE   CPYPQ0A                                                          
         MVC   DUB(1),QLSTAT                                                    
         BAS   RE,TSTSTAT          FILTER ON STATUS                             
         BNE   CPYPQ0A                                                          
         CLC   REPSUBID,=C'ALL'    FILTER ON SUBID                              
         BE    *+14                                                             
         CLC   REPSUBID,QLSUBID                                                 
         BNE   CPYPQ0A                                                          
         CLI   REPFLAG,0           TEST IF REPORT DATE/TIME/NUM INPUT           
         BE    CPYPQ0X                                                          
         CLI   REPFLAG,X'80'       TEST IF CREATE DATE ONLY INPUT               
         BNE   CPYPQ0D                                                          
         CLC   QLAGELD,REPCDATE    CREATED DATE MUST MATCH INPUT DATE           
         BNE   CPYPQ0A                                                          
         B     CPYPQ0X                                                          
CPYPQ0D  TM    REPFLAG,X'01'       TEST IF INDIVIDUAL REPORT                    
         BZ    CPYPQ0E                                                          
         CLC   QLREPNO,REPSEQL                                                  
         BNE   CPYPQ0A                                                          
         B     CPYPQ0X                                                          
CPYPQ0E  TM    REPFLAG,X'04'       TEST IF TIME1-TIME2 INPUT                    
         BNO   CPYPQ0F                                                          
         CLC   QLAGELD,REPCDATE    CREATED DATE MUST MATCH INPUT DATE           
         BNE   CPYPQ0A                                                          
         CLC   QLAGELT,REPSEQL     TIME MUST BE IN RANGE                        
         BL    CPYPQ0A                                                          
         CLC   QLAGELT,REPSEQH                                                  
         BH    CPYPQ0A                                                          
         B     CPYPQ0X                                                          
CPYPQ0F  TM    REPFLAG,X'02'       TEST IF ONLY ONE TIME                        
         BZ    CPYPQ0G                                                          
         CLC   QLAGELD,REPCDATE                                                 
         BH    CPYPQ0A             IGNORE IF CREATED ON LATER DATE              
         BL    CPYPQ0X             OK IF CREATED ON PREVIOUS DATE               
         CLC   QLAGELT,REPSEQH                                                  
         BH    CPYPQ0A                                                          
         B     CPYPQ0X                                                          
CPYPQ0G  DC    H'0'                DIE IF UNKNOWN SITUATION                     
CPYPQ0X  EQU   *                                                                
*                                                                               
CPYPQ1   TM    INPUT,PRTQ          GET FULL EXTENDED REPORT HEADER              
         BO    CPYPQ1A                                                          
         MVC   0(128,R5),QLINDEX   MVS ALREADY HAS IT - MOVE TO BUFF            
         B     CPYPQ2                                                           
CPYPQ1A  MVC   SAVE(32),QLINDEX                                                 
         LR    RF,R5               COPY CXREC DISK BUFFER TO CIREC              
         L     RE,ACXREC                                                        
         LH    R1,=H'14336'                                                     
         MOVE  ((RF),(R1)),(RE)                                                 
         MVI   NXFLAG,X'C0'                                                     
         GOTO1 VDATAMGR,DMCB,=C'SEQ',PRTQID,INDEX,(R3),(R5)                     
         CLI   8(R1),0                                                          
         BNE   CPYPQ9                                                           
*                                                                               
CPYPQ2   MVC   SAVE(128),QLINDEX   SAVE REPORT DATA                             
         NOP   CPYPQ0A             FILTER ON NON INDEX DATA HERE                
         GOTO1 ALOADPT,EXCB,(X'01',QLINDEX)                                     
         CLC   0(4,R1),FORCEOF     TEST FOR FORCED EOF                          
         BNE   *+12                                                             
         LA    R7,=C'CLOSE'        IGNORE THIS REPORT                           
         B     CPYPQ6A                                                          
*                                                                               
CPYPQ3   TM    OUTPUT,MVS          OPEN PRTQ OUTPUT FILE                        
         BO    CPYPQ4                                                           
         CLI   REPLMODE,0                                                       
         BE    *+8                                                              
         OI    QLFLAG,QLFLKEY                                                   
         GOTO1 VDATAMGR,DMCB1,=C'OPEN',PRTQUE,INDEX,(R3),(R5)                   
         CLI   8(R1),0                                                          
         BNE   CPYPQ9                                                           
         SR    RE,RE                                                            
         IC    RE,QLREPINT         GET PRTQ FILE INTERNAL NUM                   
         SLL   RE,3                                                             
         LA    RF,COPYCTRS(RE)     SET COPY CTRS INDEX FOR PRTQ FILE            
         ST    RF,COPYINDX                                                      
*                                                                               
CPYPQ4   TM    INPUT,MVS           GET NEXT INPUT PRINT LINE                    
         BZ    CPYPQ4B                                                          
         BAS   RE,RIMVS                                                         
         B     CPYPQ5                                                           
*                                                                               
CPYPQ4A  MVC   0(2,R2),LENEOF      SEQ ERR ON INPUT                             
         MVC   0(10,R3),EOFLAB                                                  
         MVC   5(3,R3),=C'ERR'                                                  
         MVC   10(10,R3),0(R3)                                                  
         B     CPYPQ5                                                           
*                                                                               
CPYPQ4B  GOTO1 VDATAMGR,DMCB,=C'SEQ',PRTQID,INDEX,(R3),(R5)                     
         CLI   8(R1),0                                                          
         BNE   CPYPQ4A                                                          
         CLC   0(5,R3),SOFLAB                                                   
         BE    CPYPQ4A                                                          
*                                                                               
CPYPQ5   SR    R7,R7               SET R7 NONZERO IF EOF LABEL                  
         CLC   0(5,R3),EOFLAB                                                   
         BNE   CPYPQ6                                                           
*                                                                               
CPYPQ5A  CLI   XFLAG,C'E'          END REC RETURN                               
         BNE   *+8                                                              
         MVI   EXCB+8,C'Y'                                                      
         GOTO1 ALOADPT,EXCB,(X'FF',AQH) CALL EXTERN WITH EOF                    
         MVC   XFLAG,8(R1)                                                      
         CLI   XFLAG,C'R'          DOES HE WANT ME TO RETURN                    
         BNE   CPYPQ5B                                                          
         MVI   XFLAG,C'E'          SET SPECIAL END REC FLAG                     
         B     CPYPQ6A                                                          
*                                                                               
CPYPQ5B  LA    R7,=C'CLO/JOB'      SET CLOSE JOB                                
         CLI   SOON,2                                                           
         BE    *+8                                                              
         LA    R7,=C'CLOSE'        SET NORMAL EOF                               
         CLC   5(3,R3),2(R3)                                                    
         BE    *+8                                                              
         LA    R7,=C'CLO/PUR'      SET ERROR EOF                                
         B     CPYPQ6A                                                          
*                                                                               
CPYPQ6   CLI   XFLAG,C'R'          FLAG TO Y IF RETURN CALL                     
         BNE   *+8                                                              
         MVI   EXCB+8,C'Y'                                                      
         GOTO1 ALOADPT,EXCB,(X'02',AQH)   PASS RECORD TO EXTERN                 
         MVC   XFLAG,8(R1)                                                      
*                                                                               
         CLC   0(4,R1),FORCEOF     TEST FOR FORCED EOF                          
         BNE   *+12                                                             
         LA    R7,=C'CLOSE'                                                     
         B     CPYPQ6A                                                          
         CLI   0(R1),X'FF'                                                      
         BE    CPYPQ6D                                                          
*                                                                               
CPYPQ6A  TM    OUTPUT,MVS          PUT NEXT OUTPUT PRINT LINE                   
         BZ    CPYPQ6B                                                          
         LTR   R7,R7               TEST IF EOF                                  
         BNZ   CPYPQ8                                                           
         BAS   RE,WOMVS            WRITE TO TAPE                                
         B     CPYPQ6D                                                          
*                                                                               
CPYPQ6B  LTR   R7,R7               WAS THIS EOF                                 
         BNZ   CPYPQ7                                                           
         GOTO1 VDATAMGR,DMCB1,=C'ADD',PRTQUE,INDEX,(R3),(R5)                    
         CLI   8(R1),0                                                          
         BNE   CPYPQ9                                                           
*                                                                               
CPYPQ6D  CLI   XFLAG,C'E'          BACK TO EOF IF E                             
         BE    CPYPQ5A                                                          
         CLI   XFLAG,C'R'                                                       
         BE    CPYPQ6              GO BACK TO EXTERN IF R                       
         B     CPYPQ4                                                           
*                                                                               
CPYPQ7   GOTO1 VDATAMGR,DMCB1,(R7),PRTQUE,INDEX,(R3),(R5)                       
         CLI   8(R1),0                                                          
         BNE   CPYPQ9                                                           
*                                                                               
CPYPQ8   CLC   0(3,R7),=C'CLO'     END OF FILE RECORD                           
         BNE   CPYPQ9                                                           
         L     RF,TOTLCOPY         BUMP TOTAL OF REPORTS COPIED                 
         LA    RF,1(RF)                                                         
         ST    RF,TOTLCOPY                                                      
         L     RE,COPYINDX         BUMP THIS PRTQ FILE REPORTS COPIED           
         L     RF,4(RE)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,4(RE)                                                         
         MVC   SAVE(128),0(R5)     SAVE REPORT HEADER RECORD                    
         TM    INPUT,PRTQ                                                       
         BZ    CPYPQA                                                           
         MVC   SAVE(128),0(R5)     SAVE REPORT HEADER RECORD                    
         B     CPYPQA                                                           
*                                                                               
CPYPQ9   MVI   COPYERR,C'Y'        ERROR IN COPY INDEX                          
         L     RF,=A(ERRMSGE)                                                   
         MVC   P(32),0(RF)                                                      
         MVC   P+6(5),PRTQID                                                    
         GOTO1 =V(HEXOUT),P1,SAVE,P+32,20,=C'TOG'                               
         LA    R6,P+76                                                          
         MVC   0(06,R6),=C' DMCB='                                              
         LA    R6,6(R6)                                                         
         L     RE,DMCB                                                          
         MVC   0(7,R6),0(RE)                                                    
         LA    R6,7(R6)                                                         
         MVI   0(R6),C'/'                                                       
         LA    R6,1(R6)                                                         
         GOTO1 =V(HEXOUT),P1,DMCB+8,(R6),1                                      
         LA    R6,2(R6)                                                         
         GOTO1 =V(PRINTER)                                                      
         CLC   0(10,R3),10(R3)     TEST FOR MVS SEQ ERR                         
         BNE   CPYPQ0A                                                          
*                                                                               
CPYPQA   OC    TOTLCOPY,TOTLCOPY   TEST COPY COUNTER AT EOF                     
         BNZ   CPYPQB                                                           
         L     RF,=A(INFO5)        REPORT NOT FOUND                             
         BAS   RE,PINFO                                                         
         B     CPYPQX                                                           
*                                                                               
CPYPQB   L     RF,=A(INFO6)        TOTAL NNNNN REPORTS COPIED FROM ..           
         TM    INPUT,MVS                                                        
         BO    *+8                                                              
         L     RF,=A(INFO7)                                                     
         MVC   WORK(45),0(RF)                                                   
         L     RF,=A(INFO8)        OUT OF NNNNN REPORTS READ                    
         MVC   WORK+45(35),0(RF)                                                
         MVC   P(80),WORK                                                       
         L     R0,TOTLCOPY         TOTAL REPORTS COPIED COUNT                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+06(5),DUB                                                      
         L     R0,TOTLREAD         TOTAL REPORTS READ COUNT                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+52(5),DUB                                                      
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CPYPQC   TM    INPUT,MVS           REPEAT COUNTS FOR EACH PRTQ FILE             
         BZ    *+10                                                             
         MVC   WORK+45(35),SPACES                                               
         LA    R2,FILEIX+8         R2=A(NEXT PRTQ FILE ID)                      
         LA    R3,COPYCTRS+8       R3=A(NEXT READ/COPY COUNTER PAIR)            
         SR    R1,R1                                                            
         IC    R1,PRTQMAX          R1=MAX NUM OF PRTQ FILES                     
*                                                                               
CPYPQD   OC    0(8,R3),0(R3)       TEST READ/COPY COUNTERS                      
         BZ    CPYPQD2                                                          
         MVC   P(80),WORK                                                       
         MVC   P(5),0(R2)          SET PRTQ FILE ID                             
         L     R0,4(R3)            GET PRTQ COPY COUNT                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+06(5),DUB                                                      
         TM    INPUT,MVS                                                        
         BO    CPYPQD1                                                          
         L     R0,0(R3)            GET PRTQ READ COUNT                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+52(5),DUB                                                      
CPYPQD1  GOTO1 =V(PRINTER)                                                      
CPYPQD2  LA    R2,8(R2)            BUMP TO NEXT PRTQ FILE ID                    
         LA    R3,8(R3)            BUMP TO NEXT COUNTERS                        
         BCT   R1,CPYPQD                                                        
*                                                                               
CPYPQE   MVC   P,SPACES            PRINT REPORT ATTRIBUTES                      
         GOTO1 =V(PRINTER)                                                      
         MVC   0(128,R5),SAVE      RESTORE REPORT HEADER DATA                   
         BAS   RE,PREPATT                                                       
*                                                                               
CPYPQX   TM    OUTPUT,MVS          CLOSE OUTPUT MVS FILE                        
         BZ    *+8                                                              
         BAS   RE,CLSOUT                                                        
         B     GETPARM                                                          
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT A REPORT IN PRINT QUEUE                                      *          
**********************************************************************          
PRTPQ    XC    COPYCTRS,COPYCTRS   CLEAR PRINT COUNTERS                         
         XC    INDEX,INDEX                                                      
         LA    RE,PRTQINP          SET NEXT LOCN IN PRTQ INPUT LIST             
         ST    RE,APRTQINP                                                      
         L     R2,AQH              R2=A(RECORD LENGTH AREA)                     
         L     R3,AQ                                                            
         USING PQPLD,R3            R3=A(REPORT HEADER PRINT LINE)               
         L     R5,ACIREC                                                        
         USING PQRECD,R5           R5=A(CIREC FOR DISK HDR & DATA)              
*                                                                               
PRTPQ0   TM    INPUT,PRTQ          BUMP TO NEXT INPUT PRTQ FILE                 
         BZ    PRTPQ0A                                                          
         L     RE,APRTQINP                                                      
         LA    RE,1(RE)                                                         
         ST    RE,APRTQINP                                                      
         CLI   0(RE),X'FF'         TEST IF END OF INPUT LIST                    
         BE    PRTPQC                                                           
         TM    0(RE),X'0F'         TEST IF PRTQ FILE REFERENCED                 
         BZ    PRTPQ0                                                           
         LA    R0,PRTQINP                                                       
         SR    RE,R0               RE=INTERNAL PRTQ FILE NUM                    
         SLL   RE,3                                                             
         L     RF,APRTQLST         INDEX INTO PRTQ FILE LIST                    
         AR    RF,RE                                                            
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),1(RF)   SET PRTQ FILE ID FOR DATAMGR                 
         MVC   PRTQINT,0(RF)                                                    
         MVC   PRTQEXT,4(RF)                                                    
         MVC   PRTQDTF+1(3),5(RF)                                               
         LA    RF,FILEIX(RE)       SET ORIGINAL DTF NAME                        
         MVC   FILEIX(8),0(RF)                                                  
         LA    RF,FILEID(RE)       SET OVERRIDE DTF NAME                        
         MVC   FILEID(8),0(RF)                                                  
         XC    INDEX,INDEX                                                      
*                                                                               
PRTPQ0A  BAS   RE,GETREPT          GET NEXT REPORT HEADER                       
         BNZ   PRTPQ0B             NO MORE REPORTS                              
         TM    INPUT,MVS                                                        
         BO    PRTPQC                                                           
         B     PRTPQ0                                                           
PRTPQ0B  L     RF,TOTLREAD                                                      
         LA    RF,1(RF)            BUMP REPORTS READ                            
         ST    RF,TOTLREAD                                                      
*                                                                               
         MVC   DUB(2),QLSRCID                                                   
         BAS   RE,TSTUSR           FILTER ON USER ID                            
         BNE   PRTPQ0A                                                          
         MVC   DUB(1),QLCLASS                                                   
         BAS   RE,TSTCLASS         FILTER ON CLASS                              
         BNE   PRTPQ0A                                                          
         MVC   DUB(1),QLSTAT                                                    
         BAS   RE,TSTSTAT          FILTER ON STATUS                             
         BNE   PRTPQ0A                                                          
         CLC   REPSUBID,=C'ALL'    FILTER ON SUB ID                             
         BE    *+14                                                             
         CLC   REPSUBID,QLSUBID                                                 
         BNE   PRTPQ0A                                                          
         CLI   REPFLAG,0           TEST IF REPORT DATE/TIME/NUM INPUT           
         BE    PRTPQ0X                                                          
         CLI   REPFLAG,X'80'       TEST IF CREATE DATE ONLY INPUT               
         BNE   PRTPQ0D                                                          
         CLC   QLAGELD,REPCDATE    CREATED DATE MUST MATCH INPUT DATE           
         BNE   PRTPQ0A                                                          
         B     PRTPQ0X                                                          
PRTPQ0D  TM    REPFLAG,X'01'       TEST IF INDIVIDUAL REPORT                    
         BZ    PRTPQ0E                                                          
         CLC   QLREPNO,REPSEQL                                                  
         BNE   PRTPQ0A                                                          
         B     PRTPQ0X                                                          
PRTPQ0E  TM    REPFLAG,X'04'       TEST IF TIME1-TIME2 INPUT                    
         BNO   PRTPQ0F                                                          
         CLC   QLAGELD,REPCDATE    CREATED DATE MUST MATCH INPUT DATE           
         BNE   PRTPQ0A                                                          
         CLC   QLAGELT,REPSEQL     TIME MUST BE IN RANGE                        
         BL    PRTPQ0A                                                          
         CLC   QLAGELT,REPSEQH                                                  
         BH    PRTPQ0A                                                          
         B     PRTPQ0X                                                          
PRTPQ0F  TM    REPFLAG,X'02'       TEST IF ONLY ONE TIME                        
         BZ    PRTPQ0G                                                          
         CLC   QLAGELD,REPCDATE                                                 
         BH    PRTPQ0A             IGNORE IF CREATED ON LATER DATE              
         BL    PRTPQ0X             OK IF CREATED ON PREVIOUS DATE               
         CLC   QLAGELT,REPSEQH                                                  
         BH    PRTPQ0A                                                          
         B     PRTPQ0X                                                          
PRTPQ0G  DC    H'0'                DIE IF UNKNOWN SITUATION                     
PRTPQ0X  EQU   *                                                                
*                                                                               
PRTPQ1   TM    INPUT,MVS           GET FULL EXTENDED REPORT HEADER              
         BZ    PRTPQ1A                                                          
         MVC   0(128,R5),QLINDEX   HAVE IT FOR MVS SO MOVE TO BUFF              
         B     PRTPQ2                                                           
PRTPQ1A  MVC   SAVE(32),QLINDEX    SAVE INDEX ENTRY                             
         LR    RF,R5               COPY CXREC DISK BUFFER TO CIREC              
         L     RE,ACXREC                                                        
         LH    R1,=H'14336'                                                     
         MOVE  ((RF),(R1)),(RE)                                                 
         MVI   NXFLAG,X'C0'                                                     
         GOTO1 VDATAMGR,DMCB,=C'SEQ',PRTQID,INDEX,(R3),(R5)                     
         CLI   8(R1),0                                                          
         BE    *+6                 NOW HAVE IT FOR PRTQ                         
         DC    H'0'                                                             
*                                                                               
PRTPQ2   MVC   SAVE(128),QLINDEX   SAVE REPORT DATA                             
         NOP   PRTPQ0A             FILTER ON NON INDEX DATA HERE                
PRTPQ2A  OC    TOTLCOPY,TOTLCOPY   SET ACTION MSG IF FIRST TIME                 
         BNZ   PRTPQ3                                                           
         L     RF,=A(INFO9)        REPORT DESCRIPTION AND DATA ....             
         BAS   RE,PINFO                                                         
*                                                                               
PRTPQ3   MVC   P,SPACES            PRINT REPORT ATTRIBUTES                      
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PREPATT                                                       
*                                                                               
PRTPQ3X  MVC   REPUSER(10),SAVE                                                 
         GOTO1 ,DMCB,=C'SEQ',PRTQID,INDEX,(R3),(R5)                             
*                                                                               
PRTPQ4   OC    REPPAGES,REPPAGES   SET NUM OF PAGES TO PRINT                    
         BNZ   *+10                                                             
         MVC   REPPAGES,=X'7FFF'                                                
         XC    CPAGE,CPAGE                                                      
*                                                                               
PRTPQ6   TM    INPUT,MVS           GET NEXT PRINT LINE                          
         BZ    PRTPQ6A                                                          
         BAS   RE,RIMVS                                                         
*                                                                               
         OC    0(4,R2),0(R2)       TEST END OF FILE ON MVS                      
         BNZ   PRTPQ6B                                                          
         DC    H'0'                SEQ ERR ON INPUT MVS                         
PRTPQ6A  GOTO1 VDATAMGR,DMCB,=C'SEQ',PRTQID,INDEX,(R3),(R5)                     
         CLI   8(R1),0                                                          
         BNE   PRTPQA                                                           
PRTPQ6B  CLC   0(5,R3),EOFLAB      TEST FOR EOF LABEL                           
         BE    PRTPQB                                                           
*                                                                               
PRTPQ7   MVI   FLAG,0              SET NORMAL PRINT FLAG                        
         TM    PQLINET,PQLTCC      TEST IF REPORT HAS CC CHR                    
         BZ    PRTPQ8              NO                                           
         LA    RE,PLCC             YES POINT RE TO CC CHR                       
         TM    PQLINET,PQLTFL                                                   
         BO    *+8                                                              
         LA    RE,2(RE)                                                         
         CLI   0(RE),X'89'         TEST FOR NEW PAGE CC                         
         BL    PRTPQ8                                                           
         LH    R1,CPAGE            BUMP PAGE NUMBER COUNTER                     
         LA    R1,1(R1)                                                         
         STH   R1,CPAGE                                                         
         CH    R1,REPPAGES                                                      
         BNH   PRTPQ8                                                           
         MVI   FLAG,1              SET LAST PRINT FLAG                          
*                                                                               
PRTPQ8   BAS   RE,PLINEQ           PRINT LINE DATA                              
         CLI   FLAG,0                                                           
         BE    PRTPQ6                                                           
         B     PRTPQB                                                           
*                                                                               
PRTPQA   TM    8(R1),X'80'         TEST END OF FILE                             
         BO    PRTPQB                                                           
         TM    8(R1),X'40'         TEST DISK ERROR                              
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R2),X'09'                                                      
         MVC   1(12,R2),=C'*DISK*ERROR*'                                        
         LA    R0,9                                                             
         LA    R1,1(R2)                                                         
         MVC   12(12,R1),0(R1)                                                  
         LA    R1,12(R1)                                                        
         BCT   R0,*-10                                                          
         MVI   FLAG,1                                                           
         B     PRTPQ8                                                           
*                                                                               
PRTPQB   L     R1,TOTLCOPY         BUMP NUMBER OF REPORTS PRINTED               
         LA    R1,1(R1)                                                         
         ST    R1,TOTLCOPY                                                      
         B     PRTPQ0A                                                          
*                                                                               
PRTPQC   OC    TOTLCOPY,TOTLCOPY   TEST REPORTS PRINTED AT EOF                  
         BNZ   PRTPQX                                                           
         L     RF,=A(INFO5)        REPORT NOT FOUND                             
         BAS   RE,PINFO                                                         
         B     GETPARM                                                          
*                                                                               
PRTPQX   ZAP   LINE,=P'99'                                                      
         B     GETPARM                                                          
         DROP  R3                                                               
         EJECT                                                                  
* GET/PUT MESSAGE TO OPERATOR AND OPTIONALLY TO PRINTER                         
* MESSAGE IS IN WORK AND REPLY IS RETURNED IN OPERANS                           
*                                                                               
PUTMSG   MVI   DMCB+16,X'01'       SET PUT FLAG                                 
         B     PAGM1                                                            
PUTMSGP  MVI   DMCB+16,X'81'       SET PUT AND PRINT FLAG                       
         B     PAGM1                                                            
GETMSG   MVI   DMCB+16,X'02'       SET GET FLAG                                 
         B     PAGM1                                                            
PAGMSG   MVI   DMCB+16,X'03'       SET PUT AND GET FLAGS                        
         B     PAGM1                                                            
*                                                                               
PAGM1    ST    RE,PGSAVRE                                                       
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
PAGMX    L     RE,PGSAVRE                                                       
         BR    RE                                                               
*                                                                               
PINFO    ST    RE,PGSAVRE          PRINT INFO MSG AT RF                         
         MVC   P(60),0(RF)                                                      
         GOTO1 =V(PRINTER)                                                      
         L     RE,PGSAVRE                                                       
         BR    RE                                                               
         EJECT                                                                  
DATECHK  OC    0(2,R1),0(R1)       VALIDATE CMPRSD DATE FIELD AT R1             
         BNZ   *+12                                                             
         LH    RF,=H'-1'           CC=LOW IF NO DATE                            
         B     DCHKX                                                            
         LA    RF,1                CC=HIGH IF INVALID DATE                      
         SR    R0,R0               UNCOMPRESS DATE INTO DUB                     
         ICM   R0,3,0(R1)                                                       
         SRDL  R0,5                                                             
         SRL   R1,27                                                            
         STC   R1,DUB+2                                                         
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         STC   R1,DUB+1            R1=MONTH                                     
         STC   R0,DUB+0                                                         
         CLI   DUB+0,84            RANGE CHECK YEAR                             
         BL    DCHKX                                                            
         CLI   DUB+0,99                                                         
         BH    DCHKX                                                            
         CLI   DUB+1,01            RANGE CHECK MONTH                            
         BL    DCHKX                                                            
         CLI   DUB+1,12                                                         
         BH    DCHKX                                                            
         CLI   DUB+2,01            RANGE CHECK DAY                              
         BL    DCHKX                                                            
         IC    R1,DCHKT-1(R1)      GET NUMBER OF DAYS IN MONTH                  
         CLI   DUB+1,02                                                         
         BNE   *+16                                                             
         TM    DUB+0,X'03'         TEST FEB IN LEAP YEAR                        
         BNZ   *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,DUB+3                                                         
         CLC   DUB+2(1),DUB+3                                                   
         BH    DCHKX                                                            
         SR    RF,RF               CC=EQUAL IF DATE OK                          
DCHKX    LTR   RF,RF                                                            
         BR    RE                                                               
DCHKT    DC    AL1(31,28,31,30,31,30,31,31,30,31,30,31)                         
*                                                                               
TIMECHK  OC    0(2,R1),0(R1)       VALIDATE TIME FIELD AT R1                    
         BNZ   *+12                                                             
         LH    RF,=H'-1'           CC=LOW IF NO TIME                            
         B     TCHKX                                                            
         LA    RF,1                CC=HIGH IF INVALID TIME                      
         CLI   0(R1),23                                                         
         BH    TCHKX                                                            
         CLI   1(R1),59                                                         
         BH    TCHKX                                                            
         SR    RF,RF               CC=EQUAL IF OK                               
TCHKX    LTR   RF,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
TSTUSR   ST    RE,DUB+4            TEST IF USERID AT DUB(2) IS WANTED           
         LA    RE,REPTAB                                                        
*                                                                               
TSTUSR2  CLC   0(2,RE),=X'FFFF'    SEARCH REPTAB FOR USER                       
         BE    TSTUSR4                                                          
         MVC   DUB+2(2),0(RE)                                                   
         NI    DUB+2,X'7F'                                                      
         CLC   DUB(2),DUB+2                                                     
         BE    TSTUSR6                                                          
         LA    RE,L'REPTAB(RE)                                                  
         B     TSTUSR2                                                          
*                                                                               
TSTUSR4  TM    USER,X'80'          USERID NOT IN TABLE                          
         BZ    TSTUSRN                                                          
         L     RE,AREPALL          USE ALL VALUE IF USER=ALL INPUT              
         B     TSTUSRY                                                          
*                                                                               
TSTUSR6  TM    0(RE),X'80'         USERID IS IN TABLE                           
         BZ    TSTUSRY                                                          
*                                                                               
TSTUSRN  LA    RE,1                CC=NEQ IF USERID NOT WANTED                  
         B     TSTUSRX                                                          
TSTUSRY  MVC   REPUSER(10),0(RE)   CC=EQL IF USERID IS WANTED                   
         OC    REPSUBID,REPSUBID                                                
         BNZ   TSTUSRY1                                                         
         MVC   REPSUBID,=C'ALL'                                                 
TSTUSRY1 MVC   REPCDATE,10(RE)     SET REPORT CREATE DATE                       
         OC    REPCDATE,REPCDATE                                                
         BNZ   *+10                                                             
         MVC   REPCDATE,DATECPR                                                 
         MVC   CLASSL,18(RE)                                                    
         MVI   CLASSL+11,0                                                      
         MVC   REPSTAT,29(RE)                                                   
         MVC   REPPAGES,30(RE)                                                  
         ST    RE,AREPTAB                                                       
         SR    RE,RE                                                            
*                                                                               
TSTUSRX  LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
         BR    RE                                                               
         EJECT                                                                  
TSTCLASS ST    RE,DUB+4            TEST IF CLASS AT DUB(1) IS WANTED            
         CLI   CLASSL,0                                                         
         BE    TSTCLY              NO CLASS SPECIFIED                           
         LA    RE,CLASSL+1                                                      
*                                                                               
TSTCL2   CLI   0(RE),0             SEARCH CLASS LIST                            
         BE    TSTCL4                                                           
         CLC   DUB(1),0(RE)                                                     
         BE    TSTCL6                                                           
         LA    RE,1(RE)                                                         
         B     TSTCL2                                                           
*                                                                               
TSTCL4   CLI   CLASSL,C'+'         CLASS NOT IN LIST                            
         BE    TSTCLN                                                           
         B     TSTCLY                                                           
TSTCL6   CLI   CLASSL,C'+'         CLASS IN LIST                                
         BE    TSTCLY                                                           
         B     TSTCLN                                                           
*                                                                               
TSTCLN   LA    RE,1                CC=NEQ IF CLASS NOT WANTED                   
         B     *+6                                                              
TSTCLY   SR    RE,RE               CC=EQL IF CLASS IS WANTED                    
         LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
         BR    RE                                                               
         EJECT                                                                  
TSTSTAT  ST    RE,DUB+4            TEST IF STATUS AT DUB(1) IS WANTED           
         TM    DUB,PQSTTE                                                       
         BZ    *+12                                                             
         TM    DUB,PQSTAC                                                       
         BZ    TSTSTN                                                           
         CLI   REPSTAT,0                                                        
         BE    TSTSTY                                                           
*                                                                               
TSTST2   MVC   DUB+1(1),REPSTAT                                                 
         TM    DUB+1,PQSTKE                                                     
         BZ    *+12                                                             
         TM    DUB,PQSTKE                                                       
         BZ    TSTSTN                                                           
         NI    DUB+1,255-PQSTKE                                                 
         BZ    TSTSTY                                                           
         NC    DUB+1(1),DUB                                                     
         BZ    TSTSTN                                                           
*                                                                               
TSTSTY   SR    RE,RE               CC=EQL IF STATUS IS WANTED                   
         B     *+8                                                              
TSTSTN   LA    RE,1                CC=NEQ IF STATUS IS NOT WANTED               
         LTR   RE,RE                                                            
         L     RE,DUB+4                                                         
         BR    RE                                                               
         EJECT                                                                  
STATOUT  NTR1                      EXPAND REPORT STATUS TO REPSTATA             
         MVC   REPSTATA,SPACES                                                  
         LA    RE,REPSTATA                                                      
         CLI   CISTAT,0            TEST PURGED STATUS                           
         BNE   *+14                                                             
         MVC   0(2,RE),=C'..'                                                   
         B     STATOUTX                                                         
         MVI   0(RE),C'?'                                                       
*                                                                               
STATOUT1 TM    CISTAT,PQSTAC       MAIN STATUS = ACTV/HOLD/PRTD/SENT            
         BZ    *+12                                                             
         MVI   0(RE),C'A'                                                       
         B     STATOUT2                                                         
*                                                                               
         TM    CISTAT,PQSTHO                                                    
         BZ    *+12                                                             
         MVI   0(RE),C'H'                                                       
         B     STATOUT2                                                         
*                                                                               
         TM    CISTAT,PQSTPR                                                    
         BZ    *+12                                                             
         MVI   0(RE),C'P'                                                       
         B     STATOUT2                                                         
*                                                                               
         TM    CISTAT,PQSTSE                                                    
         BZ    *+12                                                             
         MVI   0(RE),C'S'                                                       
         B     STATOUT2                                                         
*                                                                               
STATOUT2 TM    CISTAT,PQSTKE       SUB STATUS-1 = KEEP                          
         BZ    STATOUTX                                                         
         MVI   1(RE),C'K'                                                       
*                                                                               
STATOUTX B     XIT1                                                             
XIT1     XIT1                                                                   
         EJECT                                                                  
TIMEOUT  MVI   DUB+2,0             EXPAND BINARY TIME IN DUB(2)                 
         SR    R0,R0                                                            
*                                                                               
         IC    R0,DUB              GET BINARY HOUR BYTE                         
         CH    R0,=H'23'                                                        
         BNH   *+10                                                             
         MVI   DUB+2,1             SET INVALID TIME FLAG                        
         SR    R0,R0                                                            
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+3(2),DUB1+6(2)                                               
         MVI   DUB+5,C'.'                                                       
*                                                                               
         IC    R0,DUB+1            GET BINARY MINUTE BYTE                       
         CH    R0,=H'59'                                                        
         BNH   *+10                                                             
         MVI   DUB+2,1             SET INVALID TIME FLAG                        
         SR    R0,R0                                                            
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+6(2),DUB1+6(2)                                               
*                                                                               
         CLI   DUB+2,0             EXIT WITH CC=EQL IF VALID TIME               
         BR    RE                                                               
         EJECT                                                                  
IDOUT    NTR1                      EXPAND REPORT ID TO REPIDA                   
         LA    R4,REPIDA                                                        
         MVC   REPIDA,SPACES                                                    
*                                                                               
IDOUT2   OC    REPUSER,REPUSER     REPORT USER ID                               
         BZ    IDOUT6                                                           
         MVC   USERN,REPUSER                                                    
         BAS   RE,GETUSER                                                       
         MVC   0(10,R4),USERA                                                   
         LA    R4,11(R4)                                                        
*                                                                               
IDOUT6   OC    REPSUBID,REPSUBID   REPORT SUB ID                                
         BZ    IDOUT8                                                           
         MVC   0(3,R4),REPSUBID                                                 
         LA    R4,4(R4)                                                         
*                                                                               
IDOUT8   OC    REPSEQL,REPSEQL     REPORT SEQ NUM                               
         BZ    IDOUTA                                                           
         EDIT  (B2,REPSEQL),(4,(R4))                                            
         LA    R4,5(R4)                                                         
*                                                                               
IDOUTA   OC    REPIDA,SPACES                                                    
         LA    R0,L'REPIDA                                                      
         GOTO1 =V(SQUASHER),DMCB,REPIDA,(C',',(R0))                             
*                                                                               
IDOUTX   B     XIT1                                                             
         EJECT                                                                  
GETUSER  NTR1                      GET USER ALPHA FROM USER NUMBER              
         MVI   DMCB1+8,0                                                        
         MVC   USERA,SPACES                                                     
         L     R5,=A(CTBUF)                                                     
*                                                                               
GUSER1   OC    0(2,R5),0(R5)       SEARCH USER ID TABLE                         
         BZ    GUSER2                                                           
         CLC   0(2,R5),=X'FFFF'                                                 
         BE    GUSER2              END OF TABLE                                 
         CLC   0(2,R5),USERN                                                    
         BE    *+12                                                             
         LA    R5,12(R5)                                                        
         B     GUSER1                                                           
         MVC   USERA,2(R5)         MOVE OUT ALPHA FROM TABLE                    
         B     GUSERX                                                           
*                                                                               
GUSER2   LR    R0,R5               SAVE A(NEXT AVAIL TABLE ENTRY)               
         L     R5,=A(CTREC)                                                     
         XC    0(25,R5),0(R5)      READ CONTROL FILE FOR NUMBER                 
         MVI   0(R5),C'I'                                                       
         MVC   23(2,R5),USERN                                                   
         GOTO1 VDATAMGR,DMCB1,DMREAD,=C'CTFILE',(R5),(R5)                       
         CLI   8(R1),0                                                          
         BNE   GUSER4                                                           
         LA    R5,28(R5)           SEARCH CONTROL RECORD FOR ALPHA              
         SR    R6,R6                                                            
GUSER3   CLI   0(R5),0                                                          
         BNE   *+12                                                             
         MVI   DMCB1+8,1                                                        
         B     GUSER4                                                           
         CLI   0(R5),2                                                          
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     GUSER3                                                           
         MVC   USERA,2(R5)         EXTRACT ALPHA                                
         B     GUSER5                                                           
*                                                                               
GUSER4   LH    R6,USERN            ERROR IN FINDING USER ID NUM                 
         CVD   R6,DUB                                                           
         UNPK  USERA(6),DUB                                                     
         OI    USERA+5,X'F0'                                                    
*                                                                               
GUSER5   LR    R5,R0               SAVE NUM/ALPHA IN TABLE                      
         OC    0(2,R5),0(R5)                                                    
         BNZ   GUSERX              NO MORE ROOM IN TABLE                        
         MVC   0(2,R5),USERN                                                    
         MVC   2(10,R5),USERA                                                   
*                                                                               
GUSERX   CLI   DMCB1+8,0           SET CC=EQL IF USER FOUND OK                  
         B     XIT1                                                             
         EJECT                                                                  
* GET NEXT REPORT FROM PRTQ OR MVS                                              
*                                                                               
GETREPT  NTR1                                                                   
         L     R2,AQH                                                           
         L     R3,AQ                                                            
         USING PQPLD,R3            R3=A(REPORT HEADER PRINT LINE)               
         TM    INPUT,MVS                                                        
         BZ    GETR4                                                            
*                                                                               
GETR2    LA    R0,QLSOFEND-PQPLD+4 SET REPORT HEADER LINE LENGTH                
         SLL   R0,16                                                            
         STCM  R0,15,0(R2)                                                      
         XC    0(256,R3),0(R3)                                                  
         MVI   QLEXTRA,X'FF'                                                    
*                                                                               
GETR3    MVC   QLSRCID,RESRCID     SET SOURCE ID                                
         MVC   QLSUBID,RESUBID     SET REPORT ID                                
         CLI   REPLMODE,0                                                       
         BE    *+10                                                             
         MVC   QLREPNO,REREPNO     SET REPORT NUMBER FOR REPLACE MODE           
         MVC   QLCLASS,RECLASS     SET CLASS                                    
         MVC   QLSTAT,RESTAT       SET STATUS                                   
         MVC   QLTYPE,RETYPE       SET TYPE                                     
         MVC   QLLINET,RELINET     SET CC CHR AND FIXED LENGTH                  
         MVC   QLLINEW,RELINEW     SET LINE WIDTH                               
         MVC   QLRETNL,RERETNL     LIVE RETAIN HOURS                            
         MVC   QLRETND,RERETND     DEAD RETAIN HOURS                            
         MVC   QLFORMS,REFORMS     FORMS CODE                                   
         MVC   QLCHARS,RECHARS     CHARACTER SET CODE                           
         MVC   QLDESC,REDESC       REPORT DESCRIPTION                           
         MVC   QLPSWD,REPSWD       REPORT PASSWORD                              
         MVC   QLTYP1,RETYP1       SET TYPE1 (ARCHIVE STATUS)                   
         B     GETRX                                                            
*                                                                               
GETR4    L     R0,ACXREC           GET NEXT INDEX ENTRY USING CXREC             
         MVI   NXFLAG,0                                                         
         GOTO1 VDATAMGR,DMCB,=C'SEQ',PRTQID,INDEX,(R3),(R0)                     
         CLI   8(R1),0                                                          
         BE    GETRX                                                            
         TM    8(R1),X'80'         TEST AND SET END OF FILE                     
         BZ    *+14                                                             
         XC    0(4,R2),0(R2)                                                    
         B     GETRX                                                            
         DC    H'0'                DISK ERROR ON INDEX                          
*                                                                               
GETRX    OC    0(4,R2),0(R2)       EXIT WITH CC=EQL IF EOF                      
         B     XIT1                                                             
         EJECT                                                                  
* READ INPUT MVS                                                                
*                                                                               
RIMVS    ST    RE,RWSAVRE          READ INPUT MVS INTO QH/Q                     
         L     R1,=A(MVSIN)                                                     
         L     RE,AIO              CLEAR DATA AREA TO SPACES                    
         LA    R0,8                                                             
         MVC   0(128,RE),SPACES                                                 
         LA    RE,128(RE)                                                       
         BCT   R0,*-10                                                          
         L     R1,=A(MVSIN)                                                     
         L     R0,AIO                                                           
         GET   (1),(0)                                                          
         L     R1,=A(MVSIN)                                                     
*                                                                               
RIMVS1   TM    36(R1),X'80'        INPUT FILE HAS F/L RECORDS                   
         BZ    RIMVS2                                                           
         MVC   MVSILEN,82(R1)      SET REC LEN FROM DCB                         
         MVC   MVSIADR,AIO                                                      
         B     RIMVS3                                                           
*                                                                               
RIMVS2   L     RE,AIO              INPUT FILE HAS V/L RECORDS                   
         MVC   MVSILEN,0(RE)       SET REC LEN FROM V/L REC HDR                 
         LH    RF,MVSILEN                                                       
         SH    RF,=H'4'                                                         
         STH   RF,MVSILEN                                                       
         LA    RE,4(RE)                                                         
         ST    RE,MVSIADR                                                       
*                                                                               
RIMVS3   TM    36(R1),X'02'        NO MACHINE CHR ON INPUT RECORD               
         BO    RIMVS4                                                           
         CLI   FILETYP+1,C'C'      TEST IF TYPE=.C REQUESTED                    
         BNE   RIMVS5                                                           
         L     RF,MVSIADR          MOVE IN DEFAULT CC CHR                       
         BCTR  RF,0                                                             
         MVI   0(RF),X'09'                                                      
         ST    RF,MVSIADR                                                       
         LH    RF,MVSILEN                                                       
         LA    RF,1(RF)                                                         
         STH   RF,MVSILEN                                                       
         B     RIMVS5                                                           
*                                                                               
RIMVS4   CLI   FILETYP+1,C'C'      CC CHR AT START OF INPUT RECORD              
         BE    RIMVS4A                                                          
         L     RF,MVSIADR          REMOVE CC CHR FROM START OF RECORD           
         LA    RF,1(RF)                                                         
         ST    RF,MVSIADR                                                       
         LH    RF,MVSILEN                                                       
         BCTR  RF,0                                                             
         STH   RF,MVSILEN                                                       
         B     RIMVS5                                                           
RIMVS4A  TM    52(R1),X'01'        TEST OPTCD=J                                 
         BZ    RIMVS5                                                           
         L     RF,MVSIADR                                                       
         BCTR  RF,0                                                             
         MVC   0(1,RF),2(RF)       MOVE FONT CHR TO LEFT OF REC                 
         MVC   2(1,RF),1(RF)       MOVE CC CHR OVER FONT CHR                    
         LA    RF,2(RF)                                                         
         ST    RF,MVSIADR                                                       
         LH    RF,MVSILEN                                                       
         BCTR  RF,0                                                             
         STH   RF,MVSILEN                                                       
*                                                                               
RIMVS5   TM    36(R1),X'80'        FIXED LENGTH RECORDS INPUT                   
         BZ    RIMVS6                                                           
         CLI   FILETYP,C'F'        OK IF WANT FIXED                             
         BE    RIMVS7                                                           
         LH    RF,MVSILEN          CONVERT TO V/L                               
         LA    RF,2(RF)                                                         
         CH    RF,=H'1024'                                                      
         BNH   RIMVS5B                                                          
         MVC   P(40),=CL40'WARNING - INPUT F/L RECORD TRUNCATED'                
         GOTO1 =V(PRINTER)                                                      
         LH    RF,=H'1024'                                                      
RIMVS5B  STH   RF,MVSILEN                                                       
         L     RF,MVSIADR                                                       
         SH    RF,=H'2'                                                         
         ST    RF,MVSIADR                                                       
         MVC   0(2,RF),MVSILEN                                                  
         B     RIMVS7                                                           
*                                                                               
RIMVS6   CLI   FILETYP,C'V'        V/L INPUT RECORD                             
         BNE   RIMVS7                                                           
RIMVS6A  LH    RF,MVSILEN          SET P/Q V/L RECORDS                          
         LA    RF,2(RF)                                                         
         CH    RF,=H'1024'                                                      
         BNH   RIMVS6B                                                          
         MVC   P(40),=CL40'WARNING - INPUT V/L RECORD TRUNCATED'                
         GOTO1 =V(PRINTER)                                                      
         LH    RF,=H'1024'                                                      
RIMVS6B  STH   RF,MVSILEN                                                       
         L     RF,MVSIADR                                                       
         SH    RF,=H'2'                                                         
         ST    RF,MVSIADR                                                       
         MVC   0(2,RF),MVSILEN                                                  
         B     RIMVS7                                                           
*                                                                               
RIMVS7   L     RF,AQ               MOVE RECORD                                  
         LA    R1,1024                                                          
         L     RE,MVSIADR                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         B     RIMVSXX                                                          
*                                                                               
RIMVSX   MVC   0(2,R2),LENEOF      SET END OF FILE ON MVS INPUT                 
         MVC   0(10,R3),EOFLAB                                                  
*                                                                               
RIMVSX2  BAS   RE,CLSIN            END OF INPUT FILE                            
         L     RE,AQH                                                           
         XC    0(4,RE),0(RE)       SET EOF IN RECORD HDR                        
         B     *+8                                                              
*                                                                               
RIMVSX4  ST    RE,RWSAVRE          CLOSE OUTPUT MVS IF SPECIFIED                
         TM    OUTPUT,MVS                                                       
         BZ    RIMVSXX                                                          
         BAS   RE,CLSOUT                                                        
*                                                                               
RIMVSXX  L     RE,RWSAVRE                                                       
         BR    RE                                                               
*********************************************************************           
* WRITE OUTPUT DATA SET                                                         
*********************************************************************           
WOMVS    ST    RE,RWSAVRE                                                       
*                                                                               
         L     R0,AQ               PUT PRINT LINE FROM Q                        
         CLI   RECFM,C'V'          V/L INPUT RECORD                             
         BNE   WOMVS99                                                          
*                                                                               
         LR    RE,R0                                                            
         L     RF,AIO                                                           
         AHI   RF,4                                                             
         LHI   R1,1024                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         L     RF,AIO                                                           
         MVI   3(RF),X'FF'       MARKER BEFORE THE STRING                       
         LR    RE,RF                                                            
         AHI   RE,1024+4-1       LAST CHAR OF THE STRING                        
*                                                                               
WOMVS10  CLI   0(RE),0           BACKUP TO LAST NON-BLANK CHAR                  
         BE    WOMVS12                                                          
         CLI   0(RE),C' '                                                       
         BH    WOMVS14                                                          
WOMVS12  BCTR  RE,0              BACKUP, TRY AGAIN                              
         B     WOMVS10                                                          
*                                                                               
WOMVS14  AHI   RE,1                                                             
         CLI   ASCII,C'Y'                                                       
         BNE   WOMVS16                                                          
         MVI   0(RE),X'0D'                                                      
         AHI   RE,1                                                             
*                                                                               
WOMVS16  SR    RE,RF             LENGTH OF VB RECORD                            
         XC    0(4,RF),0(RF)                                                    
         STCM  RE,3,0(RF)                                                       
         LR    R0,RF                                                            
*                                                                               
WOMVS99  L     R1,=A(MVSOUT)                                                    
         PUT   (1),(0)                                                          
*                                                                               
WOMVSX   L     RE,RWSAVRE                                                       
         BR    RE                                                               
         EJECT                                                                  
OPNIN    STM   RE,R2,OCSAVREG                                                   
         L     R2,=A(MVSIN)                                                     
         OPEN  ((2),INPUT)                                                      
         LM    RE,R2,OCSAVREG                                                   
         BR    RE                                                               
*                                                                               
CLSIN    STM   RE,R2,OCSAVREG                                                   
         L     R2,=A(MVSIN)                                                     
         CLOSE ((2))                                                            
         LM    RE,R2,OCSAVREG                                                   
         BR    RE                                                               
*                                                                               
OPNOUT   STM   RE,R2,OCSAVREG                                                   
         L     R2,=A(MVSOUT)                                                    
         OPEN  ((2),OUTPUT)                                                     
         USING IHADCB,R2                                                        
         TM    DCBRECFM,DCBRECV                                                 
         BNO   *+8                                                              
         MVI   RECFM,C'V'                                                       
         DROP  R2                                                               
*                                                                               
OPNOUTX  LM    RE,R2,OCSAVREG                                                   
         BR    RE                                                               
*                                                                               
CLSOUT   STM   RE,R2,OCSAVREG                                                   
         L     R2,=A(MVSOUT)                                                    
         CLOSE ((2))                                                            
CLSOUTX  LM    RE,R2,OCSAVREG                                                   
         BR    RE                                                               
         EJECT                                                                  
* PRINT REPORT ATTRIBUTES                                                       
*                                                                               
PREPATT  NTR1                                                                   
         MVC   REPSEQL,PQREPNO                                                  
         MVC   SAVE(10),REPUSER                                                 
         L     R7,=A(REPOUTA)      POINT TO LIST OF ALPHA NAMES                 
*                                                                               
PREPA1   MVC   P(16),0(R7)         REPORT ID                                    
         MVC   REPUSER(7),PQKEY                                                 
         BAS   RE,IDOUT                                                         
         MVC   P+19(L'REPIDA),REPIDA                                            
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         REPORT CLASS                                 
         MVC   P+19(1),PQCLASS                                                  
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         REPORT STATUS                                
         MVC   CISTAT,PQSTAT                                                    
         BAS   RE,STATOUT                                                       
         MVC   P+19(3),REPSTATA                                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         REPORT NAME                                  
         MVC   P+19(11),PQDESC                                                  
**NOP    OC    P+19(11),SPACES     LEAVE LOWER CASE ALONE                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PREPA2   GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         LOCN CREATED                                 
         XC    REPSUBID(5),REPSUBID                                             
         BAS   RE,IDOUT                                                         
         MVC   P+19(L'REPIDA),REPIDA                                            
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         DATE CREATED                                 
         MVC   HALF2,PQDATEL                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF2),(5,P+19)                              
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      HALF2 NEW CMPRSD DATE                        
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
*&&UK*&& OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         TIME CREATED                                 
         MVC   DUB(2),PQTIMEL                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   P+19(5),DUB+3                                                    
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         LIVE RETAIN HOURS                            
         EDIT  (B2,PQRETNL),(6,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           DEAD RETAIN HRS                              
         MVC   P(16),0(R7)                                                      
         EDIT  (B2,PQRETND),(6,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         DATE RETAINED                                
         MVC   HALF2,PQAGERD                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF2),(5,P+19)                              
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      HALF2 NEW CMPRSD DATE                        
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
*&&UK*&& OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)                                                        
         MVC   P(16),0(R7)         TIME RETAINED                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,PQAGERT                                                       
         MH    R1,=H'10'                                                        
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         BAS   RE,TIMEOUT                                                       
         MVC   P+19(5),DUB+3                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PREPA3   GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           NUM OF PAGES                                 
         MVC   P(16),0(R7)                                                      
         EDIT  (B2,PQPAGES),(6,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           LINES PER PAGE                               
         MVC   P(16),0(R7)                                                      
         EDIT  (B1,PQLPP),(2,P+19),ALIGN=LEFT                                   
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           NUM OF LINES                                 
         MVC   P(16),0(R7)                                                      
         EDIT  (B3,PQLINES),(6,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           CHRS PER LINE                                
         MVC   P(16),0(R7)                                                      
         EDIT  (B2,PQAVCPL),(3,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           NUM OF CIS                                   
         MVC   P(16),0(R7)                                                      
         EDIT  (B1,PQAGES),(3,P+19),ALIGN=LEFT                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PREPA4   GOTO1 =V(PRINTER)                                                      
         OC    PQPRLOC,PQPRLOC     TEST IF REPORT EVER PRTD/SENT                
         BZ    PREPA6                                                           
         LA    R7,16(R7)           LOCN PRINTED                                 
         MVC   P(16),0(R7)                                                      
         CLC   PQPRLOC,FFS                                                      
         BNE   *+14                                                             
         MVC   REPIDA,INHOUSE                                                   
         B     *+20                                                             
         MVC   REPUSER,PQPRLOC                                                  
         MVC   REPSEQL+1(1),PQPRNUM                                             
         BAS   RE,IDOUT                                                         
         MVC   P+19(L'REPIDA),REPIDA                                            
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           COUNT PRINTED                                
         MVC   P(16),0(R7)                                                      
         EDIT  (B1,PQPRCNT),(3,P+19),ALIGN=LEFT                                 
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           DEVICE PRINTED                               
         MVC   P(16),0(R7)                                                      
         MVC   P+19(8),PQPRSYM                                                  
PREPA5   GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           DATE PRINTED                                 
         MVC   P(16),0(R7)                                                      
         MVC   HALF2,PQDATED                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF2),(5,P+19)                              
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      HALF2 NEW CMPRSD DATE                        
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
*&&UK*&& OI    P+19,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
         LA    R7,16(R7)           TIME PRINTED                                 
         MVC   P(16),0(R7)                                                      
         MVC   DUB(2),PQTIMED                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   P+19(5),DUB+3                                                    
         GOTO1 =V(PRINTER)                                                      
PREPA6   EQU   *                                                                
PREPAX   B     XIT1                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT LINE DATA IN Q - R3 POINTS TO DATA                                      
**********************************************************************          
PLINEQ   ST    RE,CISAVRE                                                       
         LA    RE,CTLCHR           POINT TO TABLE OF VALID CC CHRS              
         LR    RF,R3               POINT TO CC CHR IN RECORD                    
         TM    PQLINET,PQLTFL                                                   
         BO    *+8                                                              
         LA    RF,2(RF)                                                         
         TM    PQLINET,PQLTCC      TEST IF LINE HAS CC CHR                      
         BO    PLINEQ1             YES                                          
         BCTR  RF,0                NO POINT TO WHERE IT WOULD BE                
         B     PLINEQ2                                                          
PLINEQ1  CLC   0(1,RF),0(RE)       CONVERT TO V(PRINT) DEFINITION               
         BE    PLINEQ2                                                          
         LA    RE,5(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   PLINEQ1                                                          
PLINEQ2  MVC   PLINECC,1(RE)       SET C'BLXX' PARM                             
         ST    RF,PLINEPL          SET A(DATA-1) PARM                           
         GOTO1 =V(PRINT),PLINEPL                                                
PLINEQX  L     RE,CISAVRE                                                       
         BR    RE                                                               
*                                                                               
PLINEPL  DC    A(0),A(PLINECC)     PARM LIST FOR V(PRINT)                       
PLINECC  DC    F'0'                                                             
*                                                                               
UPSITAB  DC    X'8040201008040201'                                              
*                                                                               
CTLCHR   DS    0CL5                                                             
         DC    X'09',C'BL01'                                                    
         DC    X'11',C'BL02'                                                    
         DC    X'19',C'BL03'                                                    
         DC    X'89',C'BC01'                                                    
*                                                                               
         DC    X'01',C'BL00'                                                    
         DC    X'0B',C'BL01'                                                    
         DC    X'13',C'BL02'                                                    
         DC    X'1B',C'BL03'                                                    
         DC    X'8B',C'BC01'                                                    
*                                                                               
CTLCHRX  DC    X'00',C'BL01'                                                    
         DS    0H                                                               
LEFTT    EQU   X'EB'               BOX CHR LEFT HAND T JUNCTION                 
CROSS    EQU   X'8F'               BOX CHR INTERSECTION                         
RIGHTT   EQU   X'EC'               BOX CHR RIGHT T JUNCTIONTION                 
         EJECT                                                                  
FORCEOF  DC    X'FF',C'EOF'        FORCE EOF RETURN FROM EXIT                   
         LTORG                                                                  
         EJECT                                                                  
* PARAMETER TABLE - LIST OF KEYWORDS AND VALUES                                 
*                                                                               
* XL1    PARM VALUE                                                             
* XL1    PARM DEFAULT VALUE                                                     
* XL1    PARM FLAGS X'80'=REQUIRED,X'40'=LIST,X'20'=ROUT,X'01'=SINGLE           
* XL1    PARM MIN LEN                                                           
* CL8    PARM KEYWORD NAME                                                      
* AL4    PARM VALUE LIST                                                        
*                                                                               
         DS    0F                                                               
PARMTBL  DS    0CL16                                                            
MODE     DC    X'0000C004',C'MODE    ',A(MODEL)        REQUIRED                 
INPUT    DC    X'0000C001',C'INPUT   ',A(INPUTL)       REQUIRED                 
OUTPUT   DC    X'00004001',C'OUTPUT  ',A(OUTPUTL)                               
WRITE    DC    X'02022001',C'WRITE   ',A(VWRITE)                                
         DC    X'00022001',C'WRSRV   ',A(VWRITE)                                
PAGES    DC    X'00002001',C'PAGES   ',A(VPAGE)                                 
LOCK     DC    X'00024001',C'LOCK    ',A(LOCKL)                                 
PQID     DC    X'00002001',C'PQID    ',A(VPQID)                                 
FILE     DC    X'00002005',C'FILEID  ',A(VFILE)                                 
         DC    X'00002005',C'DAFILE  ',A(VFILE)                                 
LOAD     DC    X'00002005',C'LOAD    ',A(VLOAD)                                 
*                                                                               
USER     DC    X'0000A003',C'USER    ',A(VUSER)        REQUIRED                 
REPORT   DC    X'0000A002',C'REPORT  ',A(VREPT)        REQUIRED                 
CDATE    DC    X'00002001',C'CDATE   ',A(VCDATE)                                
CLASS    DC    X'00002001',C'CLASS   ',A(VCLASS)                                
STATUS   DC    X'00002001',C'STATUS  ',A(VSTAT)                                 
LRETN    DC    X'00002001',C'LRETN   ',A(VLRET)                                 
DRETN    DC    X'00002001',C'DRETN   ',A(VDRET)                                 
DESC     DC    X'00000004',C'DESC    ',A(REDESC)                                
FORMS    DC    X'00000004',C'FORMS   ',A(REFORMS)                               
CHARS    DC    X'00000004',C'CHARS   ',A(RECHARS)                               
PSWD     DC    X'00000004',C'PSWD    ',A(REPSWD)                                
TYPE     DC    X'00002001',C'TYPE    ',A(VTYPE)                                 
SOON     DC    X'00002001',C'SOON    ',A(VSOON)                                 
DATA     DC    X'00002001',C'DATA    ',A(VDATA)                                 
ARCH     DC    X'00002001',C'ARCHIVE ',A(VARCH)                                 
PARMTBLX DC    X'FFFF'                                                          
*                                                                               
CLASSL   DC    XL12'00'            MAX OF 10 CLASSES                            
*                                                                               
FILEID   DC    CL136' ',X'FF'      MAX 0F 16 PRTQ FILES                         
FILEIX   DC    CL136' ',X'FF'                                                   
PRTQINP  DC    XL17'00',X'FF'                                                   
FILETYP  DC    XL5'00'                                                          
MVSDUMP  DC    C' '                                                             
REPLMODE DC    X'00'                                                            
*                                                                               
STXITLST DC    A(0),A(0)                                                        
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
**********************************************************************          
* INFORMATION REQUIRED TO OPEN A REPORT ON PRINT QUEUE                          
**********************************************************************          
REOPNINF DS    0XL64               REPORT OPEN INFO                             
REFLAG   DC    XL1'00'                                                          
REREPRNO DC    XL2'00'                                                          
REREPRCI DC    XL2'00'                                                          
*                                                                               
RESRCID  DC    XL2'00'             USER=......                                  
RESUBID  DC    CL3' '              REPORT=...                                   
REREPNO  DC    XL2'00'                                                          
RECLASS  DC    CL1' '              CLASS=.                                      
RETYPE   DC    XL1'00'                                                          
REATTB   DC    XL1'00'                                                          
RESTAT   DC    XL1'00'             STATUS=.                                     
RELINET  DC    XL1'00'                                                          
RELINEW  DC    AL1(0)                                                           
REDATEL  DC    XL2'00'                                                          
RETIMEL  DC    XL2'00'                                                          
RERETNL  DC    XL2'00'             LRETN=N                                      
RERETND  DC    XL2'00'             DRETN=N                                      
REFORMS  DC    CL4' '              FORMS=....                                   
RECHARS  DC    CL4' '              CHARS=....                                   
REDESC   DC    CL11' '             DESC=..........                              
REPSWD   DC    CL4' '              PSWD=....                                    
RESECFS  DC    XL2'00'                                                          
RETYP1   DC    XL1'00'             ARCHIVE=.                                    
         DC    XL12'00'                                                         
*                                                                               
XXOPNINF DS    0XL64               REPORT OPEN INFO DEFAULT VALUES              
XXFLAG   DC    XL1'00'                                                          
XXREPRNO DC    XL2'00'                                                          
XXREPRCI DC    XL2'00'                                                          
*                                                                               
XXSRCID  DC    XL2'00'                                                          
XXSUBID  DC    CL3' '                                                           
XXREPNO  DC    XL2'00'                                                          
XXCLASS  DC    CL1' '                                                           
XXTYPE   DC    XL1'00'                                                          
XXATTB   DC    XL1'00'                                                          
XXSTAT   DC    XL1'00'                                                          
XXLINET  DC    XL1'00'                                                          
XXLINEW  DC    AL1(000)                                                         
XXDATEL  DC    XL2'00'                                                          
XXTIMEL  DC    XL2'00'                                                          
XXRETNL  DC    AL2(120)                                                         
XXRETND  DC    AL2(120)                                                         
XXFORMS  DC    CL4' '                                                           
XXCHARS  DC    CL4' '                                                           
XXDESC   DC    CL11' '                                                          
XXPSWD   DC    CL4' '                                                           
XXSECFS  DC    XL2'00'                                                          
         DC    XL13'00'                                                         
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                               
**********************************************************************          
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
HALF3    DS    H                                                                
PACKED   DS    PL6                                                              
XFLAG    DS    X                                                                
COPYERR  DS    C                                                                
*                                                                               
RECFM    DC    C'F'                F-FIXED BLOCK, V-VARIABLE BLOCK(VB)          
ASCII    DC    C'N'                Y-PUT X'0D' EOLN FOR VB DATASET              
*                                                                               
ERRNUM   DS    F                                                                
ACOMRG   DS    A                                                                
ALOADPT  DS    A                                                                
AQBNEXT  DS    A                                                                
ASUMNEXT DS    A                                                                
PGSAVRE  DS    A                                                                
RWSAVRE  DS    A                                                                
OCSAVREG DS    5F                                                               
FIRST    DS    F                                                                
LAST     DS    F                                                                
*                                                                               
COPYINDX DS    A                                                                
COPYCTRS DS    0XL136              COUNTERS FOR TOTAL AND 16 PRTQ FILES         
TOTLREAD DS    F                                                                
TOTLCOPY DS    F                                                                
         DS    16XL8               READ/COPY FOR 16 PRTQ FILES                  
*                                                                               
MAXSEQ   DC    F'65000'            MAXIMUM SEQUENCE NUM FOR USER ID             
COPYREAD DS    H                                                                
COPYCOPY DS    H                                                                
FRSTTIME DC    C'Y'                                                             
UPSIVAL  DC    X'00'                                                            
UPSIINP  DC    X'00'                                                            
FLAG     DC    X'00'                                                            
FLAG1    DC    X'00'                                                            
         DC    XL3'00'                                                          
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
PARAM1   DS    F                                                                
PARAM2   DS    F                                                                
PARAM3   DS    F                                                                
PARAM4   DS    F                                                                
PARAM5   DS    F                                                                
PARAM6   DS    F                                                                
*                                                                               
PARM     DS    6F                                                               
DMCB     DS    6F                                                               
EXCB     DS    6F                                                               
DMCB1    DS    6F                                                               
USERN    DS    H                                                                
USERA    DS    CL10                                                             
*                                                                               
INDEX    DS    0CL40                                                            
NXNDX    DS    CL24                PRTQ INDEX ENTRY                             
NXINFO   DS    XL2                 INFO PASSING FIELD                           
NXREPNOX DS    XL2                 UPPER LIMIT                                  
NXCIADDR DS    XL2                 TTTT OF FIRST CI                             
NXFLAG   DS    XL1                 FLAG VALUES                                  
         DS    XL1                 N/D                                          
NXUSRINF DS    XL8                 USER INFO                                    
         DS    XL24                                                             
*                                                                               
LOADEXT  DS    CL8                                                              
*                                                                               
SAVE     DS    CL256                                                            
WORK     DS    CL256                                                            
OPERANS  DS    CL8                                                              
C        DS    CL80                                                             
*                                                                               
RDID     EQU   01                                                               
DACLOSE  EQU   15                                                               
DARPT    EQU   16                                                               
VDATAMGR DC    V(DATAMGR)                                                       
*                                                                               
DADDS    DC    C'DADDS   '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMPRINT  DC    C'DMPRINT '                                                      
DMWRT    DC    C'DMWRT   '                                                      
PRTQUE   DC    C'PRTQU   '                                                      
*                                                                               
APRTQLST DC    A(0)                                                             
PRTQMAX  DC    AL1(0)                                                           
         DC    AL1(0)                                                           
PRTQINT  DC    AL1(0)                                                           
PRTQEXT  DC    AL1(0)                                                           
PRTQID   DC    CL8' '                                                           
PRTQDTF  DC    A(0)                                                             
*                                                                               
FFS      DC    8X'FF'                                                           
USCORES  DC    16X'BF'                                                          
*                                                                               
MVSIADR  DC    A(0)                                                             
MVSOADR  DC    A(0)                                                             
MVSILEN  DC    H'0'                                                             
MVSOLEN  DC    H'0'                                                             
*                                                                               
AQH      DC    A(QH)                                                            
AQ       DC    A(Q)                                                             
AIO      DC    A(IO)                                                            
*                                                                               
ACIREC   DS    A                                                                
ACXREC   DS    A                                                                
AREPTAB  DS    A                                                                
AREPALL  DS    A                                                                
APRTQINP DS    A                                                                
*                                                                               
CINDXMIN DC    H'2'                                                             
CPAGE    DS    H                                                                
*                                                                               
*DMPRTQUEW                                                                      
       ++INCLUDE DMPRTQW                                                        
*                                                                               
CISEQ    DS    XL1                                                              
CISTAT   DS    XL1                                                              
CIREPNO  DS    XL2                                                              
CINCI    DS    PL4                                                              
USERSEQ  DS    H                                                                
         DS    H                                                                
*                                                                               
SRTREC   DS    CL30                                                             
SRTRECL  DS    CL30                                                             
*                                                                               
REPUSERA DS    CL10                                                             
*                                                                               
REPDEFN  DS    0XL10                                                            
REPUSER  DS    XL2                                                              
REPSUBID DS    CL3                                                              
REPSEQL  DS    XL2                                                              
REPFLAG  DS    XL1                 DEFINES CONTENTS OF SEQL AND SEQH            
REPSEQH  DS    XL2                                                              
*                                                                               
REPCDATE DS    XL2                                                              
REPPAGES DS    XL2                                                              
REPSTAT  DS    XL1                                                              
REPSTATA DS    CL3                                                              
REPIDA   DS    CL24                                                             
REPIDASV DS    CL10                                                             
*                                                                               
REPKDATE DS    XL2                                                              
REPLDATE DS    XL2                                                              
REPDDATE DS    XL2                                                              
REPCSDAT DS    XL2                                                              
REPCEDAT DS    XL2                                                              
REMVSDAT DS    XL2                                                              
REPDEDAT DS    XL2                                                              
         DS    XL2                                                              
REPKDAYS DS    F                                                                
REPLDAYS DS    F                                                                
REPDDAYS DS    F                                                                
REPCSDAY DS    F                                                                
REPCEDAY DS    F                                                                
REMVSDAY DS    F                                                                
REPDEDAY DS    F                                                                
         EJECT                                                                  
LENSOF   DC    H'0132'             TOT LEN OF STR-OF-FILE REC                   
LENEOF   DC    H'0014'             TOT LEN OF END-OF-FILE REC                   
*                                                                               
SOFLAB   DS    0CL10                                                            
         DC    X'0000',C'SOFSOF',X'0000'                                        
EOFLAB   DS    0CL10                                                            
         DC    X'FFFF',C'EOFEOF',X'FFFF'                                        
*                                                                               
INHOUSE  DC    CL24'DDS,INHOUSE'                                                
DOTS     DC    16C'.'                                                           
ZEROS    DC    16C'0'                                                           
*                                                                               
PLXTRN   DC    A(CIREC),A(0),A(0),A(0)                                          
         DC    V(PRINTER),V(CPRINT)                                             
         EJECT                                                                  
         DS    0D                                                               
         DC    8X'FF',C'*REPTAB*',8X'FF',C'*REPTAB*'                            
REPTAB   DS    50XL32                                                           
         DC    8X'FF'                                                           
*                                                                               
         DS    0D                                                               
         DC    C'QHQHQHQH'                                                      
QH       DS    4C                                                               
Q        DS    1024C                                                            
*                                                                               
         DS    0D                                                               
         DC    C'IOIOIOIO'                                                      
         DS    4C                                                               
IO       DS    32768C                                                           
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
         DC    C'QBQBQBQB'                                                      
QBUFF    DS    8000C                                                            
*                                                                               
         DS    0D                                                               
         DC    C'WKWKWKWK'                                                      
PQWORK   DS    4000D                                                            
PRTQXFRX EQU   *                   END OF STXITER DUMP                          
*                                                                               
         DS    0D                                                               
         DC    C'UIDSUIDS'                                                      
CTBUF    DC    1000XL12'00'                                                     
         DC    2X'FF'                                                           
*                                                                               
         DS    0D                                                               
         DC    C'SUMBSUMB'                                                      
SUMBUF   DC    1001XL100'00'                                                    
         DC    6X'FF'                                                           
*                                                                               
CTREC    DS    1024C                                                            
PARMCARD DS    CL80                                                             
         EJECT                                                                  
REPOUTA  DC    CL16'REPORT ID'                                                  
         DC    CL16'REPORT CLASS'                                               
         DC    CL16'REPORT STATUS'                                              
         DC    CL16'REPORT NAME'                                                
         DC    CL16'LOCN CREATED'                                               
         DC    CL16'DATE CREATED'                                               
         DC    CL16'TIME CREATED'                                               
         DC    CL16'LIVE RETAIN HRS'                                            
         DC    CL16'DEAD RETAIN HRS'                                            
         DC    CL16'DATE RETAINED'                                              
         DC    CL16'TIME RETAINED'                                              
         DC    CL16'NUM OF PAGES'                                               
         DC    CL16'LINES PER PAGE'                                             
         DC    CL16'NUM OF LINES'                                               
         DC    CL16'CHRS PER LINE'                                              
         DC    CL16'NUM OF CIS'                                                 
         DC    CL16'LOCN PRINTED'                                               
         DC    CL16'COUNT PRINTED'                                              
         DC    CL16'DEVICE PRINTED'                                             
         DC    CL16'DATE PRINTED'                                               
         DC    CL16'TIME PRINTED'                                               
*                                                                               
INFO0    DC    CL60'---------------'                                            
INFO1    DC    CL60'MVS DATA FILE TO PRINT QUEUE '                              
INFO2    DC    CL60'PARAMETER CARDS'                                            
INFO3    DC    CL60'ACTION MESSAGES'                                            
INFO5    DC    CL60'ERROR REPORT(S) NOT FOUND'                                  
INFO6    DC    CL60'TOTAL NNNNN REPORTS COPIED FROM MVS TO PRTQU '              
INFO7    DC    CL60'TOTAL NNNNN REPORTS COPIED FROM PRTQU TO MVS '              
INFO8    DC    CL60'OUT OF NNNNN REPORTS READ'                                  
INFO9    DC    CL60'REPORT DESCRIPTION AND DATA FOLLOWS'                        
*                                                                               
ERRMSG1  DC    CL60'***ERROR*** MISSING PARAMETER - '                           
ERRMSG2  DC    CL60'***ERROR*** INVALID PARAMETER CARD SYNTAX'                  
ERRMSG3  DC    CL60'***ERROR*** INVALID PARAMETER - '                           
ERRMSG4  DC    CL60'***ERROR*** INVALID VALUE FOR PARAMETER - '                 
ERRMSG5  DC    CL60'***ERROR*** LOAD EXTERNAL NOT FOUND - '                     
*                                                                               
ERRMSGA  DC    CL60'ERROR ERROR DISK END OF FILE'                               
ERRMSGB  DC    CL60'ERROR ERROR DISK WRITE ERROR'                               
ERRMSGC  DC    CL60'ERROR ERROR DISK READ ERROR'                                
ERRMSGD  DC    CL60'ERROR ERROR INVALID CI DATA'                                
ERRMSGE  DC    CL60'ERROR ERROR ERROR IN COPY INDEX='                           
         EJECT                                                                  
**********************************************************************          
* LISTS OF PARAMETER VALUES AND EQUATES                                         
**********************************************************************          
YES      EQU   X'02'                                                            
NO       EQU   X'01'                                                            
PRTQ     EQU   X'01'                                                            
MVS      EQU   X'02'                                                            
*                                                                               
PRNT     EQU   X'02'                                                            
COPY     EQU   X'04'                                                            
REPLACE  EQU   X'05'                                                            
*                                                                               
MODEL    DC    X'02',CL7'PRINT'                                                 
         DC    X'04',CL7'COPY'                                                  
         DC    X'05',CL7'REPLACE'                                               
MODELX   DC    X'FF'                                                            
*                                                                               
INPUTL   DC    X'01',CL7'PQ  '                                                  
         DC    X'01',CL7'PRTQ'                                                  
         DC    X'02',CL7'MVS '                                                  
INPUTLX  DC    X'FF'                                                            
*                                                                               
OUTPUTL  DC    X'01',CL7'PQ  '                                                  
         DC    X'01',CL7'PRTQ'                                                  
         DC    X'02',CL7'MVS '                                                  
OUTPUTLX DC    X'FF'                                                            
*                                                                               
LOCKL    DC    X'01',CL7'NO'                                                    
         DC    X'02',CL7'YES'                                                   
LOCKLX   DC    X'FF'                                                            
         EJECT                                                                  
MVSIN    DCB   DDNAME=MVSIN,DSORG=PS,MACRF=(GM),EODAD=RIMVSX                    
*                                                                               
MVSOUT   DCB   DDNAME=MVSOUT,DSORG=PS,MACRF=(PM)                                
         DS    0D                                                               
*                                                                               
         DS    0D                                                               
         DC    C'*SSB*SSB*SSB*SSB'                                              
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSB+(SSOSTAT2-SSBD)                                              
         DC    AL1(SSOSNRCV)       SET RECOVERY OFF                             
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    X'01'                                                            
         ORG                                                                    
         DROP  R8,R9,RA            DROP MVPQINT CSECT'S EXTRA BASE REGS         
         EJECT                                                                  
***********************************************************************         
* READ/PRINT/VALIDATE A SET OF PARAMETER CARDS                                  
* 2ND PASS ON CARDS                                                             
***********************************************************************         
VALPARM  NMOD1 0,**VALP**                                                       
         LR    RA,R1                                                            
         USING PARMTBL,RA          RA=A(MVPQINT CSECT'S STORAGE)                
         LR    RC,R0                                                            
         USING DPRINT,RC           RC=A(MVPQINT CSECT'S PRINTER)                
*                                                                               
         LA    R1,MODE+4           CLEAR ERRNUM AND SET A(PARMTBL NTRY)         
         ST    R1,ERRNUM                                                        
         CLI   FRSTTIME,C'Y'       READ FIRST CARD                              
         BNE   VPARM2                                                           
         MVI   FRSTTIME,C'N'                                                    
         B     VPARM1A                                                          
*                                                                               
VPARM1   GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
VPARM1A  MVC   P(80),C                                                          
         CLI   C,C'*'              IGNORE COMMENT CARDS                         
         BE    VPARM1P                                                          
*                                                                               
         CLC   C(5),=CL8'DATE='    DATE=DD/MM/YY TO SET SYSTEM DATE             
         BNE   VPARM1C                                                          
         GOTO1 =V(DATVAL),DMCB,(0,C+5),DUB                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   VPARM1B                                                          
         LA    R1,=CL8'DATE='                                                   
         ST    R1,ERRNUM                                                        
         MVI   ERRNUM,4                                                         
         B     VPARMX                                                           
VPARM1B  MVC   DATEIPL,C+5         OVERRIDE SYSTEM IPL DATE                     
         GOTO1 =V(DATCON),DMCB,(4,DATEIPL),(0,DATEYMD)                          
         GOTO1 (RF),(R1),(0,DATEYMD),(2,DATECPR)                                
*                                                                               
         L     RF,=A(SSB)                                                       
         USING SSBD,RF                                                          
         OI    SSOMTIND,SSOMTAPL   SET OFFLINE PASS OF DATE IN V(SSB)           
         OI    SSOFLAG1,SSOFYMD    DATE IN SSODATE=C'YYMMDD  '                  
         MVC   SSODATE(6),DATEYMD  PASS DATE AS C'YYMMDD'                       
         B     VPARM1P                                                          
         DROP  RF                                                               
*                                                                               
VPARM1C  CLC   C(5),=CL5'DUMP='    DUMP=MVS OR OS FOR FULL MVS DUMP             
         BNE   VPARM1D                                                          
         MVC   MVSDUMP,C+5                                                      
         L     RF,=A(PRTQXFR)      SET UP AREA TO DUMP                          
         ST    RF,STXITLST                                                      
         L     RF,=A(PRTQXFRX)                                                  
         ST    RF,STXITLST+4                                                    
         MVI   STXITLST+4,X'80'                                                 
         GOTO1 =V(STXITER),DMCB,STXITLST                                        
         B     VPARM1P                                                          
*                                                                               
VPARM1D  CLC   =C'ASCII=',C        ASCII=YES/NO                                 
         BNE   VPARM1X                                                          
         CLC   =C'YES',C+6                                                      
         BNE   VPARM1P                                                          
         MVI   ASCII,C'Y'                                                       
         B     VPARM1P                                                          
*                                                                               
VPARM1P  GOTO1 =V(PRINTER)                                                      
         B     VPARM1                                                           
***********************************************************************         
* DO SOME INITIALIZATION BEFORE CONTINUING                                      
***********************************************************************         
VPARM1X  GOTO1 VDATAMGR,DMCB,=C'OPEN',=C'SER',=C'NCTFILE X'                     
         L     RF,ACIREC                                                        
         GOTO1 VDATAMGR,DMCB,=C'GLIST',PRTQUE,INDEX,,(RF)                       
         L     RE,NXUSRINF                                                      
         ST    RE,APRTQLST         SAVE A(PRTQ FILE LIST)                       
         MVC   PRTQMAX,0(RE)       SAVE NUMBER OF PRTQ FILES IN LIST            
         CLI   PRTQMAX,16                                                       
         BNH   *+6                                                              
         DC    H'0'                MAX OF 16 FILES THIS VERSION                 
         LA    RE,8(RE)                                                         
         LA    RF,FILEIX+8                                                      
         SR    R1,R1                                                            
VPARM1X1 CLI   0(RE),0             TEST END OF PRTQ LIST                        
         BE    VPARM1X2                                                         
         ICM   R1,7,5(RE)          GET A(DTF)                                   
         MVC   0(7,RF),22(R1)      SAVE ORIGIONAL DTF FILE ID                   
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         B     VPARM1X1                                                         
*                                                                               
VPARM1X2 L     RE,=V(DMISGENQ)     SAVE A(DDS ENQ/DEQ ROUTINE)                  
         ST    RE,CIENQDEQ                                                      
*                                                                               
VPARM2   CLC   C(2),MODE+4         FIRST CARD OF SET MUST BE MODE CARD          
         BE    *+12                                                             
         MVI   ERRNUM,1                                                         
         B     VPARMX                                                           
         LA    R1,PARMTBL          INITIALIZE VALUES                            
VPARM2A  CLI   0(R1),X'FF'                                                      
         BE    VPARM2B                                                          
         MVI   0(R1),0                                                          
         LA    R1,L'PARMTBL(R1)                                                 
         B     VPARM2A                                                          
VPARM2B  MVI   REPLMODE,0          SET NORMAL MODE                              
*                                                                               
VPARM4   MVC   FILEIX(8),=CL8' '   RESET FILE DTF NAMES                         
         MVC   FILEID,FILEIX                                                    
         XC    PRTQINP,PRTQINP                                                  
         MVC   REOPNINF,XXOPNINF                                                
         XC    FILETYP,FILETYP                                                  
*                                                                               
VPARM5   LA    R1,REPTAB                                                        
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
***********************************************************************         
* ACTUALLY 3RD PASS ON CARDS                                                    
***********************************************************************         
VPARM6   GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLI   C,C'*'                                                           
         BE    VPARMP              IGNORE COMMENT CARDS                         
         CLC   C(2),=C'/*'                                                      
         BNE   *+12                                                             
         MVI   FRSTTIME,C'X'                                                    
         B     VPARMX                                                           
         CLC   C(2),MODE+4                                                      
         BE    VPARMX                                                           
*                                                                               
VPARM8   MVC   P(80),C                                                          
         MVC   C+72(8),SPACES                                                   
         L     R2,ACIREC                                                        
         GOTO1 =V(SCANNER),DMCB,(C'C',C),(R2)                                   
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
         CLI   1(R2),10                                                         
         BH    VPARME                                                           
         SR    RE,RE                                                            
         IC    RE,1(R2)                                                         
         BCTR  RE,0                                                             
         L     RF,12(R4)                                                        
         TM    2(R4),X'40'         KEYWORD VALUE IN LIST                        
         BO    VPARMF              YES                                          
         TM    2(R4),X'20'         KEYWORD VALUE BY ROUTINE                     
         BO    VPARMG              YES                                          
         MVC   0(10,RF),22(R2)     NO- SAVE VALUE                               
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
VPARMX   B     XIT2                                                             
XIT2     XIT1                                                                   
***********************************************************************         
* PAGE NUMBER                                                                   
***********************************************************************         
VPAGE    MVI   PAGES,1             NUMBER OF PAGES                              
         CLI   MODE,PRNT                                                        
         BE    *+10                                                             
         MVI   ERRNUM,3                                                         
         BR    RE                                                               
         TM    3(R2),X'80'                                                      
         BZ    VPAGEERR                                                         
         L     RF,8(R2)                                                         
         LTR   RF,RF                                                            
         BZ    VPAGEERR                                                         
         STH   RF,REPPAGES                                                      
         L     RF,AREPTAB          SAVE IN REPTAB FOR USERID                    
         TM    0(RF),X'80'                                                      
         BO    VPAGEERR                                                         
         MVC   30(2,RF),REPPAGES                                                
         B     *+8                                                              
VPAGEERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
***********************************************************************         
* CREATION DATE                                                                 
***********************************************************************         
VCDATE   NTR1  ,                   CREATE DATE                                  
         GOTO1 =V(DATVAL),DMCB,(0,22(R2)),DUB                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    VCDERR                                                           
         GOTO1 =V(DATCON),DMCB,(0,DUB),(2,DUB1)                                 
         L     RF,AREPTAB          SAVE IN REPTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VCDERR                                                           
         OI    7(RF),X'80'         SET CDATE INPUT                              
         MVC   10(2,RF),DUB1                                                    
         B     *+8                                                              
VCDERR   MVI   ERRNUM,4                                                         
VCDX     B     XIT2                                                             
***********************************************************************         
* CLASS                                                                         
***********************************************************************         
VCLASS   NTR1                      REPORT CLASS                                 
         MVI   CLASS,1                                                          
         CLI   MODE,PRNT                                                        
         BNL   *+12                                                             
         MVI   ERRNUM,3                                                         
         B     VCLASSX                                                          
         XC    CLASSL,CLASSL                                                    
         CLC   22(4,R2),=C'ALL '                                                
         BE    VCLASSX                                                          
VCLASS2  SR    R0,R0               R0=NUM OF CLASS INPUT CHRS                   
         IC    R0,1(R2)                                                         
         LTR   R0,R0                                                            
         BZ    VCLASSX                                                          
         LA    R5,22(R2)           R5=A(INPUT CLASS CHR)                        
         LA    R6,CLASSL+1         R6=A(CLASS LIST CHR)                         
*                                                                               
         MVI   CLASSL,C'+'         FIRST CHR CAN BE + OR -                      
         CLI   0(R5),C'+'                                                       
         BE    *+16                                                             
         CLI   0(R5),C'-'                                                       
         BNE   VCLASS4                                                          
         MVI   CLASSL,C'-'                                                      
         SH    R0,=H'1'                                                         
         BNP   VCLASSE                                                          
         LA    R5,1(R5)                                                         
*                                                                               
VCLASS4  CLI   0(R5),C'*'          EACH INPUT CHR MUST BE VALID                 
         BE    *+12                                                             
         CLI   0(R5),C'A'                                                       
         BL    VCLASSE                                                          
         MVC   0(1,R6),0(R5)                                                    
         LA    R6,1(R6)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,VCLASS4                                                       
         MVC   RECLASS,CLASSL+1    SAVE REPORT CLASS FOR OPEN INFO              
         L     RF,AREPTAB                                                       
         TM    0(RF),X'80'                                                      
         BO    VCLASSE                                                          
         MVC   18(11,RF),CLASSL    SAVE IN REPTAB FOR USERID                    
         B     *+8                                                              
*                                                                               
VCLASSE  MVI   ERRNUM,4                                                         
VCLASSX  B     XIT2                                                             
***********************************************************************         
* SOON                                                                          
***********************************************************************         
VSOON    NTR1  ,                   SOON JOB                                     
         MVI   SOON,1                                                           
         CLC   22(4,R2),=C'NO  '                                                
         BE    VSOONX                                                           
         CLC   22(4,R2),=C'YES '                                                
         BNE   VSOONX                                                           
         OI    REATTB,X'01'        SET SOON JOB                                 
         MVI   SOON,2                                                           
VSOONX   B     XIT2                                                             
***********************************************************************         
* DATA                                                                          
***********************************************************************         
VDATA    NTR1  ,                   DATA YES NO                                  
         MVI   DATA,1                                                           
         CLI   22(R2),C'N'                                                      
         BE    VDATAX                                                           
         CLI   22(R2),C'Y'                                                      
         BNE   VDATAX                                                           
         OI    RETYPE,QLTYDL                                                    
         MVI   DATA,2                                                           
VDATAX   B     XIT2                                                             
***********************************************************************         
* STATUS                                                                        
***********************************************************************         
VSTAT    NTR1  ,                   REPORT STATUS                                
         MVI   STATUS,1                                                         
         CLI   MODE,PRNT                                                        
         BNL   *+12                                                             
         MVI   ERRNUM,3                                                         
         B     VSTATX                                                           
         MVI   REPSTAT,0                                                        
         CLI   1(R2),4                                                          
         BH    VSTATERR                                                         
         CLC   22(4,R2),=C'ALL '                                                
         BE    VSTATX                                                           
VSTAT2   SR    R0,R0               R3=NUM OF STATUS CHRS                        
         IC    R0,1(R2)                                                         
         LTR   R0,R0                                                            
         BZ    VSTATX                                                           
         LA    R5,22(R2)           R5=A(STATUS CHR)                             
*                                                                               
VSTAT4   CLI   0(R5),C'A'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTAC                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'H'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTHO                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'L'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTLIVE                                                 
         B     VSTAT6                                                           
         CLI   0(R5),C'K'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTKE                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'P'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTPR                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'S'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTSE                                                   
         B     VSTAT6                                                           
         CLI   0(R5),C'D'                                                       
         BNE   *+12                                                             
         OI    REPSTAT,PQSTDEAD                                                 
         B     VSTAT6                                                           
         B     VSTATERR                                                         
*                                                                               
VSTAT6   LA    R5,1(R5)                                                         
         BCT   R0,VSTAT4                                                        
         MVC   RESTAT,REPSTAT      SAVE REPORT STATUS FOR OPEN INFO             
         L     RF,AREPTAB                                                       
         TM    0(RF),X'80'                                                      
         BO    VSTATERR                                                         
         MVC   29(1,RF),REPSTAT    SAVE IN REPTAB FOR USER                      
         B     *+8                                                              
VSTATERR MVI   ERRNUM,4                                                         
VSTATX   B     XIT2                                                             
***********************************************************************         
* FILE                                                                          
***********************************************************************         
VFILE    NTR1  ,                   PRTQ FILE DTF/DD OVERRIDE NAME               
         CLI   1(R2),5                                                          
         BL    VFILEERR                                                         
         CLI   1(R2),7                                                          
         BH    VFILEERR                                                         
         IC    RF,FILE             BUMP NUMBER OF FILE NAMES INPUT              
         LA    RF,1(RF)                                                         
         STC   RF,FILE                                                          
         MVC   DUB,22(R2)                                                       
         L     RF,APRTQLST         POINT TO PRTQ FILE LIST                      
         CLI   PRTQMAX,1                                                        
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
VFILE4   LA    RF,PRTQINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'10'                                                      
         BO    VFILEERR                                                         
         OI    0(RF),X'10'         SET REFERENCED VIA FILE=XXXXX                
         SLL   R1,3                                                             
         LA    RF,FILEID(R1)                                                    
         MVC   0(7,RF),DUB         SAVE INPUT OVERRIDE IN FILEID LIST           
         B     *+8                                                              
VFILEERR MVI   ERRNUM,4                                                         
         B     XIT2                                                             
***********************************************************************         
* LOAD                                                                          
***********************************************************************         
VLOAD    NTR1                                                                   
         CLI   1(R2),3             LOAD=EXTERN                                  
         BL    VLOADER4                                                         
         CLI   1(R2),8                                                          
         BH    VLOADER4                                                         
         MVC   LOADEXT,22(R2)                                                   
         L     RF,ALOADPT                                                       
         GOTO1 =V(LOADER),DUB,LOADEXT,(RF)                                      
         L     RF,4(R1)                                                         
         LA    RF,0(RF)                                                         
         ST    RF,ALOADPT          SAVE EXTERNAL MODULE LOAD ADDRESS            
         LTR   RF,RF                                                            
         BZ    VLOADER5                                                         
         B     XIT2                                                             
*                                                                               
VLOADER4 MVI   ERRNUM,4                                                         
         B     *+8                                                              
VLOADER5 MVI   ERRNUM,5                                                         
         B     XIT2                                                             
***********************************************************************         
* PRTQ ID                                                                       
***********************************************************************         
VPQID    NTR1                                                                   
         SR    R0,R0               PRTQ FILE ID = LIST OF PRTQ ID CHRS          
         ICM   R0,1,1(R2)                                                       
         BZ    VPQIDERR            R0=NUMBER OF FILES                           
         LA    R5,22(R2)           R5=A(NEXT PRTQ FILE CHR)                     
VPQID1   L     RF,APRTQLST         POINT TO LIST OF PRTQ FILES                  
         LA    RF,8(RF)                                                         
VPQID2   CLI   0(RF),0             TEST END OF TABLE                            
         BE    VPQIDERR                                                         
         CLC   1(1,RF),0(R5)       TEST INPUT CHR WITH ALPHA ID CHR             
         BE    VPQID3                                                           
         LA    RF,8(RF)                                                         
         B     VPQID2                                                           
VPQID3   MVC   PQID(1),0(RF)       SET PQID TO INTERNAL PRTQ FILE NUM           
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    RF,PRTQINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'0F'                                                      
         BNZ   VPQID4                                                           
         IC    R1,PRTQINP          BUMP NUM OF PRTQ FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,PRTQINP                                                       
VPQID4   TM    0(RF),X'01'         TEST DUPLICATE                               
         BO    VPQIDERR                                                         
         OI    0(RF),X'01'         SET REFERENCED BY PQID INPUT                 
         LA    R5,1(R5)                                                         
         BCT   R0,VPQID1           BACK FOR NEXT PRTQ FILE ID CHR               
         B     *+8                                                              
VPQIDERR MVI   ERRNUM,4                                                         
         B     XIT2                                                             
***********************************************************************         
* USER ID                                                                       
***********************************************************************         
VUSER    NTR1                      USERID = ALL OR XXX...                       
         MVC   REPUSERA,SPACES                                                  
         XC    RESRCID,RESRCID     RESRCID IS ZERO FOR ALL USERS                
         CLI   22(R2),C'-'                                                      
         BNE   *+14                                                             
         MVC   REPUSERA(9),23(R2)                                               
         B     *+10                                                             
         MVC   REPUSERA,22(R2)                                                  
         CLC   REPUSERA(4),=C'ALL '                                             
         BE    VUSER3                                                           
         CLC   REPUSERA(3),=C'ALL '                                             
         BNE   VUSER1                                                           
         CLI   REPUSERA+4,C' '                                                  
         BE    VUSER3                                                           
         L     RF,APRTQLST         POINT TO LIST OF PRTQ FILES                  
         LA    RF,8(RF)                                                         
VUSER0A  CLI   0(RF),0             TEST END OF TABLE                            
         BE    VUSERERR                                                         
         CLC   1(1,RF),REPUSERA+3  TEST INPUT CHR WITH ALPHA ID CHR             
         BE    VUSER0B                                                          
         LA    RF,8(RF)                                                         
         B     VUSER0A                                                          
VUSER0B  SR    R1,R1               ALLX MATCHES TO PRTQ FILE ID X               
         IC    R1,0(RF)                                                         
         LA    RF,PRTQINP(R1)      POINT TO ENTRY FOR THIS FILE                 
         TM    0(RF),X'0F'                                                      
         BNZ   VUSER0C                                                          
         IC    R1,PRTQINP          BUMP NUM OF PRTQ FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,PRTQINP                                                       
VUSER0C  TM    0(RF),X'02'         TEST DUPLICATE USER=ALLX                     
         BO    VUSERERR                                                         
         OI    0(RF),X'02'         SET REFERENCED BY USER=ALLX INPUT            
         B     VUSER3                                                           
*                                                                               
VUSER1   L     R5,ACXREC           READ USER ID RECORD                          
         XC    0(25,R5),0(R5)                                                   
         MVI   0(R5),C'I'                                                       
         MVC   15(10,R5),REPUSERA                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'CTFILE',(R5),(R5)                        
         CLI   8(R1),0                                                          
         BNE   VUSERERR                                                         
         LA    R5,28(R5)                                                        
         SR    RE,RE                                                            
VUSER2   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),2                                                          
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     VUSER2                                                           
         MVC   RESRCID,2(R5)       EXTRACT USER ID NUMBER                       
*                                                                               
VUSER3   OC    RESRCID,RESRCID     ONLY ONE USER=ALL ALLOWED                    
         BNZ   VUSER4                                                           
         TM    USER,X'80'                                                       
         BZ    *+12                                                             
         CLI   REPUSERA+3,C' '                                                  
         BE    VUSERERR                                                         
         OI    USER,X'80'          SET USER=ALL INPUT                           
         B     VUSER6                                                           
*                                                                               
VUSER4   CLI   22(R2),C'-'         USER=-XXX ONLY AFTER USER=ALL                
         BNE   VUSER5                                                           
         TM    USER,X'80'                                                       
         BZ    VUSERERR                                                         
         OI    USER,X'01'          SET USER=-XXX INPUT                          
         OI    RESRCID,X'80'                                                    
         B     VUSER6                                                           
*                                                                               
VUSER5   TM    USER,X'81'          USER=XXX MUST BE FIRST                       
         BNZ   VUSERERR                                                         
         OI    USER,X'02'          SET USER=XXX INPUT                           
         B     VUSER6                                                           
*                                                                               
VUSER6   L     RF,AREPTAB          BUMP TO NEXT REPTAB ENTRY                    
         LA    RF,L'REPTAB(RF)                                                  
         CLC   0(4,RF),=32X'FF'                                                 
         BE    VUSERERR            REPTAB IS FULL                               
         ST    RF,AREPTAB                                                       
         MVC   0(2,RF),RESRCID                                                  
         OC    RESRCID,RESRCID                                                  
         BNZ   *+12                                                             
         ST    RF,AREPALL          SAVE A(ALL ENTRY)                            
         B     VUSER8                                                           
*                                                                               
VUSER7   XC    INDEX,INDEX         FIND WHICH PQ FOR USER ID                    
         MVC   NXNDX(2),RESRCID                                                 
         GOTO1 VDATAMGR,DMCB,=C'GFILE',PRTQUE,INDEX,,(R5)                       
         SR    R1,R1                                                            
         IC    R1,NXINFO           GET INTERNAL PRTQ FILE NUMBER                
         CLI   NXINFO+1,0          **TEMP** OLD STYLE CALL **REMOVE**           
         BNE   *+8                 **TEMP** OLD STYLE CALL **REMOVE**           
         IC    R1,NXNDX+20         **TEMP** OLD STYLE CALL **REMOVE**           
         LA    RF,PRTQINP(R1)                                                   
         TM    0(RF),X'0F'         TEST IF FIRST REF TO PRTQ FILE               
         BNZ   VUSER7A                                                          
         IC    R1,PRTQINP          BUMP NUM OF PRTQ FILES REFERENCED            
         LA    R1,1(R1)                                                         
         STC   R1,PRTQINP                                                       
VUSER7A  OI    0(RF),X'04'         SET FLAG TO SHOW REF VIA USERID              
*                                                                               
VUSER8   L     RF,AREPTAB          SET END OF TABLE                             
         LA    RF,L'REPTAB(RF)                                                  
         MVC   0(2,RF),=X'FFFF'                                                 
*                                                                               
VUSER9   CLI   MODE,COPY           USER MUST BE SPECIFIC FOR MODE=COPY          
         BNE   VUSER9A                                                          
         TM    USER,X'02'                                                       
         BZ    VUSERERR                                                         
         B     VUSERX                                                           
VUSER9A  B     VUSERX                                                           
*                                                                               
VUSERERR MVI   ERRNUM,4                                                         
VUSERX   B     XIT2                                                             
*********************************************************************           
* REPORT ID                                                                     
*********************************************************************           
VREPT    NTR1  ,                   REPORTID = ALL OR XXX OR XXX,NNNNN           
         ST    R2,FULL             OR XXX,HH:MM OR XXX,HH:MM-HH:MM              
         XC    REPSUBID(8),REPSUBID                                             
         BNE   VREPTERR                                                         
         MVC   REPSUBID,22(R2)     SET SUBID FROM FIRST SCANNER BLOCK           
         MVC   RESUBID,22(R2)                                                   
         CLC   REPSUBID,=C'ALL'                                                 
         BNE   *+12                                                             
         OI    REPORT,X'80'        SET SUBID=ALL FLAG                           
         B     VREPT1                                                           
         CLC   REPSUBID,SPACES                                                  
         BE    VREPT1                                                           
         OI    REPORT,X'01'        SET SUBID=SPECIFIC FLAG                      
VREPT1   LA    R4,REPSEQL                                                       
*                                                                               
VREPT2   LA    R2,32(R2)           BUMP SCANNER BLOCK                           
         BCT   R0,*+8                                                           
         B     VREPT7                                                           
         CLI   1(R2),0             EXIT IF END OR KEYWORD                       
         BNE   VREPT7                                                           
         TM    2(R2),X'80'         CHECK SEQUENCE NUMBER                        
         BZ    VREPT4                                                           
         CLC   4(4,R2),=F'1'                                                    
         BL    VREPTERR                                                         
         CLC   4(4,R2),MAXSEQ                                                   
         BH    VREPTERR                                                         
         MVC   REPSEQL,6(R2)                                                    
         MVC   REREPNO,6(R2)                                                    
         OI    REPFLAG,X'01'       SET SINGLE REPORT FLAG                       
         OI    REPORT,X'08'        SET SPECIFIC REPORT SEQ INPUT                
         B     VREPT8                                                           
VREPT4   SR    RF,RF               SEE IF VALID TIME EXPRESSION                 
         IC    RF,0(R2)                                                         
         GOTO1 =V(TIMBER),DMCB,(X'80',(RF)),(X'02',DUB),12(R2)                  
         CLI   0(R1),0                                                          
         BNE   VREPT5                                                           
         MVC   REPSEQL,DUB         SET LOW TIME                                 
         MVC   REPSEQH,DUB+2       SET HIGH TIME                                
         OI    REPFLAG,X'04'       SET LOW AND HIGH TIME INPUT                  
         B     VREPT6                                                           
VREPT5   OI    0(R1),X'40'         SET TO VALIDATE A SINGLE TIME                
         GOTO1 (RF),(R1)                                                        
         CLI   0(R1),0                                                          
         BNE   VREPTERR                                                         
         MVC   REPSEQH,DUB         SET HIGH TIME                                
         OI    REPFLAG,X'02'       SET HIGH TIME INPUT                          
VREPT6   SR    RF,RF                                                            
         ICM   RF,3,REPSEQL                                                     
         MH    RF,=H'180'                                                       
         SRL   RF,2                LOWTIME=SECS*3/4                             
         STCM  RF,3,REPSEQL                                                     
         SR    RF,RF                                                            
         ICM   RF,3,REPSEQH                                                     
         MH    RF,=H'180'                                                       
         SRL   RF,2                HIGHTIME=SECS*3/4                            
         STCM  RF,3,REPSEQH                                                     
         B     VREPT8                                                           
*                                                                               
VREPT7   SH    R2,=H'32'           DECR SCANNER BLOCK                           
         AH    R0,=H'1'                                                         
*                                                                               
VREPT8   L     RF,AREPTAB          SAVE IN REPTAB FOR USER                      
         TM    0(RF),X'80'                                                      
         BO    VREPTERR                                                         
         MVC   2(8,RF),REPSUBID                                                 
         B     VREPTX                                                           
VREPTERR L     R2,FULL                                                          
         MVI   ERRNUM,4                                                         
VREPTX   XIT1  REGS=(R0,R2)                                                     
*********************************************************************           
* LIVE RETAIN                                                                   
*********************************************************************           
VLRET    MVI   VLRET,1             LIVE RETAIN HOURS                            
         CLC   22(4,R2),=C'PERM'                                                
         BNE   *+12                                                             
         L     RF,=X'0000FFFF'     SET VALUE FOR PERMANENT RETAIN               
         B     VLRET1                                                           
         TM    3(R2),X'80'                                                      
         BZ    VLRETERR                                                         
         L     RF,8(R2)                                                         
         LTR   RF,RF                                                            
         BZ    VLRETERR                                                         
         C     RF,=F'6240'         TEST MAX OF 1 YEAR (52*5*24)                 
         BH    VLRETERR                                                         
VLRET1   STCM  RF,3,RERETNL                                                     
         B     *+8                                                              
VLRETERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
*********************************************************************           
* DEAD RETAIN                                                                   
*********************************************************************           
VDRET    MVI   VDRET,1             DEAD RETAIN HOURS                            
         CLC   22(4,R2),=C'PERM'                                                
         BNE   *+12                                                             
         L     RF,=X'0000FFFF'     SET VALUE FOR PERMANENT RETAIN               
         B     VDRET1                                                           
         TM    3(R2),X'80'                                                      
         BZ    VDRETERR                                                         
         L     RF,8(R2)                                                         
         LTR   RF,RF                                                            
         BZ    VDRETERR                                                         
         C     RF,=F'6240'         TEST MAX OF 1 YEAR (52*5*24)                 
         BH    VDRETERR                                                         
VDRET1   STCM  RF,3,RERETND                                                     
         B     *+8                                                              
VDRETERR MVI   ERRNUM,4                                                         
         BR    RE                                                               
*********************************************************************           
* TYPE                                                                          
*********************************************************************           
VTYPE    MVI   VTYPE,1             REPORT LINE TYPE                             
         MVC   FILETYP,22(R2)                                                   
VTYPE1   CLI   22(R2),C'F'         F=FIXED V=VARIABLE *=SET FROM INPUT          
         BE    VTYPE2                                                           
         CLI   22(R2),C'V'                                                      
         BE    VTYPE2                                                           
         CLI   22(R2),C'*'                                                      
         BE    VTYPE2                                                           
         B     VTYPEERR                                                         
VTYPE2   CLI   1(R2),1             TEST ONLY ONE CHR INPUT                      
         BNE   *+12                                                             
         MVI   FILETYP+1,C'*'                                                   
         B     VTYPE4                                                           
         CLI   23(R2),C'C'         C=CTLCHR N=NONE *=SET FROM INPUT             
         BE    VTYPE3                                                           
         CLI   23(R2),C'N'                                                      
         BE    VTYPE3                                                           
         CLI   23(R2),C'*'                                                      
         BE    VTYPE3                                                           
         B     VTYPEERR                                                         
VTYPE3   CLI   1(R2),2             TEST 3 CHR LENGTH INPUT AS WELL              
         BNH   VTYPE4                                                           
         CLI   1(R2),5                                                          
         BNE   VTYPEERR                                                         
         MVC   DUB(3),ZEROS                                                     
         MVZ   DUB(3),24(R2)                                                    
         CLC   DUB(3),ZEROS                                                     
         BNE   VTYPEERR                                                         
         PACK  DUB,24(3,R2)                                                     
         CVB   RF,DUB                                                           
         CH    RF,=H'255'                                                       
         BH    VTYPEERR                                                         
         STC   RF,RELINEW                                                       
VTYPE4   EQU   *                   NO MORE VALIDATION REQUIRED                  
         B     VTYPEX                                                           
VTYPEERR MVI   ERRNUM,4                                                         
VTYPEX   BR    RE                                                               
*********************************************************************           
* ARCHIVE                                                                       
*********************************************************************           
VARCH    MVI   VARCH,1             ARCHIVE STATUS                               
VARCH1   CLI   22(R2),C'E'         A=ARCHIVABLE,E=ELIGIBLE,D=ARCHIVED           
         BNE   *+12                                                             
         OI    RETYP1,QLTYAE                                                    
         B     VARCH2                                                           
         CLI   22(R2),C'A'                                                      
         BNE   *+12                                                             
         OI    RETYP1,QLTYAR                                                    
         B     VARCH2                                                           
         CLI   22(R2),C'D'                                                      
         BNE   *+12                                                             
         OI    RETYP1,QLTYAD                                                    
         B     VARCH2                                                           
         B     VARCHERR                                                         
VARCH2   B     VARCHX                                                           
VARCHERR MVI   ERRNUM,4                                                         
VARCHX   BR    RE                                                               
                                                                                
***********************************************************************         
* WRITE PARAMETER FOR SERVICE FILES TO BE OPEN READ ONLY                        
***********************************************************************         
VWRITE   CLI   22(R2),C'Y'         IS IT "YES", ALLOW UPDATE                    
         BE    VWRITX                                                           
         CLI   22(R2),C'N'         IS IT "NO", OPEN READ ONLY                   
         BNE   VWRITERR                                                         
         MVI   WRITE,NO                                                         
         L     RF,=A(SSB)                                                       
         OI    SSOMTIND-SSOOFF(RF),SSOWSRN                                      
         B     VWRITX                                                           
VWRITERR MVI   ERRNUM,4                                                         
VWRITX   BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
* OPEN PRTQ FILE(S) REFERENCED BY INPUT PARAMS                                  
**********************************************************************          
OPNPQ    NMOD1 0,*OPENPQ*                                                       
         LR    RA,R1                                                            
         USING PARMTBL,RA          RA=A(PQMAINT CSECT'S STORAGE)                
         LA    R1,PRTQINP          POINT TO LIST OF INPUT PRTQ IDS              
         ST    R1,APRTQINP                                                      
*                                                                               
OPNPQ1   L     R1,APRTQINP         BUMP TO NEXT FILE IN LIST                    
         LA    R1,1(R1)                                                         
         ST    R1,APRTQINP                                                      
         CLI   0(R1),X'FF'         TEST END OF LIST                             
         BE    OPNPQ6                                                           
         TM    0(R1),X'0F'         TEST IF FILE REFERENCED                      
         BZ    OPNPQ1                                                           
         LA    R0,PRTQINP                                                       
         SR    R1,R0               R1=INTERNAL PRTQ FILE NUM                    
         SLL   R1,3                                                             
         L     RF,APRTQLST         INDEX INRO PRTQ FILE LIST                    
         AR    RF,R1                                                            
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),1(RF)   SET PRTQ FILE ID FOR DATAMGR                 
         MVC   PRTQINT,0(RF)       SET PRTQ FILE INTERNAL NUM                   
         MVC   PRTQEXT,4(RF)       SET PRTQ FILE EXTERNAL NUM                   
         MVC   PRTQDTF+1(3),5(RF)  SET PRTQ FILE A(DTF)                         
         LA    RF,FILEIX(R1)                                                    
         MVC   FILEIX(8),0(RF)     SET ORIGINAL DTF NAME                        
         LA    RF,FILEID(R1)                                                    
         MVC   FILEID(8),0(RF)     SET OVERRIDE DTF NAME                        
*                                                                               
OPNPQ2   L     R2,PRTQDTF          R2=A(PRTQUE DTF)                             
         USING DTFPHD,R2                                                        
         MVI   DUB,C'N'                                                         
         MVC   DUB+1(7),FILEID                                                  
         MVI   DUB+8,C'X'                                                       
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         MVC   P2,ACXREC                                                        
         MVC   P4,PRTQDTF                                                       
         LA    RE,P6                                                            
         ST    RE,P5                                                            
         MVC   P6,=X'00010100'                                                  
*                                                                               
         TM    DTFOPEN,X'20'       TEST IF ALREADY OPEN                         
         BZ    OPNPQ3              NO                                           
         CLC   DTFFID,FILEID       TEST IF SAME FILE ID                         
         BE    OPNPQ4              YES                                          
         MVC   P1,=A(DACLOSE)                                                   
         GOTO1 VDATAMGR,P0,DADDS                                                
*                                                                               
OPNPQ3   MVC   DTFFID,FILEID                                                    
         GOTO1 VDATAMGR,DMCB,=C'OPEN',=C'SER',DUB                               
*                                                                               
OPNPQ4   MVC   DNEXT,=X'00010000'                                               
         MVC   P1,=A(RDID)                                                      
         GOTO1 VDATAMGR,P0,DADDS   READ FIRST RECORD                            
         DROP  R2                                                               
*                                                                               
OPNPQ5   L     R2,ACXREC           READ FIRST INDEX RECORD                      
         MVC   CXADDR,=X'00010100'                                              
         GOTO1 VDATAMGR,DMCB,DMREAD,PRTQID,CXADDR,(R2)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,PRTQDTF          SET CIDATA STORED INFRONT OF DTF             
         SH    RE,=H'40'                                                        
         MVC   0(20,RE),0(R2)      EXTRACT PART 1 INDEX DATA                    
         TM    0(RE),X'80'                                                      
         BZ    *+14                                                             
         MVC   20(20,RE),24(R2)    EXTRACT PART 2 INDEX DATA                    
         NI    0(RE),X'7F'                                                      
*                                                                               
         L     RF,PRTQDTF                                                       
         TM    DTFTYPE-DTFPHD(RF),DTFTBIGF  IS THIS A 20 BIT PQ                 
         JO    *+10                                                             
         MVC   CJHIREC-CIDATA(2,RE),CIHIREC-CIDATA(RE)                          
*                                                                               
         MVC   14(1,RE),PRTQINT    SET INTERNAL PRTQ FILE NUM                   
         MVC   15(5,RE),PRTQID     SET ALPHA PRTQ FILE NAME                     
         MVC   CIDATA,0(RE)                                                     
         L     RE,PRTQDTF          SET F/L REC LEN IN PRTQUE DTF                
         LA    RE,52(RE)                                                        
         MVC   0(2,RE),CIBLKLN                                                  
         OI    0(RE),X'80'                                                      
         B     OPNPQ1              BACK FOR NEXT PRTQ FILE                      
*                                                                               
OPNPQ6   XC    CXPAGE,CXPAGE       SET FIRST INDEX ENTRY                        
         LH    R5,CICINDX                                                       
         BCTR  R5,0                                                             
         STH   R5,CXENTRY                                                       
*                                                                               
OPNPQX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*FAUTL                                                                          
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
*FASSB                                                                          
       ++INCLUDE FASSB                                                          
         ORG     SSBD                                                           
       ++INCLUDE FASSBOFF                                                       
         ORG                                                                    
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*DMPRTQD                                                                        
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
*DMPRTQL                                                                        
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
*DDDPRINTL                                                                      
       ++INCLUDE DDDPRINTL                                                      
         EJECT                                                                  
*DDBIGBOX                                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
         DSECT                                                                  
         DCBD  DSORG=PS            GERNERATE PS-DCB DSECT                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019DMPRTQXFR 11/16/20'                                      
         END                                                                    
