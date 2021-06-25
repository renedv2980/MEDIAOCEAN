*          DATA SET DDREFUSE   AT LEVEL 009 AS OF 01/19/21                      
***********************************************************************         
* THIS PROGRAM IS DESIGNED TO READ CARDS FROM YOUR JCL AND READ THE   *         
* APPROPRIATE RECORDS FROM A RECOVERY FILE.                           *         
*                                                                     *         
* JOB STATEMENTS FOR THIS JOB                                         *         
* //RECVIN  DD (REQUIRED - INPUT FILE)                                *         
* //RECVOUT DD (OPTIONAL - OUTPUT FILE - OUTPUT=FILE OR OUTPUT=BOTH)  *         
*                                                                     *         
* FILES MUST BE DEFINED IN DMFILTAB                                   *         
*                                                                     *         
* INPUT CARDS FOR THIS JOB                                            *         
* 1. REQUIRED CARDS                                                   *         
*                                                                     *         
* OUTPUT=PRINT/DISK/BOTH                                              *         
*  NOTE: OUTPUT=DISK  STILL PRINTS INPUT PARAMETERS AND TOTALS        *         
*        OUTPUT=PRINT PARIALLY TRANSLATES RECOVERY HEADER             *         
*                                                                     *         
* FILES=##,##,##                                                      *         
*      ## IS EITHER HEX FILE NUMBER OR CHARACTER FILE NAME            *         
*                                                                     *         
* YOU ALSO NEED ONE OF THE FILTERS MARKED WITH A #                    *         
*                                                                     *         
* 2. OPTIONAL CARDS                                                   *         
*                                                                     *         
*  TERMNUM=2746   *******NO LONGER SUPPORTED**********                *         
*                                                                     *         
*# KEY= (STANDARD DECODE FORMAT)                                      *         
*      EXACT MATCH (ON INPUT LENGTH GIVEN)                            *         
*                                                                     *         
*# LOWKEY= (STANDARD DECODE FORMAT)                                   *         
*      START AT KEY (CAN USE X'00' FOR ALL RECORDS)                   *         
*                                                                     *         
*# HIGHKEY= (STANDARD DECODE FORMAT)                                  *         
*      END AT KEY                                                     *         
*                                                                     *         
*# REQ= FILTER ON REQUEST CARD DETAILS                                *         
*      EXACT MATCH (ON INPUT LENGTH GIVEN)                            *         
*                                                                     *         
*# LUID=DDSX0002                                                      *         
*      FILTER ON A SPECIFIC TERMINAL LUID - CL8                       *         
*                                                                     *         
*# LOWTIME=HH:MM:SS                                                   *         
*      START AT TIME FILTER - SECONDS OPTIONAL                        *         
*                                                                     *         
*# HIGHTIME=HH:MM:SS                                                  *         
*      END AT TIME FILTER - SECONDS OPTIONAL                          *         
*                                                                     *         
*# DISKADR=00010A00                                                   *         
*      FILTER ON DISK ADDRESS - CAN BE 6 OR 8 CHARACTERS              *         
*                                                                     *         
*# PID=NNNN                                                           *         
*      FILTER ON PERSON ID - CAN ALSO USE (PERSONID INSTEAD OF PID)   *         
*                                                                     *         
*# SYSIN=HHHHHHHH                                                     *         
*      FILTER ON SYSTEM INPUT NUMBER IN HEX - LOW ORDER 6 DIGITS ONLY *         
*      ARE USED - IF YOU INPUT 8 DIGITS THE TOP 2 ARE IGNORED         *         
*      SUPPORTS MULTIPLE SINS ON DIFFERENT LINES                      *         
*                                                                     *         
*# GIN=HHHHHHHHHHHHHHHH                                               *         
*      FILTER ON 8-BYTE GIN IN HEX                                    *         
*                                                                     *         
*# GINTS=HHHHHHHH                                                     *         
*      FILTER ON 4-BYTE GIN TIME STAMP                                *         
*                                                                     *         
*# GINNUM=HHHHHHHH                                                    *         
*      FILTER ON 4-BYTE GIN NUMBER                                    *         
*                                                                     *         
*                                                                     *         
*# USERID=NNNNN                                                       *         
*      FILTER ON USER ID NUMBER (DECIMAL 0-65K)                       *         
*                                                                     *         
*# TRNAMT=1234.56 OR ALL FOR ALL TRANSACTIONS                         *         
*      FILTER ON TRANSACTION AMOUNT                                   *         
*                                                                     *         
*# RTYPE=COPY/CHANGE/ADD                                              *         
*      FILTER ON RECORD TYPE                                          *         
*                                                                     *         
*# AC#COUNT=O ONLY PRINT RECORD COUNTS                                *         
*                                                                     *         
*# DUMP=OS (FOR SYSMDUMP TYPE DUMP)                                   *         
*                                                                     *         
*# MAXOUT=NNNNNN                                                      *         
*      MAXIMUM NUMBER OF OUTPUT MATCHES                               *         
***********************************************************************         
                                                                                
*PHASE REFUSEA                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE ACLDDCNT                                                               
*INCLUDE BIGPRNT                                                                
*INCLUDE CARDS                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DECODE                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRTREC                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE STXITER                                                                
*INCLUDE DUMMY                                                                  
         TITLE 'RCVPEEL - PEEL/SORT/SAVE ANY RECOVERY DATA'                     
REFUSE   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,REFUSE,=V(REGSAVE),RA,R8                                       
         ST    RD,SAVERD                                                        
*                                                                               
         USING DPRINT,R9                                                        
         L     R9,VCPRINT                                                       
*                                                                               
         BRAS  RE,INIT              INITIALISE - READ/VALIDATE CARDS            
         BNE   XBASE                                                            
         CLI   DUMPFLG,DUMPOS                                                   
         BE    GO                                                               
         LA    RE,REFUSE                                                        
         ST    RE,DUMPLIST                                                      
         L     RF,=V(DUMMY)                                                     
         ST    RF,DUMPLIST+4                                                    
         OI    DUMPLIST+4,X'80'                                                 
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
                                                                                
GO       BRAS  RE,MAIN              READ FILE,FILTER AND PUT RECORDS            
         B     XBASE                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* MAIN ROUTINE TO READ/PROCESS RECORDS                                *         
***********************************************************************         
                                                                                
MAIN     NTR1  ,                                                                
         MVC   TITLE,MTITLE                                                     
         ZAP   LINE,P99            NEXT PAGE                                    
*                                                                               
         OPEN  (RCVTAPE,INPUT)                                                  
*                                                                               
         TM    FLAG,FLAGDS         WRITING OUT TO FILE?                         
         BZ    MAIN02                                                           
         OPEN  (RCVOUT,(OUTPUT))   YES                                          
*                                                                               
MAIN02   LA    R0,RLEN             CLEAR BUFFER FIRST                           
         LHI   R1,RDATALNQ+4                                                    
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GET   RCVTAPE,RLEN        GET NEXT RECOVERY RECORD                     
*                                                                               
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BNE   MAIN03                                                           
         CLI   DELETE,YES                                                       
         BNE   MAIN02                                                           
*                                                                               
MAIN03   TM    RRECTY,X'80'        IGNORE POINTER COPIES/CHANGES                
         BO    MAIN02                                                           
*                                                                               
         L     R0,RECSIN           ADD 1 TO RECORDS READ                        
         AHI   R0,1                                                             
         ST    R0,RECSIN                                                        
*                                                                               
         BRAS  RE,FILTER                                                        
         BNE   MAIN02                                                           
         BRAS  RE,OUTPUT                                                        
*                                                                               
         L     RF,NUMOUT                                                        
         AHI   RF,1                                                             
         ST    RF,NUMOUT                                                        
         ICM   RE,15,MAXOUT        TEST IF MAXIMUM OUTPUT SET                   
         BZ    MAIN02                                                           
         CR    RF,RE                                                            
         BL    MAIN02                                                           
         MVC   P(14),=CL14'Reached MAXOUT'                                      
*                                                                               
MAIN04   CLOSE RCVTAPE                                                          
         TM    FLAG,FLAGDS         WRITING OUT TO FILE?                         
         BZ    MAIN06              NO                                           
         CLOSE RCVOUT                                                           
*                                                                               
MAIN06   GOTO1 VPRINTER                                                         
         MVC   P(20),=CL20'Records read ='                                      
         EDIT  (B4,RECSIN),(8,P+21),ZERO=NOBLANK                                
         GOTO1 VPRINTER                                                         
                                                                                
         MVC   P(20),=CL20'Records written ='                                   
         EDIT  (B4,RECSOUT),(8,P+21),ZERO=NOBLANK                               
         GOTO1 VPRINTER                                                         
                                                                                
         MVC   P(20),=CL20'Recovery file date ='                                
         GOTO1 VDATCON,DMCB,(3,SRDATE),(10,P+21)                                
         GOTO1 VPRINTER                                                         
                                                                                
         CLI   AC#COUNT,NO                                                      
         BE    EXITOK                                                           
         GOTOR =V(LDCOUNT),DMCB,(X'FF',0)                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FILTERING ROUTINE                                                   *         
***********************************************************************         
                                                                                
FILTER   NTR1  ,                                                                
         MVC   SRDATE,RDATE                                                     
                                                                                
         LA    R1,FILETBL          FILTER ON FILE NUMBER                        
         LHI   RF,1                                                             
FILT02   CLI   0(R1),X'FF'         EOT                                          
         BE    EXITL               YES - FAIL                                   
         CLC   RFILTY,0(R1)        MATCH FILE NUMBER                            
         BE    *+8                 YES                                          
         BXH   R1,RF,FILT02                                                     
*                                                                               
         XC    EXTLEN,EXTLEN                                                    
         TM    RTIME,X'40'         Trailer?                                     
         BZ    FILT02X                                                          
         LA    RE,RLEN             POINT TO RECORD LENGTH                       
         AH    RE,0(RE)            ADD RECORD LENGTH                            
         BCTR  RE,0                BACK UP TO THE LAST BYTE OF TRAILER          
         XR    RF,RF                                                            
         IC    RF,0(RE)            GET LENGTH OF TRAILER                        
         STH   RF,EXTLEN           Save off length                              
         BCTR  RF,0                                                             
         SR    RE,RF               BACK UP TO THE RECOVERY EXTENSION            
         MVC   EXTDATA(0),0(RE)                                                 
         EX    RF,*-6              Save off trailer                             
         USING RECVEXTD,EXTDATA                                                 
*                                                                               
FILT02X  TM    FLAG,FLAGKY         EXACT KEY?                                   
         BZ    FILT03              NO                                           
         LH    RF,KLEN             KEY LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,*+12                                                          
         BNE   EXITL                                                            
         B     FILT06                                                           
         CLC   RDATA(0),FKEY                                                    
*                                                                               
FILT03   TM    FLAG,FLAGRQ         REQUEST CARD KEY?                            
         BZ    FILT04              NO                                           
         LH    RF,KLEN             KEY LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,*+12                                                          
         BNE   EXITL                                                            
         B     FILT06                                                           
         CLC   RDATA+80(0),FKEY                                                 
*                                                                               
FILT04   TM    FLAG,FLAGKYL        LOW-HIGH KEY RANGE?                          
         BZ    FILT06              NO                                           
         LH    RF,LKLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BL    EXITL                                                            
         CLC   RDATA(0),FLKEY      MATCH ON LOW KEY                             
*                                                                               
         TM    FLAG,FLAGKYH        HIGH KEY INPUT?                              
         BZ    FILT06              NO                                           
         LH    RF,HKLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BH    EXITL                                                            
         CLC   RDATA(0),FHKEY      MATCH ON HIGH KEY                            
*                                                                               
FILT06   OC    FLUID,FLUID         LUID FILTER?                                 
         BZ    FILT07                                                           
         TM    RTIME,X'40'         HAS TRAILER IN RECOVERY RECORD?              
         BZ    FILT07              NO TRAILER                                   
         CLC   FLUID,RLUID                                                      
         BNE   EXITL                                                            
*                                                                               
FILT07   OC    LDATE,LDATE         LOW DATE?                                    
         BZ    FILT08                                                           
         CLC   RDATE,LDATE                                                      
         BL    EXITL                                                            
         BH    FILT10                                                           
*                                                                               
FILT08   OC    LTIME,LTIME         LOWTIME?                                     
         BZ    FILT10                                                           
         MVC   TTIME,RTIME                                                      
         NC    TTIME,=X'3FFFFFF0'  TURN OFF X'80' AND X'40' BITS                
         ICM   RE,15,TTIME                                                      
         TM    RTIME,X'80'                                                      
         BNO   *+12                                                             
         SRL   RE,8                                                             
         SLL   RE,4                RE=0HHMMSS0                                  
         C     RE,LTIME                                                         
         BL    EXITL                                                            
*                                                                               
FILT10   OC    HDATE,HDATE         HIGH DATE?                                   
         BZ    FILT11                                                           
         CLC   RDATE,HDATE                                                      
         BH    EXITL                                                            
         BL    FILT12                                                           
*                                                                               
FILT11   OC    HTIME,HTIME         HIGHTIME?                                    
         BZ    FILT12                                                           
         MVC   TTIME,RTIME                                                      
         NC    TTIME,=X'3FFFFFF0'  TURN OFF X'80' AND X'40' BITS                
         L     RE,TTIME                                                         
         TM    RTIME,X'80'                                                      
         BNO   *+12                                                             
         SRL   RE,8                                                             
         SLL   RE,4                RE=0HHMMSS0                                  
         C     RE,HTIME                                                         
         BH    EXITL                                                            
*                                                                               
FILT12   OC    DSKADR,DSKADR                                                    
         BZ    *+14                                                             
         CLC   RVCHR,DSKADR                                                     
         BNE   EXITL                                                            
*                                                                               
FILT14   OC    PERSONID,PERSONID                                                
         BZ    *+14                                                             
         CLC   RPERSON,PERSONID                                                 
         BNE   EXITL                                                            
*                                                                               
FILT16   SR    RF,RF               FILTER ON SINS IN POOL                       
         ICM   RF,3,NUMSINS                                                     
         BZ    FILT18                                                           
         LARL  RE,SINTAB           RE=A(POOL OF SINS)                           
FILT17   CLC   RSIN+1(3),1(RE)                                                  
         BE    FILT18              FOUND MY SIN                                 
         LA    RE,4(RE)                                                         
         BCT   RF,FILT17                                                        
         B     EXITL                                                            
*                                                                               
FILT18   OC    PROGNUM,PROGNUM                                                  
         BZ    *+14                                                             
         CLC   RPRG,PROGNUM                                                     
         BNE   EXITL                                                            
*                                                                               
FILT20   OC    USER,USER                                                        
         BZ    *+14                                                             
         CLC   RUSER,USER+2                                                     
         BNE   EXITL                                                            
*                                                                               
FILT21   CLI   ALFA,C' '                                                        
         BNH   *+14                                                             
         CLC   RAGYSEC,ALFA                                                     
         BNE   EXITL                                                            
*                                                                               
FILT22   CLI   RECTYPE,0                                                        
         BE    *+14                                                             
         CLC   RRECTY,RECTYPE                                                   
         BNE   EXITL                                                            
*                                                                               
         OC    GIN(16),GIN         FILTER ON GIN/GINTS/GINNU                    
         BZ    FILT24                                                           
         LA    R4,GIN                                                           
         LA    R5,8                                                             
         LA    RF,DUB                                                           
         OC    GIN,GIN                                                          
         BNZ   FILT23                                                           
*                                                                               
         LA    R4,GINTS                                                         
         LA    R5,4                                                             
         LA    RF,DUB                                                           
         OC    GINTS,GINTS                                                      
         BNZ   FILT23                                                           
*                                                                               
         LA    R4,GINNUM                                                        
         LA    R5,4                                                             
         LA    RF,DUB+4                                                         
*                                                                               
FILT23   TM    RTIME,X'40'         ANY EXT REC?                                 
         BZ    FILT24              NO-SKIP OVER                                 
*                                                                               
         MVC   DUB,RGIN                                                         
         BCTR  R5,0                                                             
         EX    R5,FILT23EX                                                      
         BNE   EXITL                                                            
         B     FILT24                                                           
FILT23EX CLC   0(0,RF),0(R4)                                                    
*                                                                               
FILT24   CLI   RFILTY,X'6A'        ONLY CHECK FOR TRNAMT ON ACCMST              
         BNE   EXITOK                                                           
         CLI   AC#COUNT,NO                                                      
         BE    FILT26                                                           
         LA    R2,RDATA                                                         
         GOTOR =V(LDCOUNT),DMCB,(X'21',(R2))                                    
*                                                                               
FILT26   OC    TRANAMT,TRANAMT                                                  
         BZ    EXITOK                                                           
*                                                                               
FILT28   LA    R2,RDATA                                                         
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING TRNELD,R2                                                        
         XR    RF,RF                                                            
FILT30   CLI   TRNEL,0                                                          
         BE    EXITL                                                            
         CLI   TRNEL,TRNELQ        X'44' TRANS ELEM                             
         BE    *+12                                                             
         IC    RF,TRNLN                                                         
         BXH   R2,RF,FILT30                                                     
*                                                                               
FILT32   CLC   TRANAMT,EFFS        ALL TRANSACTION                              
         BE    EXITOK                                                           
         CP    TRANAMT,TRNAMNT                                                  
         BE    EXITOK                                                           
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT RECORD TO DISK AND/OR PRINT                       *         
***********************************************************************         
                                                                                
OUTPUT   NTR1  ,                                                                
         L     R0,RECSOUT                                                       
         AHI   R0,1                                                             
         ST    R0,RECSOUT                                                       
*                                                                               
         TM    FLAG,FLAGDS         WRITE TO DISK FILE?                          
         BZ    OPUT02              NO                                           
         PUT   RCVOUT,RLEN                                                      
*                                                                               
OPUT02   TM    FLAG,FLAGPR         PRINT OUT?                                   
         BZ    EXITOK              NO                                           
*                                                                               
         CLI   AC#COUNT,ONLY       RECORD COUNTS ONLY?                          
         BE    EXITOK              YES                                          
         XR    R5,R5                                                            
         IC    R5,RFILTY           FIND FILE ENTRY IN DMFILTAB                  
         MHI   R5,DMFLLEN          TABLE IS INDEXED BY FILE NUMBER              
         A     R5,AFILTAB          R5 = A(ENTRY)                                
         USING FILTABD,R5                                                       
*                                                                               
         USING PLHDR,P                                                          
         MVC   PLFILEH,=CL05'File='                                             
         GOTO1 VHEXOUT,DMCB,RFILTY,PLFHEX,1,0                                   
         MVI   PLFHEXX,C'/'                                                     
         MVC   PLFALPH,DMFLNAME     PRINT FILE NAME                             
         CLC   PLFALPH,SPACES                                                   
         BNE   *+10                                                             
         MVC   PLFALPH,QUERY        NO FILENAME IN DMFILTAB                     
*                                                                               
         MVC   PLTYPEH,=CL05'Trns='                                             
         MVC   PLTYPE,=CL04'????'                                               
         CLI   RRECTY,X'01'        COPY?                                        
         BNE   *+10                                                             
         MVC   PLTYPE,=CL04'Copy'                                               
         CLI   RRECTY,X'02'        CHANGE?                                      
         BNE   *+10                                                             
         MVC   PLTYPE,=CL04'Chg '                                               
         CLI   RRECTY,X'03'        ADD?                                         
         BNE   *+10                                                             
         MVC   PLTYPE,=CL04'Add '                                               
*                                                                               
         TM    RTIME,X'40'         HAS TRAILER IN RECOVERY RECORD?              
         BO    OPUT04              NO TRAILER                                   
         MVC   PLTERMH,=CL06'Term#='                                            
         XR    R0,R0                                                            
         ICM   R0,3,RTRM                                                        
         EDIT  (R0),(5,PLTERM),0,ALIGN=LEFT,ZERO=NOBLANK                        
         B     OPUT06                                                           
*                                                                               
OPUT04   TM    RTIME,X'40'         ANY EXT REC?    LENGTH                       
         BZ    OPUT06              NO-SKIP OVER                                 
         MVC   PLLUIDH,=CL05'LUID='                                             
         MVC   PLLUID,RLUID                                                     
*                                                                               
         MVC   PLAGYH,=CL07'Agency='                                            
         MVC   PLAGY,RAGYSEC                                                    
*                                                                               
         MVC   PLFACH,=CL07'Offline'                                            
         MVI   PLAOR,C' '                                                       
         XR    RF,RF                                                            
         ICM   RF,1,RSYSIX                                                      
         JZ    OPUT06              None to report                               
         LR    R1,RF                                                            
         NILL  GRF,X'000F'         TOR                                          
         NILL  GR1,X'00F0'         AOR potentially                              
         CHI   R1,0                Must be TOR                                  
         JE    OPUT04A                                                          
         SRL   R1,4                                                             
         STC   R1,PLAOR                                                         
         OI    PLAOR,X'C0'         Convert to A, B , C or D                     
*                                                                               
         USING FACIDD,RE                                                        
OPUT04A  MHI   RF,L'FACITAB                                                     
         L     RE,AFACITAB                                                      
OPUT04B  CLI   0(RE),X'FF'         EOT                                          
         JE    OPUT06              Bad DSPACE=                                  
         CLC   DSPACE,FACIDSPC     Match on DSPACE                              
         JE    OPUT04C                                                          
         AHI   RE,FACIDLNQ                                                      
         J     OPUT04B                                                          
*                                                                               
         USING FACITABD,RF                                                      
OPUT04C  A     RF,FACAID           Address of correct enviroment                
         MVC   PLFACH,=CL07'Facpak='                                            
         MVC   PLFAC,FACISN4                                                    
         DROP  RE,RF                                                            
*                                                                               
OPUT06   MVC   TTIME,RTIME                                                      
         NC    TTIME,=X'3FFFFFF0'  TURN OFF X'80' AND X'40' BITS                
         GOTO1 VHEXOUT,DMCB,TTIME,DUB,4                                         
*                                                                               
         LA    RE,DUB+1                                                         
         TM    RTIME,X'80'         NEW STYLE TIME FORMAT                        
         BNO   *+8                                                              
         LA    RE,DUB                                                           
*                                                                               
         MVC   PLTIMEH,=CL05'Time='                                             
         MVI   PLHHS,C':'                                                       
         MVI   PLMMS,C':'                                                       
         MVC   PLHH,0(RE)                                                       
         MVC   PLMM,2(RE)                                                       
         MVC   PLSS,4(RE)                                                       
*                                                                               
         MVC   PLDATEH,=CL05'Date='                                             
         GOTO1 VDATCON,DMCB,(3,RDATE),(11,PLDATE)                               
*                                                                               
         MVC   PLDSKH,=CL04'D/A='                                               
         GOTO1 VHEXOUT,DMCB,RVCHR,PLDSK,4                                       
*                                                                               
         MVC   PLUSERH,=CL05'User='                                             
         XR    R0,R0                                                            
         ICM   R0,3,RUSER                                                       
         EDIT  (R0),(5,PLUSER),ALIGN=LEFT,ZERO=NOBLANK                          
         GOTO1 VPRINTER                                                         
         GOTO1 (RF)                                                             
*                                                                               
         MVC   P(L'HEADER),HEADER  PRINT RECOVERY HEADER IN HEX                 
         GOTO1 VHEXOUT,DMCB,RECVHDR,P+L'HEADER,L'RECVHDR                        
         CLI   RSIN,X'FF'          Deleted                                      
         BNE   *+10                                                             
         MVC   P+L'HEADER+((L'RECVHDR*2)+16)(7),=C'Deleted'                     
         GOTO1 VPRINTER                                                         
*                                                                               
         TM    RTIME,X'40'         HAS TRAILER IN RECOVERY RECORD?              
         BZ    OPUT08              NO TRAILER                                   
         LH    RF,EXTLEN                                                        
         MVC   P(L'TRAILER),TRAILER                                             
         GOTO1 VHEXOUT,DMCB,EXTDATA,P+L'TRAILER,(RF)                            
         GOTO1 VPRINTER                                                         
*                                                                               
OPUT08   GOTO1 VPRINTER                                                         
         CLI   DMFLLEND,0          RECORD LENGTH PRESENT IN RECORD?             
         BNE   OPUT10              YES: USE PRTREC TO PRINT IT                  
         LH    R0,RECVHDR-4        NO: USE PRNTBL                               
         SHI   R0,24+4             SUBRACT 24 FOR HEADER, 4 FOR LENGTH          
         SH    R0,EXTLEN           SUBTRACT RECOVERY EXTENSION LENGTH           
         GOTO1 VPRNTBL,DMCB,0,RDATA,C'DUMP',(R0),=C'1D'                         
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
*                                                                               
OPUT10   XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(1),DMFLELD   DISPLACEMENT TO FIRST ELEMENT                
         MVC   DMCB+7(1),DMFLLEND  DISPLACEMENT TO RECORD LENGTH                
         MVC   DMCB,=A(RDATA)      A(RECORD)                                    
         MVI   DMCB,C'R'           DEFAULT TO NON-ELEMENTAL FORMAT              
         CLI   RFILTY,X'22'        SPOT STAFILE RECS HAVE NO ELEMENTS           
         BE    OPUT12                                                           
         TM    DMFLSTYP,DMFLREQ+DMFLFIX  NOR DO REQFILES/DIRECTORIES            
         BNZ   OPUT12                                                           
         CLI   RFILTY,X'21'        NOR DO SPTFILE HEADER RECORDS                
         BNE   *+12                                                             
         CLI   RDATA,0                                                          
         BE    OPUT12                                                           
         MVI   DMCB,C'E'           THIS RECORD SHOULD HAVE ELEMENTS             
*                                                                               
OPUT12   GOTO1 VPRTREC,DMCB,,,VPRINT,VHEXOUT                                    
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
                                                                                
INIT     NTR1  ,                                                                
         MVC   TITLE,CTITLE                                                     
         ZAP   LINE,P99                                                         
         MVI   AC#COUNT,NO         DEFAULT                                      
*                                                                               
         L     R3,ASVCARD          READ IN ALL CARDS                            
INIT02   GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
         CLC   =C'/*',0(R3)        END OF CARDS?                                
         BE    *+12                YES                                          
         AHI   R3,80                                                            
         B     INIT02                                                           
*                                                                               
         L     R3,ASVCARD          NOW PROCESS CARDS INDIVIDUALLY               
INIT04   CLC   =C'/*',0(R3)                                                     
         BE    INIT06                                                           
         MVC   P(80),0(R3)                                                      
         GOTO1 VPRINTER            PRINT PARAMETER CARD                         
         MVC   CARD,0(R3)                                                       
         BRAS  RE,CARDVAL          VALIDATE KEYWORD=VALUE                       
         BE    *+8                                                              
         BRAS  RE,ERROUT           OUTPUT ERROR MESSAGE                         
         AHI   R3,80                                                            
         B     INIT04                                                           
*                                                                               
INIT06   TM    FLAG,FLAGOP         WE NEED AN OUTPUT= ALWAYS                    
         BNZ   *+12                                                             
         MVI   FERN,22                                                          
         BRAS  RE,ERROUT                                                        
*                                                                               
         TM    FLAG,FLAGFL         WE NEED A FILES= ALWAYS                      
         BNZ   *+12                                                             
         MVI   FERN,23                                                          
         BRAS  RE,ERROUT                                                        
*                                  1 OF THE FOLLOWING FILTERS REQUIRED          
         TM    FLAG,FLAGKY+FLAGKYL+FLAGKYH+FLAGRQ                               
         BNZ   INITX               KEY FILTER                                   
         OC    FLUID,FLUID                                                      
         BNZ   INITX               LUID FILTER                                  
         OC    LTIME,LTIME                                                      
         BNZ   INITX               START TIME FILTER                            
         OC    HTIME,HTIME                                                      
         BNZ   INITX               END TIME FILTER                              
         OC    DSKADR,DSKADR                                                    
         BNZ   INITX               DISK ADDRESS FILTER                          
         OC    PERSONID,PERSONID                                                
         BNZ   INITX               PERSON ID FILTER                             
         OC    NUMSINS,NUMSINS                                                  
         BNZ   INITX               SYSTEM INPUT NUMBER                          
         OC    USER,USER                                                        
         BNZ   INITX               USER ID FILTER                               
         OC    TRANAMT,TRANAMT                                                  
         BNZ   INITX               AMOUNT FILTER                                
         OC    GIN(16),GIN                                                      
         BNZ   INITX               GIN FILTER                                   
*                                                                               
         MVI   FERN,24                                                          
         BRAS  RE,ERROUT                                                        
*                                                                               
INITX    MVI   FERN,0                                                           
         OC    ERRCNT,ERRCNT       ERRORS?                                      
         BNZ   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
                                                                                
CARDVAL  NTR1  ,                                                                
         LA    R2,CARD             R2=A(CARD START)                             
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 VSCAN31,DMCB,(R2),SCANBLK,0,(10,SCICARD),68                      
         CLI   4(R1),0                                                          
         BNE   *+12                INVALID LINE                                 
         MVI   FERN,01                                                          
         B     EXITL                                                            
*                                                                               
         LA    R2,SCANBLK                                                       
         USING SCANBLKD,R2                                                      
         LA    R3,CARDTAB                                                       
         USING CARDTABD,R3                                                      
         LHI   R1,CARDTABL                                                      
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       EOT                                          
         BNE   *+12                                                             
         MVI   FERN,2              INVALID KEYWORD                              
         B     EXITL                                                            
*                                                                               
         IC    RF,CXLEN                                                         
         EX    RF,CARDVCLC         MATCH KEYWORD IN TABLE                       
         BE    CARDV04                                                          
         BXH   R3,R1,CARDV02                                                    
*                                                                               
CARDVCLC CLC   SC1STFLD(0),CNAME   COMPARE KEYWORD                              
*                                                                               
CARDV04  CLI   CTYPE,CTNUM     *** NUMERIC INPUT?                               
         BNE   CARDV06             NO                                           
         CLI   SC2NDLEN,0          ENSURE SECOND VALUE                          
         BNE   *+12                                                             
         MVI   FERN,3              MISSING VALUE                                
         B     EXITL                                                            
         TM    SC2NDVAL,SCNUMQ     ENSURE VALUE IS NUMERIC                      
         BO    *+12                                                             
         MVI   FERN,4              NOT A NUMBER                                 
         B     EXITL                                                            
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BNL   *+12                                                             
         MVI   FERN,5              TOO SMALL                                    
         B     EXITL                                                            
         CLC   SC2NDNUM,CMAX                                                    
         BNH   *+12                                                             
         MVI   FERN,6              TOO BIG                                      
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,COUT          GET A(OUTPUT AREA)                           
         MVC   0(4,RF),SC2NDNUM    SET VALUE INTO OUTPUT AREA                   
         B     EXITOK                                                           
*                                                                               
CARDV06  CLI   CTYPE,CTCHR     *** CHARACTER INPUT                              
         BNE   CARDV08             NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN       ENSURE SECOND VALUE                          
         BNZ   *+12                                                             
         MVI   FERN,3              MISSING VALUE                                
         B     EXITL                                                            
         C     RF,CMIN             SCOPE FOR LENGTH OF TEXT INPUT               
         BNL   *+12                                                             
         MVI   FERN,7              TOO SHORT                                    
         B     EXITL                                                            
         C     RF,CMAX                                                          
         BNH   *+12                                                             
         MVI   FERN,8              TOO LONG                                     
         B     EXITL                                                            
*                                                                               
         ICM   RE,15,COUT          MOVE IN FIELD                                
         IC    RF,CLEN                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SC2NDFLD                                                 
         B     EXITOK                                                           
*                                                                               
CARDV08  CLI   CTYPE,CTHEX     *** HEX INPUT                                    
         BNE   CARDV10             NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN       ENSURE SECOND VALUE                          
         BNZ   *+12                                                             
         MVI   FERN,3              MISSING VALUE                                
         B     EXITL                                                            
         TM    SC2NDVAL,SCHEXQ     ENSURE DATA IS VALID HEX                     
         BO    *+12                                                             
         MVI   FERN,9              INVALID HEX DATA                             
         B     EXITL                                                            
         C     RF,CMIN             SCOPE FOR LENGTH OF HEX INPUT                
         BNL   *+12                                                             
         MVI   FERN,10             TOO SHORT                                    
         B     EXITL                                                            
         C     RF,CMAX                                                          
         BNH   *+12                                                             
         MVI   FERN,11             TOO LONG                                     
         B     EXITL                                                            
*                                                                               
         MVI   WORK,C'0'           ZERO FILL TEMP AREA                          
         MVC   WORK+1(L'WORK-1),WORK                                            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,CLEN                                                        
         SLL   RF,1                LENGTH OF OUTPUT AREA AS EBCDIC              
         LR    R1,RF                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,SC2NDLEN                                                    
         SR    R1,RE               R1=DISPLACEMENT INTO WORK FOR MOVE           
         LA    R1,WORK(R1)                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SC2NDFLD    RIGHT ALIGN INPUT DATA                       
*                                                                               
         ICM   R0,15,COUT                                                       
         GOTO1 VHEXIN,DMCB,WORK,(R0),(RF)                                       
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
CARDV10  CLI   CTYPE,CTRTN     *** SELF VALIDATING ROUTINE                      
         BNE   CARDV12             NO                                           
         ICM   RF,15,COUT                                                       
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
CARDV12  DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT ERROR MESSAGE (FROM FERN) TO PRINT LINE           *         
***********************************************************************         
                                                                                
ERROUT   NTR1  ,                                                                
         MVC   P(15),=CL15'!! ***ERROR*** '                                     
         XR    R1,R1                                                            
         IC    R1,FERN                                                          
         BCTR  R1,0                                                             
         MHI   R1,50                                                            
         A     R1,AERRTAB                                                       
         MVC   P+15(50),0(R1)                                                   
         GOTO1 VPRINTER                                                         
         LH    R0,ERRCNT                                                        
         AHI   R0,1                                                             
         STH   R0,ERRCNT                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE OUTPUT TYPE                                     *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALOP    NTR1  ,                                                                
         TM    FLAG,FLAGOP         ANOTHER OUTPUT= CARD INPUT?                  
         BZ    *+12                NO                                           
         MVI   FERN,12                                                          
         B     EXITL                                                            
*                                                                               
         OI    FLAG,FLAGOP         SET OUTPUT CARD FOUND                        
*                                                                               
         LA    R3,VALOPTAB         MATCH TEXT INPUT                             
         LHI   RF,L'VALOPTAB                                                    
         XR    R1,R1                                                            
         ICM   R1,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3              NO SECOND INPUT                              
         B     EXITL                                                            
         BCTR  R1,0                                                             
*                                                                               
VALOP02  CLI   0(R3),EOT           EOT?                                         
         BNE   *+12                                                             
         MVI   FERN,13             BAD VALUE                                    
         B     EXITL                                                            
         EX    R1,VOPCLC           MATCH VALUE                                  
         BE    VALOP04                                                          
         BXH   R3,RF,VALOP02                                                    
*                                                                               
VOPCLC   CLC   SC2NDFLD(0),0(R3)                                                
*                                                                               
VALOP04  OC    FLAG,7(R3)          SET OUTPUT TYPES                             
         B     EXITOK                                                           
*                                                                               
VALOPTAB DS    0XL8                                                             
         DC    CL7'PRINT  ',AL1(FLAGPR)                                         
         DC    CL7'DISK   ',AL1(FLAGDS)                                         
         DC    CL7'BOTH   ',AL1(FLAGPR+FLAGDS)                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DUMP TYPE                                       *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALDUMP  NTR1  ,                                                                
         MVI   DUMPFLG,DUMPOS                                                   
         CLI   SC2NDFLD,C'O'       FOR DUMP=OS                                  
         BE    EXITOK                                                           
         MVI   FERN,26                                                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE RECORD TYPE                                     *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALRTY   NTR1  ,                                                                
         LA    R3,VALRTTAB         MATCH TEXT INPUT                             
         LHI   RF,L'VALRTTAB                                                    
         XR    R1,R1                                                            
         ICM   R1,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3              NO SECOND INPUT                              
         B     EXITL                                                            
         BCTR  R1,0                                                             
*                                                                               
VALRT02  CLI   0(R3),EOT           EOT?                                         
         BNE   *+12                                                             
         MVI   FERN,14             BAD VALUE                                    
         B     EXITL                                                            
         EX    R1,VRTCLC           MATCH VALUE                                  
         BE    VALRT04                                                          
         BXH   R3,RF,VALRT02                                                    
*                                                                               
VRTCLC   CLC   SC2NDFLD(0),0(R3)                                                
*                                                                               
VALRT04  MVC   RECTYPE,7(R3)       SET RECORD TYPE                              
         B     EXITOK                                                           
*                                                                               
VALRTTAB DS    0XL8                                                             
         DC    CL7'COPY   ',AL1(1)                                              
         DC    CL7'CHANGE ',AL1(2)                                              
         DC    CL7'ADD    ',AL1(3)                                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE TRANSACTION AMOUNT                              *         
***********************************************************************         
                                                                                
         USING SCANBLKD,R2                                                      
VALAMT   NTR1  ,                                                                
         CLI   1(R2),0             ANY ADDRESS?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3              NO SECOND INPUT                              
         B     EXITL                                                            
*                                                                               
         TM    SC2NDVAL,SCNUMQ     NUMERIC INPUT                                
         BO    VALAM02                                                          
*                                                                               
         CHI   RF,3                ALL REQUESTED                                
         BH    VALAM04                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   VALAM04                                                          
         CLC   SC2NDFLD(0),=C'ALL'                                              
         MVC   TRANAMT,EFFS                                                     
         B     EXITOK                                                           
*                                                                               
VALAM02  GOTO1 VCASHVAL,DMCB,(X'82',SC2NDFLD),(RF)                              
         CLI   DMCB,FF                                                          
         BNE   VALAM04                                                          
         ZAP   TRANAMT,DMCB+4(8)                                                
         B     EXITOK                                                           
*                                                                               
VALAM04  MVI   FERN,15             INVALID FORMAT                               
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILE LIST                                                  *         
***********************************************************************         
                                                                                
VALFILE  NTR1  ,                                                                
         MVI   CARD+5,C','         SET FILES= TO FILES  THEN RESCAN             
         GOTO1 VSCAN31,DMCB,CARD,SCANBLK,0,(10,SCICARD),60                      
         XR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         CHI   R0,2                                                             
         BNL   *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         LA    R2,SCANBLK                                                       
         AHI   R2,SCANROW          GO PAST FILES= CARD                          
         AHI   R0,-1                                                            
         USING SCANBLKD,R2                                                      
         LA    R3,FILETBL                                                       
*                                                                               
VFIL02   CLI   SC1STLEN,2          MUST BE LENGTH 2                             
         BNE   VFIL04                                                           
         CLI   SC2NDLEN,0          NO SECOND INPUT                              
         BNE   VFIL04                                                           
         TM    SC1STVAL,SCHEXQ     AND HEX                                      
         BZ    VFIL04                                                           
         GOTO1 VHEXIN,DMCB,SC1STFLD,0(R3),2                                     
         B     VFIL10                                                           
*                                                                               
VFIL04   CLI   SC1STLEN,7          TRY TO MATCH NAME                            
         BH    VFIL12                                                           
         L     R5,AFILTAB          R5 = A(ENTRY)                                
         USING FILTABD,R5                                                       
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
*                                                                               
VFIL06   EX    RF,VFILCLC          MATCH FILENAME                               
         BE    VFIL08                                                           
         CLI   DMFLNUM,X'FF'       REACHED END OF TABLE?                        
         BE    *+12                YES                                          
         AHI   R5,DMFLLEN                                                       
         B     VFIL06                                                           
*                                                                               
         LH    R0,ERRCNT           INCREMENT ERROR COUNT                        
         AHI   R0,1                                                             
         STH   R0,ERRCNT                                                        
         MVC   P(15),=CL15'!! ***ERROR*** '                                     
         MVC   P+15(27),=CL27'Unable to find this file - '                      
         MVC   P+42(07),SC1STFLD                                                
         GOTO1 VPRINTER                                                         
         B     VFIL10                                                           
*                                                                               
VFILCLC  CLC   SC1STFLD(0),DMFLNAME                                             
*                                                                               
VFIL08   MVC   0(1,R3),DMFLNUM     SAVE FILE NUMBER                             
         B     VFIL10                                                           
         DROP  R5                                                               
*                                                                               
VFIL10   AHI   R2,SCANROW                                                       
         AHI   R3,1                                                             
         BCT   R0,VFIL02                                                        
         MVI   0(R3),FF                                                         
         OI    FLAG,FLAGFL                                                      
         B     EXITOK                                                           
*                                                                               
VFIL12   MVI   FERN,21                                                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TERMINAL NUMBER                                            *         
***********************************************************************         
                                                                                
VALTRM   NTR1  ,                                                                
         MVI   FERN,18             NO LONGER SUPPORTED                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY FILTER                                      *         
***********************************************************************         
                                                                                
         USING SCANBLKD,R2                                                      
VALKEY   NTR1  ,                                                                
         TM    FLAG,FLAGKYL+FLAGKY+FLAGRQ                                       
         BZ    VKEY02                                                           
         MVI   FERN,20             PRIOR MATCH KEY                              
         TM    FLAG,FLAGKY                                                      
         BO    VKEY01                                                           
         MVI   FERN,19             PRIOR START KEY                              
         TM    FLAG,FLAGKYL                                                     
         BO    *+8                                                              
         MVI   FERN,25             ONLY ONE KEY ALLOWED                         
*                                                                               
VKEY01   B     EXITL                                                            
*                                                                               
VKEY02   OI    FLAG,FLAGKY         FOUND KEY CARD FLAG                          
         GOTO1 VDECODE,DMCB,SC2NDFLD,FKEY                                       
         CLI   DMCB+8,X'FF'        INVALID STRING?                              
         JE    VDECNO                                                           
         MVC   KLEN,DMCB+10        KEY LENGTH                                   
         B     EXITOK                                                           
*                                                                               
VDECNO   DS    0H                                                               
         MVI   FERN,28                                                          
         SR    RF,RF                                                            
         ICM   RF,7,9(R1)                                                       
         MVC   WORK(30),0(RF)                                                   
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REQUEST FILTER                                  *         
***********************************************************************         
                                                                                
         USING SCANBLKD,R2                                                      
VALREQ   NTR1  ,                                                                
         TM    FLAG,FLAGKYL+FLAGKY+FLAGRQ                                       
         BZ    VREQ02                                                           
         MVI   FERN,20             PRIOR MATCH KEY                              
         TM    FLAG,FLAGKY                                                      
         BO    VREQ01                                                           
         MVI   FERN,19             PRIOR START KEY                              
         TM    FLAG,FLAGKYL                                                     
         BO    *+8                                                              
         MVI   FERN,25             ONLY ONE KEY ALLOWED                         
*                                                                               
VREQ01   B     EXITL                                                            
*                                                                               
VREQ02   OI    FLAG,FLAGRQ         FOUND KEY CARD FLAG                          
         GOTO1 VDECODE,DMCB,SC2NDFLD,FKEY                                       
         CLI   DMCB+8,X'FF'        INVALID STRING?                              
         JE    VDECNO                                                           
         MVC   KLEN,DMCB+10        KEY LENGTH                                   
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE LOWKEY FILTER                                   *         
***********************************************************************         
                                                                                
         USING SCANBLKD,R2                                                      
VALKYL   NTR1  ,                                                                
         TM    FLAG,FLAGKYL+FLAGKY+FLAGRQ                                       
         BZ    VKYL02                                                           
         MVI   FERN,20             PRIOR MATCH KEY                              
         TM    FLAG,FLAGKY                                                      
         BO    VKYL01                                                           
         MVI   FERN,19             PRIOR START KEY                              
         TM    FLAG,FLAGKYL                                                     
         BO    *+8                                                              
         MVI   FERN,25             ONLY ONE KEY ALLOWED                         
*                                                                               
VKYL01   B     EXITL                                                            
*                                                                               
VKYL02   OI    FLAG,FLAGKYL        FOUND LOWKEY CARD FLAG                       
         GOTO1 VDECODE,DMCB,SC2NDFLD,FLKEY                                      
         CLI   DMCB+8,X'FF'        INVALID STRING?                              
         JE    VDECNO                                                           
         MVC   LKLEN,DMCB+10       KEY LENGTH                                   
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE HIGHKEY FILTER                                  *         
***********************************************************************         
                                                                                
         USING SCANBLKD,R2                                                      
VALKYH   NTR1  ,                                                                
         TM    FLAG,FLAGKY+FLAGRQ                                               
         BZ    VKYH02                                                           
         MVI   FERN,20             PRIOR MATCH KEY                              
         B     EXITL                                                            
*                                                                               
VKYH02   OI    FLAG,FLAGKYH        FOUND LOWKEY CARD FLAG                       
         GOTO1 VDECODE,DMCB,SC2NDFLD,FHKEY                                      
         CLI   DMCB+8,X'FF'        INVALID STRING?                              
         JE    VDECNO                                                           
         MVC   HKLEN,DMCB+10       KEY LENGTH                                   
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS A START TIME                                     *         
***********************************************************************         
                                                                                
         USING SCANBLKD,R2                                                      
VALSTIM  NTR1  ,                                                                
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVC   WORK,SPACES                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SC2NDFLD                                                 
*                                                                               
         BRAS  RE,SETTIME                                                       
         BNE   EXITL                                                            
         MVC   LTIME,FULL                                                       
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS A START DATE                                     *         
***********************************************************************         
                                                                                
         USING SCANBLKD,R2                                                      
VALSDTE  NTR1  ,                                                                
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVC   WORK,SPACES                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SC2NDFLD                                                 
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,LDATE)                                  
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS AN END TIME                                      *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALETIM  NTR1  ,                                                                
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVC   WORK,SPACES                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SC2NDFLD                                                 
*                                                                               
         BRAS  RE,SETTIME                                                       
         BNE   EXITL                                                            
         MVC   HTIME,FULL                                                       
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS A END DATE                                       *         
***********************************************************************         
                                                                                
         USING SCANBLKD,R2                                                      
VALEDTE  NTR1  ,                                                                
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVC   WORK,SPACES                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SC2NDFLD                                                 
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,HDATE)                                  
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD SIN TO TABLE                                         *         
***********************************************************************         
                                                                                
         USING SCANBLKD,R2                                                      
VALSIN   NTR1  ,                                                                
         GOTO1 VSCAN31,DMCB,CARD,SCANBLK,0,(10,SCICARD+SCIHEXIN),60             
         MVI   FERN,9              INVALID HEX                                  
         TM    SC2NDFLD,SCHEXQ     HEX VALUE?                                   
         BZ    EXITL               NO                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN       ANY INPUT?                                   
         BNZ   VALSIN12                                                         
         MVI   FERN,3              NO                                           
         B     EXITL                                                            
*                                                                               
VALSIN12 MVI   FERN,10                                                          
         CHI   RF,6                HEX INPUT LENGTH = 6?                        
         BL    EXITL                NO, TOO SHORT                               
         MVI   FERN,29                                                          
         BH    EXITL                NO, TOO LONG. REMOVE RTASKID??              
*                                                                               
         LARL  RE,SINTAB                                                        
         LLH   RF,NUMSINS                                                       
         AHI   RF,1                                                             
         MVI   FERN,27             TOO MANY SINS INPUT                          
         CHI   RF,SINMAXQ                                                       
         BH    EXITL                                                            
                                                                                
         STH   RF,NUMSINS          INCREASE NUMBER OF SINS                      
         SHI   RF,1                                                             
         MHI   RF,L'SINTAB         LENGTH OF A SIN                              
         AR    RE,RF                                                            
         MVC   0(L'SINTAB,RE),SC2NDNUM    SAVIOUR OF THE SIN                    
         MVI   FERN,0                                                           
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS A TIME VALUE - 00:00:00 TO 23:59:59 VALID                   *         
* NTRY: WORK   = HH:MM:SS EBCDIC SPACE FILLED (SS IS OPTIONAL)        *         
* EXIT: FULL   = 0HHMMSS0 BINARY                                      *         
***********************************************************************         
                                                                                
         USING SCANBLKD,R2                                                      
SETTIME  NTR1  ,                                                                
         GOTO1 VSCAN31,DMCB,WORK,SCANBLK,C',=:=',(10,SCICARD),60                
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BNZ   *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVC   DUB1,ZEROS                                                       
         LA    R2,SCANBLK                                                       
         USING SCANBLKD,R2                                                      
         CLI   SC1STLEN,2          LENGTH OF 1 OR 2                             
         BH    STERR                                                            
         TM    SC1STVAL,SCNUMQ     AND VALID NUMBER                             
         BZ    STERR                                                            
         ICM   RF,15,SC1STNUM                                                   
         CHI   RF,23               00:00:00 TO 23:59:59 VALID                   
         BH    STERR1                                                           
         LA    RF,SC1STFLD                                                      
         CLI   SC1STLEN,1                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         MVI   0(RF),C'0'                                                       
         MVC   DUB1+00(2),0(RF)    SET HOURS VALUE                              
*                                                                               
         AHI   R0,-1               ANY MINUTES?                                 
         CHI   R0,0                                                             
         BE    STVAL02             NO                                           
*                                                                               
         AHI   R2,SCANROW                                                       
         CLI   SC1STLEN,2          LENGTH OF 1 OR 2                             
         BH    STERR                                                            
         TM    SC1STVAL,SCNUMQ     AND VALID NUMBER                             
         BZ    STERR                                                            
         ICM   RF,15,SC1STNUM                                                   
         CHI   RF,59               00 TO 59 VALID FOR MINUTE                    
         BH    STERR1                                                           
         LA    RF,SC1STFLD                                                      
         CLI   SC1STLEN,1                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         MVI   0(RF),C'0'                                                       
         MVC   DUB1+02(2),0(RF)    SET MINUTES VALUE                            
*                                                                               
         AHI   R0,-1               ANY SECONDS?                                 
         CHI   R0,0                                                             
         BE    STVAL02             NO                                           
*                                                                               
         AHI   R2,SCANROW                                                       
         CLI   SC1STLEN,2          LENGTH OF 1 OR 2                             
         BH    STERR                                                            
         TM    SC1STVAL,SCNUMQ     AND VALID NUMBER                             
         BZ    STERR                                                            
         ICM   RF,15,SC1STNUM                                                   
         CHI   RF,59               00 TO 59 VALID FOR SECONDS                   
         BH    STERR1                                                           
         LA    RF,SC1STFLD                                                      
         CLI   SC1STLEN,1                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         MVI   0(RF),C'0'                                                       
         MVC   DUB1+04(2),0(RF)    SET SECONDS VALUE                            
*                                                                               
STVAL02  PACK  DUB,DUB1(6)                                                      
         ICM   RF,15,DUB+4         RF=0HHMMSSC                                  
         N     RF,=X'0FFFFFF0'                                                  
         ST    RF,FULL             FULL=0HHMMSS0                                
         B     EXITOK                                                           
*                                                                               
STERR    MVI   FERN,16             FORMAT HH:MM:SS                              
         B     EXITL                                                            
*                                                                               
STERR1   MVI   FERN,17             ONLY VALUES 00:00:00 - 23:59:59              
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL AND CONSTANTS                                          *         
***********************************************************************         
                                                                                
FF       EQU   X'FF'                                                            
EOT      EQU   X'FF'                                                            
SCANROW  EQU   82                                                               
SCANLHS  EQU   10                                                               
SCANRHS  EQU   68                                                               
*                                                                               
EFFS     DC    16X'FF'                                                          
QUERY    DC    16C'?'                                                           
ZEROS    DC    16C'0'                                                           
P99      DC    P'99'                                                            
PONE     DC    P'1'                                                             
PZERO    DC    P'0'                                                             
*                                                                               
RECSIN   DC    A(0)                                                             
RECSOUT  DC    A(0)                                                             
*                                                                               
ASVCARD  DC    A(SVCARD)                                                        
AERRTAB  DC    A(ERRTAB)                                                        
AFILTAB  DC    A(FILTAB)                                                        
AFACITAB DC    A(FACIDTAB)                                                      
*                                                                               
VCARDS   DC    V(CARDS)                                                         
VCASHVAL DC    V(CASHVAL)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VDATCON  DC    V(DATCON)                                                        
VDECODE  DC    V(DECODE)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VPRINTER DC    V(PRINTER)                                                       
VPRINT   DC    V(PRINT)                                                         
VPRTREC  DC    V(PRTREC)                                                        
VPRNTBL  DC    V(PRNTBL)                                                        
VSCAN31  DC    V(SCAN31)                                                        
*                                                                               
HEADER   DC    C'Header :'                                                      
TRAILER  DC    C'Trailer:'                                                      
*                                                                               
CTITLE   DC    CL60'REFUSE - Processing input parameters'                       
MTITLE   DC    CL60'REFUSE - Recovery peel program'                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         EJECT                                                                  
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
                                                                                
SAVERD   DS    F                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
DSKADR   DS    F                                                                
LTIME    DS    PL4                                                              
HTIME    DS    PL4                                                              
LDATE    DS    PL4                                                              
HDATE    DS    PL4                                                              
TRANAMT  DS    PL6                                                              
WORK     DS    CL80                                                             
USER     DS    XL4                                                              
MAXOUT   DS    F                                                                
NUMOUT   DS    F                                                                
TTIME    DS    F                                                                
RECTYPE  DC    X'00'                                                            
PROGNUM  DS    XL1                                                              
DALEN    DS    XL1                                                              
SILEN    DS    XL1                                                              
DSPACE   DC    C'A'                DSPACE supplied                              
DELETE   DC    AL1(YES)            Show delete Yes/No                           
ALFA     DC    CL2' '                                                           
AC#COUNT DS    C                   YES/NO/ONLY                                  
EXTLEN   DS    H                                                                
ERRCNT   DS    H                                                                
PERSONID DS    XL2                 FILTERING ON PID                             
*                                                                               
GIN      DS    D                                                                
GINTS    DS    F                                                                
GINNUM   DS    F                                                                
*                                                                               
DUMPLIST DS    6D                                                               
DUMPFLG  DS    C                                                                
DUMPOS   EQU   C'O'                                                             
*                                                                               
FERN     DS    X                                                                
FLAG     DS    XL1                                                              
FLAGPR   EQU   X'80'               OUTPUT TO PRINT                              
FLAGDS   EQU   X'40'               OUTPUT TO DISK                               
FLAGKY   EQU   X'20'               KEY= PARAMETER CARD FOUND                    
FLAGKYL  EQU   X'10'               LOWKEY= PARAMETER CARD FOUND                 
FLAGKYH  EQU   X'08'               HIGHKEY= PARAMETER CARD FOUND                
FLAGFL   EQU   X'04'               FILES= PARAMETER CARD FOUND                  
FLAGOP   EQU   X'02'               OUTPUT= PARAMETER CARD FOUND                 
FLAGRQ   EQU   X'01'               REQ= PARAMETER CARD FOUND                    
*                                                                               
KLEN     DS    H                   KEY LENGTHS                                  
LKLEN    DS    H                                                                
HKLEN    DS    H                                                                
*                                                                               
EXTDATA  DS    CL64                Trailer                                      
FKEY     DS    CL64                                                             
FLKEY    DS    CL64                                                             
FHKEY    DS    CL64                                                             
*                                                                               
SRDATE   DS    XL3                                                              
*                                                                               
FILETBL  DS    XL20                                                             
FLUID    DS    CL8                                                              
CARD     DS    CL80                                                             
SCANBLK  DS    CL820                                                            
         EJECT                                                                  
***********************************************************************         
* DCBS AND RECOVERY RECORD DEFINITION                                 *         
***********************************************************************         
                                                                                
         DS    0D                                                               
         DC    CL8'RECVIN**'                                                    
RCVTAPE  DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,MACRF=GM,EODAD=MAIN04            
*                                                                               
         DS    0D                                                               
         DC    CL8'RECVOUT*'                                                    
RCVOUT   DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=8200,BLKSIZE=27648                                         
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
                                                                                
CARDTAB  DC    CL8'ALPHA   ',F'001',F'002'                                      
         DC    AL1(04,CTCHR,L'ALFA,0),AL4(ALFA)                                 
         DC    CL8'DSPACE  ',AL4(1,1)                                           
         DC    AL1(05,CTCHR,1,0),AL4(DSPACE)                                    
         DC    CL8'DELETE  ',AL4(1,1)                                           
         DC    AL1(05,CTCHR,1,0),AL4(DELETE)                                    
         DC    CL8'OUTPUT  ',AL4(0,0)                                           
         DC    AL1(05,CTRTN,0,0),AL4(VALOP)                                     
         DC    CL8'DUMP    ',AL4(0,0)                                           
         DC    AL1(04,CTRTN,0,0),AL4(VALDUMP)                                   
         DC    CL8'KEY     ',AL4(0,0)                                           
         DC    AL1(02,CTRTN,0,0),AL4(VALKEY)                                    
         DC    CL8'REQ     ',AL4(0,0)                                           
         DC    AL1(02,CTRTN,0,0),AL4(VALREQ)                                    
         DC    CL8'LOWKEY  ',AL4(0,0)                                           
         DC    AL1(05,CTRTN,0,0),AL4(VALKYL)                                    
         DC    CL8'HIGHKEY ',AL4(0,0)                                           
         DC    AL1(06,CTRTN,0,0),AL4(VALKYH)                                    
         DC    CL8'RTYPE   ',AL4(0,0)                                           
         DC    AL1(04,CTRTN,0,0),AL4(VALRTY)                                    
         DC    CL8'FILES   ',AL4(0,0)                                           
         DC    AL1(04,CTRTN,0,0),AL4(VALFILE)                                   
         DC    CL8'TERMINAL',AL4(0,0)                                           
         DC    AL1(07,CTRTN,0,0),AL4(VALTRM)                                    
         DC    CL8'LOWTIME ',AL4(0,0)                                           
         DC    AL1(06,CTRTN,0,0),AL4(VALSTIM)                                   
         DC    CL8'HIGHTIME',AL4(0,0)                                           
         DC    AL1(07,CTRTN,0,0),AL4(VALETIM)                                   
         DC    CL8'LOWDATE ',AL4(0,0)                                           
         DC    AL1(06,CTRTN,0,0),AL4(VALSDTE)                                   
         DC    CL8'HIGHDATE',AL4(0,0)                                           
         DC    AL1(07,CTRTN,0,0),AL4(VALEDTE)                                   
         DC    CL8'MAXOUT  ',F'001',F'100000'                                   
         DC    AL1(05,CTNUM,0,0),AL4(MAXOUT)                                    
         DC    CL8'LUID    ',F'008',F'008'                                      
         DC    AL1(03,CTCHR,L'FLUID,0),AL4(FLUID)                               
         DC    CL8'PROG    ',F'001',F'002'                                      
         DC    AL1(03,CTHEX,L'PROGNUM,0),AL4(PROGNUM)                           
         DC    CL8'PROGRAM ',F'001',F'002'                                      
         DC    AL1(03,CTHEX,L'PROGNUM,0),AL4(PROGNUM)                           
         DC    CL8'USER    ',F'001',F'65536'                                    
         DC    AL1(03,CTNUM,0,0),AL4(USER)                                      
         DC    CL8'USERID  ',F'001',F'65536'                                    
         DC    AL1(05,CTNUM,0,0),AL4(USER)                                      
         DC    CL8'HUSER   ',F'001',F'004'                                      
         DC    AL1(04,CTHEX,L'USER,0),AL4(USER)                                 
         DC    CL8'HUSERID ',F'001',F'004'                                      
         DC    AL1(06,CTHEX,L'USER,0),AL4(USER)                                 
         DC    CL8'PID     ',F'001',F'004'                                      
         DC    AL1(02,CTHEX,L'PERSONID,0),AL4(PERSONID)                         
         DC    CL8'PERSONID',F'001',F'004'                                      
         DC    AL1(02,CTHEX,L'PERSONID,0),AL4(PERSONID)                         
         DC    CL8'SYSIN   ',F'001',F'008'                                      
         DC    AL1(04,CTRTN,L'SINTAB,0),AL4(VALSIN)                             
*                                                                               
         DC    CL8'GINTS   ',F'001',F'008'                                      
         DC    AL1(04,CTHEX,L'GINTS,0),AL4(GINTS)                               
         DC    CL8'GINNUM  ',F'001',F'008'                                      
         DC    AL1(06,CTHEX,L'GINNUM,0),AL4(GINNUM)                             
         DC    CL8'GIN     ',F'001',F'016'                                      
         DC    AL1(02,CTHEX,L'GIN,0),AL4(GIN)                                   
*                                                                               
         DC    CL8'DISKADR ',F'004',F'008'                                      
         DC    AL1(06,CTHEX,L'DSKADR,0),AL4(DSKADR)                             
         DC    CL8'AC#COUNT',F'001',F'001'                                      
         DC    AL1(07,CTCHR,L'AC#COUNT,0),AL4(AC#COUNT)                         
CARDTABX DC    AL1(CARDEOT)                                                     
                                                                                
NUMSINS  DC    H'0'                                                             
SINTAB   DC    (SINMAXQ)F'0'                                                    
SINMAXQ  EQU   20                                                               
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
                                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CTHEX    EQU   3                   HEX                                          
CTRTN    EQU   4                   VALIDATION ROUTINE                           
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
REFUSE   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* INPUT RECORD                                                        *         
***********************************************************************         
                                                                                
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
       ++INCLUDE DMRCVRHDR                                                      
                                                                                
RDATA    DS    (RDATALNQ)X                                                      
RDATALNQ EQU   9000                                                             
*                                                                               
SVCARD   DC    50CL80' '           SAVED INPUT CARDS                            
***********************************************************************         
* FACIDTABL - new version by DSPACE                                   *         
***********************************************************************         
*FACIDTABL                                                                      
         PRINT OFF                                                              
       ++INCLUDE FACIDTABL                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* DMFILTAB                                                            *         
***********************************************************************         
                                                                                
         DS    0D                                                               
         DC    C'DMFILTAB'                                                      
       ++INCLUDE DMFILTAB                                                       
         EJECT                                                                  
***********************************************************************         
* ERROR TABLE                                                         *         
***********************************************************************         
                                                                                
ERRTAB   DS    0D                                                               
ERR01    DC    CL50'Invalid line format                               '         
ERR02    DC    CL50'Invalid Keyword                                   '         
ERR03    DC    CL50'Missing value for Keyword=Value parameter         '         
ERR04    DC    CL50'Value not a valid number                          '         
ERR05    DC    CL50'Numeric value too small                           '         
ERR06    DC    CL50'Numeric value too large                           '         
ERR07    DC    CL50'Length of input string too short                  '         
ERR08    DC    CL50'Length of input string too long                   '         
ERR09    DC    CL50'Value not a valid hex string                      '         
ERR10    DC    CL50'Length of hex string too short                    '         
ERR11    DC    CL50'Length of hex string too long                     '         
ERR12    DC    CL50'This card is a duplicate - not sure what to do now'         
ERR13    DC    CL50'Bad Value - Only 1 of PRINT/DISK/BOTH is ok here  '         
ERR14    DC    CL50'Bad Value - Only 1 of COPY/CHANGE/ADD is ok here  '         
ERR15    DC    CL50'Bad Value - ALL or a 2 DP number only valid here  '         
ERR16    DC    CL50'Bad Format - needs to be HH:MM:SS - SS optional   '         
ERR17    DC    CL50'Bad Format - HH:MM:SS cannot be more than 23:59:59'         
ERR18    DC    CL50'No longer supported - Use LUID= instead           '         
ERR19    DC    CL50'Prior start key (LOWKEY=) makes this card invalid '         
ERR20    DC    CL50'Prior match key (KEY=) makes this card invalid    '         
ERR21    DC    CL50'Input must be =XX,XX,.. XX is File Name or Hex #'           
ERR22    DC    CL50'Required card OUTPUT=XX not found                 '         
ERR23    DC    CL50'Required card FILES=XX not found                  '         
ERR24    DC    CL50'You must specify a Key a Lowkey or another filter '         
ERR25    DC    CL50'Only one of Key/Lowkey/Req allowed                '         
ERR26    DC    CL50'Only DUMP=OS is valid                             '         
ERR27    DC    CL50'No more SINs allowed - Max reached                '         
ERR28    DC    CL50'Length of key invalid                             '         
ERR29    DC    CL50'SYSIN is 3 byte hex. Do not include RTASKID       '         
         EJECT                                                                  
***********************************************************************         
* PRINT LINE HEADER                                                   *         
***********************************************************************         
                                                                                
PLHDR    DSECT                                                                  
PLFILEH  DS    CL05'FILE='                                                      
PLFHEX   DS    CL02' '                                                          
PLFHEXX  DS    CL01'/'                                                          
PLFALPH  DS    CL07' '                                                          
         DS    CL02' '                                                          
PLTYPEH  DS    CL05'TRNS='                                                      
PLTYPE   DS    CL04' '                                                          
         DS    CL02' '                                                          
PLTERMH  DS    CL06'TERM#='                                                     
PLTERM   DS    CL05' '                                                          
         ORG   PLTERMH                                                          
PLLUIDH  DS    CL05'LUID='                                                      
PLLUID   DS    CL08' '                                                          
         DS    CL02                                                             
PLDATEH  DS    CL05'DATE='                                                      
PLDATE   DS    CL08' '                                                          
         DS    CL02                                                             
PLTIMEH  DS    CL05'TIME='                                                      
PLHH     DS    CL02' '                                                          
PLHHS    DS    CL01':'                                                          
PLMM     DS    CL02' '                                                          
PLMMS    DS    CL01':'                                                          
PLSS     DS    CL02' '                                                          
         DS    CL02' '                                                          
PLDSKH   DS    CL04'D/A='                                                       
PLDSK    DS    CL08' '                                                          
         DS    CL02' '                                                          
PLUSERH  DS    CL05'USER='                                                      
PLUSER   DS    CL05' '                                                          
         DS    CL02' '                                                          
PLAGYH   DS    CL07'AGENCY='                                                    
PLAGY    DS    CL02' '                                                          
         DS    CL02' '                                                          
PLFACH   DS    CL07'FACPAK='                                                    
PLFAC    DS    CL04' '                                                          
PLAOR    DS    CL01' '                                                          
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
                                                                                
*FACIDTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
*DMFILTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMFILTABD                                                      
         PRINT ON                                                               
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
*                                                                               
RECVEXTD DSECT                                                                  
       ++INCLUDE DMRCVREXT                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DDREFUSE  01/19/21'                                      
         END                                                                    
