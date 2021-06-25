*          DATA SET CTREPIDS   AT LEVEL 016 AS OF 01/26/17                      
*PHASE CTREPIDA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE GETIDS                                                                 
*INCLUDE FATABOFF                                                               
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE SORTER                                                                 
*                                                                               
         TITLE 'CTREPIDS - USER-ID REPORT'                                      
*                                                                               
REPIDS   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**REPIDS,WORK=V(REGSAVE),RA,R9                                 
         L     RC,VCPRINT                                                       
         USING DPRINT,RC                                                        
         MVI   CHRULT,X'BF'                                                     
         GOTO1 VDMGR,DMCB,OPEN,CONTROL,DMFLIST,IO                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECDCARD,0                                 
*                                                                               
         GOTO1 VCARDS,DMCB,IO,=C'RE00'                                          
         CLC   IO(2),=C'/*'                                                     
         BE    ID0                                                              
         MVI   RSWS,C'N'                                                        
         MVC   RSWS+1(RSWL-1),RSWS                                              
         LA    R1,IO                                                            
REPID10  CLI   0(R1),C' '                                                       
         BE    ID0                                                              
         TM    0(R1),X'F0'                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(1),0(R1)                                                     
         NI    DUB,X'0F'                                                        
         ZIC   RE,DUB                                                           
         LA    RE,RSWS-1(RE)                                                    
         MVI   0(RE),C'Y'                                                       
         LA    R1,1(R1)                                                         
         B     REPID10                                                          
                                                                                
***********************************************************************         
* READ CTFILE AND PROCESS RECORDS                                     *         
***********************************************************************         
ID0      CLI   R1SW,C'Y'                                                        
         BNE   ID1                                                              
         BRAS  RE,GETAIDS          GET ALL ALPHA IDS                            
*                                                                               
ID1      LA    R2,IO                                                            
         USING CTIKEY,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVI   CTIKID,C' '         GO PAST NUMERICS                             
*                                                                               
         GOTO1 VDMGR,DMCB,DMRDHI,CTFILE,IO,IO                                   
         CLI   8(R1),0                                                          
         BNE   ID4A                                                             
         B     ID5                                                              
*                                                                               
ID2      GOTO1 VDMGR,DMCB,DMREAD,CTFILE,IO,IO                                   
         CLI   8(R1),0                                                          
         BNE   ID4A                                                             
*                                                                               
ID3      GOTO1 VDMGR,DMCB,DMRSEQ,CTFILE,IO,IO                                   
         CLI   8(R1),0                                                          
         BNE   ID4A                                                             
         B     ID5                                                              
*                                                                               
ID4A     CLI   CTIKTYP,C'I'        STILL AN ID RECORD?                          
         BNE   ID100               NO                                           
         MVC   P+01(17),=CL17'BAD DMGR RC FOR '                                 
         MVC   P+18(L'CTIKID),CTIKID                                            
         GOTO1 VPRINTER,DMCB,P,=C'BC01'                                         
         B     ID3                                                              
*                                                                               
ID5      CLI   CTIKTYP,C'I'        STILL DOING ID RECORDS                       
         BNE   ID100               NO - GO PRINT THEM OUT                       
*                                                                               
         LA    R0,INVALS                                                        
         LHI   R1,INVALSL                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   INUSRID,CTIKID                                                   
         LA    R3,CTIDATA                                                       
ID6      CLI   0(R3),0                                                          
         BE    ID50                                                             
         CLI   0(R3),X'02'                                                      
         BE    IDX02                                                            
         CLI   0(R3),X'06'                                                      
         BE    IDX06                                                            
         CLI   0(R3),X'20'                                                      
         BE    IDX20                                                            
         CLI   0(R3),X'21'                                                      
         BE    IDX21                                                            
         CLI   0(R3),X'30'                                                      
         BE    IDX30                                                            
*&&UK                                                                           
         CLI   0(R3),X'33'                                                      
         BE    IDX33                                                            
*&&                                                                             
         CLI   0(R3),X'34'                                                      
         BE    IDX34                                                            
         CLI   0(R3),X'36'                                                      
         BE    IDX36                                                            
*&&US                                                                           
         CLI   0(R3),X'4C'                                                      
         BE    IDX4C                                                            
*&&                                                                             
ID8      ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     ID6                                                              
         EJECT                                                                  
IDX02    MVC   INUSRNO,2(R3)                                                    
         B     ID8                                                              
*                                                                               
         USING CTAGYD,R3                                                        
IDX06    MVC   INAGYID,CTAGYID                                                  
         B     ID8                                                              
*                                                                               
IDX20    OC    INCINUM,INCINUM     TEST FIRST TIME                              
         BNZ   ID8                                                              
         GOTO1 VGETIDS,DMCB,(C'C',IO),A(BIGWORK),V(DATAMGR)                     
         CLI   4(R1),0                                                          
         BE    IDX20A                                                           
*                                                                               
         MVC   P+01(19),=CL19'BAD GETIDS RC FOR '                               
         MVC   P+20(L'CTIKID),CTIKID                                            
         GOTO1 VPRINTER,DMCB,P,=C'BC01'                                         
         B     ID2                                                              
*                                                                               
IDX20A   SR    R0,R0                                                            
         ICM   R0,1,0(R1)                                                       
         ICM   R0,2,8(R1)                                                       
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLM   R0,3,=Y(INCIMAX)                                                 
         BNH   *+8                                                              
         LH    R0,=Y(INCIMAX)                                                   
         STCM  R0,3,INCINUM                                                     
         L     R1,=A(BIGWORK)                                                   
         L     RE,=A(INCILST)                                                   
         MVC   0(L'INCILST,RE),0(R1)                                            
         LA    RE,L'INCILST(RE)                                                 
         LA    R1,12(R1)                                                        
         BCT   R0,*-14                                                          
         B     ID8                                                              
*                                                                               
         USING CTSYSD,R3                                                        
IDX21    ZIC   R1,INSYSTN                                                       
         LA    R0,1(R1)                                                         
         STC   R0,INSYSTN                                                       
         MH    R1,=Y(L'INSYSTS)                                                 
         LA    R1,INSYSTS(R1)                                                   
         MVC   0(1,R1),CTSYSNUM                                                 
         MVC   1(1,R1),CTSYSSE                                                  
         MVC   4(4,R1),CTSYSLMT                                                 
         L     RE,=A(SYSLST)                                                    
IDX212   CLI   0(RE),X'FF'                                                      
         BE    ID8                                                              
         CLC   0(1,R1),0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'SYSLST(RE)                                                  
         B     IDX212                                                           
         TM    1(RE),X'80'                                                      
         BZ    *+10                                                             
         MVC   2(1,R1),CTSYSAGB                                                 
         TM    1(RE),X'40'                                                      
         BZ    *+10                                                             
         MVC   2(2,R1),INAGYID                                                  
         B     ID8                                                              
*                                                                               
         USING CTDSTD,R3                                                        
IDX30    MVC   INLOGO1,CTDSTLG1                                                 
         MVC   INLOGO2,CTDSTLG2                                                 
         MVC   INSPLID,CTDSTPOW                                                 
         MVC   INUSRNM,CTDSTNAM                                                 
         MVC   INUSRAD1,CTDSTADD                                                
         CLI   CTDSTLEN,166                                                     
         BL    *+10                                                             
         MVC   INUSRAD2,CTDSTAD2                                                
         B     ID8                                                              
*&&UK                                                                           
*                                                                               
         USING CTUKAD,R3                                                        
IDX33    MVC   INUKALIN,CTUKALIN                                                
         MVC   INUKAIPA,CTUKAIPA                                                
         B     ID8                                                              
*&&                                                                             
*                                                                               
IDX34    CLI   INVDNUM,0           TEST FIRST TIME                              
         BNE   ID8                                                              
         GOTO1 VGETIDS,DMCB,(C'D',IO),A(BIGWORK),V(DATAMGR)                     
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)                                                       
         BZ    ID8                                                              
         STC   R0,INVDNUM                                                       
         L     R1,=A(BIGWORK)                                                   
         L     RE,=A(INVDLST)                                                   
         MVC   0(L'INVDLST,RE),0(R1)                                            
         LA    RE,L'INVDLST(RE)                                                 
         LA    R1,12(R1)                                                        
         BCT   R0,*-14                                                          
         B     ID8                                                              
*                                                                               
         USING CTPRND,R3                                                        
IDX36    ZIC   R1,INPRNUM                                                       
         LA    R0,1(R1)                                                         
         STC   R0,INPRNUM                                                       
         MHI   R1,L'INPRLST                                                     
         A     R1,=A(INPRLST)                                                   
         MVC   0(1,R1),CTPRNNUM                                                 
         MVC   1(8,R1),CTPRNLIN                                                 
         B     ID8                                                              
*&&US                                                                           
*                                                                               
         USING CTSHPD,R3                                                        
IDX4C    MVC   INSHPUN,CTSHPINS                                                 
         B     ID8                                                              
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* FINISHED PULLING FIELDS OUT OF RECORD - NOW PUT TO SORT             *         
***********************************************************************         
ID50     OC    INSPLID,SPACES                                                   
         OC    INLOGO1,SPACES                                                   
         OC    INLOGO2,SPACES                                                   
         OC    INUSRNM,SPACES                                                   
         OC    INUSRAD1,SPACES                                                  
         OC    INUSRAD2,SPACES                                                  
*                                                                               
ID51     CLI   R1SW,C'Y'                                                        
         BNE   ID52                                                             
         MVI   S1RECTY,S1RECTYQ                                                 
*&&UK*&& MVC   S1UKALIN,INUKALIN                                                
*&&UK*&& MVC   S1UKAIPA,INUKAIPA                                                
         MVC   S1AGYID,INAGYID                                                  
         MVC   S1USRID,INUSRID                                                  
         MVC   S1SPLID,INSPLID                                                  
*&&US*&& MVC   S1SHPUN,INSHPUN                                                  
         MVC   S1SYSTN(S1SYSTL),INSYSTN                                         
         MVC   S1LOGO1,INLOGO1                                                  
         MVC   S1LOGO2,INLOGO2                                                  
         MVC   S1USRNM,INUSRNM                                                  
         MVC   S1USRAD1,INUSRAD1                                                
         MVC   S1USRAD2,INUSRAD2                                                
         GOTO1 VSORTER,DMCB,PUT,SORTREC                                         
*                                                                               
ID52     CLI   R2SW,C'Y'                                                        
         BNE   ID53                                                             
         MVI   S2RECTY,S2RECTYQ                                                 
*&&UK                                                                           
         MVC   S2UKALIN,INUKALIN                                                
         MVC   S2UKAIPA,INUKAIPA                                                
*&&                                                                             
         MVC   S2SPLID,INSPLID                                                  
         MVC   S2AGYID,INAGYID                                                  
         MVC   S2USRID,INUSRID                                                  
         MVC   S2SYSTN(S2SYSTL),INSYSTN                                         
         MVC   S2LOGO1,INLOGO1                                                  
         MVC   S2LOGO2,INLOGO2                                                  
*&&US*&& MVC   S2SHPUN,INSHPUN                                                  
         MVC   S2USRNM,INUSRNM                                                  
         MVC   S2USRAD1,INUSRAD1                                                
         MVC   S2USRAD2,INUSRAD2                                                
         GOTO1 VSORTER,DMCB,PUT,SORTREC                                         
*                                                                               
ID53     DS    0H                                                               
         CLI   R3SW,C'Y'                                                        
         BNE   ID54                                                             
         MVI   S3RECTY,S3RECTYQ                                                 
*&&US*&& MVC   S3SHPUN,INSHPUN                                                  
*&&UK                                                                           
         MVC   S3UKALIN,INUKALIN                                                
         MVC   S3UKAIPA,INUKAIPA                                                
*&&                                                                             
         MVC   S3SPLID,INSPLID                                                  
         MVC   S3AGYID,INAGYID                                                  
         MVC   S3USRID,INUSRID                                                  
         MVC   S3SYSTN(S3SYSTL),INSYSTN                                         
         MVC   S3LOGO1,INLOGO1                                                  
         MVC   S3LOGO2,INLOGO2                                                  
         MVC   S3USRNM,INUSRNM                                                  
         MVC   S3USRAD1,INUSRAD1                                                
         MVC   S3USRAD2,INUSRAD2                                                
         GOTO1 VSORTER,DMCB,PUT,SORTREC                                         
*                                                                               
ID54     CLI   R4SW,C'Y'                                                        
         BNE   ID55                                                             
         SR    R0,R0                                                            
         ICM   R0,1,INSYSTN                                                     
         BZ    ID55                                                             
         LA    R3,INSYSTS                                                       
ID542    MVI   S4RECTY,S4RECTYQ                                                 
         MVC   S4SYSTM,0(R3)                                                    
*&&UK*&& MVC   S4UKALIN,INUKALIN                                                
*&&UK*&& MVC   S4UKAIPA,INUKAIPA                                                
*&&US*&& MVC   S4SHPUN,INSHPUN                                                  
         MVC   S4SPLID,INSPLID                                                  
         MVC   S4AGYID,INAGYID                                                  
         MVC   S4USRID,INUSRID                                                  
         MVC   S4LOGO1,INLOGO1                                                  
         MVC   S4LOGO2,INLOGO2                                                  
         MVC   S4USRNM,INUSRNM                                                  
         GOTO1 VSORTER,DMCB,PUT,SORTREC                                         
         LA    R3,L'INSYSTS(R3)                                                 
         BCT   R0,ID542                                                         
*                                                                               
ID55     CLI   R5SW,C'Y'           WANT OUTPUT BY ID NUMBER                     
         BNE   ID56                                                             
         MVI   S5RECTY,S5RECTYQ                                                 
         MVC   S5IDNUM,INUSRNO                                                  
         MVC   S5USRID,INUSRID                                                  
         MVC   S5USRID,INUSRNO                                                  
         MVC   S5AGYID,INAGYID                                                  
         MVC   S5USRID,INUSRID                                                  
         MVC   S5USRNM,INUSRNM                                                  
         MVC   S5USRAD1,INUSRAD1                                                
         MVC   S5USRAD2,INUSRAD2                                                
         GOTO1 VSORTER,DMCB,PUT,SORTREC                                         
ID56     B     ID2                                                              
         EJECT                                                                  
***********************************************************************         
* FINISHED DOING THE PUT TO THE SYSTEM                                *         
***********************************************************************         
ID100    GOTO1 VSORTER,DMCB,GET                                                 
         ICM   R1,15,4(R1)                                                      
         BNZ   *+12                                                             
         MVI   SORTREC,X'FF'                                                    
         B     *+10                                                             
         MVC   SORTREC,0(R1)                                                    
*                                                                               
         CLC   SORTREC(1),LSORTY                                                
         BE    ID102                                                            
         CLI   LSORTY,0                                                         
         BE    ID102                                                            
         L     R1,=V(BOXAREA)                                                   
         USING BOXD,R1                                                          
         MVI   BOXOFF,C'Y'                                                      
         GOTO1 VPRINTER,DMCB,P,=C'BC01'                                         
         L     R1,=V(BOXAREA)                                                   
         MVC   BOXCOLS(L'PBLOCK),SPACES                                         
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXYORN,0                                                        
         CLI   SORTREC,X'FF'                                                    
         BE    IDX                                                              
*                                                                               
ID102    LA    R1,REPTAB                                                        
         CLC   SORTREC(1),0(R1)                                                 
         BE    *+12                                                             
         LA    R1,L'REPTAB(R1)                                                  
         B     *-14                                                             
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         CLC   SORTREC(1),LSORTY                                                
         BNE   *+8                                                              
         ICM   RF,7,4(R1)                                                       
         BASR  RE,RF                                                            
         MVC   LSORTY,SORTREC                                                   
         B     ID100                                                            
IDX      XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
R1FST    NTR1  ,                                                                
         BAS   RE,CLEARPRT                                                      
         MVC   TITLE(L'R1TITLE),R1TITLE                                         
         MVI   MID1,0                                                           
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB1+(R1SYSTM-R1LINED)(7),=C'SYSTEM/'                            
         MVC   SUB2+(R1SYSTM-R1LINED)(7),=C'AGENCY#'                            
         MVC   SUB1+(R1SPLID-R1LINED)(5),=C'SPOOL'                              
         MVC   SUB2+(R1SPLID-R1LINED)(5),=C' ID  '                              
         MVC   SUB1+(R1AGYID-R1LINED)(3),=C'AGY'                                
         MVC   SUB2+(R1AGYID-R1LINED)(3),=C'ID '                                
         MVC   SUB2+(R1USRID-R1LINED)(7),=C'USER-ID'                            
*&&US                                                                           
         MVC   SUB1+(R1SHPUN-R1LINED)(4),=C'SHIP'                               
         MVC   SUB2+(R1SHPUN-R1LINED)(4),=C'UNIT'                               
         MVC   SUB2+(R1LOGO1-R1LINED)(7),=C'-LOGOS-'                            
*&&                                                                             
*&&UK                                                                           
         MVC   SUB2+(R1LOGO2-R1LINED)(7),=C'SHP/UNT'                            
         MVC   SUB2+(R1LOGO1-R1LINED)(7),=C'-LOGO1-'                            
         MVC   SUB1+(R1UKALIN-R1LINED)(4),=C'LINE'                              
         MVC   SUB2+(R1UKALIN-R1LINED)(4),=C' ID '                              
         MVC   SUB1+(R1UKAIPA-R1LINED)(3),=C'IPA'                               
         MVC   SUB2+(R1UKAIPA-R1LINED)(3),=C'AGY'                               
*&&                                                                             
         MVC   SUB2+(R1USRNM-R1LINED)(19),=C'USER NAME && ADDRESS'              
         MVI   SPACEAFT,C'N'                                                    
         XC    LAGYID,LAGYID                                                    
         L     R1,=V(BOXAREA)                                                   
         USING BOXD,R1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXCOLS+(R1BC001-R1LINED),C'L'                                   
         MVI   BOXCOLS+(R1BC002-R1LINED),C'C'                                   
         MVI   BOXCOLS+(R1BC003-R1LINED),C'C'                                   
         MVI   BOXCOLS+(R1BC004-R1LINED),C'C'                                   
*&&UK                                                                           
         MVI   BOXCOLS+(R1BC005-R1LINED),C'C'                                   
         MVI   BOXCOLS+(R1BC008-R1LINED),C'C'                                   
*&&                                                                             
         MVI   BOXCOLS+(R1BC006-R1LINED),C'C'                                   
         MVI   BOXCOLS+(R1BC007-R1LINED),C'C'                                   
         MVI   BOXCOLS+(R1BC009-R1LINED),C'C'                                   
         MVI   BOXCOLS+(R1BC010-R1LINED),C'R'                                   
         MVI   BOXROWS+04,C'T'                                                  
         MVI   BOXROWS+07,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         B     R1NXT0                                                           
*                                                                               
R1NXT    NTR1  ,                                                                
R1NXT0   LA    R1,PBLOCK                                                        
         OC    LAGYID,LAGYID                                                    
         BZ    R1NXT2                                                           
         CLC   S1AGYID,LAGYID                                                   
         BNE   *+12                                                             
         LA    R1,L'PBLOCK(R1)                                                  
         B     R1NXT2                                                           
         MVI   PCTLBOX,C'B'                                                     
*                                                                               
         USING R1LINED,R1                                                       
R1NXT2   MVC   R1AGYID,S1AGYID                                                  
*                                                                               
         CLI   S1USRID,X'FF'           IS THIS AN AGENCY ALPHA ONLY?            
         BNE   R1NXT3                  NO: CONTINUE                             
         CLC   S1AGYID,LAGYID          DID WE HAVE ANY IDS?                     
         BNE   R1NXT2A                 NO: PRINT ALPHA ID                       
         MVC   LAGYID,S1AGYID                                                   
         MVC   R1AGYID,SPACES                                                   
         B     EXIT                                                             
R1NXT2A  MVC   R1USRID+1(8),=C'* NONE *'                                        
         MVC   R1USRNM(22),=CL22'NO USER-IDS FOR AGENCY'                        
         B     R1NXT6                                                           
*                                                                               
R1NXT3   DS    0H                                                               
*&&UK                                                                           
         MVC   R1UKALIN,S1UKALIN                                                
         MVC   R1UKAIPA,S1UKAIPA                                                
*&&                                                                             
         MVC   R1USRID,S1USRID                                                  
         MVC   R1SPLID,S1SPLID                                                  
*&&US*&& MVC   R1SHPUN+1(2),S1SHPUN                                             
         MVC   R1LOGO1,S1LOGO1                                                  
*&&US*&& MVC   R1LOGO1+L'PBLOCK,S1LOGO2                                         
*&&UK*&& MVC   R1LOGO2,S1LOGO2                                                  
         MVC   R1USRNM,S1USRNM                                                  
         MVC   R1USRAD+(L'PBLOCK*1),S1USRAD1                                    
         MVC   R1USRAD+(L'PBLOCK*2),S1USRAD2                                    
         LA    R2,S1SYSTS                                                       
         SR    R0,R0                                                            
         ICM   R0,1,S1SYSTN                                                     
         BZ    R1NXT6                                                           
R1NXT4   BAS   RE,GETSYS                                                        
         MVC   R1SYSTM(L'SYSNAME),SYSNAME                                       
         LA    RE,R1SYSTM+L'SYSNAME-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLC   AGYBIN,SPACES                                                    
         BE    *+14                                                             
         MVI   1(RE),C'/'                                                       
         MVC   2(L'AGYBIN,RE),AGYBIN                                            
         LA    R1,L'PBLOCK(R1)                                                  
         LA    R2,L'S1SYSTS(R2)                                                 
         BCT   R0,R1NXT4                                                        
R1NXT6   MVC   LAGYID,S1AGYID                                                   
         BAS   RE,PRINTBLK                                                      
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
R2FST    NTR1  ,                                                                
         BAS   RE,CLEARPRT                                                      
         MVC   TITLE(L'R2TITLE),R2TITLE                                         
         MVI   MID1,0                                                           
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB1+(R2SYSTM-R2LINED)(7),=C'SYSTEM/'                            
         MVC   SUB2+(R2SYSTM-R2LINED)(7),=C'AGENCY#'                            
         MVC   SUB1+(R2SPLID-R2LINED)(5),=C'SPOOL'                              
         MVC   SUB2+(R2SPLID-R2LINED)(5),=C' ID  '                              
         MVC   SUB1+(R2AGYID-R2LINED)(3),=C'AGY'                                
         MVC   SUB2+(R2AGYID-R2LINED)(3),=C'ID '                                
         MVC   SUB2+(R2USRID-R2LINED)(7),=C'USER-ID'                            
*&&US                                                                           
         MVC   SUB1+(R2SHPUN-R2LINED)(4),=C'SHIP'                               
         MVC   SUB2+(R2SHPUN-R2LINED)(4),=C'UNIT'                               
         MVC   SUB2+(R2LOGO1-R2LINED)(7),=C'-LOGOS-'                            
*&&                                                                             
*&&UK                                                                           
         MVC   SUB2+(R2LOGO2-R2LINED)(7),=C'SHP/UNT'                            
         MVC   SUB2+(R2LOGO1-R2LINED)(7),=C'-LOGO1-'                            
         MVC   SUB1+(R2UKALIN-R2LINED)(4),=C'LINE'                              
         MVC   SUB2+(R2UKALIN-R2LINED)(4),=C' ID '                              
         MVC   SUB1+(R2UKAIPA-R2LINED)(3),=C'IPA'                               
         MVC   SUB2+(R2UKAIPA-R2LINED)(3),=C'AGY'                               
*&&                                                                             
         MVC   SUB2+(R2USRNM-R2LINED)(19),=C'USER NAME && ADDRESS'              
         MVI   SPACEAFT,C'N'                                                    
         L     R1,=V(BOXAREA)                                                   
         USING BOXD,R1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXCOLS+(R2BC001-R2LINED),C'L'                                   
         MVI   BOXCOLS+(R2BC002-R2LINED),C'C'                                   
         MVI   BOXCOLS+(R2BC003-R2LINED),C'C'                                   
         MVI   BOXCOLS+(R2BC004-R2LINED),C'C'                                   
*&&UK                                                                           
         MVI   BOXCOLS+(R2BC005-R2LINED),C'C'                                   
         MVI   BOXCOLS+(R2BC008-R2LINED),C'C'                                   
*&&                                                                             
         MVI   BOXCOLS+(R2BC006-R2LINED),C'C'                                   
         MVI   BOXCOLS+(R2BC007-R2LINED),C'C'                                   
         MVI   BOXCOLS+(R2BC009-R2LINED),C'C'                                   
         MVI   BOXCOLS+(R2BC010-R2LINED),C'R'                                   
         MVI   BOXROWS+04,C'T'                                                  
         MVI   BOXROWS+07,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         B     R2NXT0                                                           
*                                                                               
R2NXT    NTR1  ,                                                                
R2NXT0   LA    R1,PBLOCK                                                        
         OC    LSPLID,LSPLID                                                    
         BZ    R2NXT2                                                           
         CLC   S2SPLID(L'LSPLID),LSPLID                                         
         BNE   *+12                                                             
         LA    R1,L'PBLOCK(R1)                                                  
         B     R2NXT2                                                           
         MVI   PCTLBOX,C'B'                                                     
         USING R2LINED,R1                                                       
R2NXT2   MVC   R2SPLID,S2SPLID                                                  
*&&UK                                                                           
         MVC   R2UKALIN,S2UKALIN                                                
         MVC   R2UKAIPA,S2UKAIPA                                                
         MVC   R2LOGO2,S2LOGO2                                                  
*&&                                                                             
*&&US                                                                           
         MVC   R2LOGO1+L'PBLOCK,S2LOGO2                                         
         MVC   R2SHPUN+1(2),S2SHPUN                                             
*&&                                                                             
         MVC   R2AGYID,S2AGYID                                                  
         MVC   R2USRID,S2USRID                                                  
         MVC   R2LOGO1,S2LOGO1                                                  
         MVC   R2USRNM,S2USRNM                                                  
         MVC   R2USRAD+(L'PBLOCK*1),S2USRAD1                                    
         MVC   R2USRAD+(L'PBLOCK*2),S2USRAD2                                    
         LA    R2,S2SYSTS                                                       
         SR    R0,R0                                                            
         ICM   R0,1,S2SYSTN                                                     
         BZ    R2NXT6                                                           
R2NXT4   BAS   RE,GETSYS                                                        
         MVC   R2SYSTM(L'SYSNAME),SYSNAME                                       
         LA    RE,R2SYSTM+L'SYSNAME-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLC   AGYBIN,SPACES                                                    
         BE    *+14                                                             
         MVI   1(RE),C'/'                                                       
         MVC   2(L'AGYBIN,RE),AGYBIN                                            
         LA    R1,L'PBLOCK(R1)                                                  
         LA    R2,L'S2SYSTS(R2)                                                 
         BCT   R0,R2NXT4                                                        
R2NXT6   MVC   LSPLID,S2SPLID                                                   
         BRAS  RE,PRINTBLK                                                      
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
R3FST    NTR1  ,                                                                
         BAS   RE,CLEARPRT                                                      
         MVC   TITLE(L'R3TITLE),R3TITLE                                         
         MVI   MID1,0                                                           
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB1+(R3SYSTM-R3LINED)(7),=C'SYSTEM/'                            
         MVC   SUB2+(R3SYSTM-R3LINED)(7),=C'AGENCY#'                            
         MVC   SUB1+(R3SPLID-R3LINED)(5),=C'SPOOL'                              
         MVC   SUB2+(R3SPLID-R3LINED)(5),=C' ID  '                              
         MVC   SUB1+(R3AGYID-R3LINED)(3),=C'AGY'                                
         MVC   SUB2+(R3AGYID-R3LINED)(3),=C'ID '                                
         MVC   SUB2+(R3USRID-R3LINED)(7),=C'USER-ID'                            
*&&US                                                                           
         MVC   SUB1+(R3SHPUN-R3LINED)(4),=C'SHIP'                               
         MVC   SUB2+(R3SHPUN-R3LINED)(4),=C'UNIT'                               
         MVC   SUB2+(R3LOGO1-R3LINED)(7),=C'-LOGOS-'                            
*&&                                                                             
*&&UK                                                                           
         MVC   SUB2+(R3LOGO2-R3LINED)(7),=C'SHP/UNT'                            
         MVC   SUB2+(R3LOGO1-R3LINED)(7),=C'-LOGO1-'                            
         MVC   SUB1+(R3UKALIN-R3LINED)(4),=C'LINE'                              
         MVC   SUB2+(R3UKALIN-R3LINED)(4),=C' ID '                              
         MVC   SUB1+(R3UKAIPA-R3LINED)(3),=C'IPA'                               
         MVC   SUB2+(R3UKAIPA-R3LINED)(3),=C'AGY'                               
*&&                                                                             
         MVC   SUB2+(R3USRNM-R3LINED)(19),=C'USER NAME && ADDRESS'              
         MVI   SPACEAFT,C'N'                                                    
         L     R1,=V(BOXAREA)                                                   
         USING BOXD,R1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXCOLS+(R3BC001-R3LINED),C'L'                                   
         MVI   BOXCOLS+(R3BC002-R3LINED),C'C'                                   
         MVI   BOXCOLS+(R3BC003-R3LINED),C'C'                                   
*&&UK                                                                           
         MVI   BOXCOLS+(R3BC004-R3LINED),C'C'                                   
         MVI   BOXCOLS+(R3BC005-R3LINED),C'C'                                   
*&&                                                                             
         MVI   BOXCOLS+(R3BC006-R3LINED),C'C'                                   
         MVI   BOXCOLS+(R3BC007-R3LINED),C'C'                                   
         MVI   BOXCOLS+(R3BC008-R3LINED),C'C'                                   
         MVI   BOXCOLS+(R3BC009-R3LINED),C'C'                                   
         MVI   BOXCOLS+(R3BC010-R3LINED),C'R'                                   
         MVI   BOXROWS+04,C'T'                                                  
         MVI   BOXROWS+07,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         B     R3NXT0                                                           
*                                                                               
R3NXT    NTR1  ,                                                                
R3NXT0   LA    R1,PBLOCK                                                        
*&&UK                                                                           
         OC    LLOGO2,LLOGO2                                                    
         BZ    R3NXT2                                                           
         CLC   S3LOGO2,LLOGO2                                                   
         BNE   *+12                                                             
         LA    R1,L'PBLOCK(R1)                                                  
         B     R3NXT2                                                           
*&&                                                                             
*&&US                                                                           
         OC    LSHPUN,LSHPUN                                                    
         BZ    R3NXT2                                                           
         CLC   S3SHPUN,LSHPUN                                                   
         BNE   *+12                                                             
         LA    R1,L'PBLOCK(R1)                                                  
         B     R3NXT2                                                           
*&&                                                                             
         MVI   PCTLBOX,C'B'                                                     
         USING R3LINED,R1                                                       
R3NXT2   DS    0H                                                               
*&&UK                                                                           
         MVC   R3LOGO2,S3LOGO2                                                  
         MVC   R3UKALIN,S3UKALIN                                                
         MVC   R3UKAIPA,S3UKAIPA                                                
*&&                                                                             
*&&US                                                                           
         MVC   R3LOGO1+L'PBLOCK,S3LOGO2                                         
         MVC   R3SHPUN+1(2),S3SHPUN                                             
*&&                                                                             
         MVC   R3AGYID,S3AGYID                                                  
         MVC   R3USRID,S3USRID                                                  
         MVC   R3SPLID,S3SPLID                                                  
         MVC   R3LOGO1,S3LOGO1                                                  
         MVC   R3USRNM,S3USRNM                                                  
         MVC   R3USRAD+(L'PBLOCK*1),S3USRAD1                                    
         MVC   R3USRAD+(L'PBLOCK*2),S3USRAD2                                    
         LA    R2,S3SYSTS                                                       
         SR    R0,R0                                                            
         ICM   R0,1,S3SYSTN                                                     
         BZ    R3NXT6                                                           
R3NXT4   BAS   RE,GETSYS                                                        
         MVC   R3SYSTM(L'SYSNAME),SYSNAME                                       
         LA    RE,R3SYSTM+L'SYSNAME-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLC   AGYBIN,SPACES                                                    
         BE    *+14                                                             
         MVI   1(RE),C'/'                                                       
         MVC   2(L'AGYBIN,RE),AGYBIN                                            
         LA    R1,L'PBLOCK(R1)                                                  
         LA    R2,L'S3SYSTS(R2)                                                 
         BCT   R0,R3NXT4                                                        
R3NXT6   DS    0H                                                               
*&&UK*&& MVC   LLOGO2,S3LOGO2                                                   
*&&US*&& MVC   LSHPUN,S3SHPUN                                                   
         BAS   RE,PRINTBLK                                                      
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
R4FST    NTR1  ,                                                                
         BRAS  RE,CLEARPRT                                                      
         MVC   TITLE(L'R4TITLE),R4TITLE                                         
         MVI   MID1,0                                                           
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB1+(R4SYSTM-R4LINED)(7),=C'SYSTEM/'                            
         MVC   SUB2+(R4SYSTM-R4LINED)(7),=C'AGENCY#'                            
         MVC   SUB1+(R4SPLID-R4LINED)(5),=C'SPOOL'                              
         MVC   SUB2+(R4SPLID-R4LINED)(5),=C' ID  '                              
         MVC   SUB1+(R4AGYID-R4LINED)(3),=C'AGY'                                
         MVC   SUB2+(R4AGYID-R4LINED)(3),=C'ID '                                
         MVC   SUB2+(R4USRID-R4LINED)(7),=C'USER-ID'                            
         MVC   SUB2+(R4LOGO1-R4LINED)(7),=C'-LOGO1-'                            
*&&US                                                                           
         MVC   SUB2+(R4LOGO2-R4LINED)(7),=C'-LOGO2-'                            
         MVC   SUB1+(R4SHPUN-R4LINED)(4),=C'SHIP'                               
         MVC   SUB2+(R4SHPUN-R4LINED)(4),=C'UNIT'                               
*&&                                                                             
*&&UK                                                                           
         MVC   SUB2+(R4LOGO2-R4LINED)(7),=C'SHP/UNT'                            
         MVC   SUB1+(R4UKALIN-R4LINED)(4),=C'LINE'                              
         MVC   SUB2+(R4UKALIN-R4LINED)(4),=C' ID '                              
         MVC   SUB1+(R4UKAIPA-R4LINED)(3),=C'IPA'                               
         MVC   SUB2+(R4UKAIPA-R4LINED)(3),=C'AGY'                               
*&&                                                                             
         MVC   SUB2+(R4USRNM-R4LINED)(9),=C'USER NAME && ADDRESS'               
         MVI   SPACEAFT,C'N'                                                    
         L     R1,=V(BOXAREA)                                                   
         USING BOXD,R1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXCOLS+(R4BC001-R4LINED),C'L'                                   
         MVI   BOXCOLS+(R4BC002-R4LINED),C'C'                                   
         MVI   BOXCOLS+(R4BC003-R4LINED),C'C'                                   
         MVI   BOXCOLS+(R4BC004-R4LINED),C'C'                                   
*&&UK                                                                           
         MVI   BOXCOLS+(R4BC005-R4LINED),C'C'                                   
*&&                                                                             
         MVI   BOXCOLS+(R4BC006-R4LINED),C'C'                                   
         MVI   BOXCOLS+(R4BC007-R4LINED),C'C'                                   
         MVI   BOXCOLS+(R4BC008-R4LINED),C'C'                                   
         MVI   BOXCOLS+(R4BC009-R4LINED),C'C'                                   
         MVI   BOXCOLS+(R4BC010-R4LINED),C'R'                                   
         MVI   BOXROWS+04,C'T'                                                  
         MVI   BOXROWS+07,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         B     R4NXT0                                                           
*                                                                               
R4NXT    NTR1  ,                                                                
R4NXT0   LA    R1,PBLOCK                                                        
         OC    LSYSTM,LSYSTM                                                    
         BZ    R4NXT2                                                           
         CLC   S4SYSTM(4),LSYSTM                                                
         BE    R4NXT2                                                           
         MVI   PCTLBOX,C'B'                                                     
         USING R4LINED,R1                                                       
R4NXT2   LA    R2,S4SYSTM                                                       
         BAS   RE,GETSYS                                                        
         MVC   R4SYSTM(L'SYSNAME),SYSNAME                                       
         LA    RE,R4SYSTM+L'SYSNAME-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLC   AGYBIN,SPACES                                                    
         BE    *+14                                                             
         MVI   1(RE),C'/'                                                       
         MVC   2(L'AGYBIN,RE),AGYBIN                                            
*&&US                                                                           
         MVC   R4SHPUN+1(2),S4SHPUN                                             
*&&                                                                             
*&&UK                                                                           
         MVC   R4UKALIN,S4UKALIN                                                
         MVC   R4UKAIPA,S4UKAIPA                                                
*&&                                                                             
         MVC   R4AGYID,S4AGYID                                                  
         MVC   R4USRID,S4USRID                                                  
         MVC   R4SPLID,S4SPLID                                                  
         MVC   R4LOGO1,S4LOGO1                                                  
         MVC   R4LOGO2,S4LOGO2                                                  
         MVC   R4USRNM,S4USRNM                                                  
R4NXT6   MVC   LSYSTM,S4SYSTM                                                   
         BRAS  RE,PRINTBLK                                                      
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
R5FST    NTR1  ,                                                                
         BRAS  RE,CLEARPRT                                                      
         MVC   TITLE(L'R5TITLE),R5TITLE                                         
         MVI   MID1,0                                                           
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB1+(R5IDNUM-R5LINED)(2),=C'ID'                                 
         MVC   SUB2+(R5IDNUM-R5LINED)(3),=C'NUM'                                
         MVC   SUB2+(R5USRID-R5LINED)(7),=C'USER-ID'                            
         MVC   SUB1+(R5AGYID-R5LINED)(3),=C'AGY'                                
         MVC   SUB2+(R5AGYID-R5LINED)(3),=C'ID '                                
         MVC   SUB2+(R5USRNM-R5LINED)(9),=C'USER NAME'                          
*                                                                               
         MVI   SPACEAFT,C'N'                                                    
         L     R1,=V(BOXAREA)                                                   
         USING BOXD,R1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXCOLS+(R5BC001-R5LINED),C'L'                                   
         MVI   BOXCOLS+(R5BC002-R5LINED),C'C'                                   
         MVI   BOXCOLS+(R5BC003-R5LINED),C'C'                                   
         MVI   BOXCOLS+(R5BC004-R5LINED),C'C'                                   
         MVI   BOXCOLS+(R5BC005-R5LINED),C'R'                                   
         MVI   BOXROWS+04,C'T'                                                  
         MVI   BOXROWS+07,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         B     R5NXT0                                                           
*                                                                               
R5NXT    NTR1  ,                                                                
*                                                                               
         USING R5LINED,R1                                                       
R5NXT0   LA    R1,PBLOCK                                                        
         XR    R0,R0                                                            
         ICM   R0,3,S5IDNUM                                                     
         EDIT  (R0),(5,R5IDNUM),0,ALIGN=LEFT,ZERO=NOBLANK                       
         MVC   R5USRID,S5USRID                                                  
         MVC   R5AGYID,S5AGYID                                                  
         MVC   R5USRNM,S5USRNM                                                  
         BRAS  RE,PRINTBLK                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
GETSYS   NTR1  ,                                                                
         L     R1,=V(SELIST)                                                    
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SESYS,1(R2)                                                      
         BE    GETSYS2                                                          
         BXLE  R1,RE,*-10                                                       
         MVC   SYSNAME,SPACES                                                   
         MVC   SYSNAME(4),=C'SYS='                                              
         XOUT  1(R2),SYSNAME+4,1                                                
         B     GETSYS4                                                          
GETSYS2  MVC   SYSNAME,SENAME                                                   
GETSYS4  MVC   AGYBIN,SPACES                                                    
         CLI   2(R2),0                                                          
         BE    GETSYSX                                                          
         MVC   AGYBIN,2(R2)                                                     
         CLI   3(R2),0                                                          
         BNE   GETSYSX                                                          
         XOUT  2(R2),AGYBIN,1                                                   
GETSYSX  XIT1  ,                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET ALPHA IDS                                                                 
***********************************************************************         
GETAIDS  NTR1                                                                   
*                                                                               
         LA    R2,IO                                                            
         USING CT5KEY,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    C'5'                                         
*                                                                               
         GOTO1 VDMGR,DMCB,DMRDHI,CTFILE,IO,IO                                   
         CLI   8(R1),0                                                          
         BNE   GAID040                                                          
         B     GAID050                                                          
*                                                                               
GAID020  GOTO1 VDMGR,DMCB,DMREAD,CTFILE,IO,IO                                   
         CLI   8(R1),0                                                          
         BNE   GAID040                                                          
*                                                                               
GAID030  GOTO1 VDMGR,DMCB,DMRSEQ,CTFILE,IO,IO                                   
         CLI   8(R1),0                                                          
         BNE   GAID040                                                          
         B     GAID050                                                          
*                                                                               
GAID040  CLI   CT5KTYP,CT5KTYPQ    STILL AN ACCESS RECORD?                      
         BNE   GAID100             NO                                           
         MVC   P+01(17),=CL17'BAD DMGR RC FOR '                                 
         MVC   P+18(L'CT5KALPH),CT5KALPH                                        
         GOTO1 VPRINTER,DMCB,P,=C'BC01'                                         
         B     GAID030                                                          
*                                                                               
GAID050  CLI   CT5KTYP,CT5KTYPQ    STILL DOING ACCESS RECORDS                   
         BNE   GAID100             NO                                           
*                                                                               
         MVC   INAGYID,CT5KALPH                                                 
         MVI   INUSRID,X'FF'                                                    
         OC    INSPLID,SPACES                                                   
         OC    INLOGO1,SPACES                                                   
         OC    INLOGO2,SPACES                                                   
         OC    INUSRNM,SPACES                                                   
         OC    INUSRAD1,SPACES                                                  
         OC    INUSRAD2,SPACES                                                  
*                                                                               
         MVI   S1RECTY,S1RECTYQ                                                 
*&&UK*&& MVC   S1UKALIN,SPACES                                                  
*&&UK*&& MVC   S1UKAIPA,SPACES                                                  
         MVC   S1AGYID,INAGYID                                                  
         MVC   S1USRID,INUSRID                                                  
         MVC   S1SPLID,SPACES                                                   
*&&US*&& MVC   S1SHPUN,SPACES                                                   
         MVC   S1SYSTN,SPACES                                                   
         MVC   S1LOGO1,SPACES                                                   
         MVC   S1LOGO2,SPACES                                                   
         MVC   S1USRNM,SPACES                                                   
         MVC   S1USRAD1,SPACES                                                  
         MVC   S1USRAD2,SPACES                                                  
                                                                                
         GOTO1 VSORTER,DMCB,PUT,SORTREC                                         
         B     GAID030                                                          
*                                                                               
GAID100  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO CLEAR DOWN PRINT BLOCK                                   *         
***********************************************************************         
CLEARPRT NTR1  ,                                                                
*                                                                               
         MVC   TITLE,SPACES                                                     
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         MVC   MID4,SPACES                                                      
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB3,SPACES                                                      
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         MVI   COLDATE,0                                                        
         MVI   COLTIME,0                                                        
         MVI   COLTITLE,2                                                       
         MVI   COLPAGE,100                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT OUT A BLOCK OF DATA                                *         
***********************************************************************         
         SPACE 1                                                                
PRINTBLK NTR1  ,                                                                
         LA    R1,PBLOCKX                                                       
         LA    R0,10                                                            
PRINTB2  SH    R1,=Y(L'PBLOCK)                                                  
         CLC   0(L'PBLOCK,R1),SPACES                                            
         BNE   *+12                                                             
         BCT   R0,PRINTB2                                                       
         LA    R0,1                                                             
         CLI   SPACEAFT,C'Y'                                                    
         BNE   *+8                                                              
         AH    R0,=H'1'                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         AP    DUB,LINE                                                         
         CLI   PCTLBOX,C'B'                                                     
         BNE   *+10                                                             
         AP    DUB,=P'1'                                                        
         CP    DUB,=P'58'                                                       
         BL    PRINTB4                                                          
         ZAP   LINE,=P'99'                                                      
PRINTB4  LA    R1,PBLOCK                                                        
         L     RF,VPRINTER                                                      
PRINTB6  MVC   P,0(R1)                                                          
         MVC   0(L'PBLOCK,R1),SPACES                                            
         BASR  RE,RF                                                            
         LA    R1,L'PBLOCK(R1)                                                  
         BCT   R0,PRINTB6                                                       
PRINTBX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS AND USEFUL STUFF                                        *         
***********************************************************************         
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
VSORTER  DC    V(SORTER)                                                        
VGETIDS  DC    V(GETIDS)                                                        
VCPRINT  DC    V(CPRINT)                                                        
VPRINTER DC    V(PRINTER)                                                       
VBOXAREA DC    V(BOXAREA)                                                       
VDMGR    DC    V(DATAMGR)                                                       
VCARDS   DC    V(CARDS)                                                         
*                                                                               
PUT      DC    CL4'PUT '                                                        
GET      DC    CL4'GET '                                                        
OPEN     DC    CL8'OPEN    '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
CONTROL  DC    CL8'CONTROL '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
DMFLIST  DC    C'NCTFILE X'                                                     
*                                                                               
REPTAB   DS    0XL7                                                             
         DC    AL1(S1RECTYQ),AL3(R1FST,R1NXT)                                   
         DC    AL1(S2RECTYQ),AL3(R2FST,R2NXT)                                   
         DC    AL1(S3RECTYQ),AL3(R3FST,R3NXT)                                   
         DC    AL1(S4RECTYQ),AL3(R4FST,R4NXT)                                   
         DC    AL1(S5RECTYQ),AL3(R5FST,R5NXT)                                   
*                                                                               
R1TITLE  DC    C'USER-ID LIST BY AGENCY ALPHA ID'                               
R2TITLE  DC    C'USER-ID LIST BY SPOOL ID'                                      
R3TITLE  DC    C'USER-ID LIST BY SHIPPING UNIT'                                 
R4TITLE  DC    C'USER-ID LIST BY SYSTEM/AGENCY#'                                
R5TITLE  DC    C'USER-ID LIST BY AGENCY NUMBER'                                 
*                                                                               
RSWS     DS    0C                                                               
R1SW     DC    C'Y'                                                             
R2SW     DC    C'Y'                                                             
R3SW     DC    C'Y'                                                             
R4SW     DC    C'Y'                                                             
R5SW     DC    C'Y'                                                             
R6SW     DC    C'Y'                                                             
R7SW     DC    C'Y'                                                             
R8SW     DC    C'Y'                                                             
RSWL     EQU   *-RSWS                                                           
*                                                                               
SYSLST   DS    0XL2                                                             
*&&US                                                                           
         DC    X'0280'                                                          
         DC    X'0380'                                                          
         DC    X'0440'                                                          
         DC    X'0540'                                                          
         DC    X'0680'                                                          
         DC    X'0840'                                                          
         DC    X'0A40'                                                          
         DC    X'0B00'                                                          
         DC    X'0C40'                                                          
         DC    X'0E80'                                                          
*&&                                                                             
*&&UK                                                                           
         DC    X'0480'                                                          
         DC    X'0540'                                                          
         DC    X'0680'                                                          
         DC    X'0A40'                                                          
         DC    X'0B00'                                                          
         DC    X'0E80'                                                          
*&&                                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
SORTCARD DC    C'SORT FIELDS=(1,80,A),FORMAT=BI,WORK=1 '                        
RECDCARD DC    C'RECORD TYPE=F,LENGTH=256 '                                     
SPACEAFT DC    C'N'                                                             
LSORTY   DC    X'00'                                                            
INR5SW   DC    C'N'                                                             
LAGYID   DC    XL2'00'                                                          
*&&US                                                                           
LSPLID   DC    XL2'00'                                                          
*&&                                                                             
*&&UK                                                                           
LSPLID   DC    XL3'00'                                                          
*&&                                                                             
*&&UK                                                                           
LLOGO2   DC    XL7'00'                                                          
*&&                                                                             
*&&US                                                                           
LSHPUN   DS    CL2                                                              
*&&                                                                             
LSYSTM   DC    XL4'00'                                                          
SYSNAME  DC    CL7' '                                                           
AGYBIN   DC    CL2' '                                                           
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    XL20                                                             
IO       DS    1000C                                                            
PBLOCK   DC    10CL198' '                                                       
PBLOCKX  EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
SORTREC  DS    XL256                                                            
         ORG   SORTREC                                                          
S1RECTY  DS    CL1                                                              
S1RECTYQ EQU   C'1'                                                             
S1AGYID  DS    CL2                                                              
S1USRID  DS    CL10                                                             
S1SPLID  DS    CL4                                                              
*&&US                                                                           
S1SHPUN  DS    CL2                                                              
*&&                                                                             
*&&UK                                                                           
S1UKALIN DS    CL4                                                              
S1UKAIPA DS    CL3                                                              
*&&                                                                             
S1SYSTN  DS    XL1                                                              
S1SYSTS  DS    10XL8                                                            
S1SYSTL  EQU   *-S1SYSTS                                                        
S1LOGO1  DS    CL7                                                              
S1LOGO2  DS    CL7                                                              
S1USRNM  DS    CL33                                                             
S1USRAD1 DS    CL33                                                             
S1USRAD2 DS    CL33                                                             
         ORG   SORTREC                                                          
S2RECTY  DS    CL1                                                              
S2RECTYQ EQU   C'2'                                                             
S2SPLID  DS    CL4                                                              
S2AGYID  DS    CL2                                                              
S2USRID  DS    CL10                                                             
*&&US                                                                           
S2SHPUN  DS    CL2                                                              
*&&                                                                             
*&&UK                                                                           
S2UKALIN DS    CL4                                                              
S2UKAIPA DS    CL3                                                              
*&&                                                                             
S2SYSTN  DS    XL1                                                              
S2SYSTS  DS    10XL8                                                            
S2SYSTL  EQU   *-S2SYSTS                                                        
S2LOGO1  DS    CL7                                                              
S2LOGO2  DS    CL7                                                              
S2USRNM  DS    CL33                                                             
S2USRAD1 DS    CL33                                                             
S2USRAD2 DS    CL33                                                             
*                                                                               
         ORG   SORTREC                                                          
S3RECTY  DS    CL1                                                              
S3RECTYQ EQU   C'3'                                                             
*&&UK                                                                           
S3LOGO2  DS    CL7                                                              
*&&                                                                             
*&&US                                                                           
S3SHPUN  DS    CL2                                                              
*&&                                                                             
S3AGYID  DS    CL2                                                              
S3USRID  DS    CL10                                                             
*&&UK                                                                           
S3UKALIN DS    CL4                                                              
S3UKAIPA DS    CL3                                                              
*&&                                                                             
S3SPLID  DS    CL4                                                              
S3SYSTN  DS    XL1                                                              
S3SYSTS  DS    10XL8                                                            
S3SYSTL  EQU   *-S3SYSTS                                                        
S3LOGO1  DS    CL7                                                              
*&&US                                                                           
S3LOGO2  DS    CL7                                                              
*&&                                                                             
S3USRNM  DS    CL33                                                             
S3USRAD1 DS    CL33                                                             
S3USRAD2 DS    CL33                                                             
         ORG                                                                    
*                                                                               
         ORG   SORTREC                                                          
S4RECTY  DS    CL1                                                              
S4RECTYQ EQU   C'4'                                                             
S4SYSTM  DS    XL8                                                              
S4AGYID  DS    CL2                                                              
S4USRID  DS    CL10                                                             
*&&UK                                                                           
S4UKALIN DS    CL4                                                              
S4UKAIPA DS    CL3                                                              
*&&                                                                             
*&&US                                                                           
S4SHPUN  DS    CL2                                                              
*&&                                                                             
S4SPLID  DS    CL4                                                              
S4LOGO1  DS    CL7                                                              
S4LOGO2  DS    CL7                                                              
S4USRNM  DS    CL33                                                             
         ORG                                                                    
         ORG   SORTREC                                                          
S5RECTY  DS    CL1                                                              
S5RECTYQ EQU   C'5'                                                             
S5IDNUM  DS    XL2                                                              
S5USRID  DS    CL10                                                             
S5AGYID  DS    CL2                                                              
S5USRNM  DS    CL33                                                             
S5USRAD1 DS    CL33                                                             
S5USRAD2 DS    CL33                                                             
         ORG                                                                    
         EJECT                                                                  
INVALS   DS    0X                                                               
INAGYID  DS    CL2                                                              
INUSRID  DS    CL10                                                             
INUSRNO  DS    CL2                                                              
*&&US                                                                           
INSHPUN  DS    CL2                                                              
*&&                                                                             
INSPLID  DS    CL4                                                              
INSYSTN  DS    XL1                                                              
INSYSTS  DS    10XL8                                                            
INUSRIN  DS    XL1                                                              
INLOGO1  DS    CL7                                                              
INLOGO2  DS    CL7                                                              
*&&UK                                                                           
INUKALIN DS    CL4                                                              
INUKAIPA DS    CL3                                                              
*&&                                                                             
INUSRNM  DS    CL33                                                             
INUSRAD1 DS    CL33                                                             
INUSRAD2 DS    CL33                                                             
INVDNUM  DS    XL1                                                              
INCINUM  DS    XL2                                                              
INPRNUM  DS    XL1                                                              
INPRLST  DS    20CL9                                                            
INVDLST  DS    256CL10                                                          
INCIMAX  EQU   256                                                              
INCILST  DS    (INCIMAX)CL10                                                    
INVALSL  EQU   *-INVALS                                                         
         SPACE 1                                                                
BIGWORK  DS    5000CL12                                                         
         EJECT                                                                  
R1LINED  DSECT                                                                  
R1BC001  DS    C                                                                
R1AGYID  DS    CL2                                                              
         DS    CL1                                                              
R1BC002  DS    C                                                                
R1USRID  DS    CL10                                                             
R1BC003  DS    C                                                                
R1SPLID  DS    CL4                                                              
         DS    CL1                                                              
*&&US                                                                           
R1BC004  DS    C                                                                
R1SHPUN  DS    CL4                                                              
*&&                                                                             
*&&UK                                                                           
R1BC004  DS    C                                                                
R1UKALIN DS    CL4                                                              
R1BC005  DS    C                                                                
R1UKAIPA DS    CL3                                                              
*&&                                                                             
R1BC006  DS    C                                                                
R1SYSTM  DS    CL10                                                             
R1BC007  DS    C                                                                
R1LOGO1  DS    CL7                                                              
*&&UK                                                                           
R1BC008  DS    C                                                                
R1LOGO2  DS    CL7                                                              
*&&                                                                             
R1BC009  DS    C                                                                
R1USRNM  DS    CL33                                                             
R1USRAD  EQU   R1USRNM                                                          
R1BC010  DS    C                                                                
*                                                                               
R2LINED  DSECT                                                                  
R2BC001  DS    C                                                                
R2SPLID  DS    CL4                                                              
         DS    CL1                                                              
R2BC002  DS    C                                                                
R2AGYID  DS    CL2                                                              
         DS    CL1                                                              
R2BC003  DS    C                                                                
R2USRID  DS    CL10                                                             
R2BC004  DS    C                                                                
*&&US                                                                           
R2SHPUN  DS    CL4                                                              
*&&                                                                             
*&&UK                                                                           
R2UKALIN DS    CL4                                                              
R2BC005  DS    C                                                                
R2UKAIPA DS    CL3                                                              
*&&                                                                             
R2BC006  DS    C                                                                
R2SYSTM  DS    CL10                                                             
R2BC007  DS    C                                                                
R2LOGO1  DS    CL7                                                              
*&&UK                                                                           
R2BC008  DS    C                                                                
R2LOGO2  DS    CL7                                                              
*&&                                                                             
R2BC009  DS    C                                                                
R2USRNM  DS    CL33                                                             
R2USRAD  EQU   R2USRNM                                                          
R2BC010  DS    C                                                                
*                                                                               
R3LINED  DSECT                                                                  
R3BC001  DS    C                                                                
*&&UK                                                                           
R3LOGO2  DS    CL7                                                              
*&&                                                                             
*&&US                                                                           
R3SHPUN  DS    CL4                                                              
*&&                                                                             
R3BC002  DS    C                                                                
R3AGYID  DS    CL2                                                              
         DS    CL1                                                              
R3BC003  DS    C                                                                
R3USRID  DS    CL10                                                             
*&&UK                                                                           
R3BC004  DS    C                                                                
R3UKALIN DS    CL4                                                              
R3BC005  DS    C                                                                
R3UKAIPA DS    CL3                                                              
*&&                                                                             
R3BC006  DS    C                                                                
R3SPLID  DS    CL4                                                              
         DS    CL1                                                              
R3BC007  DS    C                                                                
R3SYSTM  DS    CL10                                                             
R3BC008  DS    C                                                                
R3LOGO1  DS    CL7                                                              
R3BC009  DS    C                                                                
R3USRNM  DS    CL33                                                             
R3USRAD  EQU   R3USRNM                                                          
R3BC010  DS    C                                                                
*                                                                               
R4LINED  DSECT                                                                  
R4BC001  DS    C                                                                
R4SYSTM  DS    CL10                                                             
R4BC002  DS    C                                                                
R4AGYID  DS    CL2                                                              
         DS    CL1                                                              
R4BC003  DS    C                                                                
R4USRID  DS    CL10                                                             
R4BC004  DS    C                                                                
*&&US                                                                           
R4SHPUN  DS    CL4                                                              
*&&                                                                             
*&&UK                                                                           
R4UKALIN DS    CL4                                                              
R4BC005  DS    C                                                                
R4UKAIPA DS    CL3                                                              
*&&                                                                             
R4BC006  DS    C                                                                
R4SPLID  DS    CL4                                                              
         DS    CL1                                                              
R4BC007  DS    C                                                                
R4LOGO1  DS    CL7                                                              
R4BC008  DS    C                                                                
R4LOGO2  DS    CL7                                                              
R4BC009  DS    C                                                                
R4USRNM  DS    CL33                                                             
R4BC010  DS    C                                                                
         EJECT                                                                  
R5LINED  DSECT                                                                  
R5BC001  DS    C                                                                
R5IDNUM  DS    CL5                                                              
         DS    CL1                                                              
R5BC002  DS    C                                                                
R5USRID  DS    CL10                                                             
         DS    CL1                                                              
R5BC003  DS    C                                                                
R5AGYID  DS    CL2                                                              
         DS    CL1                                                              
R5BC004  DS    C                                                                
R5USRNM  DS    CL33                                                             
         DS    CL1                                                              
R5BC005  DS    C                                                                
*                                                                               
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016CTREPIDS  01/26/17'                                      
         END                                                                    
