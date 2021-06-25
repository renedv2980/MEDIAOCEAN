*          DATA SET NEMED2B    AT LEVEL 012 AS OF 05/01/02                      
*************************************                                           
*  INPUTS FROM EDIT : ADRIVE - A(DRIVE TABLE TO USE)                            
*                     AINTERN - A(AREA USABEL FOR INTERNAL RECORDS)             
*PHASE T31E2BA                                                                  
*INCLUDE NEWRW                                                                  
*INCLUDE UNTIME                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE CENTER                                                                 
         TITLE 'T31E2B - COPY OF NETWORK ACCOUNTING REPORT'                     
         PRINT NOGEN                                                            
T31E2B   CSECT                                                                  
         NMOD1 0,**ACPR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING T31E2B+4096,RA                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          USE W/S AREA 2 FOR W/S                       
         USING ACCTD,R7                                                         
         ST    R2,RELO                                                          
*                                                                               
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
*                                                                               
         L     R2,=V(NEWRW)                                                     
         A     R2,RELO                                                          
         ST    R2,VNEWRW                                                        
*                                                                               
         EJECT                                                                  
         MVI   NREPTYP,C'A'        SET UP AS ACCTG REPORT                       
         MVI   PERTYPE,C'W'        SET UP FOR WEEKS                             
         MVI   PERTYPE+1,1         USE MONTHS IF TOO MANY WEEKS                 
         MVI   PERTYPE+2,0         NEVER USE QUARTERS                           
*                                                                               
         MVI   PRDFLAG,1           SET PRODUCT FLAG                             
*****    CLI   NBPRD2,0                                                         
*****    BE    ALLPRODS                                                         
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    ALLPRODS                                                         
         CLC   NBSELPRD,=C'POL'                                                 
         BE    ALLPRODS                                                         
         OC    NBSELPRD,NBSELPRD                                                
         BNZ   ONEPROD                                                          
         OC    NBSELPNM,NBSELPNM                                                
         BNZ   ONEPROD                                                          
ALLPRODS MVI   PRDFLAG,0           SET TO 0 IF ALL PRODS CHOSEN                 
ONEPROD  EQU   *                                                                
*                                                                               
         SPACE 2                                                                
*                                                                               
***********************                                                         
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT     PROCESS DATES                                
         BE    IN6                                                              
         B     PROCDAT             OTHER MODES ARE IGNORED                      
*                                                                               
IN6      LA    R1,MAXMONTS         SET UP MAX SIZE OF MONTH (WEEK) LIST         
         ST    R1,NUMMONS          GET LIST INTO MONLIST. NUMMONS IS            
*                                    NEW SIZE OF LIST                           
         MVI   PERTYPE,C'M'                                                     
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
*                                                                               
         BAS   RE,MEMINIT          INITIALIZE MEMORY                            
*                                                                               
*                                                                               
         MVI   NBDATA,C'U'         SELECT UNIT RECORDS                          
         MVI   NBUSER+13,C'N'      DONT FILTER PRE-EMPTS                        
*                                                                               
GETFIRST NETGO NSNETIO,DMCB,NETBLOCK   GET FIRST UNIT                           
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN     IF  UNIT RECORD                              
         BNE   GF2                                                              
****     NETGO NVACFLT,DMCB        PROCESS ACCOUNTING FILTER                    
****     BNZ   GETFIRST            IF STATUS NZ THEN DONT PROCESS               
         MVI   NACINIT,0           A NEW UNIT SO INIT ACCTG BLOCK               
         BAS   RE,FILLAC           FILL FIRST AC BLOCK                          
         CLI   NACMODE,NACREJ      IF DATA DOESN'T MEET FILTER                  
         BE    GETFIRST                                                         
         MVC   SAVECLI,NBCLICOD    FIRST CLIENT                                 
         MVC   SAVENET,NBACTNET    FIRST NETWORK                                
         MVC   SAVEPRGC,NBACTPRG   FIRST PROGRAM                                
         BAS   RE,GETPER           FIRST PERIOD                                 
         B     CKBREAK                                                          
GF2      CLI   NBMODE,NBREQLST     IF NO UNITS                                  
         BE    TXMOD                THEN DONE                                   
         B     GETFIRST            OTHER MODES ARE IGNORED                      
*                                                                               
GETNEXT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN     IF  UNIT RECORD                              
         BNE   GN2                                                              
****     NETGO NVACFLT,DMCB        PROCESS ACCOUNTING FILTER                    
****     BNZ   GETNEXT             IF STATUS NZ THEN DONT PROCESS               
         MVI   NACINIT,0           A NEW UNIT SO INIT ACCTG BLOCK               
         BAS   RE,FILLAC           FILL FIRST AC BLOCK                          
         CLI   NACMODE,NACREJ      IF DATA DOESN'T MEET FILTER                  
         BE    GETNEXT                                                          
         B     CKBREAK                                                          
GN2      CLI   NBMODE,NBREQLST     IF NO UNITS                                  
         BE    TOTALS                                                           
         B     GETNEXT             OTHER MODES ARE IGNORED                      
*                                                                               
CKBREAK  CLI   NBSEQ,C'X'                                                       
         BE    CKBRX                                                            
         CLI   NBSEQ,C'D'                                                       
         BE    CKBRD                                                            
         CLI   NBSEQ,C'P'                                                       
         BE    CKBRP                                                            
         CLI   NBSEQ,C'Q'                                                       
         BE    CKBRQ                                                            
         CLI   NBSEQ,C'N'                                                       
         BE    CKBRN                                                            
         B     CKBRD               D IS DEFAULT SEQUENCE                        
*                                                                               
CKBRD    BAS   RE,CKCLI                                                         
         BZ    CKBRD1                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         BAS   RE,MONTOTS                                                       
         BAS   RE,CLITOTS                                                       
         B     CKOVER                                                           
CKBRD1   BAS   RE,CKDATE                                                        
         BZ    CKBRD2                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         BAS   RE,MONTOTS                                                       
         B     CKOVER                                                           
CKBRD2   BAS   RE,CKNET                                                         
         BZ    CKBRD4                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         B     CKOVER                                                           
CKBRD4   BAS   RE,CKPROG                                                        
         BZ    CKBRD6                                                           
         BAS   RE,PROGTOTS                                                      
         B     CKOVER                                                           
CKBRD6   B     CKOVER                                                           
*                                                                               
*                                                                               
CKBRP    BAS   RE,CKCLI                                                         
         BZ    CKBRP1                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         BAS   RE,CLITOTS                                                       
         B     CKOVER                                                           
CKBRP1   BAS   RE,CKNET                                                         
         BZ    CKBRP2                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         B     CKOVER                                                           
CKBRP2   BAS   RE,CKPROG                                                        
         BZ    CKBRP4                                                           
         BAS   RE,PROGTOTS                                                      
         B     CKOVER                                                           
CKBRP4   B     CKOVER                                                           
*                                                                               
CKBRQ    BAS   RE,CKCLI                                                         
         BZ    CKBRQ1                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         BAS   RE,CLITOTS                                                       
         B     CKOVER                                                           
CKBRQ1   BAS   RE,CKNET                                                         
         BZ    CKBRQ2                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         B     CKOVER                                                           
CKBRQ2   BAS   RE,CKPROG                                                        
         BZ    CKBRQ4                                                           
         BAS   RE,PROGTOTS                                                      
         B     CKOVER                                                           
CKBRQ4   B     CKOVER                                                           
*                                                                               
CKBRN    BAS   RE,CKCLI                                                         
         BZ    CKBRN1                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,MONTOTS                                                       
         BAS   RE,GETPER           RESET DATE LIST                              
         BAS   RE,NETTOTS                                                       
         BAS   RE,CLITOTS                                                       
         B     CKOVER                                                           
CKBRN1   BAS   RE,CKNET                                                         
         BZ    CKBRN2                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,MONTOTS                                                       
         BAS   RE,GETPER           RESET DATE LIST                              
         BAS   RE,NETTOTS                                                       
         B     CKOVER                                                           
CKBRN2   BAS   RE,CKDATE                                                        
         BZ    CKBRN4                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,MONTOTS                                                       
         B     CKOVER                                                           
CKBRN4   BAS   RE,CKPROG                                                        
         BZ    CKBRN6                                                           
         BAS   RE,PROGTOTS                                                      
         B     CKOVER                                                           
CKBRN6   B     CKOVER                                                           
*                                                                               
CKBRX    BAS   RE,CKCLI                                                         
         BZ    CKBRX1                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         BAS   RE,MONTOTS                                                       
         BAS   RE,CLITOTS                                                       
         B     CKOVER                                                           
CKBRX1   BAS   RE,CKDATE                                                        
         BZ    CKBRX2                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         BAS   RE,MONTOTS                                                       
         B     CKOVER                                                           
CKBRX2   BAS   RE,CKNET                                                         
         BZ    CKBRX4                                                           
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         B     CKOVER                                                           
CKBRX4   BAS   RE,CKPROG                                                        
         BZ    CKBRX6                                                           
         BAS   RE,PROGTOTS                                                      
         B     CKOVER                                                           
CKBRX6   B     CKOVER                                                           
*                                                                               
CKOVER   BAS   RE,UNIT             FILL IN UNIT LINE                            
         CLI   NACMODE,NACBP       CK IF MORE TO FILTER                         
         BNE   GETNEXT                                                          
         GOTO1 INTINIT,DMCB,AINTERN    CLEAR INTERNAL RECS                      
ACCTLOOP BAS   RE,FILLAC           FILL NEXT AC BLOCK                           
         CLI   NACMODE,NACBP      CK IF LAST ACCT BLOCK                         
         BNE   GETNEXT                                                          
         BAS   RE,PRACC                                                         
         B     ACCTLOOP                                                         
*                                                                               
CKAGY    BR    RE                                                               
*                                                                               
CKPACK   BR    RE                                                               
*                                                                               
CKPROG   LA    R1,0                                                             
         CLC   SAVEPRGC,NBACTPRG   IF A NEW PROGRAM                             
         BZ    CKPXIT                                                           
         LA    R1,1                                                             
CKPXIT   LTR   R1,R1                                                            
         BR    RE                                                               
*                                                                               
CKCLI    LA    R1,0                                                             
         CLC   SAVECLI,NBCLICOD   IF A NEW CLIENT                               
         BZ    CKCLXIT                                                          
         LA    R1,1                                                             
CKCLXIT  LTR   R1,R1                                                            
         BR    RE                                                               
*                                                                               
CKDP     BR    RE                                                               
*                                                                               
CKPROD   BR    RE                                                               
*                                                                               
CKEST    BR    RE                                                               
*                                                                               
CKNET    LA    R1,0                                                             
         CLC   SAVENET,NBACTNET   IF A NEW NETWORK                              
         BZ    CKNXIT                                                           
         LA    R1,1                                                             
CKNXIT   LTR   R1,R1                                                            
         BR    RE                                                               
*                                                                               
CKDATE   LA    R1,0                                                             
         CLC   NBACTDAT,SAVEPER    IF IN CURRENT MONTH SET                      
         BL    NEWMONTH                                                         
         CLC   NBACTDAT,SAVEPER+2                                               
         BH    NEWMONTH                                                         
         B     CKDXIT                                                           
NEWMONTH LA    R1,1                                                             
CKDXIT   LTR   R1,R1                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
TOTALS   CLI   NBSEQ,C'X'                                                       
         BE    TOTX                                                             
         CLI   NBSEQ,C'D'                                                       
         BE    TOTD                                                             
         CLI   NBSEQ,C'P'                                                       
         BE    TOTP                                                             
         CLI   NBSEQ,C'Q'                                                       
         BE    TOTQ                                                             
         CLI   NBSEQ,C'N'                                                       
         BE    TOTN                                                             
         B     TOTD                D IS DEFAULT SEQUENCE                        
*                                                                               
TOTD     BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         BAS   RE,MONTOTS                                                       
         B     TOTMAIN                                                          
*                                                                               
TOTP     BAS   RE,MONTOTS                                                       
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         B     TOTMAIN                                                          
*                                                                               
TOTQ     BAS   RE,MONTOTS                                                       
         BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         BAS   RE,CKEST                                                         
         B     TOTMAIN                                                          
*                                                                               
TOTN     BAS   RE,PROGTOTS                                                      
         BAS   RE,MONTOTS                                                       
         BAS   RE,NETTOTS                                                       
         B     TOTMAIN                                                          
*                                                                               
TOTX     BAS   RE,PROGTOTS                                                      
         BAS   RE,NETTOTS                                                       
         BAS   RE,MONTOTS                                                       
         B     TOTMAIN                                                          
*                                                                               
TOTMAIN  BAS   RE,CLITOTS          CHANGE TO TOTS EVENTUALLY                    
         BAS   RE,CKAGY                                                         
*                                                                               
         CLI   TOTSUBT,C'Y'                                                     
         BNE   TXMOD                                                            
         BAS   RE,TOTTOTS                                                       
*                                                                               
TXMOD    XMOD1                                                                  
*                                                                               
PROCERR  DC    F'0'                                                             
*                                                                               
*                                                                               
*****************************************************************               
         EJECT                                                                  
*****************************************************************               
* MEMINIT - INITIALIZE MEMORY                                                   
*       CALLS TO: INTINIT - INITIALIZE INTERNAL RECORD.                         
*       OUTPUTS: AINTERN    A(INTERNAL RECORD)                                  
*                APREVREC   A(PREVIOUS INTERNAL RECORD)                         
*                AWEEKREC   A(WEEK TOTAL RECORD)                                
*                ANETTOT    A(NET TOTAL RECORD)                                 
*                ACLITOT    A(CLIENT TOTAL RECORD)                              
*                AREPTOT    A(REPORT TOTAL RECORD)                              
*                APRGTOT    A(PROG TOTAL RECORD)                                
*                AENDTOT    A(END OF INTERNAL RECORDS)                          
*                IRECLEN - LENGTH OF INTERNAL RECORD                            
*       INTERNALS: R5 -  DRIVE TABLE                                            
*                  R6 - CURRENT LEN OF INTERNAL RECORD                          
*                  R4 - CURRENT LENGTH OF PRINT LINE                            
*                                                                               
MEMINIT  NTR1                                                                   
         L     R5,ADRIVE                                                        
         USING RWINBLOK,R5           GET LENGTH OF INTERNAL RECORD              
         SR    R6,R6                                                            
         SR    R4,R4                                                            
MILOOP   CLI   RWINTYPE,0                                                       
         BE    MIEND                                                            
         ZIC   R1,RWOUTLEN                                                      
         LA    R6,0(R6,R1)         SUM INTERNAL RECORD LENGTH                   
         ZIC   R1,RWPRLEN                                                       
         LA    R4,1(R4,R1)         SUM PRINT LINE + 1 COL FOR SPACE             
*                                                                               
         LA    R5,RWBLKLEN(R5)      NEXT ENTRY IN DRIVE TABLE                   
         B     MILOOP                                                           
MIEND    ST    R6,IRECLEN                                                       
         LA    R1,132                                                           
         CR    R4,R1               CK IF EXCEED PRINT LINE                      
         BL    MI4                                                              
         DC    H'0'                                                             
MI4      L     R2,AINTERN          SET ADDRESSES                                
         LA    R2,0(R2,R6)                                                      
         ST    R2,APREVREC                                                      
         LA    R2,0(R2,R6)                                                      
         ST    R2,AWEEKREC                                                      
         LA    R2,0(R2,R6)                                                      
         ST    R2,ANETTOT                                                       
         LA    R2,0(R2,R6)                                                      
         ST    R2,ACLITOT                                                       
         LA    R2,0(R2,R6)                                                      
         ST    R2,AREPTOT                                                       
         LA    R2,0(R2,R6)                                                      
         ST    R2,APRGTOT                                                       
         LA    R2,0(R2,R6)         END OF INTERNAL REC                          
         ST    R2,AENDTOT                                                       
         GOTO1 INTINIT,DMCB,AINTERN    INITIALIZE INTERNAL RECS                 
         GOTO1 INTINIT,DMCB,APREVREC                                            
         GOTO1 INTINIT,DMCB,AWEEKREC                                            
         GOTO1 INTINIT,DMCB,ANETTOT                                             
         GOTO1 INTINIT,DMCB,ACLITOT                                             
         GOTO1 INTINIT,DMCB,AREPTOT                                             
         GOTO1 INTINIT,DMCB,APRGTOT                                             
         B     XIT                                                              
*****************************************************************               
         DROP  R5                                                               
         EJECT                                                                  
*****************************************************************               
* INTINIT - INITIALIZE THE INTERNAL RECORD POINTED TO BY ARG1                   
*           MOVE ZEROES FOR NUMERICS AND ALPHAS                                 
*           PACKED ZEROES FOR COST                                              
*       INPUTS: ARG1 (R6) - A(INTERNAL RECORD TO INITIALIZE)                    
INTINIT  NTR1                                                                   
         L     R6,0(R1)                                                         
         L     R5,ADRIVE           GET A(DRIVE TABLE)                           
         USING RWINBLOK,R5                                                      
ININ2    CLI   RWINTYPE,0                                                       
         BE    ININXIT                                                          
         ZIC   RF,RWOUTLEN          PUT LENGTH IN RF                            
         CLI   RWINTYPE,NRICOST      CK FOR COST                                
         BE    ININCOST                                                         
         XCEF  (R6),(RF)            CLEAR IT                                    
         B     ININNEXT                                                         
*                                                                               
ININCOST MVI   0(R6),0              FOR COST 1ST BYTE IS ZERO                   
         ZAP   1(8,R6),ZERO         AND ZAP NEXT 8                              
*                                                                               
ININNEXT ZIC   R1,RWOUTLEN         BUMP TO NEXT PIECE OF INTERNAL REC           
         LA    R6,0(R1,R6)                                                      
         LA    R5,RWBLKLEN(R5)      BUMP TO NEXT DRIVE                          
         B     ININ2                                                            
*                                                                               
ININXIT  B     XIT                                                              
************************************************************                    
         DROP  R5                                                               
         EJECT                                                                  
*****************************************************************               
* MONTH TOTALS - PRINT THE MONTH TOTALS , ADD MONTH TOTAL TO NET TOTAL          
*           INPUTS: SAVEPER - CURRENT PERIOD                                    
*                                                                               
MONTOTS  NTR1                                                                   
*                                                                               
         CLI   DATSUBT,C'Y'        DONT PRINT IF NE Y                           
         BNE   DOMT                                                             
         OC    SAVEPER,SAVEPER     DONT PRINT IF NO SAVED PER                   
         BZ    DOMT                                                             
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
*                                                                               
         USING RWINBLOK,R5                                                      
         L     R5,ADRIVE           GET A(DRIVE TABLE)                           
         L     R6,AWEEKREC         GET A(INTERNAL RECORD)                       
         LA    R4,P1               GET PRINT LINE                               
MON2     CLI   RWINTYPE,0                                                       
         BE    MONPRIN                                                          
         CLI   RWOUTTYP,NROALPH    DONT PRINT ALPHAS                            
         BE    MON4                                                             
         ST    R6,RWAOUT                                                        
         ST    R4,RWAPR                                                         
         MVI   RWFUNC,RWPRINT                                                   
         GOTO1 VNEWRW,DMCB,NETSYSD,(R5)                                         
*                                                                               
MON4     ZIC   R2,RWPRLEN          BUMP PRINT POSITION + 1 SPACE                
         LA    R4,1(R4,R2)                                                      
         ZIC   R2,RWOUTLEN         BUMP TO NEXT PIECE OF INTERNAL REC           
         LA    R6,0(R2,R6)                                                      
         LA    R5,RWBLKLEN(R5)     BUMP TO NEXT DRIVE                           
         B     MON2                                                             
MONPRIN  CLC   P1(132),SPACES                                                   
         BE    XITMP                                                            
         OC    P1(132),P1                                                       
         BZ    XITMP                                                            
         GOTO1 DATCON,DMCB,(2,SAVEPER),(4,P1)                                   
         MVI   P1+6,C'-'                                                        
         GOTO1 DATCON,DMCB,(2,SAVEPER+2),(4,P1+8)                               
         MVC   P1+14(5),=C'TOTAL'                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT                                     
         GOTO1 SPOOL,DMCB,(R8)     AND BLANK LINE AFTER                         
*                                                                               
DOMT     BAS   RE,GETPER           RESET SAVEPER TO NEW PERIOD                  
XITMP    GOTO1 INTINIT,DMCB,AWEEKREC     RESET WEEK TOTALS TO 0                 
         B     XIT                                                              
         DROP  R5                                                               
************************************************************                    
         EJECT                                                                  
************************************************************                    
*              ROUTINE TO BUILD INTERNAL RECORD                                 
*  LOCALS - R5 -A(CURRENT PIECE OF DRIVE TABLE)                                 
*           R6 -A(CURRENT INTERNAL RECORD)                                      
*           R4 -A(CUR POSITON IN PRINT LINE)                                    
*   OUTPUT: FILLS PREVIOUS RECORD                                               
*           FILLS INTERNAL RECORD                                               
*           ADDS INTERNAL RECORD TO WEEK TOTALS                                 
*                                                                               
UNIT     NTR1                                                                   
         L     R5,AINTERN                                                       
         L     R4,APREVREC                                                      
         L     R3,IRECLEN                                                       
         BCTR  R3,0                                                             
         EXMVC R3,0(R4),0(R5)      MOVE CURRENT RECORD TO PREV RECORD           
*                                                                               
         USING RWINBLOK,R5                                                      
         L     R5,ADRIVE           GET A(DRIVE TABLE)                           
         L     R6,AINTERN          GET A(INTERNAL RECORD)                       
         LA    R4,P1               GET PRINT LINE                               
UN2      CLI   RWINTYPE,0                                                       
         BE    UNPRIN                                                           
         ST    R6,RWAOUT                                                        
         ST    R4,RWAPR                                                         
         MVI   RWFUNC,RWMOVE                                                    
         GOTO1 VNEWRW,DMCB,NETSYSD,(R5)                                         
         CLI   UNPRINF,C'N'                                                     
         BE    UN4                                                              
         MVI   RWFUNC,RWPRINT                                                   
         GOTO1 VNEWRW,DMCB,NETSYSD,(R5)                                         
*                                                                               
UN4      ZIC   R2,RWPRLEN          BUMP PRINT POSITION + 1 SPACE                
         LA    R4,1(R4,R2)                                                      
         ZIC   R2,RWOUTLEN         BUMP TO NEXT PIECE OF INTERNAL REC           
         LA    R6,0(R2,R6)                                                      
         LA    R5,RWBLKLEN(R5)     BUMP TO NEXT DRIVE                           
         B     UN2                                                              
UNPRIN   CLI   UNPRINF,C'N'                                                     
         BE    UN6                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*                                  ADD CURRENT RECORD TO WEEK TOTALS            
UN6      GOTO1 ADDINTS,DMCB,AWEEKREC,AINTERN                                    
*                                  ADD CURRENT RECORD TO NET TOTALS             
         GOTO1 ADDINTS,DMCB,ANETTOT,AINTERN                                     
         GOTO1 ADDINTS,DMCB,ACLITOT,AINTERN   ADD TO CLIENT TOTALS              
         GOTO1 ADDINTS,DMCB,AREPTOT,AINTERN   ADD TO REPORT TOTALS              
         GOTO1 ADDINTS,DMCB,APRGTOT,AINTERN   ADD TO PROGRAM TOTALS             
         B     XIT                                                              
         DROP  R5                                                               
**********************************************************                      
         EJECT                                                                  
************************************************************                    
* PRACC        ROUTINE TO PRINT ACCCTG SUB-RECORD                               
*  LOCALS - R5 -A(CURRENT PIECE OF DRIVE TABLE)                                 
*           R6 -A(CURRENT INTERNAL RECORD)                                      
*           R4 -A(CUR POSITON IN PRINT LINE)                                    
*                                                                               
PRACC    NTR1                                                                   
         USING RWINBLOK,R5                                                      
         L     R5,ADRIVE           GET A(DRIVE TABLE)                           
         L     R6,AINTERN          GET A(INTERNAL RECORD)                       
         LA    R4,P1               GET PRINT LINE                               
PAC2     CLI   RWINTYPE,0                                                       
         BE    PACPRIN                                                          
         ST    R6,RWAOUT                                                        
         ST    R4,RWAPR                                                         
         MVI   RWFUNC,RWMOVE                                                    
         GOTO1 VNEWRW,DMCB,NETSYSD,(R5)                                         
         CLI   RWFILTER,C'A'      ONLY PRINT AUDIT TRAIL FIELDS                 
         BNE   PAC4                                                             
         MVI   RWFUNC,RWPRINT                                                   
         GOTO1 VNEWRW,DMCB,NETSYSD,(R5)                                         
*                                                                               
PAC4     ZIC   R2,RWPRLEN          BUMP PRINT POSITION + 1 SPACE                
         LA    R4,1(R4,R2)                                                      
         ZIC   R2,RWOUTLEN         BUMP TO NEXT PIECE OF INTERNAL REC           
         LA    R6,0(R2,R6)                                                      
         LA    R5,RWBLKLEN(R5)     BUMP TO NEXT DRIVE                           
         B     PAC2                                                             
PACPRIN  CLC   P1(131),SPACES      DONT PRINT IF ALL SPACES                     
         BE    PAC6                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*                                  ADD CURRENT RECORD TO WEEK TOTALS            
PAC6     GOTO1 ADDINTS,DMCB,AWEEKREC,AINTERN                                    
*                                  ADD CURRENT RECORD TO NET TOTALS             
         GOTO1 ADDINTS,DMCB,ANETTOT,AINTERN                                     
         GOTO1 ADDINTS,DMCB,ACLITOT,AINTERN   ADD TO CLIENT TOTALS              
         GOTO1 ADDINTS,DMCB,AREPTOT,AINTERN   ADD TO REPORT TOTALS              
         GOTO1 ADDINTS,DMCB,APRGTOT,AINTERN   ADD TO PROGRAM TOTALS             
         B     XIT                                                              
         DROP  R5                                                               
**********************************************************                      
         EJECT                                                                  
**********************************************************                      
* GETPER - SET SAVEPER TO THE CORRECT PERIOD                                    
*       INPUTS: MONLIST - PERIOD LIST                                           
*               NBACTDAT - CURRENT DATE                                         
*       OUTPUT: SAVEPER - THE PERIOD THAT NBACTDAT BELONGS TO                   
**********************************************************                      
GETPER   NTR1                                                                   
         LA    R6,MONLIST                                                       
GPLOOP   CLC   NBACTDAT,2(R6)                                                   
         BNH   GOTPER                                                           
         LA    R6,4(R6)            NEXT DATE SET                                
         B     GPLOOP                                                           
GOTPER   MVC   SAVEPER,0(R6)                                                    
         XIT1                                                                   
         EJECT                                                                  
**********************************************************                      
PRLUP    NTR1                                                                   
         MVC   WORK,SPACES                                                      
         LA    R4,NBPRD                                                         
         LA    R5,WORK                                                          
         BAS   RE,PRLUP2                                                        
         LA    R4,NBPRD2                                                        
         LA    R5,WORK+3                                                        
         BAS   RE,PRLUP2                                                        
         B     XIT                                                              
         SPACE 2                                                                
PRLUP2   L     R2,ANETWS1          A(CLIENT RECORD)                             
         USING CLTHDR,R2                                                        
         LA    R2,CLIST            PRODUCT LIST                                 
         DROP  R2                                                               
         LA    R3,220                                                           
         CLI   0(R4),0                                                          
         BER   RE                                                               
         CLI   0(R4),X'FF'                                                      
         BER   RE                                                               
         SPACE 2                                                                
PRLUP4   CLC   0(1,R4),3(R2)                                                    
         BE    PRLUP6                                                           
         LA    R2,4(R2)                                                         
         BCT   R3,PRLUP4                                                        
         BR    RE                                                               
         SPACE 2                                                                
PRLUP6   MVC   0(3,R5),0(R2)                                                    
         BR    RE                                                               
         EJECT                                                                  
**************************************************                              
* NETTOTS - PRINT NET TOTALS                                                    
*                                                                               
NETTOTS  NTR1                                                                   
*                                                                               
         OC    NBSELNET,NBSELNET   DONT GIVE TOTAL IF 1 NETWORK                 
         BNZ   DONT                                                             
         CLI   NETSUBT,C'Y'          DONT PRINT EXCEPT NET SUBTOTALS=Y          
         BNE   DONT                                                             
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
*                                                                               
         USING RWINBLOK,R5                                                      
         L     R5,ADRIVE           GET A(DRIVE TABLE)                           
         L     R6,ANETTOT          GET A(NET INTERNAL RECORD)                   
         LA    R4,P1               GET PRINT LINE                               
NET2     CLI   RWINTYPE,0                                                       
         BE    NETPRIN                                                          
         CLI   RWOUTTYP,NROALPH    DONT PRINT ALPHAS                            
         BE    NET4                                                             
         ST    R6,RWAOUT                                                        
         ST    R4,RWAPR                                                         
         MVI   RWFUNC,RWPRINT                                                   
         GOTO1 VNEWRW,DMCB,NETSYSD,(R5)                                         
*                                                                               
NET4     ZIC   R2,RWPRLEN          BUMP PRINT POSITION + 1 SPACE                
         LA    R4,1(R4,R2)                                                      
         ZIC   R2,RWOUTLEN         BUMP TO NEXT PIECE OF INTERNAL REC           
         LA    R6,0(R2,R6)                                                      
         LA    R5,RWBLKLEN(R5)     BUMP TO NEXT DRIVE                           
         B     NET2                                                             
NETPRIN  MVC   P1(4),SAVENET                                                    
         MVC   P1+5(5),=C'TOTAL'                                                
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT                                     
         GOTO1 SPOOL,DMCB,(R8)     AND BLANK LINE AFTER                         
*                                                                               
DONT     GOTO1 INTINIT,DMCB,ANETTOT      RESET NETWORK TOTALS TO 0              
         MVC   SAVENET,NBACTNET                                                 
XITNET   B     XIT                                                              
         DROP  R5                                                               
*******************************************************                         
         EJECT                                                                  
**************************************************                              
* CLITOTS - PRINT CLIENT TOTALS                                                 
*                                                                               
CLITOTS  NTR1                                                                   
*                                                                               
         OC    NBSELCLI,NBSELCLI   DONT GIVE TOTAL IF 1 CLI                     
         BNZ   DOCLI                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
*                                                                               
         USING RWINBLOK,R5                                                      
         L     R5,ADRIVE           GET A(DRIVE TABLE)                           
         L     R6,ACLITOT          GET A(CLI INTERNAL RECORD)                   
         LA    R4,P1               GET PRINT LINE                               
CLI2     CLI   RWINTYPE,0                                                       
         BE    CLIPRIN                                                          
         CLI   RWOUTTYP,NROALPH    DONT PRINT ALPHAS                            
         BE    CLI4                                                             
         ST    R6,RWAOUT                                                        
         ST    R4,RWAPR                                                         
         MVI   RWFUNC,RWPRINT                                                   
         GOTO1 VNEWRW,DMCB,NETSYSD,(R5)                                         
*                                                                               
CLI4     ZIC   R2,RWPRLEN          BUMP PRINT POSITION + 1 SPACE                
         LA    R4,1(R4,R2)                                                      
         ZIC   R2,RWOUTLEN         BUMP TO NEXT PIECE OF INTERNAL REC           
         LA    R6,0(R2,R6)                                                      
         LA    R5,RWBLKLEN(R5)     BUMP TO NEXT DRIVE                           
         B     CLI2                                                             
CLIPRIN  MVC   P1(3),SAVECLI                                                    
         MVC   P1+5(5),=C'TOTAL'                                                
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT                                     
         GOTO1 SPOOL,DMCB,(R8)     AND BLANK LINE AFTER                         
*                                                                               
DOCLI    GOTO1 INTINIT,DMCB,ACLITOT      RESET CLIENT TOTALS TO 0               
         MVC   SAVECLI,NBCLICOD                                                 
XITCLI   B     XIT                                                              
         DROP  R5                                                               
*******************************************************                         
         EJECT                                                                  
**************************************************                              
* PROGTOTS - PRINT PROG TOTALS                                                  
*                                                                               
PROGTOTS NTR1                                                                   
*                                                                               
         CLI   PRGSUBT,C'Y'          DONT PRINT EXCEPT PRG SUBTOTALS=Y          
         BNE   DOPT                                                             
*                                                                               
         USING RWINBLOK,R5                                                      
         L     R5,ADRIVE           GET A(DRIVE TABLE)                           
         L     R6,APRGTOT          GET A(NET INTERNAL RECORD)                   
         LA    R4,P1               GET PRINT LINE                               
PRG2     CLI   RWINTYPE,0                                                       
         BE    PRGPRIN                                                          
         ST    R6,RWAOUT                                                        
         ST    R4,RWAPR                                                         
         MVI   RWFUNC,RWPRINT                                                   
         GOTO1 VNEWRW,DMCB,NETSYSD,(R5)                                         
*                                                                               
PRG4     ZIC   R2,RWPRLEN          BUMP PRINT POSITION + 1 SPACE                
         LA    R4,1(R4,R2)                                                      
         ZIC   R2,RWOUTLEN         BUMP TO NEXT PIECE OF INTERNAL REC           
         LA    R6,0(R2,R6)                                                      
         LA    R5,RWBLKLEN(R5)     BUMP TO NEXT DRIVE                           
         B     PRG2                                                             
PRGPRIN  GOTO1 SPOOL,DMCB,(R8)     PRINT IT                                     
         GOTO1 SPOOL,DMCB,(R8)     AND BLANK LINE AFTER                         
*                                                                               
DOPT     GOTO1 INTINIT,DMCB,APRGTOT      RESET PROG TOTALS TO 0                 
         MVC   SAVEPRGC,NBACTPRG                                                
XITPRG   B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*******************************************************                         
         SPACE 3                                                                
TOTTOTS  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
*                                                                               
         USING RWINBLOK,R5                                                      
         L     R5,ADRIVE           GET A(DRIVE TABLE)                           
         L     R6,AREPTOT          GET A(REPORT TOTALS)                         
         LA    R4,P1               GET PRINT LINE                               
TOT2     CLI   RWINTYPE,0                                                       
         BE    TOTPRIN                                                          
         CLI   RWOUTTYP,NROALPH    DONT PRINT ALPHAS                            
         BE    TOT4                                                             
         ST    R6,RWAOUT                                                        
         ST    R4,RWAPR                                                         
         MVI   RWFUNC,RWPRINT                                                   
         GOTO1 VNEWRW,DMCB,NETSYSD,(R5)                                         
*                                                                               
TOT4     ZIC   R2,RWPRLEN          BUMP PRINT POSITION + 1 SPACE                
         LA    R4,1(R4,R2)                                                      
         ZIC   R2,RWOUTLEN         BUMP TO NEXT PIECE OF INTERNAL REC           
         LA    R6,0(R2,R6)                                                      
         LA    R5,RWBLKLEN(R5)     BUMP TO NEXT DRIVE                           
         B     TOT2                                                             
TOTPRIN  MVC   P1+2(13),=C'REPORT TOTAL '                                       
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT                                     
*                                                                               
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
********************************************************                        
* FILLAC - ROUTINE TO FILL ACCOUNTING BLOCK                                     
*          SUPPORTS AUDIT TRAIL AND PAID/CLEARED FILTER DATES                   
*          LOCALS:  R5 - NACCTBLOCK                                             
*                   R6 - USED FOR NEXTEL                                        
********************************************************                        
FILLAC   NTR1                                                                   
         CLI   NACINIT,1                                                        
         BNE   FAC2                                                             
         XC    NBACTUAL,NBACTUAL   IF SECOND PASS, SET COSTS TO ZERO.           
         XC    NBASSIGN,NBASSIGN   THEY HAVE ALREADY BEEN PRINTED,ADDED         
         XC    NBINTEG,NBINTEG                                                  
         XC    NBCALCOS,NBCALCOS                                                
         MVI   NBUNITST,0          SET UNIT STATUS TO 0 FOR THESE LINES         
         B     FAC6                                                             
*                                                                               
FAC2     L     R3,NBAIO            INITIALIZE                                   
         USING NURECD,R3                                                        
         LA    R4,NUMAINEL                                                      
         DROP  R3                                                               
         ST    R4,NACABILE         SET CURRENT ELEMENTS TO FIRST EL             
         ST    R4,NACAPAYE                                                      
         MVI   NACMODE,0           INITIALIZE                                   
         MVI   NACINIT,1                                                        
*                                                                               
         TM    NBUNITST,X'42'      IF PRE-EMPT OR MISSED                        
         BZ    PU1                                                              
         XC    NBACTUAL,NBACTUAL   SET COSTS TO 0 BUT                           
         XC    NBASSIGN,NBASSIGN   LEAVE BILLING AND PAYING                     
         XC    NBINTEG,NBINTEG                                                  
         XC    NBCALCOS,NBCALCOS                                                
*                                                                               
*  CONDITINAL ASSIGNED COST                                                     
PU1      CLI   NBUSER+8,C'Y'       IF FLAG SET TO USE ASSIGNED                  
         BNE   PU3                                                              
         OC    NBASSIGN,NBASSIGN   AND IF ASSIGNED COST NON-ZERO                
         BNZ   PU2                                                              
         TM    NBUNITST,X'08'      OR ASSIGNED COST TRULY ZERO                  
         BZ    PU3                                                              
PU2      MVC   NBCALCOS,NBASSIGN     THEN USE ASSIGNED COST                     
         B     PU4                                                              
PU3      MVC   NBCALCOS,NBACTUAL   ELSE USE ACTUAL COST                         
*                                                                               
PU4      CLI   NBSELFLT,0          IF A FILTER                                  
         BE    FILTDON                                                          
         CLI   NBSELFLT,4          UNBILLED                                     
         BNE   FLT2                                                             
         L     R1,NBCALCOS                                                      
         A     R1,NBINTEG                                                       
         S     R1,NBBILTGR         FOR FILTER = UNCLEARED                       
         S     R1,NBBILIGR                                                      
         LTR   R1,R1               IF ZERO THEN REJECT                          
         BZ    FACREJ                                                           
         B     FILTDON                                                          
FLT2     CLI   NBSELFLT,2                                                       
         BNE   FILTDON             OTHER FILTS TAKEN CARE OF BY NETIO           
         L     R1,NBACTUAL                                                      
         A     R1,NBINTEG                                                       
         S     R1,NBPAYTGR         FOR FILTER = UNCLEARED                       
         S     R1,NBPAYIGR                                                      
         LTR   R1,R1               IF ZERO THEN REJECT                          
         BZ    FACREJ                                                           
         B     FILTDON                                                          
*                                                                               
FILTDON  CLI   PRDFLAG,1           IF ONE PRODUCT                               
         BNE   PU5                                                              
         CLI   NBPRD2,0                                                         
         BE    PU5                                                              
*                                                                               
PRDPCT   SR    R6,R6                                                            
         ICM   R6,3,NBP1SHR        PCTG FOR FIRST PRODUCT                       
         CLC   NBEFFPNM,NBPRD      IF THIS FIRST PRODUCT THEN GO ON             
         BE    APRD2               IF NOT ITS PROD2                             
         CLC   NBEFFPNM,NBPRD2                                                  
         BE    APRD1                                                            
         DC    H'0'                                                             
APRD1    L     R5,=F'10000'        PUT 100.00 IN R5                             
         SR    R5,R6               THIS IS PCTG FOR PROD 2                      
         LR    R6,R5                                                            
APRD2    DS    0H                                                               
*                                                                               
         L     R5,NBACTUAL         ADJUST ACTUAL                                
         MR    R4,R6                                                            
         SLDA  R4,1                DOUBLE FOR ROUNDING                          
         D     R4,=F'10000'                                                     
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         ST    R5,NBACTUAL                                                      
*                                                                               
         L     R5,NBASSIGN         ADJUST ASSIGNED COST                         
         MR    R4,R6                                                            
         SLDA  R4,1                DOUBLE FOR ROUNDING                          
         D     R4,=F'10000'                                                     
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         ST    R5,NBASSIGN                                                      
*                                                                               
         L     R5,NBCALCOS         ADJUST CALCULATED ASSIGNED COST              
         MR    R4,R6                                                            
         SLDA  R4,1                DOUBLE FOR ROUNDING                          
         D     R4,=F'10000'                                                     
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         ST    R5,NBCALCOS                                                      
*                                                                               
         L     R5,NBINTEG          ADJUST INTEGRATION COST                      
         MR    R4,R6                                                            
         SLDA  R4,1                DOUBLE FOR ROUNDING                          
         D     R4,=F'10000'                                                     
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         ST    R5,NBINTEG                                                       
*                                                                               
*                                                                               
*                                                                               
PU5      EQU   *                                                                
FAC6     XC    NACDTFLD(NACDTLN),NACDTFLD   CLEAR DATA FIELDS                   
*                                                                               
         L     R6,NACABILE         GET NEXT BILL ELEMENT                        
         MVI   SRCHEL,X'10'                                                     
NEXTBEL  BAS   RE,NEXTEL                                                        
         BNE   DOPAY               LAST ELEMENT                                 
         USING NUBILD,R6                                                        
         ST    R6,NACABILE         NEW CURRENT ELEMENT                          
         TM    NUBILST,X'20'       CHECK IF UNBILLED                            
         BO    NEXTBEL                                                          
         CLI   PRDFLAG,0           EXTRA FILTERING IF 1 PROD                    
         BE    DOBILL1                                                          
         CLC   NBEFFPNM,NUBILPRD                                                
         BNE   NEXTBEL                                                          
*                                                                               
DOBILL1  CLI   NACSBTY,0                CK IF BILL TYPE FILTER                  
         BE    DOBILL4                                                          
         CLC   NACSBTY,NUBILBTY                                                 
         BNE   NEXTBEL                                                          
*                                                                               
DOBILL4  OC    NACSBDT(2),NACSBDT                                               
         BZ    DOBILL6               DATE FILTER                                
         CLC   NACSBDT(2),NUBILDAT                                              
         BH    NEXTBEL                                                          
DOBILL6  OC    NACSBDT+2(2),NACSBDT+2                                           
         BZ    DOBILL8                                                          
         CLC   NACSBDT+2(2),NUBILDAT                                            
         BL    NEXTBEL                                                          
DOBILL8  CLI   NACSMOD,C'A'        FOR AUDIT TRAIL                              
         BNE   DOBILL16                                                         
         CLI   NUBILTYP,C'T'        TIME                                        
         BNE   DOBILL10                                                         
         MVI   NACBTFLG,1          ELEMENT EXISTS                               
         MVC   NACBTDAT(21),NUBILDAT                                            
         BAS   RE,NEXTEL           IF CORRESPONDING INT, DO IT TOO              
         BNE   DOBILL11                                                         
         CLI   NUBILTYP,C'I'                                                    
         BNE   DOBILL11                                                         
         CLC   NACBTDAT,NUBILDAT                                                
         BNE   DOBILL11                                                         
         CLC   NACBTPRD,NUBILPRD                                                
         BNE   DOBILL11                                                         
         ST    R6,NACABILE         GOT ONE                                      
         MVC   NACBIDAT(21),NUBILDAT                                            
         MVI   NACBIFLG,1                                                       
         B     DOBILL11                                                         
DOBILL10 MVC   NACBIDAT(21),NUBILDAT                                            
         MVI   NACBIFLG,1          ELEMENT EXISTS                               
         BAS   RE,NEXTEL           IF CORRESPONDING TIME, DO IT TOO             
         BNE   DOBILL11                                                         
         CLI   NUBILTYP,C'T'                                                    
         BNE   DOBILL11                                                         
         CLC   NACBIDAT,NUBILDAT                                                
         BNE   DOBILL11                                                         
         CLC   NACBIPRD,NUBILPRD                                                
         BNE   DOBILL11                                                         
         ST    R6,NACABILE         GOT ONE                                      
         MVC   NACBTDAT(21),NUBILDAT                                            
         MVI   NACBTFLG,1                                                       
         B     DOBILL11                                                         
DOBILL11 B     DOPAY                                                            
*                                                                               
DOBILL16 CLI   NUBILTYP,C'T'       FOR NON-AUDIT TRAIL, DO SUMS                 
         BNE   DOBILL18                                                         
         MVI   NACBTFLG,1          ELEMENT EXISTS                               
         L     R2,NUBILGRS                                                      
         A     R2,NACBTGRS                                                      
         ST    R2,NACBTGRS                                                      
         L     R2,NUBILNET                                                      
         A     R2,NACBTNET                                                      
         ST    R2,NACBTNET                                                      
         B     NEXTBEL                                                          
DOBILL18 MVI   NACBIFLG,1          ELEMENT EXISTS                               
         L     R2,NUBILGRS         INTEGRATION                                  
         A     R2,NACBIGRS                                                      
         ST    R2,NACBIGRS                                                      
         L     R2,NUBILNET                                                      
         A     R2,NACBINET                                                      
         ST    R2,NACBINET                                                      
         B     NEXTBEL                                                          
*                                                                               
DOPAY    L     R6,NACAPAYE         GET NEXT PAY ELEMENT                         
         MVI   SRCHEL,X'12'                                                     
NEXTPEL  BAS   RE,NEXTEL                                                        
         BNE   FACXIT              LAST ELEMENT                                 
         USING NUPAYD,R6                                                        
         ST    R6,NACAPAYE         NEW CURRENT ELEMENT                          
*                                                                               
         OC    NACSPDT(2),NACSPDT                                               
         BZ    PAY6                DATE FILTER                                  
         CLC   NACSPDT(2),NUPAYDAT                                              
         BH    NEXTPEL                                                          
PAY6     OC    NACSPDT+2(2),NACSPDT+2                                           
         BZ    PAY8                                                             
         CLC   NACSPDT+2(2),NUPAYDAT                                            
         BL    NEXTPEL                                                          
PAY8     CLI   NACSMOD,C'A'        FOR AUDIT TRAIL                              
         BNE   PAY16                                                            
         CLI   NUPAYTYP,C'T'        TIME                                        
         BNE   PAY10                                                            
         MVC   NACPTDAT(21),NUPAYDAT                                            
         MVI   NACPTFLG,1          ELEMENT EXISTS                               
         BAS   RE,NEXTEL           IF CORRESPONDING INT, DO IT TOO              
         BNE   PAY11                                                            
         CLI   NUPAYTYP,C'I'                                                    
         BNE   PAY11                                                            
         CLC   NACPTDAT,NUPAYDAT                                                
         BNE   PAY11                                                            
         ST    R6,NACAPAYE         GOT ONE                                      
         MVC   NACPIDAT(21),NUPAYDAT                                            
         MVI   NACPIFLG,1                                                       
         B     PAY11                                                            
PAY10    MVC   NACPIDAT(21),NUPAYDAT                                            
         MVI   NACPIFLG,1          ELEMENT EXISTS                               
         BAS   RE,NEXTEL           IF CORRESPONDING TIME, DO IT TOO             
         BNE   PAY11                                                            
         CLI   NUPAYTYP,C'T'                                                    
         BNE   PAY11                                                            
         CLC   NACPIDAT,NUPAYDAT                                                
         BNE   PAY11                                                            
         ST    R6,NACAPAYE         GOT ONE                                      
         MVC   NACPTDAT(21),NUPAYDAT                                            
         MVI   NACPTFLG,1                                                       
         B     PAY11                                                            
PAY11    B     FACXIT                                                           
*                                                                               
PAY16    CLI   NUPAYTYP,C'T'       FOR NON-AUDIT TRAIL, DO SUMS                 
         BNE   PAY18                                                            
         MVI   NACPTFLG,1          ELEMENT EXISTS                               
         L     R2,NUPAYGRS                                                      
         A     R2,NACPTGRS                                                      
         ST    R2,NACPTGRS                                                      
         L     R2,NUPAYNET                                                      
         A     R2,NACPTNET                                                      
         ST    R2,NACPTNET                                                      
         B     NEXTPEL                                                          
PAY18    MVI   NACPIFLG,1          ELEMENT EXISTS                               
         L     R2,NUPAYGRS         INTEGRATION                                  
         A     R2,NACPIGRS                                                      
         ST    R2,NACPIGRS                                                      
         L     R2,NUPAYNET                                                      
         A     R2,NACPINET                                                      
         ST    R2,NACPINET                                                      
         B     NEXTPEL                                                          
         DROP  R6                                                               
*                                                                               
FACXIT   CLI   NACPTFLG,0          IF ANY FLAGS SET, DONT REJECT                
         BNZ   FAC10                                                            
         CLI   NACPIFLG,0                                                       
         BNZ   FAC10                                                            
         CLI   NACBTFLG,0                                                       
         BNZ   FAC10                                                            
         CLI   NACBIFLG,0                                                       
         BNZ   FAC10                                                            
*                                  NOW CK FILTERS                               
         OC    NACSPDT,NACSPDT     IF ANY FILTER, THEN REJECT                   
         BNZ   FAC8                                                             
         OC    NACSBDT,NACSBDT                                                  
         BNZ   FAC8                                                             
         OC    NACSBTY,NACSBTY                                                  
         BNZ   FAC8                                                             
         B     FAC12                                                            
FACREJ   EQU   *                   REJECT                                       
FAC8     MVI   NACMODE,NACREJ                                                   
         B     XIT                                                              
*                                                                               
FAC10    MVI   NACMODE,NACBP       NACBP IF AUDIT TRAIL, ELSE NACEND            
         CLI   NACSMOD,C'A'                                                     
         BNE   FAC12                                                            
         B     XIT                                                              
FAC12    MVI   NACMODE,NACEND      ALWAYS END UNLESS AUDIT TRAIL                
         B     XIT                                                              
*                                                                               
********************************************************                        
         EJECT                                                                  
********************************************************                        
* ADDINTS      ROUTINE TO ADD INTERNAL RECORDS                                  
*      ADDS INTERNAL RECORD IN ARG2 TO INTERNAL RECORD IN ARG1                  
*        INPUTS :ARG1 (R4) - A(INTERNAL RECORD TO ADD INTO)                     
*                ARG2 (R5) - A(INTERNAL RECORD TO ADD FROM)                     
*                                                                               
ADDINTS  NTR1                                                                   
         L     R4,0(R1)                                                         
         L     R5,4(R1)                                                         
*                                                                               
         USING RWINBLOK,R6                                                      
         L     R6,ADRIVE           GET A(DRIVE TABLE)                           
ADDI2    CLI   RWINTYPE,0                                                       
         BE    ADDIXIT                                                          
*                                                                               
         ST    R5,RWAIN                                                         
         ST    R4,RWAOUT                                                        
         MVI   RWFUNC,RWADD                                                     
         GOTO1 VNEWRW,DMCB,NETSYSD,(R6)                                         
*                                                                               
ADDI4    ZIC   R2,RWOUTLEN                                                      
         LA    R5,0(R5,R2)                                                      
         LA    R4,0(R4,R2)                                                      
         LA    R6,RWBLKLEN(R6)      BUMP TO NEXT DRIVE                          
         B     ADDI2                                                            
ADDIXIT  B     XIT                                                              
         DROP  R6                                                               
*********************************************************                       
         EJECT                                                                  
********************************************************                        
*              HEADLINE ROUTINES                                                
*                                                                               
HOOK     NTR1                                                                   
*                                                                               
         L     R6,ATWA                                                          
         USING T31EFFD,R6                                                       
*                                                                               
         LA    R5,HEAD1            BASE ADDRESS FOR OFFSETS                     
         USING PHEAD,R5                                                         
         MVC   PHCLI,SPLCLI                                                     
         MVC   PHCLNM,SPLCLIN                                                   
         MVC   PHPRD,SPLPRO                                                     
         MVC   PHPRNM,SPLPRON                                                   
         EDIT  NBSELEST,(4,PHEST),ALIGN=LEFT                                    
         MVC   PHESNM,SPLESTN                                                   
         MVC   PHNET,=C'ALL '                                                   
         OC    NBSELNET,NBSELNET                                                
         BZ    HH2                                                              
         MVC   PHNET,NBACTNET                                                   
HH2      CLI   NBSELPAK,0          IF ALL PACKAGES DONT PRINT                   
         BE    HH4                                                              
         MVC   PHPAKH(10),=C'PACKAGE - '                                        
         EDIT  NBPACK,(4,PHPAK),ALIGN=LEFT                                      
         MVC   PHPKNM(L'PHPKNM),SPLPAKN                                         
HH4      MVC   PHDP(3),=C'ALL'                                                  
         CLI   NBSELDP,0                                                        
         BE    HH6                                                              
         MVC   PHDP,SPLDPTN                                                     
HH6      EQU   *                                                                
         CLI   NBSELFLT,0                                                       
         BE    HH10                                                             
         CLI   NBSELFLT,1                                                       
         BNE   HH7                                                              
         MVC   PHFILT,=CL11'(CLEARED)'                                          
HH7      CLI   NBSELFLT,2                                                       
         BNE   HH8                                                              
         MVC   PHFILT,=CL11'(UNCLEARED)'                                        
HH8      CLI   NBSELFLT,3                                                       
         BNE   HH9                                                              
         MVC   PHFILT,=CL11'(BILLED)'                                           
HH9      CLI   NBSELFLT,4                                                       
         BNE   HH10                                                             
         MVC   PHFILT,=CL11'(BILLABLE)'                                         
HH10     EQU   *                                                                
         LA    R4,PHBPLINE         A(PRINT LINE)                                
         OC    NACSBDT,NACSBDT     PRINT BILL LINE IF DATES GIVEN               
         BZ    HH20                                                             
         MVC   0(L'PHBPLINE,R4),SPACES    CLEAR LINE                            
         MVC   0(6,R4),=C'BILLED'                                               
         OC    NACSBDT(2),NACSBDT         IF START DATE                         
         BZ    HH14                                                             
         OC    NACSBDT+2(2),NACSBDT+2     ....IF END DATE                       
         BZ    HH12                                                             
         GOTO1 DATCON,DMCB,(2,NACSBDT),(5,8(R4))                                
         MVI   17(R4),C'-'                                                      
         GOTO1 DATCON,DMCB,(2,NACSBDT+2),(5,19(R4))                             
         B     HH16                       ....ELSE                              
HH12     MVC   8(5,R4),=C'AFTER'                                                
         GOTO1 DATCON,DMCB,(2,NACSBDT),(5,14(R4))                               
         B     HH16                       ....FI                                
HH14     MVC   8(6,R4),=C'BEFORE'         ELSE (END, NO START)                  
         GOTO1 DATCON,DMCB,(2,NACSBDT+2),(5,15(R4))                             
HH16     EQU   *                          FI                                    
         LA    R4,PHBPLIN2                                                      
*                                                                               
HH20     OC    NACSPDT,NACSPDT     PRINT PAY LINE IF DATES GIVEN                
         BZ    HH30                                                             
         MVC   0(L'PHBPLINE,R4),SPACES    CLEAR LINE                            
         MVC   0(4,R4),=C'PAID'                                                 
         OC    NACSPDT(2),NACSPDT         IF START DATE                         
         BZ    HH24                                                             
         OC    NACSPDT+2(2),NACSPDT+2     ....IF END DATE                       
         BZ    HH22                                                             
         GOTO1 DATCON,DMCB,(2,NACSPDT),(5,6(R4))                                
         MVI   15(R4),C'-'                                                      
         GOTO1 DATCON,DMCB,(2,NACSPDT+2),(5,17(R4))                             
         B     HH26                       ....ELSE                              
HH22     MVC   6(5,R4),=C'AFTER'                                                
         GOTO1 DATCON,DMCB,(2,NACSPDT),(5,14(R4))                               
         B     HH26                       ....FI                                
HH24     MVC   6(6,R4),=C'BEFORE'         ELSE (END, NO START)                  
         GOTO1 DATCON,DMCB,(2,NACSPDT+2),(5,13(R4))                             
HH26     EQU   *                          FI                                    
HH30     EQU   *                                                                
         DROP  R5                                                               
*                                                                               
* COLUMN HEADERS                                                                
*                                                                               
         USING RWINBLOK,R5                                                      
         L     R5,ADRIVE           GET A(DRIVE TABLE)                           
         LA    R4,H9               GET HEAD LINE                                
HK2      CLI   RWINTYPE,0                                                       
         BE    HKXIT                                                            
         MVI   RWHEDILN,HEDLEN                                                  
         ST    R4,RWAHEDOU                                                      
         MVI   RWFUNC,RWHEAD                                                    
         CLI   RWHEDTYP,0          DEFAULT TO ALPHA                             
         BNE   HK3                                                              
         MVI   RWHEDTYP,NRHALPH                                                 
HK3      GOTO1 VNEWRW,DMCB,NETSYSD,(R5)                                         
         ZIC   R2,RWPRLEN                                                       
         EXCLC R2,132(R4),SPACES                                                
         BNE   HK4                 UNDERLINE IF OTHING THERE ALREADY            
         GOTO1 UNDERLIN,DMCB,(RWPRLEN,0(R4)),132(R4)                            
HK4      ZIC   R2,RWPRLEN          BUMP PRINT POSITION + 1 SPACE                
         LA    R4,1(R4,R2)                                                      
         LA    R5,RWBLKLEN(R5)      BUMP TO NEXT DRIVE                          
         B     HK2                                                              
HKXIT    B     XIT                                                              
         DROP  R5                                                               
         DROP  R6                                                               
*********************************************************                       
         EJECT                                                                  
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,NBDTADSP,SRCHEL                                               
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                                                               
ZERO     DC    XL1'0F'             PACKED ZERO                                  
         SPACE 2                                                                
         EJECT                                                                  
HEDLEN   EQU   10                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR W/S                                                    
         SPACE 3                                                                
       ++INCLUDE NEACCWS                                                        
         EJECT                                                                  
PHEAD    DSECT                     DSECT FOR HEADERS                            
*                                  MUST MATCH SSPEC                             
PHLENGTH EQU   132                                                              
*                                                                               
         ORG   PHEAD+3*PHLENGTH+11                                              
PHCLI    DS    CL3                 CLIENT ABBR                                  
         DS    CL1                                                              
PHCLNM   DS    CL20                CLIENT NAME                                  
         ORG   PHEAD+4*PHLENGTH+11                                              
PHPRD    DS    CL3                 PRODUCT ABBR                                 
         DS    CL1                                                              
PHPRNM   DS    CL20                PRODUCT NAME                                 
         ORG   PHEAD+4*PHLENGTH+40                                              
PHPAKH   DS    CL10                C'PACKAGE - '                                
PHPAK    DS    CL3                 PACKAGE ABBR                                 
         ORG   PHEAD+4*PHLENGTH+86                                              
PHNET    DS    CL4                 NETWORK                                      
         ORG   PHEAD+5*PHLENGTH+11                                              
PHEST    DS    CL1                 ESTIMATE ABBR                                
         DS    CL3                                                              
PHESNM   DS    CL24                ESTIMATE NAME                                
         DS    CL1                                                              
PHPKNM   DS    CL27                PACKAGE NAME                                 
         ORG   PHEAD+5*PHLENGTH+50                                              
PHFILT   DS    CL11                                                             
         ORG   PHEAD+5*PHLENGTH+86                                              
PHDP     DS    CL8                 DAYPART                                      
         ORG   PHEAD+6*PHLENGTH+40                                              
PHBPLINE DS    CL30                BILLED OR PAID DATE FILTER LINE              
         ORG   PHEAD+7*PHLENGTH+40                                              
PHBPLIN2 DS    CL30                BILLED OR PAID DATE FILTER LINE              
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFBD                                                       
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE NERWINCLS                                                      
       ++INCLUDE NERWEQUS                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEMED2B   05/01/02'                                      
         END                                                                    
