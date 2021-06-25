*          DATA SET SRDIP00    AT LEVEL 009 AS OF 08/03/20                      
*PHASE T14600A                                                                  
*INCLUDE SQUASHER                                                               
         TITLE '$DIP - DISPLAY PRINT QUEUE REPORT'                              
         PRINT NOGEN                                                            
DIP      CSECT                                                                  
         NMOD1 DIPWORKX-DIPWORK,**$DIP**,R9,CLEAR=YES                           
         USING DIPWORK,RC          RC=A(W/S)                                    
         ST    R1,APARM                                                         
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(PARMS)                                  
         L     R8,SRPARM1                                                       
         L     R8,VSSB-SYSFACD(R8)                                              
         MVC   RECLEN,SSBTWAL-SSBD(R8)                                          
         L     R8,SRPARM2                                                       
         USING SRSD,R8             R8=A(TIA - USED FOR SR SAVE PAGE)            
         L     RA,SRPARM6                                                       
         USING SRDIPFFD,RA         RA=A(TWA)                                    
         L     R5,SRPARM3                                                       
         USING UTLD,R5             R5=A(UTL)                                    
         ST    R5,AUTL                                                          
*                                                                               
         MVI   DDS,X'00'           INITIALISE TERMINAL FLAG                     
         TM    TSTAT,X'60'                                                      
         BZ    *+8                                                              
         MVI   DDS,X'01'           SET DDS TERMINAL                             
*                                                                               
         MVC   REPUSER,TUSER       SET USER ID FROM $CT VALUE                   
         LA    R4,SRVIDH                                                        
         CLC   SRVID+1(6),=C'DIP,DL'                                            
         BNE   *+8                                                              
         OI    DDS,X'80'           SET DOWN LOAD FLAG                           
         CLC   SRVID+1(4),=C'DIPP'                                              
         BNE   *+10                                                             
         MVC   REPUSER,PUBLICID    SET USER ID TO PUBLIC VALUE                  
         OC    REPUSER,REPUSER                                                  
         BZ    ERR0                ERROR IF NO USER ID AVAILABLE                
         MVC   TRMNUM,TNUM                                                      
*                                                                               
         L     R5,SRPARM4                                                       
         USING COMFACSD,R5                                                      
         MVC   ASCANNER,CSCANNER                                                
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ATERMVAL,CTERMVAL                                                
*                                                                               
         XC    DMCB(4),DMCB        GET TODAYS DATE                              
         GOTO1 CGETFACT,DMCB                                                    
         L     R1,0(R1)                                                         
         MVC   DUB,4(R1)                                                        
         GOTO1 CDATCON,DMCB,(4,DUB),(0,DATE)                                    
         GOTO1 (RF),(R1),,(30,DATEN)                                            
         MVC   DATEO,DATEN                                                      
         OI    DATEO,X'80'         OLD CMPRSD DATE                              
         MVC   DATEC,DATEN         SET DATE TO NEW CMPRSD                       
*                                                                               
         XC    MSG,MSG                                                          
         NI    SRVSPIDH+6,X'BF'                                                 
         MVI   SRVFLAG,C'0'        SET FLAG FOR DOWNLOAD                        
*                                                                               
         L     R5,=A(CIREC-DIPWORK)                                             
         LA    R5,DIPWORK(R5)                                                   
         ST    R5,ACIREC                                                        
*                                                                               
         XC    NDX,NDX             FIND PRTQ ID FOR USER REPORT                 
         MVC   NDX+UKSRCID-UKINDEX(2),REPUSER                                   
         GOTO1 ADATAMGR,DMCB,(X'00',GFILE),PRTQUE,NDX,SAVE,(R5)                 
         MVC   PRTQID,NDX+UKUSRINF-UKINDEX                                      
         EJECT                                                                  
P1VAL    LA    R4,SRVSPIDH         VALIDATE SPOOL ID                            
         CLI   5(R4),0                                                          
         BE    ERR1                                                             
         GOTO1 ASCANNER,DMCB,(R4),(2,TEMP)                                      
         CLI   4(R1),2                                                          
         BNE   ERR2                                                             
         LA    R6,TEMP                                                          
         CLI   1(R6),0             FIRST HALF IS 2 OR 3 SUBID                   
         BNE   ERR2                                                             
         CLI   0(R6),3                                                          
         BH    ERR2                                                             
         CLI   0(R6),2                                                          
         BL    ERR2                                                             
         MVC   REPSUBID,12(R6)                                                  
*                                                                               
         LA    RE,REPSUBID         MASSAGE SUBID                                
         LA    RF,3                                                             
         CLI   0(RE),C'A'                                                       
         BNL   *+8                                                              
         MVI   0(RE),C'.'                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,*-16                                                          
*                                                                               
         LA    R6,32(R6)                                                        
         CLI   1(R6),0             SECOND HALF IS SEQUENCE NUMBER               
         BNE   ERR2                                                             
         TM    2(R6),X'80'                                                      
         BZ    ERR2                                                             
         OC    4(4,R6),4(R6)                                                    
         BZ    ERR2                                                             
         CLC   4(4,R6),MAXSEQ                                                   
         BH    ERR2                                                             
         MVC   REPSEQL,6(R6)                                                    
         B     P2VAL                                                            
         EJECT                                                                  
P2VAL    LA    R4,SRVPAGEH         VALIDATE PAGE NUMBER                         
         MVC   SPAGE,=F'1'                                                      
         CLI   5(R4),0             DEFAULT TO PAGE 1 IF NOT INPUT               
         BE    P2VALX                                                           
         TM    4(R4),X'08'                                                      
         BO    P2VAL2                                                           
*                                                                               
         CLI   8(R4),C'F'          ALLOW F FOR FIRST                            
         BNE   *+14                                                             
         MVC   SPAGE,=F'1'                                                      
         B     P2VALX                                                           
         CLI   8(R4),C'T'          ALLOW T FOR TOP                              
         BNE   *+14                                                             
         MVC   SPAGE,=F'1'                                                      
         B     P2VALX                                                           
         CLI   8(R4),C'L'          ALLOW L FOR LAST                             
         BNE   *+14                                                             
         MVC   SPAGE,=4X'FF'                                                    
         B     P2VALX                                                           
         CLI   8(R4),C'B'          ALLOW B FOR BOTTOM                           
         BNE   *+14                                                             
         MVC   SPAGE,=4X'FF'                                                    
         B     P2VALX                                                           
         B     P2VALX              ANY OTHER VALUE DEFAULTS TO PAGE 1           
*                                                                               
P2VAL2   ZIC   R1,5(R4)            NUMERIC PAGE NUMBER INPUT                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   R1,DUB                                                           
         ST    R1,DUB                                                           
         OC    DUB(4),DUB                                                       
         BZ    P2VALX                                                           
         MVC   SPAGE,DUB                                                        
*                                                                               
P2VALX   EQU   *                                                                
         EJECT                                                                  
P3VAL    LA    R4,SRVLINEH         VALIDATE LINE NUMBER                         
         MVC   SLINE,=F'1'                                                      
         CLI   5(R4),0             DEFAULT TO LINE 1 IF NOT INPUT               
         BE    P3VALX                                                           
         TM    4(R4),X'08'                                                      
         BO    P3VAL2                                                           
*                                                                               
         CLI   8(R4),C'F'          ALLOW F FOR FIRST                            
         BNE   *+14                                                             
         MVC   SLINE,=F'1'                                                      
         B     P3VALX                                                           
         CLI   8(R4),C'T'          ALLOW T FOR TOP                              
         BNE   *+14                                                             
         MVC   SLINE,=F'1'                                                      
         B     P3VALX                                                           
         CLI   8(R4),C'L'          ALLOW L FOR LAST                             
         BNE   *+14                                                             
         MVC   SLINE,=4X'FF'                                                    
         B     P3VALX                                                           
         CLI   8(R4),C'B'          ALLOW B FOR BOTTOM                           
         BNE   *+14                                                             
         MVC   SLINE,=4X'FF'                                                    
         B     P3VALX                                                           
         B     P3VALX              ANY OTHER VALUE DEFAULTS TO LINE 1           
*                                                                               
P3VAL2   ZIC   R1,5(R4)            NUMERIC LINE NUMBER INPUT                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   R1,DUB                                                           
         ST    R1,DUB                                                           
         OC    DUB(4),DUB                                                       
         BZ    P3VALX                                                           
         MVC   SLINE,DUB                                                        
*                                                                               
P3VALX   EQU   *                                                                
         EJECT                                                                  
P4VAL    LA    R4,SRVFORMH         VALIDATE DISPLAY FORMAT                      
         MVI   FMTLR,C'L'          SET LEFT JUSTIFIED                           
         MVI   FMTCP,C'C'          SET COMPACT FORMAT                           
         CLI   5(R4),0                                                          
         BE    P4VALX                                                           
*                                                                               
P4VAL1   MVC   DUB(1),8(R4)        FIRST CHR IS LINE POSITION                   
         CLI   DUB,C' '                                                         
         BE    P4VAL4                                                           
         CLI   DUB,C'L'            L FOR LEFT                                   
         BE    P4VAL3                                                           
         CLI   DUB,C'R'            R FOR RIGHT                                  
         BE    P4VAL3                                                           
         CLI   DUB,C'M'            M FOR MIDDLE                                 
         BE    P4VAL3                                                           
         CLI   DUB,C'C'            C FOR CENTRE                                 
         BNE   P4VAL4                                                           
         MVI   DUB,C'M'                                                         
P4VAL3   MVC   FMTLR,DUB           SET LEFT/RIGHT                               
*                                                                               
P4VAL4   MVC   DUB(1),9(R4)        SECOND CHR IS LINE SPACING                   
         CLI   DUB,C' '                                                         
         BE    P4VALX                                                           
         CLI   DUB,C'C'            C FOR COMPACT                                
         BE    P4VAL6                                                           
         CLI   DUB,C'N'            N FOR NOT                                    
         BE    P4VAL6                                                           
         CLI   DUB,C'X'            X FOR EXPANDED                               
         BE    P4VAL6                                                           
         B     P4VALX                                                           
P4VAL6   MVC   FMTCP,DUB           SET COMPACT/EXPANDED                         
*                                                                               
P4VALX   EQU   *                                                                
         EJECT                                                                  
SVDATA   L     R5,ACIREC           INITIALISE BUFFER                            
         USING PQRECD,R5                                                        
         GOTO1 ADATAMGR,DMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R5),0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEBUFF,0(R5)      SAVE RETURN DATA IN BUFFER                   
         L     R7,SBSAVE                                                        
         AR    R7,R5                                                            
         USING SKBUFFD,R7          R7=A(SAVE DATA AT END OF BUFFER)             
         MVC   MYLABEL,SKLABEL     SAVE PQ BUFFER LABEL FOR THIS TASK           
         MVI   LAST,0              SET LAST REPORT DATA FLAG                    
*                                                                               
SVD1     LA    RF,SRPAGENO         READ IN S/R SAVE TWA                         
         SLL   RF,32-8                                                          
         ICM   RF,3,TRMNUM                                                      
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(RF),(R8)                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SPQSV(SPQSVLN),SR$HELP                                           
*                                                                               
SVD2     CLC   SPQSRID,MYID        INITIALISE IF NOT MY SAVE DATA               
         BE    SVD3                                                             
         XC    SPQSV(SPQSVLN),SPQSV                                             
         MVC   SPQSRID,MYID                                                     
         B     NEWREP              NOTHING KNOWN - TREAT AS NEW REPORT          
*                                                                               
SVD3     CLC   SPQUSER,REPUSER     TEST IF SAME REPORT                          
         BNE   NEWREP                                                           
         CLC   SPQSUBID,REPSUBID                                                
         BNE   NEWREP                                                           
         CLC   SPQSEQL,REPSEQL                                                  
         BNE   NEWREP                                                           
*                                                                               
SVD4     MVC   SKBUFFD(SKEND-SKLABEL),SPQBUFF                                   
         MVC   SKLABEL,MYLABEL                                                  
         XC    SKFCTRL,SKFCTRL                                                  
         MVC   SKFSTCI,SPQBUFF+SKFSTCI-SKLABEL                                  
         XC    NDX,NDX                                                          
         MVC   NDX(L'SKINDEX),SKINDEX                                           
*                                                                               
SVD5     XC    SAVE(4),SAVE        READ HEADER RECORD FOR REPORT                
         MVC   SAVE+4(4),=C'PAGE'                                               
         GOTO1 ADATAMGR,DMCB,(X'00',RANDOM),PRTQID,NDX,SAVE,(R5),0              
         CLI   8(R1),0                                                          
         BNE   NEWREP                                                           
         CLC   SPQUSER,PQSRCID     TEST IF SAME REPORT AS BEFORE                
         BNE   NEWREP                                                           
         CLC   SPQSUBID,PQSUBID                                                 
         BNE   NEWREP                                                           
         CLC   SPQSEQL,PQREPNO                                                  
         BNE   NEWREP                                                           
         OI    LAST,X'01'          SET THIS REPORT SAME AS LAST SAVED           
         B     SCX4                                                             
         EJECT                                                                  
NEWREP   TM    DDS,X'01'           NEW REPORT SPECIFIED                         
         BO    NEWREPX                                                          
         TM    DDS,X'80'           TEST IF PC DOING DOWN LOAD                   
         BZ    NEWREPX                                                          
         LA    R1,DMCB                                                          
         XC    0(16,R1),0(R1)      SET TERMVAL PARAM LIST                       
         MVI   0(R1),X'30'         SET WANT TERM REC AND DEFN ELM               
         MVC   5(3,R1),AUTL+1                                                   
         GOTO1 ATERMVAL                                                         
         ICM   RE,7,13(R1)         GET A(TERMINAL DEFN ELEMENT)                 
         BZ    NEWREPX         ===>OK UNTIL ALL PC'S ARE AUTH'D (ERR8)          
         TM    CTTRMAT1-CTTRMD(RE),X'80'                                        
         BZ    NEWREPX         ===>NOT AUTHD FOR DOWN LOADING   (ERR8)          
NEWREPX  EQU   *                                                                
         EJECT                                                                  
SCHNDX   LA    R5,NDX              SEARCH PRTQUE INDEX FOR REPORT               
         USING UKRECD,R5                                                        
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKSRCID,REPUSER     SET REPORT ID IN USER INDEX                  
         MVC   UKSUBID,REPSUBID                                                 
         MVC   UKREPNO,REPSEQL                                                  
         L     RF,ACIREC                                                        
         LA    R4,SRVSPIDH         POINT TO REPORT ID INPUT FIELD               
*                                                                               
SCX1     GOTO1 ADATAMGR,DMCB,(X'08',INDEX),PRTQID,NDX,SAVE,(RF),0               
         CLI   8(R1),0                                                          
         BE    SCX2                                                             
         TM    8(R1),X'80'         EOF - REPORT NOT FOUND                       
         BO    ERR5                                                             
         DC    H'0'                ERR - DIE ON INDEX READ DISK ERROR           
*                                                                               
SCX2     L     R5,ACIREC           READ REPORT HEADER RECORD                    
         USING PQRECD,R5                                                        
         XC    SAVE(4),SAVE        RECORD ZERO DEFINES HEADER                   
         MVC   SAVE+4(4),=C'PAGE'                                               
         GOTO1 ADATAMGR,DMCB,(X'00',RANDOM),PRTQID,NDX,SAVE,(R5),0              
         CLI   8(R1),0                                                          
         BE    SCX3                                                             
         DC    H'0'                DIE IF CANT READ REPORT HEADER               
*                                                                               
SCX3     TM    PQATTB,PQATNP       IGNORE NON PRINTABLE REPORTS                 
         BO    SCX3A                                                            
         TM    PQATTB,PQATJOBI     IGNORE REPORTS WITH JCL IN THEM              
         BZ    SCX3B                                                            
SCX3A    TM    DDS,X'01'           BUT DDS TERMINALS CAN ACCESS                 
         BZ    ERR5                                                             
*                                                                               
SCX3B    TM    PQATTB,PQATPW       TEST IF PASSWORD PROTECTED                   
         BNO   SCX3C                                                            
         TM    DDS,X'80'           TEST DOWNLOAD                                
         BO    SCX3C                                                            
         B     ERR5                SET REPORT NOT FOUND                         
*                                                                               
SCX3C    MVC   HALF,DATEN          SET HALF TO NEW/OLD CMPRSD DATE              
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+10                                                             
         MVC   HALF,DATEO                                                       
         CLC   PQAGELD,HALF        TEST IF FUTURE DATED REPORT                  
         BNH   SCX3D               NO                                           
         TM    DDS,X'01'           YES ONLY DDS CAN SEE                         
         BZ    ERR5                SET REPORT NOT FOUND                         
*                                                                               
SCX3D    EQU   *                   REPORT HAS PASSED ALL TESTS                  
         EJECT                                                                  
SCX4     XC    LINEDATA,LINEDATA   CLEAR REPORT LINE DATA                       
         MVI   SAVE,C' '           CLEAR PRINT LINE TO SPACES                   
         MVC   SAVE+1(255),SAVE                                                 
         SR    RF,RF                                                            
         ICM   RF,3,PQPAGES                                                     
         ST    RF,MAXPAGE          SAVE MAXIMUM PAGE NUMBER                     
         ICM   RF,7,PQLINES                                                     
         ST    RF,MAXLINE          SAVE MAXIMUM LINE NUMBER                     
         MVC   MAXCPL,PQMAXCPL     SAVE MAXIMUM CHRS PER LINE                   
         CLI   MAXCPL,0                                                         
         BNE   *+8                                                              
         MVI   MAXCPL,132                                                       
SCX5     TM    PQLINET,PQLTCC      TEST IF REPORT HAS CC CHRS                   
         BZ    SCX5B                                                            
*                                                                               
SCX5A    LA    RF,SAVE             SET A(CC CHR) FOR REPORT WITH PAGES          
         ST    RF,LACC                                                          
         LA    RF,SAVE+1           SET A(DATA)                                  
         ST    RF,LADATA                                                        
         MVC   LLENMIN,=H'1'       SET MINIMUM LEN                              
         MVC   LLENMAX+1(1),PQLINEW                                             
         TM    PQLINET,PQLTFL      TEST IF F/L DATA                             
         BO    SCX6                                                             
         LA    RF,SAVE+2           SET A(CC CHR)                                
         ST    RF,LACC                                                          
         LA    RF,SAVE+3           SET A(DATA)                                  
         ST    RF,LADATA                                                        
         MVC   LLENMIN,=H'3'       SET MINIMUM LEN                              
         XC    LLENMAX,LLENMAX                                                  
         B     SCX6                                                             
*                                                                               
SCX5B    LA    RF,SAVE             SET A(DATA) FOR REPORT WITH LINES            
         ST    RF,LADATA                                                        
         MVC   LLENMIN,=H'0'       SET MINIMUM LEN                              
         MVC   LLENMAX+1(1),PQLINEW                                             
         TM    PQLINET,PQLTFL      TEST IF F/L DATA                             
         BO    SCX6                                                             
         LA    RF,SAVE+2           SET A(DATA)                                  
         ST    RF,LADATA                                                        
         MVC   LLENMIN,=H'2'       SET MINIMUM LEN                              
         XC    LLENMAX,LLENMAX                                                  
*                                                                               
SCX6     LA    R4,SRVPAGEH         REPORT HAS PAGES                             
         OC    LACC,LACC                                                        
         BZ    SCX8                                                             
         CLC   SPAGE,MAXPAGE       CHECK SPAGE DOES NOT EXCEED MAX              
         BNH   SCX6A                                                            
         CLC   SPAGE,=4X'FF'       SET TO MAX IF LAST SPECIFIED                 
         BNE   ERR7                                                             
         MVC   SPAGE,MAXPAGE                                                    
*                                                                               
SCX6A    TM    LAST,X'01'          TEST IF SAME REPORT AS LAST                  
         BZ    SCX6B                                                            
         CLC   SPAGE,SPQPAGE       TEST IF SAME PAGE/LINE AS LAST               
         BNE   SCX6B                                                            
         CLC   SLINE,SPQLINE                                                    
         BNE   SCX6B                                                            
         MVC   SKBUFFD(SKEND-SKLABEL),SPQBUFF                                   
         MVC   SKLABEL,MYLABEL                                                  
         MVC   FULL,SKADDR         READ LAST BLOCK INTO BUFFER                  
         OC    FULL,FULL                                                        
         BZ    SCX6B                                                            
         GOTO1 ADATAMGR,DMCB,(X'01',DMREAD),PRTQID,FULL,(R5)                    
         CLI   8(R1),0                                                          
         BNE   SCX6B                                                            
         CLC   SPQUSER,PQSRCID     CHECK REPORT ID CHECKS OUT                   
         BNE   SCX6B                                                            
         CLC   SPQSUBID,PQSUBID                                                 
         BNE   SCX6B                                                            
         CLC   SPQSEQL,PQREPNO                                                  
         BNE   SCX6B                                                            
         MVC   LLEN,SKLEN                                                       
         OI    LAST,X'02'          SET SAME REPORT/PAGE/LINE AS LAST            
         B     DSPLINES                                                         
*                                                                               
SCX6B    XC    SAVE(12),SAVE       SET PAGE NUMBER REQUIRED                     
         MVC   SAVE+0(4),SPAGE                                                  
         MVC   SAVE+4(4),=C'PAGE'                                               
         MVC   SAVE+8(4),=F'0'     SET LINE NUMBER REQUIRED                     
         GOTO1 ADATAMGR,DMCB,(X'01',RANDOM),PRTQID,NDX,SAVE,(R5),0              
         MVC   LLEN,22(R1)                                                      
         CLI   8(R1),0                                                          
         BE    DSPLINES                                                         
         TM    8(R1),X'40'                                                      
         BZ    ERR7                INVALID PAGE IF EOF/NOTFOUND                 
         DC    H'0'                DIE IF DISK/FORMAT ERROR                     
*                                                                               
SCX8     LA    R4,SRVLINEH         REPORT HAS LINES ONLY                        
         CLC   SLINE,MAXLINE       CHECK SLINE DOES NOT EXCEED MAX              
         BNH   SCX8A                                                            
         CLC   SLINE,=4X'FF'       SET TO MAX IF LAST SPECIFIED                 
         BNE   ERR7                                                             
         MVC   SLINE,MAXLINE                                                    
*                                                                               
SCX8A    TM    LAST,X'01'          TEST IF SAME REPORT AS LAST                  
         BZ    SCX8B                                                            
         CLC   SLINE,SPQLINE       TEST IF SAME LINE AS LAST                    
         BNE   SCX8B                                                            
         MVC   SKBUFFD(SKEND-SKLABEL),SPQBUFF                                   
         MVC   SKLABEL,MYLABEL                                                  
         MVC   FULL,SKADDR         READ LAST BLOCK INTO BUFFER                  
         OC    FULL,FULL                                                        
         BZ    SCX8B                                                            
         GOTO1 ADATAMGR,DMCB,(X'01',DMREAD),PRTQID,FULL,(R5)                    
         CLI   8(R1),0                                                          
         BNE   SCX8B                                                            
         CLC   SPQUSER,PQSRCID     CHECK REPORT ID CHECKS OUT                   
         BNE   SCX8B                                                            
         CLC   SPQSUBID,PQSUBID                                                 
         BNE   SCX8B                                                            
         CLC   SPQSEQL,PQREPNO                                                  
         BNE   SCX8B                                                            
         MVC   LLEN,SKLEN                                                       
         OI    LAST,X'02'          SET SAME REPORT/LINE AS LAST                 
         B     DSPLINES                                                         
*                                                                               
SCX8B    XC    SAVE(12),SAVE       SET LINE NUMBER REQUIRED                     
         MVC   SAVE+0(4),SLINE                                                  
         MVC   SAVE+4(4),=C'LINE'                                               
         GOTO1 ADATAMGR,DMCB,(X'01',RANDOM),PRTQID,NDX,SAVE,(R5),0              
         MVC   LLEN,22(R1)                                                      
         CLI   8(R1),0                                                          
         BE    DSPLINES                                                         
         DC    H'0'                DIE IF CANT FIND LINE NUMBER                 
         EJECT                                                                  
DSPLINES XC    NLINES,NLINES       CLEAR LINE COUNTER                           
         MVI   FLAG,0              SET LINE NOT FOUND                           
         LA    R6,SRVL1H                                                        
         ZAP   QO,=P'0'            SET NUMBER OF LINES DISPLAYED                
         ZAP   QT,=P'0'                                                         
         TM    LAST,X'03'          TEST IF SAME REPORT/PAGE/LINE                
         BO    DSPL2                                                            
         OC    LACC,LACC           MUST READ FIRST LINE OF PAGE                 
         BZ    DSPL4                                                            
*                                                                               
DSPL2    GOTO1 ADATAMGR,DMCB,(X'01',READ),PRTQID,NDX,SAVE,(R5),0                
         MVC   LLEN,22(R1)                                                      
         CLI   8(R1),0                                                          
         BE    DSPL4                                                            
         TM    8(R1),X'80'         TEST EOF                                     
         BZ    DSPL2A                                                           
         MVC   LLEN,=X'FFFF'       SET END OF REPORT                            
         OI    FLAG,X'02'                                                       
         B     DSPLF                                                            
DSPL2A   DC    H'0'                DIE ON DISK ERROR                            
*                                                                               
DSPL4    L     RF,LACC             TEST IF REPORT HAS PAGES                     
         LTR   RF,RF               RF=A(CC CHR)                                 
         BZ    DSPL5                                                            
         CLI   0(RF),X'89'                                                      
         BNL   DSPL8                                                            
         TM    FLAG,X'01'                                                       
         BO    DSPLA               LINE HAS BEEN FOUND                          
         CLC   LLEN,LLENMIN                                                     
         BE    DSPL2                                                            
         L     RE,NLINES           COUNT NON BLANK DATA LINES                   
         LA    RE,1(RE)                                                         
         ST    RE,NLINES                                                        
         TM    LAST,X'03'          TEST IF SAME LAST PAGE/LINE                  
         BNO   DSPL4A                                                           
         MVI   LAST,X'80'          SET TO SHOW VIA LAST DIRECT                  
         L     RE,SLINE                                                         
         ST    RE,NLINES                                                        
DSPL4A   C     RE,SLINE            TEST REQUIRED START LINE                     
         BNE   DSPL2                                                            
         OI    FLAG,X'01'          SET LINE WITHIN PAGE FOUND                   
         B     DSPLA                                                            
*                                                                               
DSPL5    OI    FLAG,X'01'          SET LINE FOUND                               
         MVI   LAST,X'80'          SET TO SHOW VIA LAST DIRECT                  
         B     DSPLA                                                            
*                                                                               
DSPL8    OI    FLAG,X'02'          SET END OF PAGE PENDING FLAG                 
         TM    FLAG,X'01'                                                       
         BZ    DSPL8A                                                           
         CLC   LLEN,LLENMIN                                                     
         BH    DSPLA               LAST LINE HAS DATA                           
         B     DSPLF                                                            
DSPL8A   CLC   LLEN,LLENMIN                                                     
         BNH   DSPLF                                                            
         L     RE,NLINES           CHECK LAST LINE                              
         LA    RE,1(RE)                                                         
         C     RE,SLINE                                                         
         BNE   DSPLF               START LINE = LAST LINE OF PAGE               
         OI    FLAG,X'01'                                                       
*                                                                               
DSPLA    L     RF,LADATA           MOVE DATA TO DISPLAY LINE                    
         LH    RE,LLEN                                                          
         SH    RE,=H'2'                                                         
         TM    PQLINET,PQLTFL      TEST IF FIXED LENGTH DATA                    
         BZ    DSPLA1                                                           
         SR    RE,RE                                                            
         IC    RE,PQLINEW          USE DEFINED CPL FOR LEFT                     
         CLI   FMTLR,C'L'                                                       
         BE    DSPLA1                                                           
         IC    RE,MAXCPL           USE MAX CPL FOR RIGHT/CENTRE                 
DSPLA1   OC    LACC,LACC           ADJUST LENGTH FOR CC CHR                     
         BNZ   *+6                                                              
         BCTR  RE,0                                                             
DSPLA2   CLI   FMTLR,C'L'          LEFT JUSTIFIED                               
         BNE   DSPLA3                                                           
         MVC   8(79,R6),0(RF)      DISPLAY FIRST 79 CHRS                        
         B     DSPLB                                                            
DSPLA3   CLI   FMTLR,C'R'          RIGHT JUSTIFIED                              
         BNE   DSPLA4                                                           
         SH    RE,=H'79'           DATA LENGTH MINUS DISPLAY WIDTH              
         BM    DSPLC                                                            
         AR    RF,RE                                                            
         MVC   8(79,R6),0(RF)      DISPLAY LAST 79 CHRS                         
         B     DSPLB                                                            
DSPLA4   CLI   FMTLR,C'M'          CENTRE JUSTIFIED                             
         BNE   DSPLA5                                                           
         SRL   RE,1                HALF THE DATA LENGTH                         
         SH    RE,=H'38'           MINUS HALF THE DISPLAY WIDTH                 
         BM    DSPLC                                                            
         AR    RF,RE                                                            
         MVC   8(79,R6),0(RF)      DISPLAY MIDDLE 79 CHRS                       
         B     DSPLB                                                            
DSPLA5   EQU   *                                                                
*                                                                               
DSPLB    TR    8(79,R6),VALOCHRS   TRANSLATE TO SCREEN DISPLAYABLE              
*                                                                               
DSPLC    CLI   FMTCP,C'C'          COMPACT FORMAT                               
         BNE   DSPLD                                                            
         CLC   LLEN,LLENMIN        IGNORE BLANK LINES                           
         BE    DSPLE                                                            
         AP    QO,=P'1'            BUMP TO NEXT DISPLAY LINE                    
         LA    R6,87(R6)                                                        
         AP    QT,=P'1'            BUMP NON BLANK LINE COUNT                    
         B     DSPLE                                                            
*                                                                               
DSPLD    AP    QO,=P'1'            NON COMPACT FORMAT                           
         LA    R6,87(R6)                                                        
         CLC   LLEN,LLENMIN                                                     
         BE    *+10                                                             
         AP    QT,=P'1'            BUMP NON BLANK LINE COUNT                    
         LA    RE,1                                                             
         L     RF,LACC                                                          
         LTR   RF,RF               TEST IF LINE HAS CC CHR                      
         BZ    DSPLD2                                                           
         IC    RE,0(RF)            CC=B...LL.I.' WHERE LL IS LINE SKIP          
         SLL   RE,27                                                            
         SRL   RE,30                                                            
         TM    0(RF),X'02'         SUBTRACT ONE IF IMMEADIATE                   
         BZ    *+6                                                              
         BCTR  RE,0                                                             
DSPLD2   SH    RE,=H'1'            RE=NUM OF EXTRA BLANK LINES                  
         BNP   DSPLE                                                            
         AP    QO,=P'1'                                                         
         LA    R6,87(R6)                                                        
         BCT   RE,*-10                                                          
         B     DSPLE                                                            
*                                                                               
DSPLE    TM    FLAG,X'02'          LINE WAS DISPLAYED                           
         BO    DSPLF               BUT WAS LAST                                 
         CP    QO,DSPMAX           CHECK FOR MORE ROOM                          
         BL    DSPL2               YES                                          
         B     DSPLX               NO                                           
*                                                                               
DSPLF    CP    QT,=P'0'            END OF PAGE OR END OF REPORT                 
         BNE   DSPLX                                                            
         CLC   LLEN,=X'FFFF'                                                    
         BE    DSPLF2                                                           
         OC    LACC,LACC           TEST IF REPORT HAS PAGES                     
         BZ    DSPLF2                                                           
         L     RE,SPAGE            IF NO LINES FOUND TRY NEXT PAGE              
         LA    RE,1(RE)                                                         
         ST    RE,SPAGE                                                         
         LA    RE,1                                                             
         ST    RE,SLINE                                                         
         XC    NLINES,NLINES       CLEAR LINE COUNTER                           
         MVI   FLAG,0              SET LINE NOT FOUND                           
         LA    R6,SRVL1H                                                        
         ZAP   QO,=P'0'            SET NUMBER OF LINES DISPLAYED                
         ZAP   QT,=P'0'                                                         
         B     DSPL2               BACK TO READ FIRST LINE OF NEXT              
DSPLF2   LA    R4,SRVSPIDH                                                      
         XC    MSG,MSG                                                          
         B     ERR6                                                             
*                                                                               
DSPLX    OC    LACC,LACC           TEST IF REPORT HAS PAGES                     
         BZ    DSPLX1                                                           
         MVC   MSG(41),=C'PAGE NNNN - LINES NNNNN THRU NNNNN LISTED'            
         MVC   MSG+41(19),DQUMSG                                                
         L     R0,SPAGE                                                         
         EDIT  (R0),(4,MSG+5)                                                   
         L     R0,SLINE                                                         
         EDIT  (R0),(5,MSG+18)                                                  
         ZAP   DUB,QT                                                           
         CVB   R0,DUB                                                           
         A     R0,SLINE                                                         
         BCTR  R0,0                                                             
         EDIT  (R0),(5,MSG+29)                                                  
         B     DSPLX2                                                           
*                                                                               
DSPLX1   MVC   MSG(31),=C'LINES NNNNNN THRU NNNNNN LISTED'                      
         MVC   MSG+31(19),DQUMSG                                                
         L     R0,SLINE                                                         
         EDIT  (R0),(6,MSG+6)                                                   
         ZAP   DUB,QT                                                           
         CVB   R0,DUB                                                           
         A     R0,SLINE                                                         
         BCTR  R0,0                                                             
         EDIT  (R0),(6,MSG+18)                                                  
*                                                                               
DSPLX2   GOTO1 =V(SQUASHER),DMCB,MSG,60,RR=RB                                   
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         EJECT                                                                  
NXTIN    XC    SRVPAGE,SRVPAGE     SET UP FOR NEXT INPUT                        
         XC    SRVLINE,SRVLINE                                                  
         OI    SRVPAGEH+6,X'80'                                                 
         OI    SRVLINEH+6,X'80'                                                 
         ZAP   DUB,QT              COUNT OF NON BLANK LINES DISPLAYED           
         CVB   R3,DUB                                                           
         A     R3,SLINE            R3=NEXT LINE                                 
         SR    R2,R2               R2=NEXT PAGE                                 
         OC    LACC,LACC                                                        
         BZ    NXTIN2                                                           
*                                                                               
NXTIN1   L     R2,SPAGE            REPORT HAS PAGES                             
         TM    FLAG,X'02'          TEST NEW PAGE PENDING                        
         BZ    *+12                                                             
         LA    R2,1(R2)            SET TO NEXT PAGE                             
         LA    R3,1                FIRST LINE                                   
         C     R2,MAXPAGE                                                       
         BH    DSPLF2                                                           
         EDIT  (R2),(4,SRVPAGE),ALIGN=LEFT                                      
         EDIT  (R3),(5,SRVLINE),ALIGN=LEFT                                      
         OI    SRVTABH+6,X'40'                                                  
         B     NXTIN3                                                           
*                                                                               
NXTIN2   C     R3,MAXLINE          REPORT HAS LINES                             
         BH    DSPLF2                                                           
         EDIT  (R3),(5,SRVLINE),ALIGN=LEFT                                      
         OI    SRVTABH+6,X'40'                                                  
*                                                                               
NXTIN3   MVC   SPQSRID,MYID        SAVE DIP'S ID                                
         MVC   SPQUSER,REPUSER     SAVE REPORT ID                               
         MVC   SPQSUBID,REPSUBID                                                
         MVC   SPQSEQL,REPSEQL                                                  
         ST    R2,SPQPAGE          SAVE PAGE/LINE                               
         ST    R3,SPQLINE                                                       
         MVC   SPQBUFF,SKBUFFD     SAVE PRTQ BUFF SAVE DATA                     
         MVC   SR$HELP(SPQSVLN),SPQSV                                           
*                                                                               
NXTIN4   LA    RF,SRPAGENO         WRITE BACK S/R SAVE DATA                     
         SLL   RF,32-8                                                          
         ICM   RF,3,TRMNUM                                                      
         GOTO1 ADATAMGR,DMCB,(X'00',DMWRT),TEMPSTR,(RF),(R8)                    
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
ERR0     MVC   MSG(13),=C'NOT CONNECTED'                                        
         B     ERRX                                                             
ERR1     MVC   MSG(19),=C'MISSING INPUT FIELD'                                  
         B     ERRX                                                             
ERR2     MVC   MSG(19),=C'INVALID INPUT FIELD'                                  
         B     ERRX                                                             
ERR3     MVC   MSG(19),=C'INVALID LINE NUMBER'                                  
         B     ERRX                                                             
ERR4     MVC   MSG(27),=C'START LINE GREATER THAN END'                          
         B     ERRX                                                             
ERR5     MVC   MSG(16),=C'REPORT NOT FOUND'                                     
         B     ERRX                                                             
ERR6     XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(13),=C'END OF REPORT'                                     
         B     ERRX1                                                            
ERR7     MVC   MSG(19),=C'INVALID PAGE NUMBER'                                  
         B     ERRX                                                             
ERR8     MVC   MSG(34),=C'PC NOT AUTHORIZED FOR DOWN LOADING'                   
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     MVC   SRVMSG(12),=C'***ERROR*** '                                      
         MVC   SRVMSG+12(48),MSG                                                
ERRX1    OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'                                                      
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
MAXSEQ   DC    F'65000'                                                         
PUBLICID DC    AL2(32000)                                                       
DSPMAX   DC    PL2'20'                                                          
MYID     DC    XL2'0159'                                                        
*                                                                               
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READ     DC    CL8'READ'                                                        
PRTQUE   DC    CL8'PRTQUE'                                                      
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
TEMPSTR  DC    CL8'TEMPSTR'                                                     
*&&UK                                                                           
DQUMSG   DC    CL19' - PLEASE TRY =DQU '                                        
*&&                                                                             
*&&US                                                                           
DQUMSG   DC    CL19'                   '                                        
*&&                                                                             
VALOCHRS DC    XL16'404E40404E40404E40404E40404E4040'  00-0F **TEMP**           
         DC    XL16'4E40404E40404E40404E40406040407A'  10-1F **TEMP**           
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404040404040404040404A4B4C4D4E4F'  40-4F                    
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'4081828384858687888940404040404E'  80-8F                    
         DC    XL16'40919293949596979899404040404040'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404E4E404040'  A0-AF                    
         DC    XL16'40404040404040404040404E4E404060'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404E4E404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-D1                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404E4E404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F97A4040404040'  F0-FF                    
         SPACE 1                                                                
*              EXTRA CHRACTERS ADDED FOR GERMANY                                
*        A1    DOUBLE S SYMBOL                                                  
*        C0    SMALL A UMLAUT                                                   
*        D0    SMALL U UMLAUT                                                   
*        E0    CAPITAL O UMLAUT                                                 
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
DIPWORK  DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
APARM    DS    A                                                                
AUTL     DS    A                                                                
ASCANNER DS    V                                                                
ADATAMGR DS    V                                                                
ATERMVAL DS    V                                                                
ACIREC   DS    A                                                                
RECLEN   DS    H                                                                
         DS    H                                                                
*                                                                               
SPAGE    DS    F                                                                
SLINE    DS    F                                                                
MAXPAGE  DS    F                                                                
MAXLINE  DS    F                                                                
NLINES   DS    F                                                                
MAXCPL   DS    X                                                                
FMTLR    DS    C                                                                
FMTCP    DS    C                                                                
FLAG     DS    C                                                                
LAST     DS    X                                                                
DDS      DS    X                                                                
QO       DS    PL3                                                              
QT       DS    PL3                                                              
REPPSW   DS    CL6                                                              
DATE     DS    CL8                                                              
DATEC    DS    XL2                                                              
TRMNUM   DS    XL2                                                              
DATEN    DS    XL2                 NEW CMPRSD DATE BASE=1964                    
DATEO    DS    XL2                 OLD CMPRSD DATE BASE=1900                    
         DS    0F                                                               
*                                                                               
REPID    DS    0CL8                                                             
REPUSER  DS    CL2                                                              
REPSUBID DS    CL3                                                              
REPSEQL  DS    CL2                                                              
REPCLASS DS    CL1                                                              
*                                                                               
LINEDATA DS    0XL16                                                            
LACC     DS    AL4                 A(CC CHR)                                    
LADATA   DS    AL4                 A(FIRST DATA CHR)                            
LLEN     DS    XL2                 LINE LENGTH                                  
LLENMIN  DS    XL2                 LINE LENGTH MIN                              
LLENMAX  DS    XL2                 LINE LENGTH MAX                              
         DS    XL2                                                              
*                                                                               
SAVEBUFF DS    0XL12                                                            
SBVPQTAB DS    A                   A(PQTAB)                                     
SBVPQFIL DS    A                   A(PQFILE DTF)                                
SBSAVE   DS    A                   DISPLACEMENT TO START OF SAVE AREA           
*                                                                               
MYLABEL  DS    CL8                                                              
PRTQID   DS    CL8                                                              
NDX      DS    XL40                                                             
DMCB     DS    6F                                                               
WORK     DS    CL24                                                             
MSG      DS    CL60                                                             
SAVE     DS    50F                                                              
TEMP     DS    CL256                                                            
*                                                                               
SPQSV    DS    0X                                                               
SPQSRID  DS    XL2                 ID INFO FOR $DIP                             
SPQUSER  DS    XL2                 REPORT USER ID                               
SPQSUBID DS    CL3                 REPORT SUB ID                                
SPQSEQL  DS    XL2                 REPORT SEQUENCE NUMBER                       
SPQCLASS DS    XL1                 REPORT CLASS                                 
SPQSEQH  DS    XL2                                                              
SPQPAGE  DS    XL4                 PAGE NUMBER                                  
SPQLINE  DS    XL4                 LINE NUMBER                                  
SPQBUFF  DS    XL96                SAVE AREA AT END OF BUFFER                   
SPQSVLN  EQU   *-SPQSV                                                          
*                                                                               
CIREC    DS    14336C                                                           
*                                                                               
DIPWORKX EQU   *                                                                
         EJECT                                                                  
SRDIPFFD DSECT                                                                  
         DS    CL64                                                             
* SRDIPFFD                                                                      
       ++INCLUDE SRDIPFFD                                                       
         EJECT                                                                  
* DMPRTQK                                                                       
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
* DMPRTQD                                                                       
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
* DMPRTQS                                                                       
       ++INCLUDE DMPRTQS                                                        
         EJECT                                                                  
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SRDIP00   08/03/20'                                      
         END                                                                    
