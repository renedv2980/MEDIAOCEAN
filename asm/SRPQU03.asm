*          DATA SET SRPQU03    AT LEVEL 005 AS OF 01/24/20                      
*PHASE T13103A                                                                  
         TITLE '$PQ - LIST FUNCTIONS'                                           
         PRINT NOGEN                                                            
PQLIST   CSECT                                                                  
         NMOD1 000,**$PQ3**,RA,RR=R4                                            
         LR    RC,R1                                                            
         USING PQUWKD,RC                                                        
         ST    R4,RELO                                                          
         SAM24                     SET IN 24-BIT MODE                           
*                                                                               
         L     R1,APARM                                                         
         L     R2,ASAVESTR                                                      
         USING PQSAVED,R2                                                       
         L     R3,20(R1)                                                        
         USING SRPQUFFD,R3         R3=A(TWA)                                    
         L     R8,00(R1)                                                        
         USING SYSFACD,R8          R8=A(SYS FAC LIST)                           
         EJECT                                                                  
P3VAL    DS    0H                  P3=LIST FORMAT                               
         MVC   FMTLR(1),SR@LEFT    SET LEFT JUSTIFY                             
         MVC   FMTCP(1),SR@CMPCT   SET COMPACT LINES                            
         LA    R4,SRVP3H                                                        
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0                                                        
         BE    P3VX                                                             
*                                                                               
P3V1     MVC   DUB(1),FLDDATA      FIRST CHR IS JUSTIFICATION L/R/M             
         CLI   DUB,C' '                                                         
         BE    P3V2                                                             
         CLC   DUB(1),SR@LEFT      L FOR LEFT                                   
         BE    P3V1A                                                            
         CLC   DUB(1),SR@RIGHT     R FOR RIGHT                                  
         BE    P3V1A                                                            
         CLC   DUB(1),SR@MIDDL     M FOR MIDDLE                                 
         BE    P3V1A                                                            
         CLC   DUB(1),SR@CENTR     C FOR CENTRE                                 
         BNE   *+14                                                             
         MVC   DUB(1),SR@MIDDL                                                  
         B     P3V1A                                                            
         TM    DDS,DDSTRM                                                       
         BZ    ERR1                                                             
         CLI   DUB,C'H'            H FOR HEX DISPLAY FOR DDS ONLY               
         BNE   ERR1                                                             
P3V1A    MVC   FMTLR,DUB                                                        
*                                                                               
P3V2     CLI   FLDILEN,1           SECOND CHR IS LINE SPACING                   
         BNH   P3VX                                                             
         MVC   DUB(1),FLDDATA+1                                                 
         CLI   DUB,C' '                                                         
         BE    P3VX                                                             
         CLC   DUB(1),SR@CMPCT     C FOR COMPACT                                
         BE    P3VX                                                             
         TM    DDS,DDSTRM                                                       
         BZ    P3V2A                                                            
         CLI   DUB,C'H'            DDS CAN ASK FOR EXPANDED HEX DISPLAY         
         BNE   P3V2A                                                            
         MVI   FMTCP,C'H'                                                       
         B     P3VX                                                             
P3V2A    MVI   FMTCP,C'.'          ANY OTHER CHR FOR EXPANDED                   
*                                                                               
P3VX     DS    0H                                                               
         EJECT                                                                  
P4VAL    DS    0H                  P4=PAGE(,LINE)                               
         MVC   SPAGE,=F'1'                                                      
         MVC   SLINE,=F'1'                                                      
         LA    R4,SRVP4H                                                        
         CLI   FLDILEN,0                                                        
         BE    P4VX                                                             
         L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(2,(R6))                                      
         CLI   4(R1),0                                                          
         BE    ERR2                                                             
*                                                                               
P4V1     CLI   0(R6),6             FIRST PART IS PAGE NUMBER                    
         BH    ERR3                                                             
         CLI   1(R6),0                                                          
         BNE   ERR3                                                             
         TM    2(R6),X'80'                                                      
         BO    P4V1A                                                            
         CLC   12(1,R6),SR@1ST     F FOR FIRST PAGE                             
         BNE   *+14                                                             
         MVC   SPAGE,=F'1'                                                      
         B     P4V1B                                                            
         CLC   12(1,R6),SR@TOP     T FOR TOP OF REPORT                          
         BNE   *+14                                                             
         MVC   SPAGE,=F'1'                                                      
         B     P4V1B                                                            
         CLC   12(1,R6),SR@LAST    L FOR LAST PAGE                              
         BNE   *+14                                                             
         MVC   SPAGE,=4X'FF'                                                    
         B     P4V1B                                                            
         CLC   12(1,R6),SR@BOTTM   B FOR BOTTOM OF REPORT                       
         BNE   *+14                                                             
         MVC   SPAGE,=4X'FF'                                                    
         B     P4V1B                                                            
         B     ERR3                                                             
P4V1A    L     R0,4(R6)            PAGE NUMBER INPUT                            
         LTR   R0,R0                                                            
         BZ    ERR3                                                             
         ST    R0,SPAGE                                                         
P4V1B    CLI   4(R1),1                                                          
         BE    P4VX                                                             
*                                                                               
P4V2     LA    R6,32(R6)           SECOND PART IS LINE NUMBER                   
         CLI   0(R6),7                                                          
         BH    ERR4                                                             
         CLI   1(R6),0                                                          
         BNE   ERR4                                                             
         TM    2(R6),X'80'                                                      
         BO    P4V2A                                                            
         CLC   12(1,R6),SR@1ST     F FOR FIRST LINE                             
         BNE   *+14                                                             
         MVC   SLINE,=F'1'                                                      
         B     P4V2B                                                            
         CLC   12(1,R6),SR@TOP     T FOR TOP OF PAGE                            
         BNE   *+14                                                             
         MVC   SLINE,=F'1'                                                      
         B     P4V2B                                                            
         CLC   12(1,R6),SR@LAST    L FOR LAST LINE                              
         BNE   *+14                                                             
         MVC   SLINE,=4X'FF'                                                    
         B     P4V2B                                                            
         CLC   12(1,R6),SR@BOTTM   B FOR BOTTOM OF PAGE                         
         BNE   *+14                                                             
         MVC   SLINE,=4X'FF'                                                    
         B     P4V2B                                                            
         B     ERR4                                                             
P4V2A    L     R0,4(R6)                                                         
         LTR   R0,R0                                                            
         BZ    ERR4                                                             
         ST    R0,SLINE                                                         
P4V2B    DS    0H                                                               
*                                                                               
P4VX     DS    0H                                                               
         EJECT                                                                  
SRCHCX   XC    NDX,NDX             SEARCH PRTQUE INDEX FOR REPORT               
         MVC   NXSRCID,USERID                                                   
         MVC   NXSUBID,PQSSUBID                                                 
         MVC   NXREPNO,PQSSEQ                                                   
         MVI   NXFLAG,X'04'        SET TO LOCATE BY REPORT NUMBER               
         TM    DDS,DDSTRM                                                       
         BZ    *+8                                                              
         OI    NXFLAG,X'08'        ALLOW TEMPORARY ENTRIES                      
         L     RF,ACIREC                                                        
         LA    R4,SRVP2H           POINT TO REPORT ID INPUT FIELD               
         CLI   FMTLR,C'H'                                                       
         BE    DSPHEX              SPECIAL DDS INDEX DISPLAY                    
*                                                                               
SCX2     GOTO1 VDATAMGR,DMCB,(X'08',INDEX),PRTQID,NDX,SAVE,(RF),0               
         CLI   8(R1),0                                                          
         BE    SCX4                                                             
         TM    8(R1),X'80'         EOF - REPORT NOT FOUND                       
         BO    ERR5                                                             
         DC    H'0'                ERR - DIE ON INDEX READ DISK ERROR           
*                                                                               
SCX4     L     R5,ACIREC           READ REPORT HEADER RECORD                    
         USING PQRECD,R5                                                        
         XC    SAVE(4),SAVE        RECORD ZERO DEFINES HEADER                   
         MVC   SAVE+4(4),=C'PAGE'                                               
         GOTO1 VDATAMGR,DMCB,(X'00',RANDOM),PRTQID,NDX,SAVE,(R5),0              
         CLI   8(R1),0                                                          
         BE    SCX4A                                                            
         DC    H'0'                DIE IF CANT READ REPORT HEADER               
*                                                                               
SCX4A    TM    PQATTB,PQATNP       IGNORE NON PRINTABLE REPORTS                 
         BO    SCX4B                                                            
         TM    PQSTAT,PQSTIN       IGNORE INVISIBLE REPORTS                     
         BO    SCX4B                                                            
         TM    PQATTB,PQATJOBI     IGNORE REPORTS WITH JCL IN THEM              
         BZ    SCX4C                                                            
SCX4B    TM    DDS,DDSTRM          BUT DDS TERMINALS CAN ACCESS                 
         BZ    ERR5                                                             
*                                                                               
SCX4C    TM    PQATTB,PQATPW       TEST IF PASSWORD PROTECTED                   
         BZ    SCX4D               NO                                           
         MVC   GSECVAL,PQSECINF    YES EXTRACT SECURITY INFO                    
         L     RF,ASECVAL                                                       
         BASR  RE,RF                                                            
         BNE   ERR7                SET REPORT CONTAINS SECURE INFO              
*                                                                               
SCX4D    LA    RF,DATEC            SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,DATECO                                                        
         CLC   PQAGELD,0(RF)       TEST IF FUTURE DATED REPORT                  
         BNH   SCX4E               NO                                           
         TM    DDS,DDSTRM          YES ONLY DDS CAN SEE                         
         BZ    ERR5                SET REPORT NOT FOUND                         
*                                                                               
SCX4E    XC    LINEDATA,LINEDATA                                                
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
*                                                                               
SCX4F    TM    PQSTAT,PQSTTE       TEST REPORT BEING CREATED                    
         BO    *+12                                                             
         TM    PQATTB,PQATJOBI     TEST RUNNING SOON REPORT                     
         BZ    SCX5                                                             
         SR    RF,RF               SET VALUES FOR FIRST BLOCK ONLY              
         ICM   RF,3,PQPAGEH                                                     
         ST    RF,MAXPAGE          SAVE MAXIMUM PAGE NUMBER                     
         ICM   RF,7,PQLINEH                                                     
         ST    RF,MAXLINE          SAVE MAXIMUM LINE NUMBER                     
         EJECT                                                                  
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
SCX6     LA    R4,SRVP4H           REPORT HAS PAGES                             
         OC    LACC,LACC                                                        
         BZ    SCX8                                                             
         CLC   SPAGE,MAXPAGE       CHECK SPAGE DOES NOT EXCEED MAX              
         BNH   SCX6A                                                            
         CLC   SPAGE,=4X'FF'       SET TO MAX IF LAST SPECIFIED                 
         BNE   ERR3                                                             
         MVC   SPAGE,MAXPAGE                                                    
SCX6A    XC    SAVE(12),SAVE       SET PAGE NUMBER REQUIRED                     
         MVC   SAVE+0(4),SPAGE                                                  
         MVC   SAVE+4(4),=C'PAGE'                                               
         MVC   SAVE+8(4),=F'0'     SET LINE NUMBER REQUIRED                     
         GOTO1 VDATAMGR,DMCB,(X'01',RANDOM),PRTQID,NDX,SAVE,(R5),0              
         MVC   LLEN,22(R1)                                                      
         CLI   8(R1),0                                                          
         BE    DSPLINES                                                         
         B     ERR3                CANT FIND PAGE NUMBER                        
*                                                                               
SCX8     LA    R4,SRVP4H           REPORT HAS LINES ONLY                        
         MVC   SLINE,SPAGE                                                      
         XC    SPAGE,SPAGE                                                      
         CLC   SLINE,MAXLINE       CHECK SLINE DOES NOT EXCEED MAX              
         BNH   SCX8C                                                            
         CLC   SLINE,=4X'FF'       SET TO MAX IF LAST SPECIFIED                 
         BNE   ERR4                                                             
         MVC   SLINE,MAXLINE                                                    
SCX8C    XC    SAVE(12),SAVE       SET LINE NUMBER REQUIRED                     
         MVC   SAVE+0(4),SLINE                                                  
         MVC   SAVE+4(4),=C'LINE'                                               
         GOTO1 VDATAMGR,DMCB,(X'01',RANDOM),PRTQID,NDX,SAVE,(R5),0              
         MVC   LLEN,22(R1)                                                      
         CLI   8(R1),0                                                          
         BE    DSPLINES                                                         
         B     ERR4                CANT FIND LINE NUMBER                        
         EJECT                                                                  
DSPLINES XC    NLINES,NLINES       CLEAR LINE COUNTER                           
         MVI   FLAG,0              SET LINE NOT FOUND                           
         LA    R6,SRVLIN1H                                                      
         ZAP   QO,=P'0'            SET NUMBER OF LINES DISPLAYED                
         ZAP   QT,=P'0'            SET NUMBER OF NON BLANK LINES                
         OC    LACC,LACC           MUST READ FIRST LINE OF PAGE                 
         BZ    DSPL4                                                            
*                                                                               
DSPL2    GOTO1 VDATAMGR,DMCB,(X'01',READ),PRTQID,NDX,SAVE,(R5),0                
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
         C     RE,SLINE            TEST REQUIRED START LINE                     
         BNE   DSPL2                                                            
         OI    FLAG,X'01'          SET LINE WITHIN PAGE FOUND                   
         B     DSPLA                                                            
*                                                                               
DSPL5    OI    FLAG,X'01'          SET LINE FOUND                               
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
         AHI   RE,-2                                                            
         TM    PQLINET,PQLTFL      TEST IF FIXED LENGTH DATA                    
         BZ    DSPLA1                                                           
         SR    RE,RE                                                            
         IC    RE,PQLINEW          USE DEFINED CPL FOR LEFT                     
         CLC   FMTLR(1),SR@LEFT                                                 
         BE    DSPLA1                                                           
         IC    RE,MAXCPL           USE MAX CPL FOR RIGHT/CENTRE                 
DSPLA1   OC    LACC,LACC           ADJUST LENGTH FOR CC CHR                     
         BNZ   *+6                                                              
         BCTR  RE,0                                                             
DSPLA2   CLC   FMTLR(1),SR@LEFT    LEFT JUSTIFIED                               
         BNE   DSPLA3                                                           
         MVC   8(78,R6),0(RF)      DISPLAY FIRST 78 CHRS                        
         B     DSPLB                                                            
DSPLA3   CLC   FMTLR(1),SR@RIGHT   RIGHT JUSTIFIED                              
         BNE   DSPLA4                                                           
         AHI   RE,-78              DATA LENGTH MINUS DISPLAY WIDTH              
         BM    DSPLC                                                            
         AR    RF,RE                                                            
         MVC   8(78,R6),0(RF)      DISPLAY LAST 78 CHRS                         
         B     DSPLB                                                            
DSPLA4   CLC   FMTLR(1),SR@MIDDL   CENTRE JUSTIFIED                             
         BNE   DSPLA5                                                           
         SRL   RE,1                HALF THE DATA LENGTH                         
         AHI   RE,-39              MINUS HALF THE DISPLAY WIDTH                 
         BM    DSPLC                                                            
         AR    RF,RE                                                            
         MVC   8(78,R6),0(RF)      DISPLAY MIDDLE 78 CHRS                       
         B     DSPLB                                                            
DSPLA5   B     DSPLC                                                            
*                                                                               
DSPLB    TR    8(78,R6),OUTALLC    TRANSLATE TO SCREEN DISPLAYABLE              
*                                                                               
DSPLC    CLC   FMTCP(1),SR@CENTR   COMPACT FORMAT                               
         BNE   DSPLD                                                            
         CLC   LLEN,LLENMIN        IGNORE BLANK LINES                           
         BE    DSPLE                                                            
         AP    QO,=P'1'            BUMP TO NEXT DISPLAY LINE                    
         LA    R6,86(R6)                                                        
         AP    QT,=P'1'            BUMP NON BLANK LINE COUNT                    
         B     DSPLE                                                            
*                                                                               
DSPLD    AP    QO,=P'1'            NON COMPACT FORMAT                           
         LA    R6,86(R6)                                                        
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
DSPLD2   AHI   RE,-1               RE=NUM OF EXTRA BLANK LINES                  
         BNP   DSPLE                                                            
         AP    QO,=P'1'                                                         
         LA    R6,86(R6)                                                        
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
         LA    R6,SRVLIN1H                                                      
         ZAP   QO,=P'0'            SET NUMBER OF LINES DISPLAYED                
         ZAP   QT,=P'0'                                                         
         B     DSPL2               BACK TO READ FIRST LINE OF NEXT              
DSPLF2   LA    R4,SRVP4H                                                        
         XC    MSG,MSG                                                          
         B     ERR6                SET END OF REPORT MESSAGE                    
*                                                                               
DSPLX    XC    MSG,MSG             AREA FOR GETTXT INCLUDES                     
         LA    RE,MSG                                                           
         OC    LACC,LACC           TEST IF REPORT HAS PAGES                     
         BZ    DSPLX1                                                           
         L     R1,SPAGE                                                         
         EDIT  (R1),(5,1(RE)),ALIGN=LEFT                                        
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,0(RE)            SAVE LENGTH                                  
         LA    RE,0(R1,RE)         BUMP TO NEXT INCLUDE                         
         L     R1,SLINE                                                         
         EDIT  (R1),(6,1(RE)),ALIGN=LEFT                                        
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,0(RE)            SAVE LENGTH                                  
         LA    RE,0(R1,RE)         BUMP TO NEXT INCLUDE                         
         ZAP   DUB,QT                                                           
         CVB   R1,DUB                                                           
         A     R1,SLINE                                                         
         BCTR  R1,0                                                             
         EDIT  (R1),(6,1(RE)),ALIGN=LEFT                                        
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,0(RE)                                                         
         LA    RE,0(R1,RE)                                                      
         MVC   1(25,RE),SPACES                                                  
         MVI   0(RE),26                                                         
         TM    PQATTB,PQATERR      TEST FOR ERROR REPORT                        
         BZ    DSPLX0                                                           
*                                                                               
         MVCDD 1(25,RE),SR#REPER    SET REPORT IS IN ERROR                      
*                                                                               
DSPLX0   LA    RE,172              PAGE N - LINES N THRU N LISTED               
         B     DSPLX2                                                           
*                                                                               
DSPLX1   L     R1,SLINE                                                         
         EDIT  (R1),(6,1(RE)),ALIGN=LEFT                                        
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,0(RE)            SAVE LENGTH                                  
         LA    RE,0(R1,RE)         BUMP TO NEXT INCLUDE                         
         ZAP   DUB,QT                                                           
         CVB   R1,DUB                                                           
         A     R1,SLINE                                                         
         BCTR  R1,0                                                             
         EDIT  (R1),(6,1(RE)),ALIGN=LEFT                                        
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,0(RE)                                                         
         LA    RE,173              LINES N THRU N LISTED                        
*                                                                               
DSPLX2   GOTO1 AGETTXT,DMCB,(RE),0,(C'I',0),0,MSG,X'010000'                     
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP4H+6,X'40'                                                   
         B     EXIT                                                             
         EJECT                                                                  
DSPHEX   L     R5,ACIREC           DISPLAY INDEX/FILE INFO IN HEX               
         USING PQRECD,R5                                                        
         LA    R6,SRVLIN1H         R6=A(NEXT DISPLAY LINE)                      
         USING HLINH,R6                                                         
         XC    MSG,MSG                                                          
         MVC   MSG(5),PRTQID                                                    
         MVC   MSG+05(3),=C' - '                                                
         MVC   MSG+08(3),PQSSUBID                                               
         MVI   MSG+11,C','                                                      
         EDIT  (B2,PQSSEQ),(5,MSG+12),ALIGN=LEFT                                
         MVC   MSG+60(20),=CL20'REPORT IS PERFECT'                              
*                                                                               
DSPH1    MVC   CIRSN,PQSSEQ        LOCATE REPORT BY ITS SEQUENCE NUMBER         
         SAM31                                                                  
         MVC   FIWRES,PRTQID                                                    
         BRAS  RE,FIRSET                                                        
         JNE   *+2                                                              
         XC    FIWREF,FIWREF                                                    
         MVC   FIWREF+2(2),CIRSN                                                
         BRAS  RE,FIRRN                                                         
         BRAS  RE,FIRRC                                                         
         MVC   CIADDR,FIWCIA                                                    
         SAM24                                                                  
*                                                                               
         MVC   CIADDR+2(2),=X'0100'                                             
         LH    RE,CICINDX                                                       
         MH    RE,CITRKS                                                        
         CLM   RE,3,CIADDR         TEST CI DISK ADDR WITH MIN VALUE             
         BH    *+14                                                             
         CLC   CJSTTRK,CIADDR      TEST CI DISK ADDR WITH MAX VALUE             
         BH    DSPH2                                                            
         MVC   MSG+60(20),=CL20'INVALID REPORT NUM '                            
         B     DSPHX                                                            
*                                                                               
DSPH2    SAM31                                                                  
         L     R1,FIWNDA                                                        
         MVC   FIWNDX,SI1NDX-SI1PAR(R1)                                         
         SAM24                                                                  
         XC    NDX,NDX             SAVE INDEX ENTRY INFO                        
         MVC   NDX(L'PQINDEX),FIWNDX                                            
         CLC   NXSRCID,USERID                                                   
         BNE   DSPH2A                                                           
         CLC   NXSUBID,PQSSUBID                                                 
         BNE   DSPH2A                                                           
         CLC   NXREPNO,PQSSEQ                                                   
         BNE   DSPH2A                                                           
         OI    NXFLAG,X'80'        SET INDEX REPORT EQUALS INPUT REPORT         
         B     DSPH3                                                            
DSPH2A   MVC   MSG+60(20),=CL20'INDEX NEQ INPUT '                               
*                                                                               
DSPH3    MVC   HLINE,SPACES        DISPLAY INDEX ENTRY FOUND                    
         MVC   HLINNAM(3),NXSUBID                                               
         MVI   HLINNAM+3,C','                                                   
         EDIT  (B2,NXREPNO),(5,HLINNAM+4),ALIGN=LEFT                            
         GOTO1 AHEXOUT,PARMS,CXADDR,HLINDA,4,=C'MIX'                            
         LH    R0,CXENTRY                                                       
         MH    R0,CINDXLN                                                       
         STH   R0,DUB                                                           
         GOTO1 AHEXOUT,PARMS,DUB,HLINDSP,2,=C'MIX'                              
         GOTO1 AHEXOUT,PARMS,NDX,HLINXND,24,=C'MIX'                             
         LA    R6,86(R6)                                                        
*                                                                               
DSPH4    L     R5,ACIREC                                                        
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),PRTQID,CIADDR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   DSPH4B                                                           
         CLC   NXSRCID,PQSRCID     CHECK CI VALUES WITH INDEX VALUE             
         BNE   DSPH4A                                                           
         CLC   NXSUBID,PQSUBID                                                  
         BNE   DSPH4A                                                           
         CLC   NXREPNO,PQREPNO                                                  
         BNE   DSPH4A                                                           
         OI    NXFLAG,X'40'        SET INDEX REPORT EQUALS FILE VALUE           
         B     DSPH5                                                            
DSPH4A   MVC   MSG+60(20),=CL20'INDEX NEQ FILE '                                
         B     DSPH5                                                            
DSPH4B   MVC   MSG+60(20),=CL20'DSK ERR AT 00010101'                            
         GOTO1 AHEXOUT,PARMS,CIADDR,MSG+71,4,=C'MIX'                            
         B     DSPHX                                                            
*                                                                               
DSPH5    LA    R6,86(R6)           DISPLAY REPORT CONTROL INTERVAL              
         MVC   HLINE,SPACES                                                     
         MVC   HLINNAM(3),PQSUBID                                               
         MVI   HLINNAM+3,C','                                                   
         EDIT  (B2,PQREPNO),(5,HLINNAM+4),ALIGN=LEFT                            
         GOTO1 AHEXOUT,PARMS,CIADDR,HLINDA,4,=C'MIX'                            
         MVC   HLINDSP,=C'0000'                                                 
         GOTO1 AHEXOUT,PARMS,000(R5),HLINXND,24,=C'MIX'                         
         LA    R6,86(R6)                                                        
         MVC   HLINDSP,=C'0018'                                                 
         GOTO1 AHEXOUT,PARMS,024(R5),HLINXND,16,=C'MIX'                         
         LA    R6,86(R6)                                                        
         MVC   HLINDSP,=C'0028'                                                 
         GOTO1 AHEXOUT,PARMS,040(R5),HLINXND,24,=C'MIX'                         
         LA    R6,86(R6)                                                        
         MVC   HLINDSP,=C'0040'                                                 
         GOTO1 AHEXOUT,PARMS,064(R5),HLINXND,24,=C'MIX'                         
         LA    R6,86(R6)                                                        
         MVC   HLINDSP,=C'0058'                                                 
         GOTO1 AHEXOUT,PARMS,088(R5),HLINXND,24,=C'MIX'                         
         LA    R6,86(R6)                                                        
         MVC   HLINDSP,=C'0070'                                                 
         GOTO1 AHEXOUT,PARMS,112(R5),HLINXND,16,=C'MIX'                         
         LA    R6,86(R6)                                                        
         MVC   HLINDSP,=C'0080'                                                 
         GOTO1 AHEXOUT,PARMS,128(R5),HLINXND,24,=C'MIX'                         
         LA    R6,86(R6)                                                        
*                                                                               
DSPH6    CLI   FMTCP,C'H'          TEST EXTENDED INDEX DISPLAY                  
         BNE   DSPHX                                                            
         MVC   FULL,=X'00010100'                                                
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),PRTQID,FULL,(R5)                    
         CLI   8(R1),0                                                          
         BE    DSPH7                                                            
         MVC   MSG+60(20),=CL20'DSK ERR AT 00010101'                            
         GOTO1 AHEXOUT,PARMS,FULL,MSG+71,4,=C'MIX'                              
         B     DSPHX                                                            
*                                                                               
DSPH7    LA    R6,86(R6)           DISPLAY 1ST/2ND INDEX ENTRIES                
         MVC   HLINE,SPACES                                                     
         MVC   HLINNAM(5),PRTQID                                                
         MVC   HLINDA,=C'00010100'                                              
         MVC   HLINDSP,=C'0000'                                                 
         GOTO1 AHEXOUT,PARMS,00(R5),HLINXND,24,=C'MIX'                          
         LA    R6,86(R6)                                                        
         MVC   HLINDSP,=C'0018'                                                 
         GOTO1 AHEXOUT,PARMS,24(R5),HLINXND,24,=C'MIX'                          
         LA    R6,86(R6)                                                        
*                                                                               
DSPH8    SR    RE,RE               COMPUTE END OF PART#1 INDEX                  
         ICM   RE,3,CICITOT                                                     
         MH    RE,CITRKS                                                        
         STCM  RE,3,CIADDR                                                      
         MVC   CIADDR+2(2),=X'0100'                                             
         SAM31                                                                  
         MVC   FIWCIA,CIADDR                                                    
         BRAS  RE,FIRC1                                                         
         SAM24                                                                  
         BNH   DSPH9                                                            
         MVC   MSG+60(20),=CL20'DSK ERR AT 00010101'                            
         GOTO1 AHEXOUT,PARMS,CXADDR,MSG+71,4,=C'MIX'                            
         B     DSPHX                                                            
*                                                                               
DSPH9    LA    R6,86(R6)           DISPLAY ENDPART#1 INDEX ENTRY                
         MVC   HLINE,SPACES                                                     
         MVC   HLINNAM(9),=C'ENDPART#1'                                         
         LA    R6,86(R6)                                                        
*                                                                               
DSPH10   SR    RE,RE               COMPUTE END OF PART#2 INDEX                  
         ICM   RE,3,CICITOT                                                     
         MH    RE,CITRKS                                                        
         SR    RF,RF                                                            
         ICM   RF,3,CJCITOT                                                     
         MH    RF,CJTRKS                                                        
         AR    RE,RF                                                            
         STCM  RE,3,CIADDR                                                      
         MVC   CIADDR+2(2),=X'0100'                                             
         SAM31                                                                  
         MVC   FIWCIA,CIADDR                                                    
         BRAS  RE,FIRC1                                                         
         SAM24                                                                  
         BH    DSPH11                                                           
         MVC   MSG+60(20),=CL20'DSK ERR AT 00010101'                            
         GOTO1 AHEXOUT,PARMS,CXADDR,MSG+71,4,=C'MIX'                            
         B     DSPHX                                                            
*                                                                               
DSPH11   LA    R6,86(R6)           DISPLAY ENDPART#2 INDEX ENTRY                
         MVC   HLINE,SPACES                                                     
         MVC   HLINNAM(9),=C'ENDPART#2'                                         
         GOTO1 AHEXOUT,PARMS,CXADDR,HLINDA,4,=C'MIX'                            
         LH    R0,CXENTRY                                                       
         AHI   R0,1                                                             
         MH    R0,CINDXLN                                                       
         STH   R0,DUB                                                           
         AR    R0,R5                                                            
         GOTO1 AHEXOUT,PARMS,DUB,HLINDSP,2,=C'MIX'                              
         GOTO1 AHEXOUT,PARMS,(R0),HLINXND,24,=C'MIX'                            
         LA    R6,86(R6)                                                        
*                                                                               
DSPHX    LA    RE,MSG+12           FIND END OF FIRST PART OF MESSAGE            
         LA    RF,10                                                            
DSPHX1   CLI   0(RE),C' '                                                       
         BNH   DSPHX2                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,DSPHX1                                                        
DSPHX2   MVC   0(3,RE),=C' - '                                                  
         MVC   3(20,RE),MSG+60     APPEND SECOND PART OF MESSAGE                
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP4H+6,X'40'                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXITS AND ERRORS                                                    *         
***********************************************************************         
ERR1     LA    RE,178              FIRST CHR NOT L OR R OR M                    
         B     ERRX                                                             
ERR2     LA    RE,179              FORMAT NOT PAGE,LINE                         
         B     ERRX                                                             
ERR3     LA    RE,SREIPN           INVALID PAGE NUMBER                          
         B     ERRX                                                             
ERR4     LA    RE,SREILN           INVALID LINE NUMBER                          
         B     ERRX                                                             
ERR5     LA    RE,SRERNF           REPORT NOT FOUND                             
         B     ERRX                                                             
ERR6     LA    RE,154              END OF REPORT                                
         B     INFX                                                             
ERR7     LLC   RE,GSECRES          SECURITY ERROR                               
         B     ERRX                                                             
*                                                                               
ERRX     GOTO1 AGETTXT,DMCB,(RE),0,(C'E',0),0,0,X'010000'                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'                                                      
         B     EXIT                                                             
INFX     GOTO1 AGETTXT,DMCB,(RE),0,(C'I',0),0,0,X'010000'                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'                                                      
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* SHARED MEMORY ROUTINES                                              *         
***********************************************************************         
       ++INCLUDE DDSHFIR                                                        
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
PQBLKLN  DC    H'13680'            DEFAULT BLK LEN (GOOD FOR 3390)              
DSPMAX   DC    PL2'18'                                                          
BUFFER   DC    CL8'BUFFER'                                                      
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READ     DC    CL8'READ'                                                        
DMREAD   DC    CL8'DMREAD'                                                      
FFS      DC    X'FFFFFFFFFFFFFFFF'                                              
SPACES   DC    80C' '                                                           
                                                                                
***********************************************************************         
* ALL CHARACTERS ABOVE X'40' ARE CONSIDERED VALID                     *         
* BOX CHRS 8F,AB,AC,BB,BC,BF,CB,CC,EB,EC,FA ARE TRANSLATED TO + - :   *         
* SHADING CHR 42 IS TRANSLATED TO SPACE                               *         
***********************************************************************         
OUTALLC  DC    XL16'40404040404040404040404040404040'  00-0F                    
         DC    XL16'40404040404040404040404040404040'  10-1F                    
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404140434445464748494A4B4C4D4E4F'  40-4F 42                 
         DC    XL16'505152535455565758595A5B5C5D5E5F'  50-5F                    
         DC    XL16'606162636465666768696A6B6C6D6E6F'  60-6F                    
         DC    XL16'707172737475767778797A7B7C7D7E7F'  70-7F                    
         DC    XL16'808182838485868788898A8B8C8D8E4E'  80-8F 8F                 
         DC    XL16'909192939495969798999A9B9C9D9E9F'  90-9F                    
         DC    XL16'A0A1A2A3A4A5A6A7A8A9AA4E4EADAEAF'  A0-AF AB/AC              
         DC    XL16'B0B1B2B3B4B5B6B7B8B9BA4E4EBDBE60'  B0-BF BB/BC/BF           
         DC    XL16'C0C1C2C3C4C5C6C7C8C9CA4E4ECDCECF'  C0-CF CB/CC              
         DC    XL16'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'  D0-D1                    
         DC    XL16'E0E1E2E3E4E5E6E7E8E9EA4E4EEDEEEF'  E0-EF EB/EC              
         DC    XL16'F0F1F2F3F4F5F6F7F8F97AFBFCFDFEFF'  F0-FF FA                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DSECT FOR HEADER LINE                                               *         
***********************************************************************         
HLIND    DSECT                                                                  
HLINH    DS    CL8                                                              
HLINE    DS    CL78                                                             
         ORG   HLINE                                                            
HLINNAM  DS    CL9                 FILE NAME                                    
         DS    C                                                                
HLINDA   DS    CL8                 DISK ADDRESS                                 
         DS    C                                                                
HLINDSP  DS    CL4                 DISPLACEMENT                                 
         DS    C                                                                
HLINXND  DS    CL48                INDEX ENTRY                                  
         DS    CL6                                                              
HLINX    DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
*PQUDD                                                                          
       ++INCLUDE SRPQUDD                                                        
         EJECT                                                                  
*SRPQUWK                                                                        
       ++INCLUDE SRPQUWK                                                        
         EJECT                                                                  
SRPQUFFD DSECT                                                                  
         DS    CL64                                                             
*SRPQUFFD                                                                       
       ++INCLUDE SRPQUFFD                                                       
*                                                                               
         ORG   SRVEX2H                                                          
*SRPQUFCD                                                                       
       ++INCLUDE SRPQUFED                                                       
         EJECT                                                                  
*FASYSFAC                                                                       
       ++INCLUDE FASYSFAC                                                       
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*DDFLDHDR                                                                       
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
*SRERREQUS                                                                      
       ++INCLUDE SRERREQUS                                                      
         EJECT                                                                  
         IHAASCB LIST=YES                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SRPQU03   01/24/20'                                      
         END                                                                    
