*          DATA SET SRFWK03    AT LEVEL 099 AS OF 03/02/18                      
*PHASE T13D03A                                                                  
         TITLE '=FWK - BROWSE FUNCTIONS'                                        
         PRINT NOGEN                                                            
WKBROWSE CSECT                                                                  
         NMOD1 000,**FWK3**,RA,RR=R4                                            
         LR    RC,R1                                                            
         USING FWKWKD,RC                                                        
         ST    RD,PHASERD                                                       
         ST    R4,RELO                                                          
         L     R1,APARM                                                         
         L     R2,ASAVESTR                                                      
         USING WKSAVED,R2                                                       
         L     R3,20(R1)                                                        
         USING SRFWKFFD,R3         R3=A(TWA)                                    
         L     R8,00(R1)                                                        
         USING SYSFACD,R8          R8=A(SYS FAC LIST)                           
*                                                                               
         MVCDD SRVP3A(6),SR#REC,S  P3=REC,COL                                   
         MVI   SRVP3A+6,C','                                                    
         MVCDD SRVP3A+7(6),SR#COL,S                                             
         MVCDD SRVP4A,SR#FMT       P4=FORMAT                                    
*                                                                               
         MVI   FLAG,0              RESET THIS FLAG                              
*                                                                               
         GOTO1 ADATAMGR,DMCB,(0,=CL8'FILTAB'),0                                 
         MVC   SVAFILTAB,DMCB+4                                                 
*                                                                               
         TM    INTFLAG,INTRUN      TEST INTERNAL CALL                           
         BZ    P3VAL                                                            
         XC    SRVP3,SRVP3         CLEAR P3,P4                                  
         XC    SRVP4,SRVP4                                                      
         MVI   SRVP3H+5,0                                                       
         MVI   SRVP4H+5,0                                                       
*                                                                               
         B     P3VAL                                                            
         EJECT                                                                  
*************************************************************                   
*        VALIDATE RECORD,COLUMN                             *                   
*************************************************************                   
P3VAL    EQU   *                                                                
         LA    R1,1                SET DEFAULTS                                 
         ST    R1,COLUMN                                                        
         ST    R1,RECNUM                                                        
         MVI   COLCHR,C'H'                                                      
*                                                                               
         LA    R4,SRVP3H           P3=LINE COLUMN                               
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0           TEST NO INPUT                                
         BE    P3VX                                                             
*                                                                               
P3V00    L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(4,(R6))                                      
         SR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         LTR   R0,R0               R0=NUMBER OF INPUT FIELDS                    
         BZ    ERR1                                                             
*                                                                               
P3V1     TM    2(R6),X'80'         MUST BE NUMERIC                              
         BNO   ERR1                                                             
         MVC   RECNUM,4(R6)        SET RECORD NUMBER                            
*                                                                               
         LA    R6,32(R6)                                                        
         CH    R0,=H'1'                                                         
         BE    P3VX                                                             
*                                                                               
         TM    2(R6),X'80'         CAN BE NUMERIC                               
         BNO   P3V2                                                             
         MVC   COLUMN,4(R6)        SET COLUMN NUMBER                            
         MVI   COLCHR,C'R'         SET RECORD DISPLAY                           
*                                                                               
P3V2     SR    R1,R1               OF CAN BE HDR OR KEY                         
         ICM   R1,1,0(R6)                                                       
         BZ    P3VX                                                             
         CLI   12(R6),C'H'                                                      
         BNE   *+12                                                             
         MVI   COLCHR,C'H'                                                      
         MVI   COLUMN,0                                                         
*                                                                               
         CLI   12(R6),C'K'                                                      
         BNE   *+12                                                             
         MVI   COLCHR,C'K'                                                      
         MVI   COLUMN,0                                                         
*                                                                               
P3VX     EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        VALIDATE FORMAT                                    *                   
*************************************************************                   
P4VAL    EQU   *                                                                
         CLI   WKACT,X'32'         TEST FOR ACTION CHANGE                       
         BNE   *+14                                                             
         MVC   SRVP4(4),=C'DUMP'   FORCE DUMP FORMAT                            
         MVI   SRVP4H+5,4                                                       
*                                                                               
         LA    R4,SRVP4H           P3=LINE COLUMN                               
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0           TEST NO INPUT                                
         BNE   P4V00                                                            
         MVC   SRVP4(3),=C'CHR'    DEFAULT CHR                                  
         MVI   SRVP4H+5,3                                                       
*                                                                               
P4V00    L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(4,(R6))                                      
         SR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         LTR   R0,R0               R0=NUMBER OF INPUT FIELDS                    
         BZ    ERR1                                                             
*                                                                               
         LA    RF,FORMTAB          POINT RF TO FORMAT TABLE                     
         SR    R1,R1                                                            
         IC    R1,0(R6)            GET I/P LEN                                  
         BCTR  R1,0                                                             
P4V010   EX    R1,*+8              MATCH FORMAT                                 
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P4V020                                                           
         LA    RF,8(RF)            TRY NEXT ENTRY                               
         CLI   0(RF),0                                                          
         BNE   P4V010                                                           
         B     ERR2                INVALID FORMAT                               
*                                                                               
P4V020   MVC   FORMAT,7(RF)        SET FORMAT                                   
*                                                                               
         LA    R6,32(R6)                                                        
         CH    R0,=H'1'                                                         
         BE    P4VX                                                             
*                                                                               
         TM    2(R6),X'80'         MUST BE NUMERIC                              
         BNO   ERR1                                                             
         MVC   DSTART,4(R6)        START OFFSET                                 
*                                                                               
         LA    R6,32(R6)                                                        
         CH    R0,=H'2'                                                         
         BE    P4VX                                                             
*                                                                               
         TM    2(R6),X'80'         MUST BE NUMERIC                              
         BNO   ERR1                                                             
         ICM   R1,15,4(R6)                                                      
         LA    R1,4(R1)            ADD 4 FOR LEN                                
         ST    R1,DEND             END OFFSET                                   
*                                                                               
P4VX     MVC   HDISP,=H'78'        SET UP DISPS FOR FORMAT                      
         MVC   VDISP,=H'17'                                                     
         CLI   FORMAT,C'C'                                                      
         BE    *+10                                                             
         MVC   HDISP,=H'39'                                                     
         CLI   FORMAT,C'M'                                                      
         BNE   *+10                                                             
         MVC   VDISP,=H'8'                                                      
         EJECT                                                                  
*************************************************************                   
*        VALIDATE OPTIONS                                   *                   
*************************************************************                   
         SPACE 1                                                                
OPVAL    EQU   *                                                                
         XC    WORK,WORK           USE WORK AS FIND STRING AREA                 
         LA    R4,SRVOPTNH         OPTION IS - "KEYWORD DATA"                   
         ST    R4,CURSOR                                                        
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0           TEST NO INPUT                                
         BE    OPV990                                                           
*                                                                               
         CLI   PFKEY,0             MUST BE ENTER                                
         BNE   OPV990              OR OPTIONS IGNORED                           
*                                                                               
OPV000   SR    R1,R1               R1 = FIELD LEN                               
         IC    R1,FLDILEN                                                       
         LR    RF,R1                                                            
*                                                                               
         BCTR  RF,0                RF = EXECUTE LEN                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    FLDDATA(0),SPACES   UPPER CASE FOR ALL                           
*                                                                               
         LR    RE,R1                                                            
         LA    R6,FLDDATA          FIND FIRST SPACE                             
OPV010   CLI   0(R6),C' '                                                       
         BNH   OPV020                                                           
         LA    R6,1(R6)                                                         
         BCT   R1,OPV010                                                        
*                                                                               
OPV020   LR    RF,R1               RF=LEN OF DATA  R6=A(DATA-1)                 
*                                                                               
         CLC   FLDDATA(1),SR@FIND                                               
         BNE   ERR5                INVALID OPTION                               
*                                                                               
         MVC   WORK,SPACES                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R6)                                                    
         STC   RF,WORK             SAVE L'STRING IN WORK                        
*                                                                               
         GOTO1 ADECODE,DMCB,(00,WORK+1),SWORK,0                                 
         CLI   8(R1),X'FF'                                                      
         BE    ERR6                                                             
         MVC   WORK,11(R1)                                                      
         MVC   WORK+1(L'WORK-1),SWORK                                           
*                                                                               
OPV990   EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        GET CIADDR LOAD SCREEN AND INIT BUFFER             *                   
*************************************************************                   
BROW000  OC    CIPASS,CIPASS       DO WE HAVE A DA ALREADY                      
         BNZ   BROW010                                                          
         OC    WKFILEN,WKFILEN                                                  
         BZ    ERR4                                                             
*                                                                               
         XC    NDX,NDX                                                          
         MVC   NXSRCID,USERID                                                   
         MVC   NXSYSPG(7),WKSYSPG                                               
         CLI   NXSUBPG,C'*'        SET ASTERISK TO ZERO                         
         BNE   *+8                                                              
         MVI   NXSUBPG,X'00'                                                    
         CLI   NXCLASS,0           SET ZERO TO SPACE                            
         BNE   *+8                                                              
         MVI   NXCLASS,C' '                                                     
         MVC   NXFILENO,WKFILEN                                                 
         GOTO1 ADATAMGR,WKDMCB,(X'08',=C'INDEX')                                
         CLI   8(R1),0                                                          
         BNE   ERR4                FILE NOT FOUND                               
*                                                                               
         MVC   CIPASS,NXCIADDR                                                  
         MVC   CIPASS+2(2),=X'0100'                                             
*                                                                               
BROW010  MVI   SCREENR,X'FE'       GET FE SCREEN                                
         CLI   FORMAT,C'D'                                                      
         BNE   *+8                                                              
         MVI   SCREENR,X'FC'       GET FC FOR DUMP                              
         GOTO1 ALOADSCR                                                         
*                                                                               
         L     R5,ACIREC           INIT BUFFER     < WAS CXREC???               
         USING WKRECD,R5                      < CXRECS WERE CTREC               
         GOTO1 VDATAMGR,WKDMCB,(X'00',BUFFER),WRKFID,NDX,ACXREC,(R5)            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CIADDR,CIPASS                                                    
         L     RE,8(R5)            SET DISK ADDR IN SAVE AREA                   
         AR    RE,R5                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKEXTNO-SKBUFFD(1,RE),BUWFFILE                                   
*                                                                               
         XC    CXREC(12),CXREC     READ HEADER REC                              
         MVC   CXREC+4(4),=C'REC '                                              
         GOTO1 VDATAMGR,WKDMCB,(X'00',RANDOM)                                   
         MVC   FULL1,WKRECS        SAVE MAX RECS FOR PF8 CHECK                  
*                                                                               
         MVC   WKUSER,WKUSRID      COPY USERID                                  
         MVC   WKSYSPG(8),WKSYSPRG COPY SPPSDCNN                                
         CLI   WKSUBPG,C'*'        SET ASTERISK TO ZERO                         
         BNE   *+8                                                              
         MVI   WKSUBPG,X'00'                                                    
         MVC   WKFILEN,WKFILNO                                                  
         XC    WKTIMES,WKTIMES     CLEAR TIMES                                  
         GOTO1 ARIDXPND,SRVP2H                                                  
         EJECT                                                                  
*************************************************************                   
*        HANDLE FIND OPTION (WORK HOLDS FIND STRING)        *                   
*************************************************************                   
FIND     CLI   FORMAT,C'D'         IS THIS THE DUMP FORMAT                      
         BNE   FIND000                                                          
*                                                                               
DFIND000 CLI   WORK,0              YES                                          
         BE    DFINDXXX                                                         
*                                                                               
         XC    SFULL,SFULL                                                      
         SR    RE,RE               SEPARATE CURSOR COMPONENTS                   
         LH    RF,CURADR                                                        
         D     RE,=F'80'                                                        
         SH    RF,=H'6'            SUB 6 FOR TOP OF SCREEN                      
         BM    DFIND009                                                         
         CH    RE,=H'53'           TEST FOR RHS                                 
         BH    DFIND009                                                         
         SH    RE,=H'15'           SUB 15 FOR LHS                               
         BM    DFIND009                                                         
         MH    RF,=H'20'           20 BYTES PER LINE                            
         SRL   RE,1                                                             
         AR    RF,RE               ADD H TO V                                   
         SR    R1,R1                                                            
         ICM   R1,3,WKSDIS1        ADD DISP+1                                   
         LA    RF,1(RF,R1)                                                      
         ST    RF,SFULL            SET DISPLACEMENT                             
*                                                                               
DFIND009 BAS   RE,GET              GET CURRENT RECORD                           
*                                                                               
DFIND010 L     RF,CXREC                                                         
         LA    R6,CXREC+4          R6=REC                                       
         SRL   RF,16                                                            
         SH    RF,=H'4'            RF=L'REC                                     
         BZ    DFIND050                                                         
*                                                                               
         A     R6,SFULL            SET DISPLACEMENT                             
         S     RF,SFULL                                                         
         BNP   DFIND050                                                         
         XC    SFULL,SFULL         FIRST TIME ONLY                              
*                                                                               
DFIND020 SR    R1,R1               R1=L'SEARCH STRING                           
         IC    R1,WORK                                                          
         CR    R1,RF                                                            
         BH    DFIND050            STRING MUST NOT BE > L'REC                   
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),WORK+1      LOOK FOR STRING                              
         BE    DFIND900                                                         
*                                                                               
         LA    R6,1(R6)            NEXT BYTE                                    
         BCTR  RF,0                LEN=LEN-1                                    
         B     DFIND020                                                         
*                                                                               
DFIND050 BAS   RE,READNXT          GET NEXT RECORD                              
         XC    WKSDIS1,WKSDIS1                                                  
         CLC   CXREC(4),EOF                                                     
         BNE   DFIND010                                                         
         MVC   MSGREF,I151                                                      
         B     EXIT                                                             
*                                                                               
DFIND900 MVC   RECNUM,RECNUM2      SET TOP RECORD                               
*                                                                               
         LA    R1,CXREC+4          SET CURSOR POSITION                          
         SR    R6,R1                                                            
         LR    RF,R6                                                            
         SR    R1,R1                                                            
         ICM   R1,3,WKSDIS1                                                     
         SR    RF,R1                                                            
         SR    RE,RE                                                            
         D     RE,=F'20'                                                        
         SLL   RE,1                                                             
         LA    RE,15(RE)                                                        
         CH    RF,=H'16'           CHECK PAGE BOUNDARY                          
         BNH   DFIND910                                                         
         SH    RF,=H'17'                                                        
         SR    R1,R1                                                            
         ICM   R1,3,WKSDIS1                                                     
         AH    R1,=H'340'                                                       
         STCM  R1,3,WKSDIS1                                                     
DFIND910 LA    RF,6(RF)                                                         
         MH    RF,=H'80'                                                        
         AR    RF,RE                                                            
         STH   RF,CURADR                                                        
*                                                                               
         OI    CURFLAG,X'80'       SET CURSOR FLAG                              
         B     DSP000                                                           
*                                                                               
DFINDXXX B     PFK000                                                           
         EJECT                                                                  
*************************************************************                   
*        HANDLE FIND OPTION (WORK HOLDS FIND STRING)        *                   
*************************************************************                   
FIND000  CLI   WORK,0              TEST FOR NULL FIND STRING                    
         BE    FINDXXX                                                          
*                                                                               
         XC    SFULL,SFULL                                                      
         MVC   SDUB,RECNUM         SAVE RECNUM                                  
         SR    RE,RE               SAPARATE CURSOR COMPONENTS                   
         LH    RF,CURADR                                                        
         D     RE,=F'80'                                                        
         SH    RF,=H'6'            SUB 6 FOR TOP OF SCREEN                      
         BM    FIND009                                                          
         CLI   FORMAT,C'M'         /2 IF MIX                                    
         BNE   *+8                                                              
         SRL   RF,1                                                             
         A     RF,RECNUM                                                        
         ST    RF,RECNUM           CALCULATE START RECORD                       
*                                                                               
         CLI   FORMAT,C'C'         /2+1 IF MIX OR HEX                           
         BE    *+12                                                             
         SRL   RE,1                                                             
         LA    RE,1(RE)                                                         
         A     RE,COLUMN                                                        
         BCTR  RE,0                                                             
         ST    RE,SFULL            SFULL=DISP                                   
*                                                                               
FIND009  BAS   RE,GET              GET CURRENT RECORD                           
         MVC   RECNUM,SDUB         RESTORE RECNUM                               
*                                                                               
FIND010  L     RF,CXREC                                                         
         LA    R6,CXREC+4          R6=REC                                       
         SRL   RF,16                                                            
         SH    RF,=H'4'            RF=L'REC                                     
         BZ    FIND050                                                          
*                                                                               
         A     R6,SFULL            SET DISPLACEMENT                             
         S     RF,SFULL                                                         
         BNP   FIND050                                                          
         XC    SFULL,SFULL         FIRST TIME ONLY                              
*                                                                               
FIND020  SR    R1,R1               R1=L'SEARCH STRING                           
         IC    R1,WORK                                                          
         CR    R1,RF                                                            
         BH    FIND050             STRING MUST NOT BE > L'REC                   
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),WORK+1      LOOK FOR STRING                              
         BE    FIND900                                                          
*                                                                               
         LA    R6,1(R6)            NEXT BYTE                                    
         BCTR  RF,0                LEN=LEN-1                                    
         B     FIND020                                                          
*                                                                               
FIND050  BAS   RE,READNXT          GET NEXT RECORD                              
         CLC   CXREC(4),EOF                                                     
         BNE   FIND010                                                          
         MVC   MSGREF,I151                                                      
         B     EXIT                                                             
*                                                                               
FIND900  L     RF,RECNUM2          CALCULATE TOP RECORD                         
         S     RF,RECNUM                                                        
         SR    RE,RE                                                            
         D     RE,VDISPF                                                        
         MH    RF,VDISP                                                         
         A     RF,RECNUM                                                        
         ST    RF,RECNUM           SET RECORD                                   
*                                                                               
         CLI   FORMAT,C'M'         X2 IF MIX                                    
         BNE   *+8                                                              
         SLL   RE,1                                                             
*                                                                               
         LA    RE,6(RE)            SET CURSOR ROW                               
         MH    RE,=H'80'                                                        
         STH   RE,CURADR                                                        
*                                                                               
         LA    R1,CXREC+4          SET CURSOR COLUMN                            
         SR    R6,R1                                                            
         LR    RF,R6                                                            
         SR    RE,RE                                                            
         D     RE,HDISPF                                                        
         MH    RF,HDISP                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COLUMN           SET COLUMN                                   
         CLI   FORMAT,C'C'         X2 IF HEX OR MIX                             
         BE    *+8                                                              
         SLL   RE,1                                                             
         AH    RE,CURADR                                                        
         LA    RE,1(RE)                                                         
         STH   RE,CURADR                                                        
         OI    CURFLAG,X'80'       SET CURSOR FLAG                              
*                                                                               
FINDXXX  B     PFK000                                                           
         EJECT                                                                  
*************************************************************                   
*        HANDLE PFKEYS SET NEW LINE,COLUMN                  *                   
*************************************************************                   
PFK000   EQU   *                                                                
         CLI   FORMAT,C'D'                                                      
         BE    PFK059                                                           
*                                                                               
         L     RF,RECNUM                                                        
         L     RE,COLUMN                                                        
*                                                                               
PFK010   CLI   PFKEY,7             PF7=UP                                       
         BNE   PFK020                                                           
         SH    RF,VDISP            SUB VERTICAL DISP                            
         BP    *+8                                                              
         LA    RF,1                FORCE <1 TO 1                                
*                                                                               
PFK020   CLI   PFKEY,8             PF8=DOWN                                     
         BNE   PFK030                                                           
         AH    RF,VDISP            ADD VERTICAL DISP                            
         C     RF,FULL1            FULL1 HOLDS MAX RECS                         
         BNH   *+8                                                              
         L     RF,FULL1            CANNOT GO > MAX RECS                         
*                                                                               
PFK030   CLI   FORMAT,C'R'         RCVR HANDLES L & R DIFFERENTLY               
         BE    PFK051                                                           
*                                                                               
         CLI   PFKEY,10            PF10=LEFT                                    
         BNE   PFK040                                                           
         SH    RE,HDISP            SUB HORIZONTAL DISP                          
         BP    *+8                                                              
         LA    RE,1                FORCE <1 TO 1                                
*                                                                               
PFK040   CLI   PFKEY,11            PF11=RIGHT                                   
         BNE   PFK050                                                           
         AH    RE,HDISP            ADD HORIZONTAL DISP                          
*                                                                               
PFK050   ST    RE,COLUMN           RESTORE NEW LINE,COLUMN                      
         ST    RF,RECNUM                                                        
         B     PFK059                                                           
*                                                                               
PFK051   CLI   PFKEY,10                                                         
         BNE   PFK052                                                           
*                                                                               
         CHI   RE,1                                                             
         BNH   PFK051A                                                          
         BCTR  RE,0                COL = COL -1                                 
         LTR   RE,RE                                                            
         BNZ   PFK053                                                           
*                                                                               
PFK051A  LA    R1,COLTAB           FIND CHR IN TAB                              
         CLC   COLCHR,0(R1)                                                     
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'0'          TEST EOT                                     
         BNE   *-18                                                             
         BCTR  R1,0                BACK UP ONE                                  
         MVC   COLCHR,0(R1)                                                     
         CLI   COLCHR,C'0'         UNTIL WE GET TO 0                            
         BNE   *+8                                                              
         MVI   COLCHR,C'H'         THEN GOTO BEGINING                           
*                                                                               
PFK052   CLI   PFKEY,11                                                         
         BNE   PFK053                                                           
*                                                                               
         CLI   COLCHR,C'R'         ARE WE ON RECORDS                            
         BNE   *+12                                                             
         LA    RE,1(RE)            BUMP ELEMENT NUMBER                          
         B     PFK053                                                           
*                                                                               
         LA    R1,COLTAB+4         FIND CHR IN TAB                              
         CLC   COLCHR,0(R1)                                                     
         BE    *+14                                                             
         BCTR  R1,0                                                             
         CLI   0(R1),C'H'          TEST SOT                                     
         BNE   *-16                                                             
         LA    R1,1(R1)            FORWARD ONE                                  
         MVC   COLCHR,0(R1)                                                     
*                                                                               
PFK053   ST    RE,COLUMN           RESTORE NEW LINE,COLUMN                      
         ST    RF,RECNUM                                                        
*                                                                               
PFK059   EQU   *                                                                
*                                                                               
PFK060   CLI   PFKEY,9                                                          
         BNE   PFK070                                                           
         LA    R1,FORMT2                                                        
PFK061   CLC   FORMAT,0(R1)                                                     
         BE    PFK062                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'0'                                                       
         BNE   PFK061                                                           
         DC    H'0'                                                             
*                                                                               
PFK062   MVC   FORMAT,1(R1)                                                     
         LA    R1,FORMTAB                                                       
         CLC   FORMAT,7(R1)                                                     
         BE    *+12                                                             
         LA    R1,8(R1)                                                         
         B     *-14                                                             
         MVC   SRVP4(4),0(R1)                                                   
*                                                                               
         MVI   SCREENR,X'FE'       DOUBLE CHECK SCREEN                          
         CLI   FORMAT,C'D'                                                      
         BNE   *+8                                                              
         MVI   SCREENR,X'FC'       GET FC FOR DUMP                              
         GOTO1 ALOADSCR                                                         
*                                                                               
PFK070   CLI   FORMAT,C'D'                                                      
         BE    PFDUMP                                                           
         BNE   DSP000                                                           
         EJECT                                                                  
*************************************************************                   
*        PFKEY HANDLING FOR DUMP FORMAT                     *                   
*************************************************************                   
PFDUMP   CLI   PFKEY,9             SWAP FORMAT                                  
         BE    *+12                                                             
         CLI   PFKEY,0             OR ENTER                                     
         BNE   PFD005                                                           
*                                                                               
         XC    WKSDIS1,WKSDIS1     USE RECNUM                                   
         MVC   WKSREC1,RECNUM+2                                                 
         B     PFDXXX                                                           
*                                                                               
PFD005   CLI   WKACT,X'32'         TEST CHANGE ACTION                           
         BE    PFCHA                                                            
*                                                                               
         CLI   PFKEY,8             PF8 DOWN (RESTORE SAVED VALUES)              
         BNE   PFD010                                                           
         MVC   WKSREC1,WKSREC2                                                  
         MVC   WKSDIS1,WKSDIS2                                                  
         B     PFDXXX                                                           
*                                                                               
PFD010   CLI   PFKEY,7             PF7 UP                                       
         BNE   PFDXXX                                                           
*                                                                               
         LA    R1,18               SET LINE TO 18                               
         ST    R1,SFULL                                                         
*                                                                               
PFD020   ICM   R1,15,SFULL         LINE=LINE-1                                  
         SH    R1,=H'1'                                                         
         BZ    PFD990              LINE=0 -- THIS IS IT                         
         STCM  R1,15,SFULL                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,WKSDIS1        DISP=DISP-20                                 
         C     R1,DSTART           IF DISP<=DSTART THEN NEXT                    
         BNH   PFD030                                                           
         SH    R1,=H'20'                                                        
         STCM  R1,3,WKSDIS1                                                     
         B     PFD020                                                           
*                                                                               
PFD030   ICM   R1,3,WKSREC1        WKSREC1=WKSREC1-1                            
         SH    R1,=H'1'                                                         
         BZ    PFD995                                                           
         STCM  R1,3,WKSREC1                                                     
         ST    R1,RECNUM                                                        
*                                                                               
         BAS   RE,GET              GET RECORD LEN                               
         L     RF,CXREC                                                         
         SRL   RF,16                                                            
*                                                                               
         ICM   R1,15,DEND          TEST FOR END PARM                            
         BZ    *+12                                                             
         CR    RF,R1               SEE IF RECORD IS SHORTER ANYWAY              
         BL    *+6                                                              
         LR    RF,R1                                                            
*                                                                               
         SH    RF,=H'4'            SUB 4 FOR HEADER                             
         S     RF,DSTART           ALWAYS SUBTRACT START                        
*                                                                               
PFD040   SR    RE,RE                                                            
         D     RE,=F'20'           DIVIDE BY 20                                 
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)            ADD 1 LINE FOR REMAINDER                     
         MH    RF,=H'20'           MILTIPLY BY 20                               
         SH    RF,=H'20'           SUBTRACT 20                                  
         STH   RF,WKSDIS1          SET NEW DISPLACEMENT                         
         B     PFD020                                                           
*                                                                               
PFD990   B     PFDXXX                                                           
*                                                                               
PFD995   LA    R1,R1               SET 1ST REC                                  
         STCM  R1,3,WKSREC1                                                     
*                                                                               
PFDXXX   SR    R1,R1               SET RECNUM                                   
         ICM   R1,3,WKSREC1                                                     
         ST    R1,RECNUM                                                        
         B     DSP000                                                           
*                                                                               
PFCHA    L     R1,RECNUM           PF 7 & 8 FOR CHANGE                          
         CLI   PFKEY,7                                                          
         BNE   PFCHA010                                                         
         CH    R1,=H'1'            NO LOWER THAN 1                              
         BNH   PFCHA010                                                         
         BCTR  R1,0                PF 7 SUBTRACT 1                              
PFCHA010 CLI   PFKEY,8                                                          
         BNE   PFCHA020                                                         
         LA    R1,1(R1)            PF 8 ADD 1                                   
PFCHA020 ST    R1,RECNUM                                                        
         BAS   RE,EDIT                                                          
         MVC   SRVP3,SWORK         REDISPLAY P3                                 
         EJECT                                                                  
*************************************************************                   
*        DISPLAY DATA ON SCREEN                             *                   
*************************************************************                   
DSP000   EQU   *                                                                
         CLI   FORMAT,C'C'         FORMAT=CHR                                   
         BNE   DSP010                                                           
         BAS   RE,BLDCHR           BUILD SCREEN                                 
         B     DSP990              OUTPUT MESSAGE                               
*                                                                               
DSP010   CLI   FORMAT,C'H'                                                      
         BNE   DSP020                                                           
         BAS   RE,BLDHEX           BUILD SCREEN                                 
         B     DSP990              OUTPUT MESSAGE                               
*                                                                               
DSP020   CLI   FORMAT,C'D'                                                      
         BNE   DSP030                                                           
         CLI   WKACT,X'32'         ACTION CHANGE                                
         BNE   DSP021                                                           
         OC    SRVP3(10),SPACES    IGNORE NULLS & SPACES                        
         OC    WKSP3FLD(10),SPACES                                              
         CLC   SRVP3(10),WKSP3FLD  SAME RECORD AS PREV                          
         BNE   DSP021                                                           
         BAS   RE,REMERGE          GO FIND OLD INPUT                            
         BAS   RE,VALDMP           VALIDATE DUMP SCREEN                         
DSP021   BAS   RE,BLDDMP           BUILD SCREEN                                 
         MVC   WKSP3FLD,SRVP3      SAVE RECORD NUM                              
         B     DSP990              OUTPUT MESSAGE                               
*                                                                               
DSP030   CLI   FORMAT,C'M'                                                      
         BNE   DSP040                                                           
         BAS   RE,BLDHEX           BUILD SCREEN                                 
         B     DSP990              OUTPUT MESSAGE                               
*                                                                               
DSP040   CLI   FORMAT,C'R'                                                      
         BNE   DSP050                                                           
         BAS   RE,BLDRCVR          BUILD SCREEN                                 
         B     DSP990              OUTPUT MESSAGE                               
*                                                                               
DSP050   DC    H'0'                UNKNOWN FORMAT                               
*                                                                               
DSP990   L     R1,RECNUM           REDISPLAY RECNUM & COLUMN                    
         BAS   RE,EDIT                                                          
         MVC   SRVP3,SWORK                                                      
         LA    RF,SRVP3                                                         
         AR    RF,R0                                                            
*                                                                               
         CLI   FORMAT,C'D'         DONT SHOW COL ON DUMP SCREEN                 
         BE    DSP995                                                           
*        CLC   COLUMN,=F'1'        IF COL=1 DON'T BOTHER                        
*        BE    DSP992                                                           
         MVI   0(RF),C','                                                       
         L     R1,COLUMN                                                        
         BAS   RE,EDIT                                                          
         MVC   1(6,RF),SWORK                                                    
*                                                                               
DSP992   CLI   FORMAT,C'R'         COLCHRS ONLY FOR RCVR                        
         BNE   DSP995                                                           
         MVI   0(RF),C','                                                       
         CLI   COLCHR,C'H'                                                      
         BNE   *+10                                                             
         MVC   1(3,RF),=C'HDR'                                                  
         CLI   COLCHR,C'K'                                                      
         BNE   *+10                                                             
         MVC   1(3,RF),=C'KEY'                                                  
*                                                                               
DSP995   CLC   CXREC,EOF           TEST EOF FOUND                               
         BNE   DSP999                                                           
         MVC   MSGREF,I190         END OF FILE                                  
         B     EXIT                                                             
*                                                                               
DSP999   MVC   MSGREF,I191                                                      
         LA    RF,MSGXTRA          BUILD MESSAGE                                
*                                                                               
         L     R1,RECNUM                                                        
         BAS   RE,EDIT                                                          
         AH    R0,=H'1'                                                         
         STC   R0,0(RF)                                                         
         MVC   1(6,RF),SWORK                                                    
         AR    RF,R0                                                            
*                                                                               
         L     R1,RECNUM2                                                       
         BAS   RE,EDIT                                                          
         AH    R0,=H'1'                                                         
         STC   R0,0(RF)                                                         
         MVC   1(6,RF),SWORK                                                    
         AR    RF,R0                                                            
         MVI   0(RF),0                                                          
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD SCREEN (CHR FORMAT)                          *                   
*************************************************************                   
BLDCHR   NTR1                                                                   
         LA    R6,SRVLIN1          R6=FIELD                                     
         SR    R0,R0                                                            
         BAS   RE,HEADER           BUILD HEADER                                 
*                                                                               
         LA    R6,87(R6)           BUMP TO NEXT FIELD                           
         BAS   RE,GET              GET FIRST RECORD                             
*                                                                               
BLDC010  MVC   0(78,R6),SPACES     FILL LINE WITH SPACES                        
*                                                                               
         L     RF,CXREC            CALCULATE MOVE LEN                           
         CLC   CXREC(4),EOF                                                     
         BE    BLDC990                                                          
         SRL   RF,16                                                            
         SH    RF,=H'4'                                                         
         S     RF,COLUMN                                                        
         BM    BLDC020             SKIP WHOLE LINE IF -VE                       
*                                                                               
         CH    RF,=H'77'           77 IS MAXIMUM LEN                            
         BNH   *+8                                                              
         LA    RF,77               FORCE 77                                     
*                                                                               
         L     R1,COLUMN                                                        
         LA    R1,3(R1)            ADD 4 FOR LEN -1 FOR COLUMN                  
         LA    R1,CXREC(R1)        INDEX TO RECORD                              
*                                                                               
         EX    RF,*+8              EXECUTE THIS MOVE                            
         B     *+10                                                             
         MVC   0(0,R6),0(R1)       MOVE CHRS OUT                                
         TR    0(78,R6),VALOCHRS   AND TRANSLATE                                
*                                                                               
BLDC020  LA    R6,87(R6)           BUMP TO NEXT FIELD                           
         LA    R1,SRVPFK1                                                       
         CR    R6,R1                                                            
         BNL   XIT1                                                             
*                                                                               
         BAS   RE,READNXT          GET NEXT RECORD                              
         B     BLDC010             NEXT IF NOT END OF SCREEN                    
*                                                                               
BLDC990  B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD SCREEN (RCVR FORMAT)                         *                   
*************************************************************                   
BLDRCVR  NTR1                                                                   
         LA    R6,SRVLIN1          R6=FIELD                                     
         SR    R0,R0                                                            
         BAS   RE,HEADER           BUILD HEADER                                 
*                                                                               
         LA    R6,87(R6)           BUMP TO NEXT FIELD                           
         BAS   RE,GET              GET FIRST RECORD                             
*                                                                               
BLDR010  MVC   0(78,R6),SPACES     FILL LINE WITH SPACES                        
*                                                                               
         L     RF,CXREC                                                         
         CLC   CXREC(4),EOF                                                     
         BE    BLDR990                                                          
*                                                                               
         LA    R1,RCVRTAB          FIND ACTION                                  
BLDR011  CLC   CXREC+5(1),0(R1)                                                 
         BE    BLDR012                                                          
         LA    R1,4(R1)                                                         
         CLI   0(R1),0             TEST EOT                                     
         BNE   BLDR011                                                          
         B     BLDR980             NOT RECOVERY FORMAT                          
*                                                                               
BLDR012  MVC   0(3,R6),1(R1)       SHOW ACTION                                  
*                                                                               
         SR    R1,R1               EXTRACT FILE NUMBER                          
         IC    R1,CXREC+4                                                       
         SLL   R1,5                INDEX INTO FILTAB                            
*        LA    R1,FILTAB(R1)                                                    
         A     R1,SVAFILTAB                                                     
         MVC   FILENTRY,0(R1)                                                   
         USING FILTABD,FILENTRY                                                 
         CLC   DMFLNAME,SPACES     TEST FOR FILE DEFINED                        
         BE    BLDR980                                                          
         MVC   4(8,R6),DMFLNAME    COPY OUT FILENAME                            
*                                                                               
         UNPK  14(7,R6),CXREC+12(4)                                             
         OI    20(R6),X'F0'        UNPACK TIME                                  
         MVC   13(2,R6),15(R6)                                                  
         MVI   15(R6),C':'                                                      
         MVC   16(2,R6),17(R6)                                                  
         MVI   18(R6),C':'                                                      
*                                                                               
         GOTO1 AHEXOUT,DMCB,CXREC+16,22(R6),4                                   
*                                                                               
         MVC   BYTE,CXREC+20       GET SE NAME                                  
         BAS   RE,GETSYS                                                        
         MVC   31(6,R6),WORK                                                    
*                                                                               
*        NOW CALCULATE HOW MUCH WILL FIT ON SCREEN                              
*                                                                               
* START AT +37 FOR 40                                                           
* THEN +0 FOR 39                                                                
*                                                                               
         CLI   COLCHR,C'H'         HEADER DISPLAY                               
         BNE   *+20                                                             
         LA    R4,37(R6)           R4=CURRENT OUTPUT POINT                      
         LA    R5,40               R5=LENGTH AVAILABLE                          
         LA    R7,CXREC            R7=RECORD ADDRESS                            
         LA    R7,28(R7)           BUMP PAST RCVR HEADER                        
*                                                                               
         CLI   COLCHR,C'K'         KEY DISPLAY                                  
         BNE   *+18                                                             
         LR    R4,R6                                                            
         LA    R5,78                                                            
         LA    R7,CXREC            R7=RECORD ADDRESS                            
         LA    R7,28(R7)           BUMP PAST RCVR HEADER                        
*                                                                               
         CLI   COLCHR,C'R'         RECORD DISPLAY                               
         BNE   BLDR020                                                          
         LR    R4,R6                                                            
         LA    R5,78                                                            
         LA    R7,CXREC            R7=RECORD ADDRESS                            
         LA    R7,28(R7)           BUMP PAST RCVR HEADER                        
         B     BLDR030                                                          
*                                                                               
BLDR020  SR    R1,R1               SET UP KEY LENGTH                            
         IC    R1,DMFLKEYL         THEN HEXOUT KEY                              
         ST    R1,FULL             SAVE KEY LEN IN FULL                         
         BAS   RE,SHOWIT                                                        
*                                                                               
BLDR022  SR    R1,R1               RECRD LENGTH DISPLACEMENT                    
         ICM   R1,1,DMFLLEND                                                    
         BZ    BLDR024                                                          
         LA    R7,0(R1,R7)                                                      
         LA    R1,2                                                             
         BAS   RE,SHOWIT                                                        
         LA    R7,CXREC+28+2                                                    
*                                                                               
BLDR024  A     R7,FULL                                                          
         SR    R1,R1               CONTROL BYTES                                
         ICM   R1,1,DMFLCTRL                                                    
         BZ    BLDR026                                                          
         BAS   RE,SHOWIT                                                        
*                                                                               
BLDR026  LA    R7,CXREC            DISK ADDRESS                                 
         SR    R1,R1                                                            
         ICM   R1,1,DMFLDAD                                                     
         BZ    BLDR028                                                          
         LA    R7,28(R1,R7)        BUMP PAST RCVR HEADER                        
         LA    R1,4                                                             
         BAS   RE,SHOWIT                                                        
*                                                                               
BLDR028  B     BLDR300                                                          
*                                                                               
BLDR030  MVC   0(78,R6),SPACES     FILL LINE WITH SPACES                        
         LA    R7,CXREC+28                                                      
         SR    R1,R1               1ST ELEMENT DISPLACEMENT                     
         ICM   R1,1,DMFLELD                                                     
         BZ    BLDR300                                                          
         LA    R7,0(R1,R7)         POINT TO 1ST ELEMENT                         
*                                                                               
         L     R1,COLUMN           COLUMN=ELEMENT NUMBER                        
BLDR031  CLI   0(R7),0                                                          
         BE    BLDR300                                                          
         BCTR  R1,0                                                             
         LTR   R1,R1               ARE WE DOWN TO ZERO                          
         BZ    BLDR035                                                          
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     BLDR031                                                          
*                                                                               
BLDR035  SR    R1,R1                                                            
         IC    R1,1(R7)                                                         
         BAS   RE,SHOWIT                                                        
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BNE   BLDR035                                                          
*                                                                               
BLDR300  LA    R6,87(R6)           BUMP TO NEXT FIELD                           
         LA    R1,SRVPFK1                                                       
         CR    R6,R1                                                            
         BNL   XIT1                                                             
*                                                                               
         BAS   RE,READNXT          GET NEXT RECORD                              
         B     BLDR010             NEXT IF NOT END OF SCREEN                    
*                                                                               
BLDR980  MVC   0(26,R6),=C'NOT VALID RECOVERY RECORD '                          
*                                                                               
BLDR990  B     XIT1                                                             
*                                                                               
SHOWIT   ST    RE,SAVERE           DISPLAY DATA ON SCREEN                       
         SLL   R1,1                                                             
         CR    R1,R5               WILL IT FIT                                  
         BL    *+6                                                              
         LR    R1,R5               SHOW AS MUCH AS WILL                         
         SRL   R1,1                                                             
         LTR   R1,R1               WILL ANY DATA FIT                            
         BZ    BLDR300                                                          
*                                                                               
         ST    R1,DMCB+8           SAVE IN P3                                   
         SLL   R1,1                                                             
         SR    R5,R1               SET REMAINING LENGTH                         
         STH   R1,HALF                                                          
         GOTO1 AHEXOUT,DMCB,(R7),(R4)                                           
         AH    R4,HALF                                                          
         SHI   R5,2                LESS TWO                                     
         BNP   BLDR300                                                          
         MVC   0(2,R4),=C'  '      FOR TWO SPACES                               
         LA    R4,2(R4)                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        BUILD SCREEN (HEX FORMAT)                          *                   
*************************************************************                   
BLDHEX   NTR1                                                                   
         LA    R6,SRVLIN1          R6=FIELD                                     
         SR    R0,R0                                                            
         BAS   RE,HEADER           BUILD HEADER                                 
*                                                                               
         LA    R6,87(R6)           BUMP TO NEXT FIELD                           
         CLI   FORMAT,C'M'                                                      
         BNE   *+8                                                              
         LA    R6,87(R6)           AND AGAIN FOR MIX                            
         BAS   RE,GET              GET FIRST RECORD                             
*                                                                               
BLDH010  MVC   0(78,R6),SPACES     FILL LINE WITH SPACES                        
*                                                                               
         L     RF,CXREC            CALCULATE MOVE LEN                           
         CLC   CXREC(4),EOF                                                     
         BE    BLDH990                                                          
         SRL   RF,16                                                            
         SH    RF,=H'3'                                                         
         S     RF,COLUMN                                                        
         BP    BLDH011                                                          
         CLI   FORMAT,C'M'         IF MIXED                                     
         BNE   BLDH020                                                          
         SH    R6,=H'87'           BUMP BACK AND SKIP                           
         B     BLDH020             WHOLE LINE IF -VE                            
*                                                                               
BLDH011  CH    RF,=H'39'           39 IS MAXIMUM LEN                            
         BNH   *+8                                                              
         LA    RF,39               FORCE 39                                     
*                                                                               
         L     R1,COLUMN                                                        
         LA    R1,3(R1)            ADD 4 FOR LEN -1 FOR COLUMN                  
         LA    R1,CXREC(R1)        INDEX TO RECORD                              
*                                                                               
         STM   RF,R1,SWORK                                                      
         ST    R1,DMCB+0           SOURCE                                       
         ST    R6,DMCB+4           DEST                                         
         ST    RF,DMCB+8           LENGTH                                       
         GOTO1 AHEXOUT,DMCB        HEXOUT RECORD                                
         LM    RF,R1,SWORK                                                      
         CLI   FORMAT,C'M'         TEST FOR MIX                                 
         BNE   BLDH020                                                          
*                                                                               
         SH    R6,=H'87'           BUMP BACK TO LAST FIELD                      
         ST    R6,SFULL                                                         
BLDH015  MVC   0(1,R6),0(R1)                                                    
         MVI   1(R6),C' '          SPACE OUT FOR MIX                            
         LA    R6,2(R6)                                                         
         LA    R1,1(R1)                                                         
         BCT   RF,BLDH015          DO (RF) TIMES                                
         L     R6,SFULL            RESTORE R6                                   
         TR    0(78,R6),VALOCHRS   AND TRANSLATE                                
*                                                                               
BLDH020  LA    R6,87(R6)           BUMP TO NEXT FIELD                           
         CLI   FORMAT,C'M'                                                      
         BNE   *+12                                                             
         LA    R6,87(R6)           AND AGAIN                                    
         LA    R6,87(R6)           AND AGAIN                                    
         LA    R1,SRVPFK1                                                       
         CR    R6,R1                                                            
         BNL   XIT1                                                             
*                                                                               
         BAS   RE,READNXT          GET NEXT RECORD                              
         B     BLDH010             NEXT IF NOT END OF SCREEN                    
*                                                                               
BLDH990  B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD SCREEN (DUMP FORMAT)                         *                   
*************************************************************                   
BLDDMP   NTR1                                                                   
         LA    R6,SRVLIN1          R6=FIELD                                     
         TWAXC (R6),PROT=N                                                      
         SR    R0,R0                                                            
         MVC   0(78,R6),DMPHDR     BUILD HEADER                                 
         LA    R6,14(R6)                                                        
         BAS   RE,HEADER                                                        
*                                                                               
         LA    R6,73(R6)           BUMP TO NEXT FIELD                           
         BAS   RE,GET              GET FIRST RECORD                             
*                                                                               
         LH    R1,WKSDIS1          PUT INITIAL DISP INTO SFULL                  
         ST    R1,SFULL                                                         
*                                                                               
BLDD000  L     RF,CXREC            CALCULATE MOVE LEN                           
         CLC   CXREC(4),EOF                                                     
         BE    BLDD990                                                          
         SRL   RF,16                                                            
         OC    DEND,DEND                                                        
         BZ    *+16                                                             
         C     RF,DEND             USE DEND IF >0 AND <REAL LEN                 
         BL    *+8                                                              
         L     RF,DEND                                                          
         ST    RF,COLUMN           SAVE IN COLUMN                               
*                                                                               
         OC    SFULL,SFULL         DO WE HAVE A DISP ALREADY                    
         BNZ   *+10                                                             
         MVC   SFULL,DSTART        PUT START INTO SFULL                         
*                                                                               
BLDD010  MVC   0(11,R6),SPACES     FILL LINE WITH SPACES                        
         MVC   19(40,R6),SPACES                                                 
         MVC   67(20,R6),SPACES                                                 
         CLI   WKACT,X'32'         PROTECT DATA IF DISPLAY                      
         BE    *+8                                                              
         OI    12(R6),X'20'                                                     
*                                                                               
         L     RF,COLUMN                                                        
         SH    RF,=H'4'                                                         
         S     RF,SFULL                                                         
         BNP   BLDD040             NEXT REC IF -VE                              
*                                                                               
         CH    RF,=H'20'           20 IS MAXIMUM LEN                            
         BNH   *+8                                                              
         LA    RF,20               FORCE 20                                     
*                                                                               
         L     R1,SFULL                                                         
         LA    R1,4(R1)            ADD 4 FOR LEN                                
         LA    R1,CXREC(R1)        INDEX TO RECORD                              
*                                                                               
         STM   RF,R1,SWORK                                                      
         ST    R1,DMCB+0           SOURCE                                       
         LA    R1,19(R6)                                                        
         ST    R1,DMCB+4           DEST                                         
         ST    RF,DMCB+8           LENGTH                                       
         GOTO1 AHEXOUT,DMCB        HEXOUT RECORD                                
         LM    RF,R1,SWORK                                                      
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8              EXECUTE THIS MOVE                            
         B     *+10                                                             
         MVC   67(0,R6),0(R1)      MOVE CHRS OUT                                
         TR    67(20,R6),VALOCHRS  AND TRANSLATE                                
*                                                                               
         L     R1,SFULL            PRINT OFFSET ON LEFT                         
         BAS   RE,EDIT1                                                         
         MVC   0(5,R6),SWORK                                                    
         LA    R1,19                                                            
         A     R1,SFULL                                                         
         CLC   SFULL,DSTART                                                     
         BNE   *+8                                                              
         MVI   5(R6),C'-'          START INDICATOR                              
         BAS   RE,EDIT1                                                         
         MVC   6(5,R6),SWORK                                                    
         AH    R1,=H'1'                                                         
         ST    R1,SFULL                                                         
*                                                                               
BLDD020  LA    R6,95(R6)           BUMP TO NEXT FIELD                           
         LA    R1,SRVPFK2                                                       
         CR    R6,R1                                                            
         BL    BLDD030                                                          
*                                                                               
         MVC   WKSREC2,RECNUM2+2   SAVE REC/DISP FOR NEXT SCREEN                
         MVC   WKSDIS2,SFULL+2                                                  
         CLC   SFULL,COLUMN        TEST END OF THIS REC                         
         BL    XIT1                                                             
         SR    R1,R1               DISP ZERO NEXT REC                           
         ICM   R1,3,WKSREC2                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,WKSREC2                                                     
         XC    WKSDIS2,WKSDIS2                                                  
         B     XIT1                                                             
*                                                                               
BLDD030  CLC   SFULL,COLUMN                                                     
         BL    BLDD010                                                          
*                                                                               
BLDD040  CLI   WKACT,X'32'         ONE RECORD ONLY FOR CHANGE                   
         BNE   BLDD050                                                          
         BE    BLDD990                                                          
*                                                                               
BLDD050  BAS   RE,READNXT          GET NEXT RECORD                              
         XC    SFULL,SFULL         CLEAR SFULL                                  
         B     BLDD000             NEXT IF NOT END OF SCREEN                    
*                                                                               
BLDD990  B     XIT1                EOF                                          
         EJECT                                                                  
*************************************************************                   
*        VALIDATE DUMP SCREEN FOR CHANGE                    *                   
*************************************************************                   
VALDMP   NTR1                                                                   
         LA    R6,SRVLIN1          R6=FIELD                                     
         SR    R0,R0                                                            
*                                                                               
         LA    R6,73(R6)           BUMP TO NEXT FIELD                           
         BAS   RE,GET              GET FIRST RECORD                             
*                                                                               
VALD000  L     RF,CXREC            CALCULATE MOVE LEN                           
         CLC   CXREC(4),EOF                                                     
         BE    VALD990                                                          
         SRL   RF,16                                                            
         OC    DEND,DEND                                                        
         BZ    *+16                                                             
         C     RF,DEND             USE DEND IF >0 AND <REAL LEN                 
         BL    *+8                                                              
         L     RF,DEND                                                          
         ST    RF,COLUMN           SAVE IN COLUMN                               
*                                                                               
         MVC   SFULL,DSTART        PUT START INTO SFULL                         
*                                                                               
VALD010  L     RF,COLUMN                                                        
         SH    RF,=H'4'                                                         
         S     RF,SFULL                                                         
         BNP   VALD990             EXIT IF -VE                                  
*                                                                               
         CH    RF,=H'20'           20 IS MAXIMUM LEN                            
         BNH   *+8                                                              
         LA    RF,20               FORCE 20                                     
*                                                                               
         L     R1,SFULL                                                         
         LA    R1,4(R1)            ADD 4 FOR LEN                                
         LA    R1,CXREC(R1)        INDEX TO RECORD                              
*                                                                               
         STM   RF,R1,SWORK                                                      
         ST    R1,DMCB+4           DEST                                         
         LA    R1,14+11+8(R6)                                                   
         ST    R1,DMCB+0           SOURCE                                       
         SLL   RF,1                                                             
         ST    RF,DMCB+8           LENGTH                                       
         GOTO1 AHEXIN,DMCB         HEXIN DATA                                   
         LM    RF,R1,SWORK                                                      
*                                                                               
*NOP     BCTR  RF,0                                                             
*        EX    RF,*+8              EXECUTE THIS MOVE                            
*        B     *+10                                                             
*        MVC   67(0,R6),0(R1)      MOVE CHRS OUT                                
*        TR    67(20,R6),VALOCHRS  AND TRANSLATE                                
*                                                                               
         L     R1,SFULL            PRINT OFFSET ON LEFT                         
         LA    R1,20(R1)                                                        
         ST    R1,SFULL                                                         
*                                                                               
VALD020  LA    R6,95(R6)           BUMP TO NEXT FIELD                           
         LA    R1,SRVPFK2                                                       
         CR    R6,R1                                                            
         BL    VALD030                                                          
         B     VALD990                                                          
*                                                                               
VALD030  CLC   SFULL,COLUMN                                                     
         BL    VALD010                                                          
*                                                                               
VALD990  BAS   RE,PUT              WRITE RECORD TO FILE                         
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        LOADING SCREEN HAS LOST OLD INPUT. GO FIND IT      *                   
*        **NOTE** VERY HARD CODE, CRUDE BUT EFFECTIVE       *                   
*************************************************************                   
REMERGE  NTR1                                                                   
         LR    R6,R3               R6=TWA                                       
         L     R1,AUTL                                                          
         L     R1,TBUFF-UTLD(R1)   R1=TBUFF                                     
*                                                                               
         LA    R6,64(R6)           FIRST FIELD                                  
         LA    R1,10(R1)           FIRST FIELD                                  
*                                                                               
REM010   TM    1(R6),X'20'         SKIP PROTECTEDS                              
         BO    REM090                                                           
         LA    RF,8(R6)                                                         
*                                                                               
REM020   CLI   0(R1),X'11'         CHECK FOR SBA                                
         BE    REM050                                                           
         CLI   0(R1),X'03'         TEST FOR END                                 
         BE    XIT1                                                             
         MVC   0(1,RF),0(R1)       MERGE INPUT                                  
         OI    0(RF),X'40'         FORCE UPPER CASE                             
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         B     REM020                                                           
*                                                                               
REM050   LA    R1,3(R1)            NEXT FIELD                                   
         CLI   0(R1),X'03'                                                      
         BE    XIT1                EXIT AFTER LAST                              
*                                                                               
REM090   SR    R0,R0               NEXT FIELD                                   
         ICM   R0,1,0(R6)                                                       
         BZ    XIT1                                                             
         AR    R6,R0                                                            
         B     REM010                                                           
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        READ RANDOM FOR RECORD AT RECNUM                   *                   
*************************************************************                   
GET      NTR1                                                                   
         TM    FLAG,X'80'          TEST EOF FOUND                               
         BO    WKCHK000                                                         
         MVC   RECNUM2,RECNUM      SET CURRENCT RECORD NUMBER                   
         XC    CXREC(12),CXREC                                                  
         MVC   CXREC(4),RECNUM                                                  
         MVC   CXREC+4(4),=C'REC '                                              
         GOTO1 VDATAMGR,WKDMCB,(X'00',RANDOM)                                   
         B     WKCHECK                                                          
         EJECT                                                                  
*************************************************************                   
*        READ RANDOM FOR RECORD AT RECNUM                   *                   
*************************************************************                   
PUT      NTR1                                                                   
         GOTO1 VDATAMGR,WKDMCB,=C'WRITE'                                        
         B     WKCHECK                                                          
         EJECT                                                                  
*************************************************************                   
*        READ SEQUENTIAL FOR RECORD AT RECNUM               *                   
*************************************************************                   
READNXT  NTR1                                                                   
         TM    FLAG,X'80'          TEST EOF FOUND                               
         BO    WKCHK000                                                         
         L     RF,RECNUM2          BUMP CURRENT RECORD NUMBER                   
         LA    RF,1(RF)                                                         
         ST    RF,RECNUM2                                                       
*        OI    NXFLAG,X'80'        SET FILE READ                                
         GOTO1 VDATAMGR,WKDMCB,(X'00',READ)                                     
         B     WKCHECK                                                          
*                                                                               
WKCHECK  CLI   8(R1),X'90'         TEST EOF                                     
         BNE   WKCHK010                                                         
         OI    FLAG,X'80'          FLAG EOF FOUND                               
WKCHK000 MVC   CXREC(4),EOF        DUMMY EOF RECORD                             
         B     XIT1                                                             
*                                                                               
WKCHK010 CLI   8(R1),X'41'         FORMAT ERROR                                 
         BE    ERR7                                                             
         CLI   8(R1),0             ANYTHING ELSE                                
         BE    XIT1                                                             
         DC    H'0'                ALL OTHER ERRORS ARE DEATH                   
         EJECT                                                                  
*************************************************************                   
*        BUILD COLUMN HEADER R6=A(FIELD)                    *                   
*************************************************************                   
HEADER   NTR1                                                                   
         L     R1,COLUMN           USE COLUMN AS A REF                          
         CLI   FORMAT,C'D'                                                      
         BNE   *+8                                                              
         L     R1,DSTART           IF DUMP USE DSTART                           
         CLI   FORMAT,C'R'                                                      
         BNE   *+8                                                              
         LA    R1,1                IF RCVR USE 1                                
*                                                                               
         D     R0,=F'100'                                                       
         CVD   R0,DUB                                                           
         LR    R1,R6               R1=A(FILED)                                  
*                                                                               
HDR001   CLC   DUB+7(1),=X'5C'     INSERT A + ON EACH 5TH                       
         BE    HDRPLS                                                           
         CLC   DUB+7(1),=X'0C'     INSERT A NUMBER ON 10TH                      
         BE    HDRDIG                                                           
HDRDAS   MVI   0(R1),C'-'          INSERT A - ON ALL OTHERS                     
         B     HDRNXT                                                           
HDRPLS   MVI   0(R1),C'+'          INSERT +                                     
         B     HDRNXT                                                           
HDRDIG   MVC   0(1,R1),DUB+6       INSERT DIGIT                                 
         OI    0(R1),X'F0'                                                      
*                                                                               
HDRNXT   AP    DUB,=P'1'           BUMP COLUMN                                  
         CLI   FORMAT,C'C'         BUMP 1 FOR CHR                               
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R1,1(R1)                                                         
         LR    R0,R1                                                            
         SR    R0,R6                                                            
         CLI   FORMAT,C'D'                                                      
         BE    *+12                                                             
         CH    R0,=H'78'           TEST END OF FIELD                            
         B     *+8                                                              
         CH    R0,=H'40'           TEST END OF FIELD                            
         BL    HDR001                                                           
         B     XIT1                EXIT                                         
         EJECT                                                                  
*************************************************************                   
*        MISCELANEOUS ROUTINES                              *                   
*************************************************************                   
EDIT     MVC   SWORK,SPACES                                                     
         EDIT  (R1),(6,SWORK),ZERO=NOBLANK,ALIGN=LEFT                           
         BR    RE                  R0=LEN ON EXIT                               
EDIT1    MVC   SWORK,SPACES                                                     
         EDIT  (R1),(5,SWORK),FILL=0                                            
         BR    RE                  R0=LEN ON EXIT                               
         EJECT                                                                  
*************************************************************                   
*        GET SENAME FROM SE                                 *                   
*************************************************************                   
GETSYS   NTR1                                                                   
         L     R4,VSELIST          MUST HAVE SYSFACS                            
         LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         USING SELISTD,R4                                                       
GETSYS1  CLC   BYTE,SESYS          TEST SE NUMBER                               
         BE    GETSYS2                                                          
         BXLE  R4,R0,GETSYS1       NEXT                                         
         MVI   BYTE,0                                                           
         XC    WORK,WORK                                                        
         B     XIT1                ERROR EXIT NOT FOUND                         
*                                                                               
GETSYS2  MVC   WORK(8),SENAME      SET NAME                                     
         B     XIT1                                                             
         DROP  R4                                                               
         EJECT                                                                  
ERR1     LA    RE,SREIIF           INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    RE,299              INVALID FORMAT                               
         B     ERRX                                                             
ERR3     LA    RE,3                MUST BE NUMERIC                              
         B     ERRX                                                             
ERR4     LA    RE,282              INVALID FILE ID                              
         B     ERRX                                                             
ERR5     LA    RE,24               INVALID OPTION                               
         B     ERRX                                                             
ERR6     LA    RE,19               INVALID FIND STRING                          
         B     ERRX                                                             
ERR7     LA    RE,298              FORMAT ERROR                                 
         B     ERRX                                                             
ERRX     MVI   MSGTYP,C'E'         SET ERROR TYPE                               
         STCM  RE,3,MSGNUM         SET NUMBER                                   
         L     RD,PHASERD          RESTORE RD                                   
         ST    R4,CURSOR                                                        
         B     EXIT                                                             
         EJECT                                                                  
EXIT     XMOD1 1                                                                
*                                                                               
XIT1     XIT1                                                                   
         EJECT                                                                  
*DMPRTQR                                                                        
       ++INCLUDE DMWRKRR                                                        
         EJECT                                                                  
SVAFILTAB DC   A(0)                                                             
RANDOM   DC    CL8'RANDOM'                                                      
BUFFER   DC    CL8'BUFFER'                                                      
READ     DC    CL8'READ  '                                                      
         SPACE 2                                                                
I151     DC    C'I',AL2(151)       CHARS &T NOT FOUND (EOR)                     
I152     DC    C'I',AL2(152)       CHARS &T NOT FOUND (1000 LINES)              
I153     DC    C'I',AL2(153)       CHARS &T FOUND                               
I191     DC    C'I',AL2(191)       RECORDS &1 THRU &2 LISTED                    
I190     DC    C'I',AL2(190)       END OF FILE                                  
         EJECT                                                                  
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
         SPACE 1                                                                
FORMTAB  DC    CL7'CHR    ',C'C'                                                
         DC    CL7'CHAR   ',C'C'                                                
         DC    CL7'HEX    ',C'H'                                                
         DC    CL7'MIX    ',C'M'                                                
         DC    CL7'DUMP   ',C'D'                                                
         DC    CL7'RCVR   ',C'R'                                                
         DC    X'00'                                                            
FORMT2   DC    C'CHMDRC0'          TAB FOR PF9 SWAP                             
*                                                                               
COLTAB   DC    C'0HKR0'            COLUMN CHR VALUES                            
*                                                                               
         SPACE 1                                                                
RCVRTAB  DC    X'01',C'CPY'                                                     
         DC    X'02',C'CHA'                                                     
         DC    X'03',C'ADD'                                                     
         DC    X'00'                                                            
         SPACE 1                                                                
SPACES   DC    80C' '                                                           
EOF      DC    XL4'FFFFFFFF'                                                    
DMPHDR   DC    C'---BYTES---                                 '                  
         DC    C'            *****CHARACTERS*****   '                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*FWKDD                                                                          
       ++INCLUDE SRFWKDD                                                        
         EJECT                                                                  
*SRFWKWK                                                                        
       ++INCLUDE SRFWKWK                                                        
         EJECT                                                                  
SRFWKFFD DSECT                                                                  
         DS    CL64                                                             
*SRFWKFFD                                                                       
       ++INCLUDE SRFWKFFD                                                       
*                                                                               
         ORG   SRVEX2H                                                          
*SRFWKFED                                                                       
       ++INCLUDE SRFWKFED                                                       
*                                                                               
         ORG   SRVEX2H                                                          
*SRFWKFCD                                                                       
       ++INCLUDE SRFWKFCD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
*FASYSFAC                                                                       
*DDCOMFACS                                                                      
*DDFLDHDR                                                                       
*SRERREQUS                                                                      
*FAUTL                                                                          
*DMFILTABD                                                                      
*FASELIST                                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE DMFILTABD                                                      
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099SRFWK03   03/02/18'                                      
         END                                                                    
