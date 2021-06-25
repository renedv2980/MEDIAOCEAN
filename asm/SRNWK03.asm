*          DATA SET SRNWK03    AT LEVEL 024 AS OF 09/27/19                      
*PHASE T13503A                                                                  
         TITLE '$WK - BROWSE FUNCTIONS'                                         
         PRINT NOGEN                                                            
WKBROWSE CSECT                                                                  
         NMOD1 000,**$WK3**,RA,RR=R4                                            
         LR    RC,R1                                                            
         USING NWKWKD,RC                                                        
         ST    RD,PHASERD                                                       
         ST    R4,RELO                                                          
         L     R1,APARM                                                         
         L     R2,ASAVESTR                                                      
         USING WKSAVED,R2                                                       
         L     R3,20(R1)                                                        
         USING SRNWKFFD,R3         R3=A(TWA)                                    
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
         TM    INTFLAG,INTRUN      TEST INTERNAL CALL                           
         BZ    P3VAL                                                            
         XC    SRVP3,SRVP3         CLEAR P3,P4                                  
         XC    SRVP4,SRVP4                                                      
         MVI   SRVP3H+5,0                                                       
         MVI   SRVP4H+5,0                                                       
                                                                                
*----------------------------------------------------------------------         
* VALIDATE RECORD,COLUMN                                                        
*----------------------------------------------------------------------         
P3VAL    LA    R1,1                SET DEFAULTS                                 
         ST    R1,COLUMN                                                        
         ST    R1,RECNUM                                                        
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
         TM    2(R6),X'80'         MUST BE NUMERIC                              
         BNO   ERR1                                                             
         MVC   COLUMN,4(R6)        SET COLUMN NUMBER                            
*                                                                               
P3VX     EQU   *                                                                
                                                                                
*----------------------------------------------------------------------         
* VALIDATE FORMAT                                                               
*----------------------------------------------------------------------         
P4VAL    CLI   WKACT,X'32'         TEST FOR ACTION CHANGE                       
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
                                                                                
*----------------------------------------------------------------------         
* VALIDATE OPTIONS                                                              
*----------------------------------------------------------------------         
OPVAL    XC    WORK,WORK           USE WORK AS FIND STRING AREA                 
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
                                                                                
*----------------------------------------------------------------------         
* GET CIADDR LOAD SCREEN AND INIT BUFFER                                        
*----------------------------------------------------------------------         
BROW000  OC    CIPASS,CIPASS       DO WE HAVE A DA ALREADY                      
         BNZ   BROW010                                                          
         OC    WKFILEN,WKFILEN                                                  
         BZ    ERR4                                                             
*                                                                               
         SAM31                                                                  
         MVC   FIWRES,WRKFID                                                    
         BRAS  RE,FIRSET                                                        
         XR    RE,RE                                                            
         ICM   RE,B'0011',WKFILEN                                               
         ST    RE,FIWREF                                                        
         BRAS  RE,FIRRC                                                         
         MVC   CIADDR,FIWCIA                                                    
         SAM24                                                                  
*                                                                               
         MVC   CIRSN,WKFILEN       CALCULATE FROM WKFILEN                       
         MVC   CIPASS,CIADDR                                                    
*                                                                               
BROW010  MVI   SCREENR,X'FE'       GET FE SCREEN                                
         CLI   FORMAT,C'D'                                                      
         BNE   *+8                                                              
         MVI   SCREENR,X'FC'       GET FC FOR DUMP                              
         GOTO1 ALOADSCR                                                         
*                                                                               
         L     R5,ACXREC           INIT BUFFER                                  
         USING W_RECD,R5                                                        
         GOTO1 VDATAMGR,WKDMCB,(X'00',BUFFER),WRKFID,NDX,ACTREC,(R5)            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CIADDR,CIPASS                                                    
         L     RE,8(R5)            SET DISK ADDR IN SAVE AREA                   
         AR    RE,R5                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKINTNO-SKBUFFD(1,RE),CFWKINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUWFFILE                                   
*                                                                               
         XC    CTREC(12),CTREC     READ HEADER REC                              
         MVC   CTREC+4(4),=C'REC '                                              
         GOTO1 VDATAMGR,WKDMCB,(X'00',RANDOM)                                   
         MVC   BYTE1,W_ATTB        SAVE ATTB FOR OBJECT CHECK                   
         MVC   FULL1,W_RECS        SAVE MAX RECS FOR PF8 CHECK                  
*                                                                               
         MVC   WKUSER,W_USRID      COPY USERID                                  
         MVC   WKSYSPG(8),W_SYSPRG COPY SPPSDCNN                                
         XC    WKTIMES,WKTIMES     CLEAR TIMES                                  
         GOTO1 ARIDXPND,SRVP2H                                                  
                                                                                
*----------------------------------------------------------------------         
* HANDLE FIND OPTION (WORK HOLDS FIND STRING)                                   
*----------------------------------------------------------------------         
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
DFIND010 L     RF,CTREC                                                         
         LA    R6,CTREC+4          R6=REC                                       
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
         CLC   CTREC(4),EOF                                                     
         BNE   DFIND010                                                         
         MVC   MSGREF,I151                                                      
         B     EXIT                                                             
*                                                                               
DFIND900 MVC   RECNUM,RECNUM2      SET TOP RECORD                               
*                                                                               
         LA    R1,CTREC+4          SET CURSOR POSITION                          
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
                                                                                
*----------------------------------------------------------------------         
* HANDLE FIND OPTION (WORK HOLDS FIND STRING)                                   
*----------------------------------------------------------------------         
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
FIND010  L     RF,CTREC                                                         
         LA    R6,CTREC+4          R6=REC                                       
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
         CLC   CTREC(4),EOF                                                     
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
         LA    R1,CTREC+4          SET CURSOR COLUMN                            
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
                                                                                
*----------------------------------------------------------------------         
* HANDLE PFKEYS SET NEW LINE,COLUMN                                             
*----------------------------------------------------------------------         
PFK000   CLI   FORMAT,C'D'                                                      
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
PFK030   CLI   PFKEY,10            PF10=LEFT                                    
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
                                                                                
*----------------------------------------------------------------------         
* PFKEY HANDLING FOR DUMP FORMAT                                                
*----------------------------------------------------------------------         
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
         L     RF,CTREC                                                         
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
         SPACE 1                                                                
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
                                                                                
*----------------------------------------------------------------------         
* DISPLAY DATA ON SCREEN                                                        
*----------------------------------------------------------------------         
DSP000   CLI   FORMAT,C'C'         FORMAT=CHR                                   
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
         OC    WKSDUMP3(10),SPACES                                              
         CLC   SRVP3(10),WKSDUMP3  SAME RECORD AS PREV                          
         BNE   DSP021                                                           
         BAS   RE,REMERGE          GO FIND OLD INPUT                            
         BAS   RE,VALDMP           VALIDATE DUMP SCREEN                         
DSP021   BAS   RE,BLDDMP           BUILD SCREEN                                 
         MVC   WKSDUMP3,SRVP3      SAVE RECORD NUM                              
         B     DSP990              OUTPUT MESSAGE                               
*                                                                               
DSP030   CLI   FORMAT,C'M'                                                      
         BNE   DSP040                                                           
         BAS   RE,BLDHEX           BUILD SCREEN                                 
         B     DSP990              OUTPUT MESSAGE                               
*                                                                               
DSP040   DC    H'0'                UNKNOWN FORMAT                               
*                                                                               
DSP990   L     R1,RECNUM           REDISPLAY RECNUM & COLUMN                    
         BAS   RE,EDIT                                                          
         MVC   SRVP3,SWORK                                                      
         LA    RF,SRVP3                                                         
         AR    RF,R0                                                            
*                                                                               
         CLI   FORMAT,C'D'         DONT SHOW COL ON DUMP SCREEN                 
         BE    DSP991                                                           
         CLC   COLUMN,=F'1'        IF COL=1 DON'T BOTHER                        
         BE    DSP991                                                           
         MVI   0(RF),C','                                                       
         L     R1,COLUMN                                                        
         BAS   RE,EDIT                                                          
         MVC   1(6,RF),SWORK                                                    
*                                                                               
DSP991   CLC   CTREC,EOF           TEST EOF FOUND                               
         BNE   DSP992                                                           
         MVC   MSGREF,I190         END OF FILE                                  
         B     EXIT                                                             
*                                                                               
DSP992   MVC   MSGREF,I191                                                      
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
                                                                                
***********************************************************************         
* BUILD SCREEN (CHR FORMAT)                                                     
***********************************************************************         
BLDCHR   NTR1                                                                   
*                                                                               
         LA    R6,SRVLIN1          R6=FIELD                                     
         SR    R0,R0                                                            
         BAS   RE,HEADER           BUILD HEADER                                 
*                                                                               
         LA    R6,87(R6)           BUMP TO NEXT FIELD                           
         BAS   RE,GET              GET FIRST RECORD                             
*                                                                               
BLDC010  MVC   0(78,R6),SPACES     FILL LINE WITH SPACES                        
*                                                                               
         L     RF,CTREC            CALCULATE MOVE LEN                           
         CLC   CTREC(4),EOF                                                     
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
         LA    R1,CTREC(R1)        INDEX TO RECORD                              
*                                                                               
         EX    RF,*+8              EXECUTE THIS MOVE                            
         B     *+10                                                             
         MVC   0(0,R6),0(R1)       MOVE CHRS OUT                                
         TR    0(78,R6),VALOCHRS   AND TRANSLATE                                
*                                                                               
         CLC   COLUMN,=F'1'        SHOW OBJECT CODES                            
         BNE   BLDC020                                                          
         TM    BYTE1,W_ATOBJ       IF COL=1 AND ATTB=OBJ                        
         BZ    BLDC020                                                          
         EDIT  (B4,0(R1)),(4,0(R6)),FILL=0                                      
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
                                                                                
***********************************************************************         
* BUILD SCREEN (HEX FORMAT)                                                     
***********************************************************************         
BLDHEX   NTR1                                                                   
*                                                                               
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
         L     RF,CTREC            CALCULATE MOVE LEN                           
         CLC   CTREC(4),EOF                                                     
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
         LA    R1,CTREC(R1)        INDEX TO RECORD                              
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
                                                                                
***********************************************************************         
* BUILD SCREEN (DUMP FORMAT)                                                    
***********************************************************************         
BLDDMP   NTR1                                                                   
*                                                                               
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
BLDD000  L     RF,CTREC            CALCULATE MOVE LEN                           
         CLC   CTREC(4),EOF                                                     
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
         BE    *+12                                                             
         OI    12(R6),X'20'                                                     
         B     *+8                                                              
         OI    17(R6),X'01'        MODIFY IF CHANGE                             
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
         LA    R1,CTREC(R1)        INDEX TO RECORD                              
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
                                                                                
***********************************************************************         
* VALIDATE DUMP SCREEN FOR CHANGE                                               
***********************************************************************         
VALDMP   NTR1                                                                   
*                                                                               
         LA    R6,SRVLIN1          R6=FIELD                                     
         SR    R0,R0                                                            
*                                                                               
         LA    R6,73(R6)           BUMP TO NEXT FIELD                           
         BAS   RE,GET              GET FIRST RECORD                             
*                                                                               
VALD000  L     RF,CTREC            CALCULATE MOVE LEN                           
         CLC   CTREC(4),EOF                                                     
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
         LA    R1,CTREC(R1)        INDEX TO RECORD                              
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
VALD990  SR    R1,R1               THIS IS A FIX FOR A BUG THAT SRFWK           
         ICM   R1,3,CTREC          DOESN'T HAVE EVEN THOUGH THE CODE            
         BZ    XIT1                IS SO SIMILAR.                               
         SHI   R1,5                THIS IS TO STOP RECORD 1 GETTING             
         EX    R1,*+8              ZAP'D TO ZERO FIRST TIME IN TO CHA           
         B     *+10                BY PREVENTING THE WRITE OF A                 
         OC    CTREC+4(0),CTREC+4  COMPLETELY ZERO RECORD WHICH IS A            
         BZ    XIT1                HELP. ALTHOUGH NOT PERFECT                   
*                                                                               
         BAS   RE,PUT              WRITE RECORD TO FILE                         
         B     XIT1                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* LOADING SCREEN HAS LOST OLD INPUT. GO FIND IT                                 
* **NOTE** VERY HARD CODE, CRUDE BUT EFFECTIVE                                  
***********************************************************************         
REMERGE  NTR1                                                                   
*                                                                               
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
                                                                                
***********************************************************************         
* READ RANDOM FOR RECORD AT RECNUM                                              
***********************************************************************         
GET      NTR1                                                                   
*                                                                               
         TM    FLAG,X'80'          TEST EOF FOUND                               
         BO    WKCHK000                                                         
         MVC   RECNUM2,RECNUM      SET CURRENCT RECORD NUMBER                   
         XC    CTREC(12),CTREC                                                  
         MVC   CTREC(4),RECNUM                                                  
         MVC   CTREC+4(4),=C'REC '                                              
         GOTO1 VDATAMGR,WKDMCB,(X'00',RANDOM)                                   
         B     WKCHECK                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ RANDOM FOR RECORD AT RECNUM                                              
***********************************************************************         
PUT      NTR1                                                                   
*                                                                               
         GOTO1 VDATAMGR,WKDMCB,=C'WRITE'                                        
         B     WKCHECK                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ SEQUENTIAL FOR RECORD AT RECNUM                                          
***********************************************************************         
READNXT  NTR1                                                                   
*                                                                               
         TM    FLAG,X'80'          TEST EOF FOUND                               
         BO    WKCHK000                                                         
         L     RF,RECNUM2          BUMP CURRENT RECORD NUMBER                   
         LA    RF,1(RF)                                                         
         ST    RF,RECNUM2                                                       
         OI    NXFLAG,X'80'        SET FILE READ                                
         GOTO1 VDATAMGR,WKDMCB,(X'00',READ)                                     
         B     WKCHECK                                                          
*                                                                               
WKCHECK  CLI   8(R1),X'80'         TEST EOF                                     
         BE    *+12                                                             
         CLI   8(R1),X'90'         TEST EOF                                     
         BNE   WKCHK010                                                         
*                                                                               
         OI    FLAG,X'80'          FLAG EOF FOUND                               
WKCHK000 MVC   CTREC(4),EOF        DUMMY EOF RECORD                             
         B     XIT1                                                             
*                                                                               
WKCHK010 CLI   8(R1),X'41'         FORMAT ERROR                                 
         BE    ERR7                                                             
         CLI   8(R1),0             ANYTHING ELSE                                
         BE    XIT1                                                             
         DC    H'0'                ALL OTHER ERRORS ARE DEATH                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD COLUMN HEADER R6=A(FIELD)                                               
***********************************************************************         
HEADER   NTR1                                                                   
*                                                                               
         L     R1,COLUMN           USE COLUMN AS A REF                          
         CLI   FORMAT,C'D'                                                      
         BNE   *+8                                                              
         L     R1,DSTART           IF DUMP USE DSTART                           
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
                                                                                
***********************************************************************         
* MISCELANEOUS ROUTINES                                                         
***********************************************************************         
EDIT     MVC   SWORK,SPACES                                                     
         EDIT  (R1),(6,SWORK),ZERO=NOBLANK,ALIGN=LEFT                           
         BR    RE                  R0=LEN ON EXIT                               
EDIT1    MVC   SWORK,SPACES                                                     
         EDIT  (R1),(5,SWORK),FILL=0                                            
         BR    RE                  R0=LEN ON EXIT                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
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
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
XIT1     XIT1                                                                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE SHFIR                                                          
*                                                                               
         EJECT                                                                  
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
         DC    X'00'                                                            
FORMT2   DC    C'CHMDC0'           TAB FOR PF9 SWAP                             
         SPACE 1                                                                
SPACES   DC    80C' '                                                           
EOF      DC    XL4'FFFFFFFF'                                                    
DMPHDR   DC    C'---BYTES---                                 '                  
         DC    C'            *****CHARACTERS*****   '                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*NWKDD                                                                          
       ++INCLUDE SRNWKDD                                                        
         EJECT                                                                  
*SRNWKWK                                                                        
       ++INCLUDE SRNWKWK                                                        
         EJECT                                                                  
SRNWKFFD DSECT                                                                  
         DS    CL64                                                             
*SRNWKFFD                                                                       
       ++INCLUDE SRNWKFFD                                                       
*                                                                               
         ORG   SRVEX2H                                                          
*SRNWKFED                                                                       
       ++INCLUDE SRNWKFED                                                       
*                                                                               
         ORG   SRVEX2H                                                          
*SRNWKFCD                                                                       
       ++INCLUDE SRNWKFCD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
*FASYSFAC                                                                       
*DDCOMFACS                                                                      
*DDFLDHDR                                                                       
*SRERREQUS                                                                      
*FAUTL                                                                          
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE FAUTL                                                          
         IHAASCB LIST=YES                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SRNWK03   09/27/19'                                      
         END                                                                    
