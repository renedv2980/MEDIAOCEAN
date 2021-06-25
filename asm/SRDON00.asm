*          DATA SET SRDON00    AT LEVEL 004 AS OF 11/12/13                      
*PHASE T11200A                                                                  
*INCLUDE SQUASHER                                                               
         TITLE '$DONE - DISCONNECT A TERMINAL FROM FACPAK'                      
         PRINT NOGEN                                                            
DONE     CSECT                                                                  
         NMOD1 SRWORKX-SRWORKD,*$DONE**,RR=R6,CLEAR=YES                         
         USING SRWORKD,RC                                                       
         ST    R1,SAVER1                                                        
         LR    R7,R1               R7=A(SR PARAM LIST)                          
         USING SRPARMD,R7                                                       
         L     RA,SRQATWA          RA=A(TWA)                                    
         USING SRDONFFD,RA                                                      
         L     R8,SRQATIA          R8=A(TIA)                                    
         L     R9,SRQASYSF         R9=A(SYSFACS)                                
         USING SYSFACD,R9                                                       
         L     R4,SRQAUTL          R4=A(COPY OF MY UTL ENTRY)                   
         USING UTLD,R4                                                          
         MVC   MYTNUM,TNUM         SAVE MY TERMINAL NUMBER                      
*                                                                               
         L     RE,VSSB             EXTRACT SSB DATA                             
         MVC   MAXSES,SSBSSMAX-SSBD(RE)                                         
         MVC   RECLEN,SSBTWAL-SSBD(RE)                                          
         MVC   CHKDSP,=Y(CHKPTDSP)                                              
         CLC   RECLEN,=H'14336'    FIX FOR OLD TEMPSTR                          
         BNE   *+10                                                             
         MVC   CHKDSP,=H'12800'    CHECKPOINT STARTS AT 12.5K FOR 1.5K          
         MVC   SYSIDTY,SSBSYSID-SSBD(RE)                                        
         MVC   SYSCHR,SSBSYSN1-SSBD(RE)                                         
         MVC   DMALET,SSBALET-SSBD(RE)                                          
         MVC   SYSFLAG,SSBSYSFL-SSBD(RE)                                        
         MVC   AFACIDTA,SSBAFID-SSBD(RE)                                        
*                                                                               
         L     RE,=V(SQUASHER)     RELOCATE V(SQUASHER)                         
         AR    RE,R6                                                            
         ST    RE,ASQUASH                                                       
         MVC   ATWASVR,VTWASVR                                                  
         ICM   RE,15,=V(TWASVR)    RELOCATE V(TWASVR) IF INCLUDED               
         BZ    *+10                                                             
         AR    RE,R6                                                            
         ST    RE,ATWASVR                                                       
*                                                                               
         SAM31                     GET INTO XA                                  
         MVC   SAVESRV,SRVSREQ                                                  
         TM    TSTAT1,TSTATDDS     TEST IF A DDS TERMINAL                       
         BZ    DONEDISC            NO DISCONNECT MYSELF                         
         CLI   SRVP1H+5,0          YES TEST IF TERM NUM INPUT                   
         BE    DONEDISC            NO DISCONNECT MYSELF                         
         CLC   SRVSREQ+1(4),=C'SWAP'                                            
         BE    DONEDISC            IF SWAP MUST BE MYSELF                       
         EJECT                                                                  
***********************************************************************         
* SRVP1  TERMINAL NUMBER OR LUID ADDRESS                              *         
* SRVP2  ACTION FOR TERMINAL - REMOVE, VIOLATE, CONNECT               *         
***********************************************************************         
DONE1    LA    R2,SRVP1H           P1 MUST BE A VALID TERMINAL NUMBER           
         L     RF,SRQACOMF                                                      
         L     RF,CTERMVAL-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2),TO24=Y                                            
         SAM31                                                                  
         ICM   R4,15,4(R1)         R4=A(INPUT TERMINAL UTL IN XA)               
         BZ    ERROR1                                                           
         MVC   INPTNUM,TNUM                                                     
         CLC   INPTNUM,MYTNUM                                                   
         BE    ERROR1              CANT NAME MYSELF                             
*                                                                               
DONE1A   SR    R1,R1               TEST TO REMOVE TERMINAL                      
         ICM   R1,1,SRVP2H+5                                                    
         BZ    DONE1B                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRVP2(0),=C'REMOVE'                                              
         BNE   DONE1B                                                           
         TM    TSTAT2,TSTATTIP     CANT BE IN PROCESS                           
         BNZ   ERROR3                                                           
         TM    TTYPE2,TTYPEDUM     CANT BE A DUMMY                              
         BNZ   ERROR1                                                           
         B     DONE6               MAKE LUID USELESS                            
*                                                                               
DONE1B   OC    TPRNT,TPRNT         ERROR IF PRINTER                             
         BNZ   ERROR1                                                           
         CLI   SRVP2H+5,0          DEFAULT IS TO RESET CONNECT                  
         BE    DONE4                                                            
*                                                                               
DONE2    LA    R2,SRVP2H           P2 CAN BE TERMINAL OPTION                    
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              CONNECT FOR NORMAL RESET CONNECT             
         B     *+10                                                             
         CLC   8(0,R2),=C'CONNECT'                                              
         BE    DONE4                                                            
         EX    R1,*+8              VIOLATE FOR RESET SECURITY ERROR             
         B     *+10                                                             
         CLC   8(0,R2),=C'VIOLATE'                                              
         BE    DONE5                                                            
         B     ERROR4              INVALID TERMINAL OPTION                      
*                                                                               
DONE4    OC    TUSER,TUSER         CONNECT - MUST BE CONNECTED                  
         BZ    DONEDISC                                                         
         TM    TSTAT2,TSTATTIP     CANT BE IN PROCESS                           
         BNZ   ERROR3                                                           
         B     DONEDISC            GO TO DISCONNECT LOGIC                       
*                                                                               
DONE5    TM    TSTAT1,TSTATSSV     VIOLATE - MUST BE SEC VIOLATOR               
         BZ    *+12                                                             
         NI    TSTAT1,255-TSTATSSV SET NOT SECURITY VIOLATOR                    
         B     DONE5A                                                           
         CLI   TSSVNUM,0           TEST VIOLATE REASON NUMBER                   
         BE    ERROR5                                                           
         NI    TSSVNUM,255-X'80'   SET VIOLATION NON-ACTIVE                     
         CLI   SRVP3,C'A'          TEST SET ACTIVES ONLY IN SRVP3               
         BE    *+12                                                             
         MVI   TSSVNUM,0           CLEAR VIOLATION REASON CODE                  
         B     DONE5A                                                           
         LA    R2,SRVP3H                                                        
         XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(33),=C'Terminal violation set non-active'                 
         B     EXIT                                                             
DONE5A   XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(24),=C'Terminal violation reset'                          
         B     EXIT                                                             
*                                                                               
DONE6    SR    RF,RF                                                            
         ICM   RF,7,TPRNT                                                       
         BZ    *+12                NOT A PRINTER                                
         TM    PRQMODE-PRQD(RF),X'80'                                           
         BO    ERROR6              CAN'T REMOVE IF AUTO-STARTED                 
         MVI   TSYM,C'*'           REMOVE - CHANGE LUID TO UNUSED NAME          
*                                                                               
         CLI   TSYM+7,C'E'                                                      
         BNE   *+8                                                              
         MVI   TSYM+7,C'*'                                                      
         EJECT                                                                  
***********************************************************************         
*OUTPUT MESSAGE AND TEST FOR =SWAP                                    *         
***********************************************************************         
DONEDISC MVC   CURSES,TSESSION     SAVE SESSION                                 
         MVC   NEWSES,TSESSION                                                  
         CLC   SRVSREQ+1(4),=C'SWAP' TEST FOR SWAP                              
         BE    DONESWAP                                                         
         CLC   SRVSREQ+1(4),=C'DONE' TEST FOR DONE                              
         BE    DONEDIS3                                                         
         ICM   R1,15,TBUFF         NEITHER SO LOOK IN TBUFF                     
         BZ    DONESWPX                                                         
*                                                                               
         LA    R0,64               SCAN FIRST 64 BYTES FOR =DONE =SWAP          
DONESWP1 CLC   0(5,R1),=C'=SWAP'                                                
         BE    DONESWP2                                                         
         CLC   0(5,R1),=C'=DONE'                                                
         BE    DONEDIS3                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,DONESWP1                                                      
         B     DONEDIS3            NOT FOUND SO DISCONNECT                      
*                                                                               
DONESWP2 MVC   SRVSREQ,SPACES                                                   
         MVC   SRVSREQ(11),0(R1)   COPY SR FIELD FROM TBUFF TO TWA              
         B     DONESWAP                                                         
*                                                                               
DONESWPX B     DONEDIS3            FAILED TO DETERMINE =SWAP OR =DONE           
*&&UK                                                                           
DONESWAP CLI   SRVSREQ+5,C','      TEST FOR =SWAP,ADV?                          
         BNE   DONESW1             NO DEFAULT SWAP SYSTEM                       
         LA    RF,SRVSREQ+6        POINT TO ADV SYSTEM NAME                     
         CLC   0(6,RF),=C'FACNEW'  DROP 'FAC' FOR 'FACNEW'                      
         BNE   *+12                                                             
         LA    RF,3(RF)                                                         
         B     DONESW1                                                          
         CLC   0(4,RF),=C'LTST'                                                 
         BNE   *+12                                                             
         LA    RF,1(RF)            DROP 'L' FOR LONDON TEST                     
         B     DONESW1                                                          
         CLC   0(4,RF),=C'LFQA'                                                 
         BNE   *+12                                                             
         LA    RF,1(RF)            DROP 'L' FOR LONDON FQA                      
         B     DONESW1                                                          
         CLC   0(4,RF),=C'LADV'    DROP 'L' FOR LONDON ADV?                     
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         B     DONESW1                                                          
         CLC   0(4,RF),=C'LCSC'    DROP 'L' FOR LONDON CSC?                     
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         B     DONESW1                                                          
         CLC   0(4,RF),=C'LP2A'    DROP 'LP2' FOR LONDON ADV?                   
         BNE   *+12                                                             
         LA    RF,3(RF)                                                         
         B     DONESW1                                                          
         CLC   0(4,RF),=C'LP2T'    DROP 'LP2' FOR LONDON TST?                   
         BNE   *+12                                                             
         LA    RF,3(RF)                                                         
         B     DONESW1                                                          
*&&                                                                             
*&&US                                                                           
DONESWAP CLI   SRVSREQ+5,C','      TEST FOR =SWAP,ADV?                          
         BNE   DONESW4             USE DEFAULT SYSTEM IF NOT NAMED              
         LA    RF,SRVSREQ+6        POINT TO ADV SYSTEM NAME                     
         CLC   0(4,RF),=C'NTST'    DROP 'N' FOR NY TST                          
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         B     DONESW1                                                          
         CLC   0(4,RF),=C'NCSC'    DROP 'N' FOR NY CSC                          
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         B     DONESW1                                                          
         CLC   0(4,RF),=C'NADV'    DROP 'N' FOR NY ADV                          
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         B     DONESW1                                                          
         CLC   0(4,RF),=C'NTST'    DROP 'N' FOR NY TST                          
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         B     DONESW1                                                          
*&&                                                                             
DONESW1  CLI   3(RF),0             PUT SPACE IN PLACE OF NULL                   
         BNE   *+8                                                              
         MVI   3(RF),C' '                                                       
         L     R1,AFACIDTA                                                      
         USING FACITABD,R1                                                      
DONESW2  CLC   FACISN4,0(RF)       MATCH ON SYSTEM NAME                         
         BE    DONESW3                                                          
         CLI   1(RF),0             TRY SINGLE CHR                               
         BNE   *+14                                                             
         CLC   FACISN1,0(RF)                                                    
         BE    DONESW3                                                          
         LA    R1,L'FACITAB(R1)    TRY NEXT ENTRY                               
         CLI   0(R1),X'FF'                                                      
         BNE   DONESW2                                                          
                                                                                
***********************************************************************         
*CHECK THE OTHER TABLES WITH DIFFERENT DSPACE VALUE                   *         
***********************************************************************         
         L     RE,AFACIDTA         RE=A(FACPAK ID TABLE)                        
         LHY   R1,-2(RE)           MAX NUM OF FACPAKS AT TABLE-2                
         CHI   R1,16               TEST IF GREATER THAN OLD MAXIMUM             
         BL    ERROR7              INVALID SYSTEM IF OLD STYLE TABLE            
*                                                                               
         LR    R0,RE               R0=A(MY FACID TABLE)                         
         LY    R1,-4(RE)           DISP TO FACID INDEX AT TABLE-4               
         SR    RE,R1               RE=A(FACID INDEX TABLE)                      
         USING FACIDD,RE                                                        
*                                                                               
DONESW2B L     R1,FACAID                                                        
         CR    R1,R0               SAME AS MY TABLE?                            
         BNE   DONESW2D            NO                                           
*                                                                               
DONESW2C AHI   RE,FACIDLNQ         NEXT FACID TABLE                             
         CLI   0(RE),X'FF'         END OF FACID INDEX TABLE                     
         BNE   DONESW2B                                                         
         B     ERROR7              INVALID SYSTEM                               
         DROP  RE                                                               
*                                                                               
DONESW2D CLC   FACISN4,0(RF)       MATCH ON SYSTEM NAME                         
         BE    DONESW3                                                          
         CLI   1(RF),0             TRY SINGLE CHR                               
         BNE   *+14                                                             
         CLC   FACISN1,0(RF)                                                    
         BE    DONESW3                                                          
         LA    R1,L'FACITAB(R1)    TRY NEXT ENTRY                               
         CLI   0(R1),X'FF'                                                      
         BNE   DONESW2D                                                         
         B     DONESW2C                                                         
*                                                                               
DONESW3  CLC   SYSIDTY,FACIID      IS THIS A SWAP TO MY SYSTEM                  
         BE    ERROR8                                                           
         MVC   TSWAPLID,FACIID     PUT SYSTEM NUMBER IN SWAPLID                 
         CLC   SYSFLAG,FACIFL                                                   
         BNE   DONESW4             IF FLAGS ARE NOT SAME THEN TRUST             
         CLI   FACIID,FAC#CSC                                                   
         BE    DONESW4             CSC SYSTEM IS SPECIAL                        
         CLI   SYSIDTY,FAC#CSC                                                  
         BE    DONESW4             CSC SYSTEM IS SPECIAL                        
         DROP  R1                                                               
*                                                                               
         LAM   AR2,AR2,DMALET      ELSE CHECK SYS IS UP AND RUNNING             
         SR    R2,R2                                                            
         USING DMDHDR,R2                                                        
         SAC   512                                                              
         L     R2,DHAJOBS          SET R2 TO ADVS BLOCK                         
         LA    R0,127                                                           
         USING DSJOBD,R2                                                        
DONESW3A CLC   DSJADV,TSWAPLID     FIND THIS ADV                                
         BE    DONESW4                                                          
         LA    R2,32(,R2)                                                       
         BCT   R0,DONESW3A                                                      
         SAC   0                                                                
         B     ERROR9              CAN'T FIND ADV IN TABLE                      
*                                                                               
DONESW4  SAC   0                                                                
         OI    TSTAT4,TST4SWAP     SET $SWAP BIT IN UTL                         
         ICM   R1,15,TBUFF         SET UP TEST MESSAGE                          
         MVI   0(R1),0                                                          
         LR    RE,R1                                                            
*                                                                               
         TM    TSTAT8,TST8STSS     TEST IN SOFT FIELD STEREO MODE               
         BO    *+12                YES                                          
         TM    TSTAT6,TST6STSS     TEST IN SOFT FIELD STEREO MODE               
         BNO   DONESW4A            NO                                           
         MVI   0(RE),2             SET DATA LENGTH                              
         MVC   1(2,R1),=C'<>'      SET FLAG FOR RECEIVING LCM                   
         LA    R1,2(R1)            SET POINTER                                  
*                                                                               
DONESW4A TM    TSTAT9,TST9CMAD     TEST COMPRESSION                             
         BNO   DONESW4X            NO                                           
         LLC   RF,0(RE)            SET DATA LENGTH                              
         AHI   RF,1                                                             
         STC   RF,0(RE)                                                         
         MVI   1(R1),C'+'          SET FLAG FOR RECEIVING LCM                   
         LA    R1,1(R1)            SET POINTER                                  
*                                                                               
         TM    TSTATC,TSTCXSES     MORE THAT 4 SESSIONS?                        
         BNO   DONESW4X            NO                                           
         LLC   RF,0(RE)            SET DATA LENGTH                              
         AHI   RF,1                                                             
         STC   RF,0(RE)                                                         
         MVI   1(R1),C'#'          SET FLAG FOR RECEIVING LCM                   
         LA    R1,1(R1)            SET POINTER                                  
*                                                                               
DONESW4X LA    R2,SRVP1H           START AT P1H                                 
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   DONESW5                                                          
         MVC   8(4,R2),=C'=RE'     SET TO =RE                                   
         MVI   5(R2),3                                                          
*                                                                               
DONESW5  SR    R0,R0               BUILD PASS DATA                              
         SR    RF,RF                                                            
         ICM   R0,1,5(R2)          INPUT LEN IN R0                              
         BZ    DONEDIS3            END IF NO INPUT                              
         IC    RF,0(RE)                                                         
         AR    RF,R0               ADD CURRENT LEN                              
         STC   RF,0(RE)                                                         
         MVC   1(16,R1),8(R2)                                                   
         AR    R1,R0               BUMP ON TBUFF POINTER                        
         LLC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         CLI   0(R2),0             NO MORE                                      
         BNE   DONESW5                                                          
*                                                                               
DONEDIS3 MVI   MSG,C' '            INITIALISE OUTPUT MESSAGE AREA               
         MVC   MSG+1(L'MSG-1),MSG                                               
         LLC   R1,TLANG            GET LANGUAGE CODE                            
         CHI   R1,7                                                             
         BNH   *+6                                                              
         SR    R1,R1                                                            
         SLL   R1,2                                                             
         EX    0,DONMSG(R1)        GET CORRECT MESSAGE FOR LANGUAGE             
         B     DONED0                                                           
DONMSG   LA    RE,ENGMSG           ENGLISH DEFAULT                              
         LA    RE,ENGMSG           ENGLISH UK                                   
         LA    RE,ENGMSG           ENGLISH US                                   
         LA    RE,GERMSG           GERMAN                                       
         LA    RE,FREMSG           FRENCH                                       
         LA    RE,SPAMSG           SPANISH                                      
         LA    RE,DUTMSG           DUTCH                                        
         LA    RE,ENGMSG           N/D                                          
*                                                                               
DONED0   MVC   MSG(40),1(RE)       OUTPUT DISCONNECTED MESSAGE                  
         L     R1,VSSB                                                          
         LA    R1,SSBVTID-SSBD(R1)                                              
         LLC   R0,0(RE)                                                         
         LA    RF,MSG                                                           
         AR    RF,R0                                                            
         MVC   0(8,RF),0(R1)                                                    
         TM    TSTAT4,TST4SWAP     IF $SWAP ONLY DO CLSDST                      
         BO    DONECLS                                                          
         EJECT                                                                  
***********************************************************************         
*DISCONNECT MULTI SESSION MODE                                        *         
***********************************************************************         
DISC     CLC   MYTNUM,TNUM         DISCONNECT ALL SESSIONS IF ME                
         BNE   DISCX                                                            
*                                                                               
DISC1    LLC   R5,TSESSION         TEST IF CURRENT SESSION ACTIVE               
         IC    RF,SSACBITS(R5)                                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TSSBITS,0                                                        
         BZ    DISC2                                                            
         XC    DMCB(12),DMCB       DISCONNECT CURRENT ACTIVE SESSION            
         ST    R9,DMCB                                                          
         STC   R5,DMCB                                                          
         MVI   DMCB+4,X'80'        SET DISCONNECT                               
         GOTO1 ATWASVR,DMCB,TO24=Y                                              
         SAM31                                                                  
         BAS   RE,CHKMVE           MOVE NEW DISCONNECTED TWA TO MYTWA           
*                                                                               
DISC2    LH    R5,MAXSES           LOOP THROUGH ALL SESSIONS                    
         SH    R5,=H'1'            R5=HIGHEST SESSION NUMBER                    
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   SSESSION,TSESSION   SAVE CURRENT SESSION ID                      
         MVC   SSSBITS,TSSBITS     SAVE SESSION ACTIVE BITS                     
*                                                                               
DISC3    IC    RF,SSACBITS(R5)     GET SESSION ACTIVE BIT MASK                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TSSBITS,0           TEST IF THIS SESSION IS ACTIVE               
         BO    DISC4               YES DISCONNECT IT                            
         LTR   R5,R5               TEST IF FIRST SESSION                        
         BZ    DISC5               YES ALWAYS LEAVE IN SESSION A                
         SH    R5,=H'1'                                                         
         B     DISC3               BACK TO NEXT SESSION                         
*                                                                               
DISC4    XC    DMCB(12),DMCB       CONNECT TO ACTIVE SESSION                    
         ST    R9,DMCB                                                          
         STC   R5,DMCB                                                          
         MVI   DMCB+4,X'20'        SET COMPLETEUTL                              
         GOTO1 ATWASVR,DMCB,TO24=Y                                              
         SAM31                                                                  
         XC    DMCB(12),DMCB       DISCONNECT FROM ACTIVE SESSION               
         ST    R9,DMCB                                                          
         STC   R5,DMCB                                                          
         MVI   DMCB+4,X'80'+X'40'  SET DISCONNECT/TWA0INTWA                     
         GOTO1 ATWASVR,DMCB,TO24=Y                                              
         SAM31                                                                  
         BAS   RE,CHKMVE           MOVE NEW DISCONNECTED TWA TO MYTWA           
         LTR   R5,R5                                                            
         BZ    DISCX               FINISHED IF FIRST SESSION                    
         SH    R5,=H'1'                                                         
         B     DISC3               BACK TO NEXT SESSION                         
*                                                                               
DISC5    XC    DMCB(12),DMCB       LEAVE TERMINAL IN SESSION A                  
         ST    R9,DMCB                                                          
         STC   R5,DMCB                                                          
         MVI   DMCB+4,X'20'        SET COMPLETE TWA/UTL/TCB                     
         GOTO1 ATWASVR,DMCB,TO24=Y                                              
         SAM31                                                                  
*                                                                               
DISCX    MVC   TSVCREQ,$XMTALL     SET TO TRANSMIT SCREEN                       
*                                                                               
         MVI   TNAHNAH,0           INITIALISE SESSION INFO                      
         MVI   TSSXBITS,0                                                       
         MVI   TSSBITS,0                                                        
         MVI   TSSSWAP,X'FF'                                                    
         MVI   TSSRSRV,X'FF'                                                    
         NI    TSTAT6,255-TST6STRO-TST6STFU-TST6STSS                            
         NI    TSTAT8,255-TST8STSS-TST8DRTY-TST8BINT                            
         NI    TSTAT8,255-TST8PC32                                              
*                                                                               
         MVI   SRPARM1,X'FF'       SET VALUE TO FORCE CHECKPOINT/WRITE          
         B     DONECLS                                                          
         EJECT                                                                  
**********************************************************************          
*COPY/WRITE CONNECTED TWA BACK TO TEMPSTR TWA#0                      *          
**********************************************************************          
CHKMVE   ST    RE,SAVERE                                                        
         LR    R0,RA               GET A(TWA)                                   
         LH    R1,RECLEN                                                        
         SR    RE,RE                                                            
         ICM   RE,7,DMCB+5         GET A(NEW TWA FROM TWASVR)                   
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE TWASVR'S NEW TWA TO MY TWA              
CHKMVX   L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
CHKWRT   NTR1                      WRITE TWA0 WITH CHECKPOINT DATA              
         SR    R0,R0                                                            
         ICM   R0,3,TNUM           SET TERMINAL NUMBER/PAGE ZERO                
         MVC   DMCB1+20(2),=C'L='  SPECIAL PARM FOR TWA0 READ/WRITES            
         MVC   DMCB1+22(2),RECLEN                                               
         NI    TSTAT5,255-TST5TCP  TURN OFF VTAM TRM CHKPNT PENDING             
         GOTO1 VDATAMGR,DMCB1,DMWRT,TEMPSTR,(R0),(RA),TO24=Y                    
         SAM31                                                                  
         CLI   DMCB1+8,0                                                        
CHKWRTX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*CLOSE VTAM SESSION AND EXIT                                          *         
***********************************************************************         
DONECLS  CLC   SAVESRV+1(5),=C'DONE,'                                           
         BNE   DONECLSX            TEST FOR =DONE,NNNN                          
*                                                                               
         ICM   RE,15,TBUFF         SET UP TBUFF MESSAGE                         
         MVI   0(RE),0                                                          
         MVC   1(6,RE),=C'=DONE,'                                               
         LA    RE,7(RE)                                                         
         LA    R1,SAVESRV+6                                                     
         LA    R0,8                                                             
*                                                                               
DONECLS2 CLI   0(R1),C' '          CONVERT SYSIDTY TO HEX                       
         BNH   DONECLS5                                                         
         LLC   RF,SYSIDTY                                                       
         AH    RF,=X'00F0'                                                      
         CH    RF,=X'00F9'                                                      
         BNH   *+8                                                              
         SH    RF,=X'0039'                                                      
         STC   RF,BYTE             HEX SYSNUM IN BYTE                           
*                                                                               
         CLC   BYTE,0(R1)          DON'T COPY THIS SYSTEM                       
         BE    *+14                                                             
         MVC   0(1,RE),0(R1)       COPY THE OTHERS                              
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,DONECLS2         DO ALL SYSTEMS                               
         B     ERROR7                                                           
*                                                                               
DONECLS5 ICM   R1,15,TBUFF         WORK OUT LENGTH                              
         SR    RE,R1                                                            
         STC   RE,0(R1)                                                         
         CLI   0(R1),7             LENGTH 7 IS END                              
         BE    DONECLSX                                                         
         OI    TSTAT4,TST4SWAP     ELSE SWAP TO ANOTHER                         
*                                                                               
         LLC   RF,7(R1)            CONVERT SWAPNUM TO BINARY                    
         SH    RF,=X'00B7'                                                      
         CH    RF,=X'0013'                                                      
         BNH   *+8                                                              
         SH    RF,=X'0039'                                                      
         STC   RF,TSWAPLID         HEX SYSNUM IN SWAPLID                        
*                                                                               
DONECLSX OI    TSTATU,TSTATDNE     SET CALL LCM FOR CLSDST FLAG                 
         LA    RF,100                                                           
         GOTO1 ASQUASH,DMCB,MSG,(RF),TO24=Y                                     
         SAM31                                                                  
         MVC   SRVMSG(60),MSG      OUTPUT SQUASHED MESSAGE                      
         LA    R2,SRVSREQH                                                      
         B     EXIT                EXIT                                         
         EJECT                                                                  
***********************************************************************         
*ERRORS                                                               *         
***********************************************************************         
ERROR1   LA    R0,115              INVALID TERMINAL ID                          
         B     ERRXIT                                                           
ERROR2   LA    R0,116              TERMINAL IS NOT CONNECTED                    
         B     ERRXIT                                                           
ERROR3   LA    R0,117              TERMINAL IS IS PROCESS                       
         B     ERRXIT                                                           
ERROR4   LA    R0,118              INVALID TERMINAL OPTION                      
         B     ERRXIT                                                           
ERROR5   LA    R0,119              TERMINAL IS NOT SECURITY VIOLATOR            
         B     ERRXIT                                                           
ERROR6   LA    R0,276              CANNOT REMOVE AUTO-STARTED PRINTER           
         B     ERRXIT                                                           
ERROR7   LA    R0,32               INVALID SYSTEM                               
         LA    R2,SRVSREQH                                                      
         B     ERRXIT                                                           
ERROR8   LA    R0,280              TERMINAL IS ALREADY CONNECTED TO &T          
         LA    R2,SRVSREQH                                                      
         MVI   XTL,4                                                            
         MVC   XTT(4),0(R1)                                                     
         B     ERRXIT                                                           
ERROR9   LA    R0,102              SYSTEM IS NOT OPERATIONAL                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB(24),DMCB       R0=SERVICE ERROR MESSAGE NUMBER              
         L     RF,SRQACOMF                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(R0),0,(C'E',0),(XTL,XTT),TO24=Y                       
         SAM31                                                                  
*                                                                               
EXIT     NI    SRVSREQH+6,X'BF'                                                 
         OI    6(R2),X'40'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*CONSTANTS AND LITERALS                                               *         
***********************************************************************         
ENGMSG   DC    X'1B',CL40'Terminal disconnected from .......      '             
         DC    X'00',CL40'Hit enter to reconnect'                               
GERMSG   DC    X'0E',CL40'Verbindung Zu .......  unterbrochen'                  
         DC    X'00',CL40'Enter zum Neuverbinden'                               
FREMSG   DC    X'17',CL40'Terminal d{connect{ de .......  '                     
         DC    X'00',CL40'Frappez Entr{e pour reconnecter'                      
SPAMSG   DC    X'19',CL40'Terminal desconectado de .......  '                   
         DC    X'00',CL40'Apretar la tecla de entrada para reconectar'          
ITAMSG   DC    X'1D',CL40'Terminale sconnessionato dal .......  '               
         DC    X'00',CL40'Abassi il tasto ENTRARE per il riconnessione'         
DUTMSG   DC    X'0F',CL40'Verbinding met .......  verbroken'                    
         DC    X'00',CL40'Druk op ENTER om de verbinding te herstellen'         
*                                                                               
CTFILE   DC    CL8'CTFILE   '                                                   
TEMPSTR  DC    CL8'TEMPSTR  '                                                   
DMREAD   DC    CL8'DMREAD   '                                                   
DMWRT    DC    CL8'DMWRT    '                                                   
$XMTALL  DC    X'0126'                                                          
SSACBITS DC    X'0102040810204080'                                              
SSNABITS DC    X'FEFDFBF7EFDFBF7F'                                              
SPACES   DC    CL20' '                                                          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*WORKING STORAGE                                                      *         
***********************************************************************         
SRWORKD  DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
SAVER1   DS    A                                                                
SAVERE   DS    A                                                                
ASQUASH  DS    A                                                                
ATWASVR  DS    A                                                                
AFACIDTA DS    A                                                                
DMALET   DS    A                                                                
MYTNUM   DS    H                                                                
INPTNUM  DS    H                                                                
TIME     DS    PL4                                                              
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
RECLEN   DS    H                                                                
CHKDSP   DS    H                                                                
MSG      DS    CL100                                                            
SYSIDTY  DS    X                                                                
SYSCHR   DS    X                                                                
SYSFLAG  DS    X                                                                
XTL      DS    X                                                                
XTT      DS    CL10                                                             
MAXSES   DS    H                                                                
SSESSION DS    X                                                                
SSSBITS  DS    X                                                                
CURSES   DS    X                                                                
NEWSES   DS    X                                                                
ERRNUM   DS    X                                                                
SESS     DS    X                                                                
SAVESRV  DS    CL17                                                             
SRWORKX  DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
*OTHER DSECTS                                                         *         
***********************************************************************         
SRDONFFD DSECT                                                                  
         DS    CL64                                                             
* SRDONFFD                                                                      
       ++INCLUDE SRDONFFD                                                       
         EJECT                                                                  
* FACIDTABD                                                                     
       ++INCLUDE FACIDTABD                                                      
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* FACHKPT                                                                       
       ++INCLUDE FACHKPT                                                        
* FAPRQ                                                                         
       ++INCLUDE FAPRQ                                                          
* DMDSHDR                                                                       
       ++INCLUDE DMDSHDR                                                        
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRDON00   11/12/13'                                      
         END                                                                    
