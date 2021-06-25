*          DATA SET PPINF00    AT LEVEL 088 AS OF 10/15/08                      
*PHASE T41A00A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41A00  - CHANGE LOG'                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BPLA 09/08    SOON/OV FILTERING FOR BILLS                                     
*                                                                               
* BPLA 04/06    HELP FIX                                                        
*                                                                               
* KWAN 10/13/05 TWO CHARACTER OFFICE CODE MODIFICATIONS                         
*                                                                               
* SMYE 04/02    NEW LIMIT ACCESS SECURITY AND SET ADDRESSES FOR                 
*               CORE RESIDENT PUBVAL AND PUBEDIT                                
*                                                                               
* KWAN 10/03/01 LIMIT DATA=LEGALW HELP TO SJ AND H9 ONLY                        
*                                                                               
* KWAN 06/19/01 FOR PRD SCR HELP, DATA=LEGALW WILL BE DISPLAYED                 
*                                                                               
* KWAN 06/00    NO OP INSTRUCTION "OI    GLVXFLG1,GLV1GOTO"                     
*                                                                               
* BPLA 05/99    FIX BUG IN GETROW TO CHECK FOR END OF SCREEN                    
*               TO PREVENT LOOPS IF THERE IS NO LOWER SCREEN                    
*                                                                               
* KWAN 05/99    CORRECT SCREEN DISPLAY                                          
*                                                                               
* SMYE 04/23/98 ADD SFH AND FRZ DISPLAY AND FILTER OPTIONS FOR CLTS             
*                                                                               
* BPLA 03/98    PFKEY DISPLAY RE-ACTIVATED                                      
*                                                                               
* BPLA 02/98    IF STEREO - SPECIAL KEYBLD ROUTINE                              
*               + FIX KEYBLD ERRORS                                             
*                                                                               
* BPLA 02/02/98 PFKEY SWITCHING DISPLAY NO-OPED                                 
*               NOTE THAT PFKEY SWITCHING STILL WORKS                           
*                                                                               
* BPLA 10/02/97 CHANGES FOR PFKEY SWITCHING TO FILE                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41A00  - PRINTPAK INFO BASE'                                   
*                                                                               
T41A00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PSINFEND-GENOLD,T41A00,R5,RR=R9                                  
         USING GENOLD,RC                                                        
*                                                                               
* NOTE USE OF R5 AS SECOND BASE REGISTER                                        
**                                                                              
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING T41AFFD,RA                                                       
         USING FLDHDRD,R2                                                       
*                                                                               
         BAS   RE,INITL                                                         
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         L     RF,0(R1)                                                         
         ST    RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID                                                       
         CHI   R0,12                                                            
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY        SAVE ADJUSTED PFKEY                              
         MVC   CURADDR,TIOBCURS     SAVE CURSOR ADDRESS                         
         DROP  RF                                                               
*                                                                               
         LA    R3,REC                                                           
         ST    R3,AREC                                                          
         LA    R3,REC2          SINCE PUBIO IS AFTER REC2                       
         LA    R3,2000(R3)    BUT NOT ADDRESSABLE                               
         ST    R3,APUBIO                                                        
         L     R0,=A(GETFLT)                                                    
         A     R0,RELO                                                          
         ST    R0,GETFLTR                                                       
         L     R0,=A(FMTALPH)                                                   
         A     R0,RELO                                                          
         ST    R0,FRMTALPH                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AB8'            PUBVAL                         
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPUBVAL,DMCB                                                     
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AB9'            PUBEDIT                        
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPUBEDIT,DMCB                                                    
*                                                                               
         LH    R0,=Y(SECBLK-GENOLD)                                             
         AR    R0,RC                                                            
         ST    R0,ASECBLK                                                       
*                                                                               
*******  CALL TO FASECRET TO BUILD A SECURITY AUTHORIZATION TABLE               
*                                                                               
*  ASECBLK POINTS TO 1024 BYTES OF SAVED STORAGE                                
         L     R0,ASECBLK                                                       
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    NOSECRET                                                         
*  INITIALIZE SECURITY BLOCK                                                    
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
NOSECRET DS    0H                                                               
*                                                                               
         XC    SINMSG,SINMSG                                                    
         FOUT  SINMSGH                                                          
         EJECT                                                                  
         FOUT  SINIFLTH            FORCE XMT OF MODIFIED FIELD                  
         EJECT                                                                  
         CLI   SVFIRST,0           IS THIS FIRST TIME IN                        
         BNE   FL1A                                                             
         MVI   SVFIRST,C'Y'                                                     
         XC    SVXFRSY,SVXFRSY                                                  
         XC    SVXFRPR,SVXFRPR                                                  
* SET STEREO FLAG (WILL SUPPRESS PFKEY DATA)                                    
         MVI   SVSTEREO,C'N'                                                    
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RE,0(R1)                                                         
         USING FACTSD,RE                                                        
         TM    FATSTAT6,X'80'      TEST STEREO                                  
         BZ    *+8                                                              
         MVI   SVSTEREO,C'Y'                                                    
         DROP  RE                                                               
* TEST IF THIS IS A TRANSFER OF CONTROL FROM ANOTHER PROGRAM                    
FL1A     L     RF,ACOMFACS                                                      
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    FL1X                                                             
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',ELEM,24,GLVXCTL                               
         CLI   8(R1),0                                                          
         BNE   FL1X                                                             
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         TM    GLVXFLG1,GLV1RETN   TEST THIS IS A RETURN CALL                   
         BZ    FL1D                                                             
*                                                                               
         XC    SVXFRSY,SVXFRSY    CLEAR JUST IN CASE                            
         XC    SVXFRPR,SVXFRPR                                                  
* ON RETURN NEED TO XMT ALL                                                     
         BAS   RE,SETPF         WILL REST PFKEYS AND FORCE XMIT                 
         B     EXIT                                                             
*                                                                               
FL1D     MVC   SVXFRSY,GLVXFRSY    SAVE CALLING SYS/PRG                         
         MVC   SVXFRPR,GLVXFRPR                                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',SINIRECH,3,GLVXREC                            
         CLI   8(R1),0                                                          
         BNE   FL1X                FORGET ABOUT IT THEN                         
         GOTO1 (RF),(R1),=C'DELE'                                               
* KEY DATA IS IN PFM ELEMENT                                                    
         GOTO1 (RF),(R1),=C'GETF',SINIKEYH,24,GLVPFM                            
         CLI   8(R1),0                                                          
         BNE   FL1X                                                             
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
FL1X     CLI   PFKEY,0             IF PFKEY HIT, LEAVE PREV MESSAGE             
         BNE   *+10                                                             
         XC    SINMSG,SINMSG       CLEAR MESSAGE AREA                           
         FOUT  SINMSGH                                                          
         LA    R2,SINIRECH                                                      
         TM    4(R2),X'20'                                                      
         BO    FL10                                                             
*                                                                               
         MVI   PFKEY,0            CLEAR PFKEY IF REC CHANGED                    
         XC    SINRMSG,SINRMSG                                                  
         FOUT  SINRMSGH                                                         
         XC    SINKMSG,SINKMSG                                                  
         FOUT  SINKMSGH                                                         
         BAS   RE,CLRKEXP                                                       
* SET TO EDIT KEY                                                               
         NI    SINIKEYH+4,X'DF'                                                 
         XC    SVDATA,SVDATA                                                    
*                                                                               
         LA    R3,INVERR                                                        
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
         CLI   5(R2),9     WAS 8 - CHANGED TO 9 TO ALLOW PUBLISHER              
         BH    FLERR                                                            
         CLI   8(R2),C'?'                                                       
         BNE   FL1                                                              
DISPREC  BAS   R9,CLRPROT                                                       
         NI    SINIRECH+4,X'DF'                                                 
         NI    SINIKEYH+4,X'DF'                                                 
         OI    6(R2),OI1C          SET CURSOR                                   
         B     DSPRECS             GO DISPLAY RECORD TYPES                      
*                                                                               
FL1      SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         CHI   RE,7                                                             
         BNH   *+8                                                              
         LHI   RE,7        MAX IS 8 CHARACTERS FOR COMPARE                      
*                      THIS ALLOWS THE TO ENTER PUBLISHER                       
*                      AND HAVE IT MATCHED TO PUBLISHE IN RECLIST               
*                                                                               
         LA    R7,RECLIST                                                       
         LH    R8,0(R7)                                                         
         L     R9,2(R7)                                                         
         A     R9,RELO                                                          
         LA    R7,6(R7)                                                         
*                                                                               
         EX    RE,FLCLC                                                         
         BE    FL2                                                              
         BXLE  R7,R8,*-8                                                        
         B     DISPREC                                                          
FLCLC    CLC   8(0,R2),0(R7)                                                    
*                                                                               
FL2      MVC   SVREC,8(R7)                                                      
         MVC   SVOVLY,9(R7)                                                     
         MVC   BYTE,10(R7)         SAVE SCREEN NUMBER                           
         MVC   SVKBLD,11(R7)       SAVE DISP INTO KEYBUILD ROUTINE              
*                                                                               
         LA    R3,ACCSERR                                                       
         CLI   T41AFFD+1,C'*'      DDS TERM                                     
         BE    FL4                                                              
         CLI   SVOVLY,X'30'                                                     
         BNL   FLERR               RESTRICTED RECORD                            
FL4      OI    4(R2),X'20'         SET REC VALID                                
         MVC   SINRMSG(6),=C'KEY IS'                                            
*                                                                               
         LA    R7,KEYLIST          FIND KEY DESCRIPTION                         
         SR    R8,R8                                                            
FL6A     CLC   SVREC,0(R7)                                                      
         BE    FL6B                                                             
         IC    R8,1(R7)                                                         
         LA    R7,2(R7,R8)                                                      
         CLI   0(R7),X'FF'                                                      
         BNE   FL6A                                                             
         DC    H'0'                                                             
FL6B     IC    R8,1(R7)                                                         
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   SINRMSG+7(0),2(R7)  *EXECUTED*                                   
* SET DUMMY ACTION                                                              
         CLI   SVREC,X'16'    SEE IF PUBLIST                                    
         BNE   FL10                                                             
         LA    R9,SINRMSG+7                                                     
         AR    R9,R8                                                            
         MVC   0(9,R9),=C' AT LIST)'                                            
FL10     MVI   SVACT,C'C'                                                       
         MVC   WORK2,SVKEY         SAVE PREVIOUS KEY DATA                       
         SPACE 2                                                                
* KEY VALIDATION                                                                
*                                                                               
FL30     LA    R2,SINIKEYH                                                      
         TM    4(R2),X'20'         TEST VALID                                   
         BZ    FL32                                                             
         B     FL40                                                             
*                                                                               
* KEY NOT VALIDATED                                                             
*                                                                               
FL32     BAS   RE,CLRKEXP                                                       
         LA    R3,MSSNGERR                                                      
         XC    PREVKEY,PREVKEY                                                  
         XC    SVKEY,SVKEY                                                      
         MVI   SVFMTSW,0           RESET FORMAT SWITCH                          
*                                                                               
         GOTO1 VCALLOV,DMCB,(1,0),(RA)                                          
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
         TM    ERRCD,X'F0'                                                      
         BO    EXXMOD                                                           
*                                                                               
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*===================================================================*           
* KEY PREVIOUSLY VALIDATED                                                      
*===================================================================*           
         SPACE 1                                                                
FL40     CLI   PFKEY,0             KEY VALID - DID THEY HIT A PFKEY             
         BE    FL45                NO                                           
         CLI   PFKEY,2             TEST XFR TO FILE MAINT                       
         BNE   FL42                                                             
         BAS   RE,KEYBLD                                                        
         CLI   ERRCD,0            SEE IF ERROR SET                              
         BNE   ERROR                                                            
*                                                                               
         XC    SINMSG,SINMSG                                                    
         MVC   SINMSG(29),HITENTER   SET APPROPIRATE MESSAGE                    
         OC    PREVKEY,PREVKEY                                                  
         BNZ   *+10                                                             
         MVC   SINMSG(29),NOMORDTA                                              
         B     EXIT                                                             
*                                                                               
FL42     CLI   PFKEY,12            TEST RETURN                                  
         BNE   FL45                                                             
*                                                                               
         CLI   SVXFRSY,C' '                                                     
         BNH   FL45               NO PLACE TO RETURN TO                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'PRI'                                                 
         MVC   GLVXFRPR,=C'INF'                                                 
         MVC   GLVXTOSY,SVXFRSY                                                 
         MVC   GLVXTOPR,SVXFRPR                                                 
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    FL45                                                             
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,24,GLVXCTL                               
         XC    SINMSG,SINMSG                                                    
         MVC   SINMSG(24),=C'** BACK TO PRINT FILE **'                          
         OI    SINMSGH+6,X'80'  TRANSMIT                                        
         LA    R2,SINIRECH                                                      
         OI    6(R2),X'40'       CURSOR                                         
         B     EXXMOD                                                           
         DROP  R1                                                               
*                                                                               
FL45     DS    0H                                                               
         CLI   SINIFLT,C'?'                                                     
         BNE   FL50                                                             
         BAS   R9,CLRPROT          CLEAR FIRST                                  
         LA    R2,SINIFLTH                                                      
         OI    6(R2),OI1C          INSERT CURSOR                                
         BAS   RE,CLRSCR                                                        
         B     DSPFLT                                                           
*                                                                               
*                                                                               
*                                                                               
FL50     DS    0H                                                               
         BAS   R9,CLRPROT          MOVED HERE FL1X                              
*                                                                               
* CLEAR RECORD AREA                                                             
*                                                                               
         LA    R0,8                                                             
         LA    R1,REC                                                           
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
         SR    RE,RE               HASH TOTAL FOR FILTERS                       
         SR    R0,R0                                                            
         LA    R1,SINIFLT                                                       
         LA    RF,64                                                            
FL51     IC    R0,0(R1)                                                         
         AR    RE,R0                                                            
         LA    R1,1(R1)                                                         
         BCT   RF,FL51                                                          
         C     RE,FLTHASH          FILTERS EQUAL                                
         BE    *+10                                                             
         XC    PREVKEY,PREVKEY     NO - RESET                                   
         ST    RE,FLTHASH                                                       
         CLC   SVKEY(26),WORK2     SAME KEY                                     
         BE    *+10                                                             
         XC    PREVKEY,PREVKEY     NO - CLEAR PREVIOUS KEY                      
*                                                                               
         MVC   SINLAST(3),=X'000101'   RESET TO NORMAL                          
*                                                                               
         CLI   SVREC,X'11'        CLIENT/PRODUCT/ESTIMATE                       
         BL    FL55                                                             
         CLI   SVREC,X'13'                                                      
         BH    FL55                                                             
         BAS   RE,SETPF           DISPLAY PF KEYS                               
*                                                                               
FL55     DS    0H                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB(1),SVOVLY                                                   
         GOTO1 VCALLOV,DMCB,,(RA)                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB             GET OVERLAY ADDRESS                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
         CLI   ERRAREA,0                                                        
         BE    FL58                                                             
*                                                                               
         BAS   RE,CLRSCR                                                        
         B     DSPFLT                                                           
         EJECT                                                                  
*                                                                               
FL58     MVC   SINMSG(29),=C'** HIT ENTER FOR NEXT PAGE **'                     
         OC    PREVKEY,PREVKEY                                                  
         BZ    FL60                                                             
         OI    SINIFLTH+1,X'01'    MODIFY FILTER FOR NEXT PAGE                  
         FOUT  SINIFLTH                                                         
         B     EXXMOD                                                           
FL60     DS    0H                                                               
         NI    SINIKEYH+4,X'DF'    TURN OFF FIELD VALIDATED                     
         MVC   SINMSG(29),=C'** END OF REQUESTED DATA **  '                     
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPFLT   L     RE,=V(FLTDESC)      DISPLAY VALID FILTERS                        
         A     RE,RELO             RELOCATE                                     
*                                                                               
* FIND FILTERS FOR THIS RECORD                                                  
*                                                                               
DSPFLT1  CLC   SVREC,0(RE)                                                      
         BE    DSPFLT2                                                          
         CLC   0(2,RE),=X'FFFF'                                                 
         BE    EXXMOD                                                           
         MVC   HALF,1(RE)                                                       
         AH    RE,HALF                                                          
         B     DSPFLT1                                                          
*                                                                               
* FOUND FILTERS NOW DISPLAY THEM                                                
*                                                                               
DSPFLT2  ST    R2,FULL                                                          
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA(19),=C'VALID FILTER FIELDS'                              
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA(19),=19C'-'                                              
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
*                                                                               
* SEND FILTER DESCRIPTIONS                                                      
*                                                                               
         LA    RE,3(RE)                                                         
DSPFLT3  CLI   0(RE),X'FF'                                                      
         BE    DSPEX                                                            
         SR    RF,RF                                                            
         IC    RF,0(RE)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),2(RE)                                                 
*                                                                               
         CLI   SVREC,X'12'         DOING PRODUCTS?                              
         BNE   DSPFLT3H            ALTER DATA DISPLAY                           
*                                  FOR SJR AND STARCOM                          
         CLC   AGYALPHA,=C'SJ'                                                  
         BE    *+14                                                             
         CLC   AGYALPHA,=C'H9'                                                  
         BNE   DSPFLT3H                                                         
         CLC   FLDDATA(05),=C'DATA='                                            
         BNE   DSPFLT3H                                                         
         XC    FLDDATA(L'F12C),FLDDATA                                          
         MVC   FLDDATA(L'SJH9HLP1),SJH9HLP1                                     
*                                                                               
DSPFLT3H FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         IC    RF,0(RE)                                                         
         AHI   RF,2                                                             
         AR    RE,RF                                                            
         B     DSPFLT3                                                          
*                                                                               
DSPEX    L     R2,FULL                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
SJH9HLP1 DC    C'DATA=ADJC,BILLF,USER1,USER2,LEGALW'                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPRECS  L     RE,=V(RECDESC)      DISPLAY VALID RECORDS                        
         A     RE,RELO             RELOCATE                                     
*                                                                               
* FOUND FILTERS NOW DISPLAY THEM                                                
*                                                                               
DSPREC2  ST    R2,FULL                                                          
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA(13),=C'VALID RECORDS'                                    
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA(13),=19C'-'                                              
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         SPACE 2                                                                
* SEND FILTER DESCRIPTIONS                                                      
         LA    RE,3(RE)                                                         
DSPREC3  CLI   0(RE),X'FF'                                                      
         BE    DSPRX                                                            
         SR    RF,RF                                                            
         IC    RF,0(RE)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),2(RE)                                                 
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         IC    RF,0(RE)                                                         
         AHI   RF,2                                                             
         AR    RE,RF                                                            
         B     DSPREC3                                                          
         SPACE 2                                                                
DSPRX    L     R2,FULL                                                          
         CLI   SINIREC,C'?'                                                     
         BNE   FLERR                                                            
         B     EXXMOD                                                           
*                                                                               
*                                                                               
*                                                                               
CLRSCR   DS    0H                                                               
         ST    R2,FULL                                                          
         LA    R2,SINHDRH                                                       
         LA    RF,18               18 LINES TO BE CLEARED                       
CLRS50   XC    8(80,R2),8(R2)                                                   
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCT   RF,CLRS50                                                        
         L     R2,FULL                                                          
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
CLRKEXP  OC    SINKMSG,SINKMSG                                                  
         BZR   RE                                                               
         XC    SINKMSG,SINKMSG                                                  
         FOUT  SINKMSGH                                                         
         BR    RE                                                               
*                                                                               
FLERR    B     ERROR                                                            
         B     EXXMOD                                                           
*                                                                               
WORK2    DS    CL48                                                             
         EJECT                                                                  
FNDUF    TM    1(R2),X'20'         TEST PROTECTED                               
         BCR   8,RE                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         DC    H'0'                                                             
         EJECT                                                                  
CLRPROT  ST    R2,FULL             CLEAR PROTECTED FIELDS                       
         LA    R2,SINHDRH                                                       
         CLI   FLDLEN,50                                                        
         BL    CLRPROT3                                                         
CLRPROT1 SR    RE,RE                                                            
         IC    RE,FLDLEN                                                        
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    FLDDATA,FLDDATA    * EXECUTED *                                  
         BZ    CLRPROT2                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA,FLDDATA    * EXECUTED *                                  
         FOUT  (R2)                                                             
CLRPROT2 LA    R2,9(RE,R2)                                                      
CLRPROT3 CLI   FLDLEN,0            END OF SCREEN                                
         BE    CLRPROTX                                                         
         CLI   FLDLEN,50                                                        
         BNL   CLRPROT1                                                         
         SR    RE,RE                                                            
         IC    RE,FLDLEN                                                        
         LA    R2,0(RE,R2)                                                      
         B     CLRPROT3                                                         
CLRPROTX L     R2,FULL                                                          
         BR    R9                                                               
*                                                                               
         EJECT                                                                  
         SPACE 3                                                                
*                  INITIALISATION CODE                                          
         SPACE 3                                                                
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R8,RD                                                            
         SR    R8,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R8,0(R2)                                                         
         AR    R8,R3                                                            
         ST    R8,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R8,2(R2)                                                         
         AR    R8,R3                                                            
         ST    R8,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(44),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LA    R3,IOAREA                                                        
         ST    R3,AREC                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
CLEARWRK LTR   R8,R8               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CHI   R8,250                                                           
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R8,=H'250'                                                       
         B     CLEARWRK                                                         
         SPACE 2                                                                
CLEAREST BCTR  R8,R0                                                            
         EX    R8,VARCLEAR                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
         SPACE 2                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
         SPACE 2                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         SPACE 2                                                                
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
         SPACE 2                                                                
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBDIR)                     
         SPACE 3                                                                
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
WRITEPUB MVC   COMMAND,=C'DMWRT'                                                
         B     PUBDIRY                                                          
         SPACE 2                                                                
ADDPUBD  MVC   COMMAND,=C'DMADD '                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBFILE)                    
         SPACE 3                                                                
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUTPUB   MVC   COMMAND,=C'PUTREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
ADDPUB   MVC   COMMAND,=C'ADDREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),APUBIO,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         CLI   ERRCD,NEWERRS                                                    
         BNE   ERROR10                                                          
*                                                                               
         XC    WORK,WORK           DEFINE CONTROL BLOCK                         
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         MVI   GTMTYP,GTMERR       SET MESSAGE TYPE TO ERRO                     
         MVC   GTMSGNO,NERRCD      AND MESSAGE NUMBER                           
         MVI   GTMSYS,4            AND MESSAGE SYSTEM                           
         L     RF,ACOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),WORK           PUT OUT SYSTEM MESSAGE                       
         MVI   ERRCD,0                                                          
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
         DROP  R1                                                               
*                                                                               
ERROR10  MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
*                                                                               
HITENTER DC    CL29'** HIT ENTER FOR MORE DATA **'                              
NOMORDTA DC    CL29'** END OF REQUESTED DATA **'                                
         EJECT                                                                  
* RECLIST ENTRIES ARE  00-07 RECORD NAME                                        
*                      08    RECORD NUMBER                                      
*                      09    OVERLAY NUMBER                                     
*              10 SPARE                                                         
         CNOP  2,4                                                              
RECLIST  DC    H'15'                                                            
         DC    A(RECLISTX-1)                                                    
*                                                                               
         DC    CL8'CLIENT  ',X'111100',AL4(KCLT)                                
         DC    CL8'CLTHDR  ',X'111100',AL4(KCLT)                                
         DC    CL8'PRODUCT ',X'121200',AL4(KCLT)                                
         DC    CL8'PRDHDR  ',X'121200',AL4(KCLT)                                
         DC    CL8'ESTIMATE',X'131300',AL4(KCLT)                                
         DC    CL8'ESTHDR  ',X'131300',AL4(KCLT)                                
         DC    CL8'BILLREC ',X'141400',AL4(0)                                   
         DC    CL8'INVOICE ',X'141400',AL4(0)                                   
         DC    CL8'ADREC   ',X'151500',AL4(0)                                   
         DC    CL8'JOBREC  ',X'151500',AL4(0)                                   
         DC    CL8'PUBLIST ',X'162000',AL4(0)                                   
*                                                                               
         DC    CL8'DIVISION',X'1A1A00',AL4(0)                                   
         DC    CL8'REGION  ',X'1B1B00',AL4(0)                                   
         DC    CL8'DISTRICT',X'1C1C00',AL4(0)                                   
         DC    CL8'DSTREC  ',X'1C1C00',AL4(0)                                   
*                                                                               
         DC    CL8'REP     ',X'202000',AL4(0)                                   
         DC    CL8'COMMENT ',X'212100',AL4(0)                                   
         DC    CL8'PUBLISHE',X'222000',AL4(0)   PUBLISHERS                      
         DC    CL8'PPUBLIST',X'232000',AL4(0)   PUBLISHER PUB LIST              
*                                                                               
*******  DC    CL8'AGENCY  ',X'303000',AL4(0)                                   
*                                                                               
*                                                                               
RECLISTX EQU   *                                                                
         SPACE 2                                                                
         EJECT                                                                  
KEYLIST  DS    0H                                                               
         DC    X'11',AL1(L'K11)                                                 
K11      DC    C'MEDIA,(START AT *CLIENT*)'                                     
         DC    X'12',AL1(L'K12)                                                 
K12      DC    C'MEDIA,CLIENT,(START AT *PRODUCT*)'                             
         DC    X'13',AL1(L'K13)                                                 
K13      DC    C'MEDIA,CLIENT,PRODUCT'                                          
         DC    X'14',AL1(L'K14)                                                 
K14      DC    C'MEDIA,CLIENT,PRODUCT'                                          
         DC    X'15',AL1(L'K15)                                                 
K15      DC    C'MEDIA,CLT,PRODUCT,(START AT *AD*)'                             
         DC    X'16',AL1(L'K16)                                     L02         
K16      DC    C'MEDIA,CLIENT(,START)'                              L02         
         DC    X'1A',AL1(L'K1A)                                                 
K1A      DC    C'MEDIA,CLIENT,(START AT *DIV*)'                                 
         DC    X'1B',AL1(L'K1B)                                                 
K1B      DC    C'MEDIA,CLIENT,DIV,(START AT *REG*)'                             
         DC    X'1C',AL1(L'K1C)                                                 
K1C      DC    C'MED,CLT,DIV,REG,(START AT *DIS*)'                              
         DC    X'20',AL1(L'K20)                                                 
K20      DC    C'MEDIA,(START AT *REP*)'                                        
         DC    X'21',AL1(L'K21)                                                 
K21      DC    C'MEDIA,(START AT *COMMENT*)'                                    
         DC    X'22',AL1(L'K22)                                                 
K22      DC    C'MEDIA,(START AT *PUBLISHER*)'                                  
         DC    X'23',AL1(L'K23)                                                 
K23      DC    C'MEDIA,PUBLISHER'                                               
         DC    X'30',AL1(L'K30)                                                 
K30      DC    C'MEDIA,(START AT *AGENCY*)'                                     
*                                                                               
         DC    X'FF'                                                            
         LTORG                                                                  
* SUBROUTINE CALLS GETFACT TO SEE IF USER SIGNED ON VIA STEREO                  
* IF SO - SET SUB SCREEN NUMBER IN FATIOB                                       
*                                                                               
         SPACE 1                                                                
GFACT    NTR1                                                                   
         XC    DMCB,DMCB                                                        
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB                                                        
         L     R2,0(R1)                                                         
         USING FACTSD,R2                                                        
         TM    FATSTAT6,TST6FLSH                                                
         BNO   TFX                                                              
         DROP  R2                                                               
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSCRN   SET TIOBCNT TO SCREEN NUMBER                 
         MVI   TIOBAID,0                                                        
         XC    TIOBCNT,TIOBCNT                                                  
         ZIC   R2,SVOVLY                                                        
         STC   R2,TIOBCNT                                                       
         OI    TIOBINDS,TIOBSUBS   SET TIOBAID TO SUB SCREEN NUMBER             
         ZIC   R2,SVSUBSC                                                       
         STC   R2,TIOBAID                                                       
*                                                                               
TFX      XIT1                                                                   
         DROP  RF                                                               
         EJECT                                                                  
*=============================================================*                 
* SET PFKEY DATA AT END OF SCREEN IF ROOM AVAILABLE           *                 
*=============================================================*                 
         SPACE 1                                                                
SETPF    NTR1                                                                   
         CLI   SVSTEREO,C'Y'                                                    
         BE    SETPFX                                                           
         LA    R2,SINLAST      SINCE THERE IS ONLY ONE SCREEN                   
*                               I CAN ALWAYS USE SINLAST                        
SETPF4   MVC   0(STRFLDX-STRFLDH+3,R2),STRFLDH  (INCLUDE X'000101')             
         OC    SVXFRSY,SVXFRSY     TEST XFRCTL FROM ANOTHER PROGRAM             
         BZ    SETPFX                                                           
         MVC   0(STRFLDX2-STRFLDH2+3,R2),STRFLDH2                               
*                                                                               
SETPFX   XIT1                                                                   
*                                                                               
STRFLDH  DC    AL1(STRFLDX-STRFLDH)                                             
         DC    X'20'               PROTECTED                                    
         DC    AL2(23*80+61)       ROW 24/COL 61                                
         DC    X'00'               INDS                                         
         DC    X'00'               INPUT LENGTH                                 
         DC    X'80'               TRANSMIT                                     
         DC    AL1(STRFLDX-STRFLDH-8)  DATA LENGTH                              
         DC    C'PF2 = ',X'C6899385'           (FILE)                           
STRFLDX  EQU   *                                                                
         DC    X'000101'                                                        
*                                                                               
STRFLDH2 DC    AL1(STRFLDX2-STRFLDH2)                                           
         DC    X'20'               PROTECTED                                    
         DC    AL2(23*80+41)       ROW 24/COL 42                                
         DC    X'00'               INDS                                         
         DC    X'00'               INPUT LENGTH                                 
         DC    X'80'               TRANSMIT                                     
         DC    AL1(STRFLDX2-STRFLDH2-8)  DATA LENGTH                            
         DC    C'PF2=',X'C6899385'         (FILE)                               
         DC    C' PF12=',X'D985A3A49995'   (RETURN))                            
STRFLDX2 EQU   *                                                                
         DC    X'000101'                                                        
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* BUILD KEY FOR TRANSFER TO FILE MAINT                           *              
*================================================================*              
         SPACE 1                                                                
KEYBLD   NTR1                                                                   
         MVI   ERRCD,0                                                          
         TM    SINIKEYH+4,X'20'    TEST KEY DATA VALID                          
         BO    KB1                                                              
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOKEYDTA)                                            
         B     KEYBXX                                                           
*                                                                               
KB1      CLI   SVSTEREO,C'Y'      SEE IF STEREO                                 
         BNE   KB2                                                              
*                                 IF STEREO I DON'T LOOK FOR CURSOR             
*                                 SINCE IT CAN'T BE SET TO PROTECTED            
*                                 FIELD - SO JUST PASS SVCLT/PRD/EST            
         MVC   MYCLT,=C'   '                                                    
         MVC   MYPRD,=C'   '                                                    
         MVC   MYEST,=C'   '                                                    
         MVC   MYCLT,SVCLT                                                      
         CLI   SVREC,X'11'                                                      
         BE    KEYBLDX                                                          
         CLC   SVPRD,=C'ALL'                                                    
         BE    KEYBLDX                                                          
         MVC   MYPRD,SVPRD                                                      
         CLI   SVREC,X'12'                                                      
         BE    KEYBLDX                                                          
         CLI   SVEST,C'0'        SEE IF I HAVE AN ESTIMATE                      
         BL    KEYBLDX                                                          
         MVC   MYEST,SVEST                                                      
         B     KEYBLDX                                                          
*                                                                               
KB2      ICM   RE,15,SVKBLD        GET DSPL TO KEYBLD ROUTINE                   
         BNZ   KB4                                                              
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOXFR)                                               
         B     KEYBXX                                                           
*                                                                               
KB4      DS    0H                  CLEAR KEY BUILD AREA                         
         MVC   MYCLT,=C'   '                                                    
         MVC   MYPRD,=C'   '                                                    
         MVC   MYEST,=C'   '                                                    
         A     RE,RELO            MUST RELOCATE                                 
         BR    RE                                                               
*                                                                               
KCLT     DS    0H                                                               
         CLI   SVREC,X'11'         TEST CLTHDR                                  
         BNE   KPRD                                                             
         MVC   COLS(4),=AL1(01,27,53,255)                                       
         BAS   RE,GETROW           POINT R1 TO DATA IN TWA                      
         MVC   MYCLT,0(R1)      MOVE SELECTED COLUMN                            
         OC    MYCLT,=C'   '                                                    
         B     KEYBLDX                                                          
*                                                                               
KPRD     MVC   MYCLT,SVCLT                                                      
         CLI   SVREC,X'12'        TEST PRD REC                                  
         BNE   KEST                                                             
* DIG OUT THE PRODUCT CODE                                                      
         MVC   COLS(4),=AL1(01,27,53,255)                                       
         BAS   RE,GETROW                                                        
         MVC   MYPRD,0(R1)         MOVE PRODUCT                                 
         OC    MYCLT,=C'   '                                                    
         B     KEYBLDX                                                          
*                                                                               
* NEED TO MOVE THE LAST DISPLAYED PRODUCT ABOVE CURSOR                          
*                                                                               
KEST     CLC   =C'ALL',SVPRD                                                    
         BNE   KEST10                                                           
*                                                                               
KEST2    DS    0H                                                               
         MVC   COLS(2),=AL1(1,255)                                              
         BAS   RE,GETROW                                                        
         MVC   MYPRD,0(R1)         PRODUCT                                      
         LA    R1,26(R1)           GET TO ESTIMATE                              
         B     KEST20                                                           
*                                                                               
KEST10   MVC   MYPRD,SVPRD       SINGLE PRODUCT                                 
         MVC   COLS(2),=AL1(27,255)                                             
         BAS   RE,GETROW                                                        
*                                                                               
KEST20   DS    0H                                                               
         MVC   MYEST(3),=C'000'                                                 
         CLC   1(2,R1),=C'  '   SEE IF I HAVE MORE THAN ONE DIGIT               
         BH    KEST20B                                                          
         MVC   MYEST+2(1),0(R1)                                                 
         B     KEST20X                                                          
*                                                                               
KEST20B  CLC   2(1,R1),=C' '    SEE IF I HAVE 3 DIGITS                          
         BH    KEST20C                                                          
         MVC   MYEST+1(2),0(R1)                                                 
         B     KEST20X                                                          
*                                                                               
KEST20C  MVC   MYEST(3),0(R1)                                                   
*                                                                               
KEST20X  B     KEYBLDX                                                          
*                                                                               
KEYBLDX  L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTD',SVEBCMED,1,GLVPRMD                            
         GOTO1 (RF),(R1),=C'PUTD',MYCLT,3,GLVPRCLT                              
         GOTO1 (RF),(R1),=C'PUTD',MYPRD,3,GLVPRPRD                              
         GOTO1 (RF),(R1),=C'PUTD',MYEST,3,GLVPREST                              
* PUT OUT RECORD NAME                                                           
         GOTO1 (RF),(R1),=C'PUTF',SINIRECH,,GLVXREC                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'PRI'                                                 
         MVC   GLVXTOSY,=C'PRI'                                                 
*                                                                               
KEYBLDX4 MVC   GLVXFRPR,=C'INF'                                                 
         MVC   GLVXTOPR,=C'FIL'                                                 
******** OI    GLVXFLG1,GLV1GOTO                                                
         OI    GLVXFLG1,GLV1SEPS                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK,14,GLVXCTL                               
* FIX THE MESSAGE !                                                             
         OI    SINIRECH+6,X'40'    CURSOR TO REC FIELD                          
KEYBXX   XIT1                                                                   
*                                                                               
GETROW   DS    0H                                                               
         LA    R2,SINHDRH          POINT TO FIRST DISPLAY LINE                  
         SR    R0,R0                                                            
*                                                                               
GETROW1  IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   0(R2),0             END OF SCREEN                                
         BE    GETROW2     SEND ERROR - COULD HAPPEN IF NO LOWER                
*                          SCREEN IS THERE                                      
         CLI   10(R2),C'-'         FIND THE LINE WITH '---'                     
         BNE   GETROW1                                                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    R0,10                                                            
         LA    R1,10(R2)                                                        
GETROW1A CLI   0(R1),C' '                                                       
         BH    GETROW1X                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,GETROW1A                                                      
* POINT TO NEXT ROW                                                             
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
GETROW1X CLC   CURADDR,2(R2)       CURSOR ADDR TO FIELD START                   
         BNL   GETROW4                                                          
GETROW2  MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(PUTCURS)                                             
         B     KEYBXX                                                           
*                                                                               
GETROW4  LR    RF,R2               SAVE PREVIOUS FIELD ADDRESS                  
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    GETROW2             THIS SHOULDN'T HAPPEN                        
         CLC   CURADDR,2(R2)                                                    
         BH    GETROW4                                                          
         BE    *+6                 EQUAL - USE CURRENT ROW                      
         LR    R2,RF               LOW - USE PREVIOUS ROW                       
* CALCULATE ROW/COLUMN                                                          
         LH    R0,CURADDR                                                       
         SRDA  R0,32                                                            
         D     R0,=F'80'           R0 HAS COL-1 ON/R1 HAS ROW NUMBER            
* POINT R1 TO COLUMN DATA                                                       
         LA    RF,COLS             POINT TO FIRST COL DSPL                      
*                                                                               
GETROW6  SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    R1,8(R2,R1)         POINT TO CODE                                
         CLM   R0,1,1(RF)          ACTUAL DSPL TO NEXT COL DSPL                 
         BLR   RE                  IF LOW, USE THIS DATA                        
         LA    RF,1(RF)            ELSE POINT TO NEXT COL DSPL                  
         CLI   0(RF),X'FF'                                                      
         BNE   GETROW6                                                          
         BR    RE                  ON RETURN R1 POINTS TO CODE                  
         LTORG                                                                  
*                                                                               
MYCLT    DS    CL3            USED FOR TRANSFER TO FILE                         
MYPRD    DS    CL3                                                              
MYEST    DS    CL3                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
GETFLT   CSECT                                                                  
         NMOD1 0,GETFLTR                                                        
*                                                                               
* GET A FIELD FROM THE FILTER FIELD                                             
*                                                                               
*        PARAMETER 1     BYTE   0 = L'DATA                                      
*                        BYTE 1-3 = A(DATA)                                     
*                                                                               
*        PARAMETER 2     BYTE   0 = L'SEARCH FIELD                              
*                        BYTE 1-3 = A(SEARCH FIELD)                             
*                                                                               
*                                    OUTPUT - A(FIELD) OR A(0)                  
*                                                                               
         LM    R6,R7,0(R1)         SET PARMS                                    
         LA    R6,0(R6)            CLEAR HIGH ORDER BITS                        
         LA    R7,0(R7)                                                         
         SR    R8,R8                                                            
         IC    R8,0(R1)            SET FIELD END                                
         AR    R8,R6                                                            
         SR    R9,R9                                                            
         IC    R9,4(R1)                                                         
         BCTR  R9,0                                                             
* REGISTER ASSIGNMENTS - R6=A(DATA)                                             
*                        R7=A(SEARCH DATA)                                      
*                        R8=A(END OF DATA)                                      
*                        R9=L'SEARCH DATA                                       
*                                                                               
GETFLD1  CLI   0(R6),0             FIND FIRST FIELD                             
         BE    GFNODATA                                                         
         CLI   0(R6),C' '                                                       
         BNE   GETFLD2             FOUND FIRST FIELD                            
         LA    R6,1(R6)            NEXT COLUMN                                  
         CR    R6,R8                                                            
         BH    GFNODATA                                                         
         B     GETFLD1                                                          
GETFLD2  EX    R9,*+8              SEE IF CORRECT FIELD                         
         B     *+10                                                             
         CLC   0(0,R6),0(R7)      *EXECUTED                                     
         BE    GFDATA                                                           
GETFLD3  LA    R6,1(R6)                                                         
         CLI   0(R6),C','                                                       
         BNE   *+12                                                             
         LA    R6,1(R6)                                                         
         B     GETFLD2                                                          
         CLI   0(R6),0                                                          
         BE    GFNODATA                                                         
         CLI   0(R6),C' '                                                       
         BE    GFNODATA                                                         
         CR    R6,R8                                                            
         BL    GETFLD3                                                          
GFNODATA XC    4(4,R1),4(R1)       DATA NOT FOUND                               
         B     GETFX                                                            
GFDATA   ST    R6,4(R1)            DATA FOUND                                   
         B     GETFX                                                            
*                                                                               
GETFX    XMOD1 1                                                                
         EJECT                                                                  
FMTALPH  CSECT                                                                  
         NMOD1 0,FRMTALPH                                                       
*                                                                               
* FORMAT TABLE IN ALPHABETIC COLUMNS                                            
*                                                                               
*        PARAMETER 1     BYTE   0 = L'EACH ENTRY                                
*                        BYTE 1-3 = A(TABLE)                                    
*        PARAMETER 2     BYTE 0-3 = NUMBER OF ENTRIES IN TABLE                  
*        PARAMETER 3     BYTE 0-3 = NUMBER OF ROWS TO BE FORMATTED              
*                                                                               
*        PARAMETER 4     BYTE   0 = NUMBER OF COLUMNS TO BE FORMATTED           
*                        BYTE 1-3 = A(FORMAT LIST)                              
*                                                                               
*        OUTPUT FORMAT LIST (ONE ENTRY FOR EACH COLUMN)                         
*                BYTE  0 = NUMBER OF ENTRIES FOR THIS COLUMN                    
*                BYTE 1-3= A(START FIELD FOR THIS COLUMN)                       
*                                                                               
*               A(TABLE) IS SET TO NEXT ENTRY POINT                             
*               NUMBER OF ENTRIES IS SET TO NUMBER OF ENTRIES LEFT IN           
*                TABLE.                                                         
*                                                                               
         L     R6,12(R1)           CLEAR FORMAT LIST                            
         LA    R6,0(R6)                                                         
         SR    R7,R7                                                            
         IC    R7,12(R1)                                                        
         XC    0(4,R6),0(R6)                                                    
         LA    R6,4(R6)                                                         
         BCT   R7,*-10                                                          
*                                                                               
         SR    RF,RF               GET END OF TABLE                             
         IC    RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         MH    RF,6(R1)            NUMBER OF ENTRIES X ENTRY LENGTH             
         L     R6,0(R1)                                                         
         LA    RF,0(RF,R6)         END OF TABLE                                 
*                                                                               
         LA    R6,0(R6)            A(TABLE START)                               
         L     R7,12(R1)           A(FORMAT LIST)                               
         SR    R8,R8                                                            
         IC    R8,12(R1)           NUMBER OF COLUMNS                            
FA1      ST    R6,0(R7)            SAVE COLUMN ADDRESS                          
         SR    R4,R4                                                            
         CLC   4(4,R1),8(R1)       NUMBER OF ENTRIES GT NO OF ROWS              
         BH    FA2                                                              
         MVC   0(1,R7),7(R1)        NO - SET ROWS FOR THIS COLUMN               
         XC    4(4,R1),4(R1)       SET ENTRIES LEFT TO ZERO                     
         LR    R6,RF                                                            
         B     FAEX                                                             
FA2      MVC   0(1,R7),11(R1)      SET TO NUMBER OF ROWS                        
         SR    R9,R9                                                            
         IC    R9,0(R1)                                                         
         MH    R9,10(R1)           X ENTRY LENGTH                               
         LA    R6,0(R9,R6)         SET TO NEXT COLUMN ADDRESS                   
         LA    R7,4(R7)                                                         
         L     R9,4(R1)            DECREMENT NUMBER OF ENTRIES                  
         S     R9,8(R1)                                                         
         ST    R9,4(R1)            SAVE ENTRIES LEFT                            
         BCT   R8,FA1              GET NEXT COLUMN ADDRESS                      
*                                                                               
FAEX     IC    RF,0(R1)            SAVE A(NEXT START)                           
         ST    R6,0(R1)                                                         
         STC   RF,0(R1)                                                         
         B     FMTALPX                                                          
*                                                                               
FMTALPX  XMOD1 1                                                                
         EJECT                                                                  
         EJECT                                                                  
FLTDESC  CSECT                                                                  
*                                                                               
* FILTERS LIST                                                                  
*        0 = OVERLAY NUMBER   X'FF' FOR END OF LIST                             
*      1-2 = DISPLACEMENT TO NEXT OVERLAY DESCRIPTION                           
*                                                                               
*   VARIBALE NUMBER OF DESCRIPTION FIELDS                                       
*        0 = LENGTH OF DESCRIPTION OR X'FF' FOR END OF LIST                     
*        1 = LENGTH OF KEY WORD                                                 
*      2-39= DESCRIPTION                                                        
F11      DC    X'11'                                                            
         DC    AL2(F12-F11)                                                     
*                                                                               
         DC    AL1(L'F11A)                                                      
         DC    AL1(5)                                                           
F11A     DC    C'DATA=PROF,OFF,GRP,FIN,AOFF,ACCO,SFH,FRZ'                       
         DC    AL1(L'F11B)                                                      
         DC    AL1(4)                                                           
F11B     DC    C'CHARNN=N'                                                      
         DC    AL1(L'F11C)                                                      
         DC    AL1(7)                                                           
F11C     DC    C'OFFICE=XX, -XX, XX-YY'                                         
         DC    AL1(L'F11D)                                                      
         DC    AL1(4)                                                           
F11D     DC    C'GRP=X'                                                         
         DC    AL1(L'F11E)                                                      
         DC    AL1(6)                                                           
F11E     DC    C'GROUP=X'                                                       
         DC    AL1(L'F11F)                                                      
         DC    AL1(4)                                                           
F11F     DC    C'FIN=Y,N'                                                       
         DC    AL1(L'F11G)                                                      
         DC    AL1(8)                                                           
F11G     DC    C'AOFFICE=XX'                                                    
         DC    AL1(L'F11H)                                                      
         DC    AL1(7)                                                           
F11H     DC    C'ACCOFF=XX'                                                     
         DC    AL1(L'F11J)                                                      
         DC    AL1(4)                                                           
F11J     DC    C'SFH=Y,N'                                                       
         DC    AL1(L'F11K)                                                      
         DC    AL1(4)                                                           
F11K     DC    C'FRZ=Y,N'                                                       
         DC    X'FF'                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
F12      DC    X'12'                                                            
         DC    AL2(F13-F12)                                                     
         DC    AL1(L'F12A)                                                      
         DC    AL1(4)                                                           
F12A     DC    C'DIV=NNN'                                                       
*                                                                               
         DC    AL1(L'F12B)                                                      
         DC    AL1(5)                                                           
F12B     DC    C'ADJC=XXX'                                                      
*                                                                               
         DC    AL1(L'F12C)                                                      
         DC    AL1(5)                                                           
F12C     DC    C'DATA=ADJC,BILLF,USER1,USER2'                                   
         DC    X'FF'                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
F13      DC    X'13'                                                            
         DC    AL2(F14-F13)                                                     
         DC    AL1(L'F13A)                                                      
         DC    AL1(4)                                                           
F13A     DC    C'EST=NNN,NNN-NNN,FIRST'                                         
*                                                                               
         DC    AL1(L'F13B)                                                      
         DC    AL1(5)                                                           
F13B     DC    C'DATE=MMMDD/YY-MMMDD/YY'                                        
*                                                                               
         DC    AL1(L'F13C)                                                      
         DC    AL1(7)                                                           
F13C     DC    C'STATUS=1,2,TEST,LIVE,1TEST,2TEST,1LIVE,2LIVE'                  
*                                                                               
         DC    AL1(L'F13D)                                                      
         DC    AL1(2)                                                           
F13D     DC    C'N=A,-A      N=FILTER POSITION,A=VALUE'                         
*                                                                               
         DC    AL1(L'F13E)                                                      
         DC    AL1(10)                                                          
F13E     DC    C'JOB OR AD=XXXXXX'                                              
*                                                                               
         DC    AL1(L'F13F)                                                      
         DC    AL1(4)                                                           
F13F     DC    C'REP=XXXX'                                                      
*                                                                               
         DC    AL1(L'F13G)                                        L01           
         DC    AL1(7)                                              L01          
F13G     DC    C'RETSCH=XX'                                        L01          
*                                                                  L01          
         DC    AL1(L'F13H)                                                      
         DC    AL1(9)                                                           
F13H     DC    C'RATETYPE=X'                                                    
*                                                                               
*                                                                               
         DC    AL1(L'F13M)                                                      
         DC    AL1(5)                                                           
F13M     DC    C'DATA=PROF,BILLF,DATES,JOB,REP,COMMENT,USER1,USER2,STATX        
               US,RATETYPE'                                                     
*                                                                               
         DC    X'FF'                                                            
*                                                                               
F14      DC    X'14'                                                            
         DC    AL2(F15-F14)                                                     
*                                                                               
         DC    AL1(L'F14A)                                                      
         DC    AL1(4)                                                           
F14A     DC    C'EST=NNN,NNN-NNN,NO'                                            
*                                                                               
         DC    AL1(L'F14B)                                                      
         DC    AL1(4)                                                           
F14B     DC    C'MOS=MMM/YY'                                                    
*                                                                               
         DC    AL1(L'F14C)                                                      
         DC    AL1(7)                                                           
F14C     DC    C'BILLMO=MMM/YY'                                                 
*                                                                               
         DC    AL1(L'F14D)                                                      
         DC    AL1(6)                                                           
F14D     DC    C'INVNO=NNNNNN,GTNNNNNN,LTNNNNNN'                                
*                                                                               
         DC    AL1(L'F14E)                                                      
         DC    AL1(9)                                                           
F14E     DC    C'BILLDATE=MMMDD/YY'                                             
*                                                                               
         DC    AL1(L'F14F)                                                      
         DC    AL1(9)                                                           
F14F     DC    C'BILLTYPE=B4-B7,M4-M7,AOR'                                      
         SPACE 2                                                                
         DC    AL1(L'F14G)                                                      
         DC    AL1(3)                                                           
F14G     DC    C'CD=YES OR NO (DEFAULT)'                                        
         SPACE 2                                                                
         DC    AL1(L'F14H)                                                      
         DC    AL1(4)                                                           
F14H     DC    C'RUN=SOON OR OV (OVERNIGHT)'                                    
         SPACE 2                                                                
         DC    AL1(L'F14I)                                                      
         DC    AL1(6)                                                           
F14I     DC    C'TOTAL=YES -SHOW ONLY TOTAL LINE'                               
         DC    X'FF'                                                            
*                                                                               
F15      DC    X'15'                                                            
         DC    AL2(F15X+1-F15)                                                  
         DC    AL1(L'F15A)                                                      
         DC    AL1(9)                                                           
F15A     DC    C'INACTIVE=YES OR NO'                                            
F15X     DC    X'FF'                                                            
*                                                                               
F16      DC    X'16'                                                L02         
         DC    AL2(F16X+1-F16)                                      L02         
         DC    AL1(L'F16A)                                          L02         
         DC    AL1(6)                                               L02         
F16A     DC    C'*** NONE EXIST FOR PUBLISTS ***'                               
F16X     DC    X'FF'                                                L02         
*                                                                               
*                                                                               
F1A      DC    X'1A'                                                L02         
         DC    AL2(F1AX+1-F1A)                                      L02         
         DC    AL1(L'F1AA)                                          L02         
         DC    AL1(6)                                               L02         
F1AA     DC    C'*** NONE EXIST FOR DIVISIONS ***'                              
F1AX     DC    X'FF'                                                L02         
*                                                                               
*                                                                               
F1B      DC    X'1B'                                                L02         
         DC    AL2(F1BX+1-F1B)                                      L02         
         DC    AL1(L'F1BA)                                          L02         
         DC    AL1(6)                                               L02         
F1BA     DC    C'*** NONE EXIST FOR REGIONS ***'                                
F1BX     DC    X'FF'                                                L02         
*                                                                               
*                                                                               
F1C      DC    X'1C'                                                L02         
         DC    AL2(F1CX+1-F1C)                                      L02         
         DC    AL1(L'F1CA)                                          L02         
         DC    AL1(6)                                               L02         
F1CA     DC    C'*** NONE EXIST FOR DISTRICTS ***'                              
F1CX     DC    X'FF'                                                L02         
*                                                                               
*                                                                               
F20      DC    X'20'                                                L02         
         DC    AL2(F20X+1-F20)                                      L02         
         DC    AL1(L'F20A)                                          L02         
         DC    AL1(6)                                               L02         
F20A     DC    C'*** NONE EXIST FOR REPS ***'                                   
F20X     DC    X'FF'                                                L02         
*                                                                               
F21      DC    X'21'                                                            
         DC    AL2(F21X+1-F21)                                                  
         DC    AL1(L'F21A)                                                      
         DC    AL1(6)                                                           
F21A     DC    C'LINES=NN,ALL'                                                  
         DC    AL1(L'F21B)                                                      
         DC    AL1(7)                                                           
F21B     DC    C'FILTER=1-6X'                                                   
F21X     DC    X'FF'                                                            
*                                                                               
F22      DC    X'22'                                                            
         DC    AL2(F22X+1-F22)                                                  
         DC    AL1(L'F22A)                                                      
         DC    AL1(6)                                                           
F22A     DC    C'*** NONE EXIST FOR PUBLISHERS ***'                             
F22X     DC    X'FF'                                                            
*                                                                               
F23      DC    X'23'                                                            
         DC    AL2(F23X+1-F23)                                                  
         DC    AL1(L'F23A)                                                      
         DC    AL1(6)                                                           
F23A     DC    C'*** NONE EXIST FOR PUBLISHER PUBS ***'                         
F23X     DC    X'FF'                                                            
*                                                                               
F30      DC    X'30'                                                            
         DC    AL2(F30X+1-F30)                                                  
         DC    AL1(L'F30A)                                                      
         DC    AL1(7)                                                           
F30A     DC    C'CHARNN=N'                                                      
F30X     DC    X'FF'                                                            
*                                                                               
F31      DS    0C                                                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
RECDESC  CSECT                                                                  
*                                                                               
* RECORD LIST                                                                   
*        0 = OVERLAY NUMBER   X'FF' FOR END OF LIST                             
*      1-2 = DISPLACEMENT TO NEXT OVERLAY DESCRIPTION                           
*                                                                               
*   VARIBALE NUMBER OF DESCRIPTION FIELDS                                       
*        0 = LENGTH OF DESCRIPTION OR X'FF' FOR END OF LIST                     
*        1 = LENGTH OF KEY WORD                                                 
*      2-39= DESCRIPTION                                                        
R11      DC    X'11'                                                            
         DC    AL2(R12-R11)                                                     
*                                                                               
         DC    AL1(L'R11A)                                                      
         DC    AL1(0)                                                           
R11A     DC    C'CLIENT OR CLTHDR'                                              
         DC    AL1(L'R11B)                                                      
         DC    AL1(0)                                                           
R11B     DC    C'PRODUCT OR PRDHDR'                                             
         DC    AL1(L'R11C)                                                      
         DC    AL1(0)                                                           
R11C     DC    C'ESTIMATE OR ESTHDR'                                            
         DC    AL1(L'R11E)                                                      
         DC    AL1(0)                                                           
R11E     DC    C'BILLREC OR INVOICE'                                            
         DC    AL1(L'R11F)                                                      
         DC    AL1(0)                                                           
R11F     DC    C'ADREC OR JOBREC'                                               
         DC    AL1(L'R11G)                                                      
         DC    AL1(0)                                                           
R11G     DC    C'DIVISION'                                                      
         DC    AL1(L'R11H)                                                      
         DC    AL1(0)                                                           
R11H     DC    C'REGION'                                                        
         DC    AL1(L'R11I)                                                      
         DC    AL1(0)                                                           
R11I     DC    C'DISTRICT OR DSTREC'                                            
         DC    AL1(L'R11J)                                                      
         DC    AL1(0)                                                           
R11J     DC    C'REP'                                                           
         DC    AL1(L'R11K)                                                      
         DC    AL1(0)                                                           
R11K     DC    C'COMMENT'                                                       
         DC    AL1(L'R11L)                                                      
         DC    AL1(0)                                                           
R11L     DC    C'PUBLIST'                                                       
         DC    AL1(L'R11M)                                                      
         DC    AL1(0)                                                           
R11M     DC    C'PUBLISHER'       TYPE OF REP                                   
         DC    AL1(L'R11N)                                                      
         DC    AL1(0)                                                           
R11N     DC    C'PPUBLIST'         PUBLISHER PUB LIST                           
         DC    X'FF'                                                            
R12      DC    X'FFFF'             END OF LIST                                  
         SPACE 2                                                                
LINLEN   EQU   88                                                               
MSSNGERR EQU   1                                                                
INVERR   EQU   2                                                                
ACCSERR  EQU   96                                                               
NEWERRS  EQU   254                                                              
NOXFR    EQU   422                                                              
PUTCURS  EQU   424                                                              
NOKEYDTA EQU   425                                                              
*                                                                               
         EJECT                                                                  
* DDCOREQUS                                                                     
* DDCOMFACS                                                                     
* FAFACTS                                                                       
* FAUTL                                                                         
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
       ++INCLUDE PPSINFOWRK                                                     
