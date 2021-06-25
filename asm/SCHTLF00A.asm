*          DATA SET SCHTLF00A  AT LEVEL 150 AS OF 05/01/02                      
*PHASE T21900A                                                                  
*INCLUDE GETPROF                                                                
*INCLUDE BRDWK                                                                  
*INCLUDE DPTRD                                                                  
         TITLE 'T21900 - SPOTPAK FILE MAINTENANCE'                              
T21900   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1500,**T21900,RR=R9                                              
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING GENOLD,RC                                                        
*                                                                               
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
*                                                                               
         GOTO1 =A(INITL),RR=RELO                                                
*                                                                               
         L     RA,VTWA                                                          
         USING T219FFD,RA                                                       
*                                                                               
         LA    R6,SPCOMMON         SET UP COMMON FAC ADDRESSES                  
         SR    R7,R7                                                            
         LA    R8,ERROR                                                         
         LA    R0,SPCOMCNT                                                      
*                                                                               
INIT10   ST    R6,0(R8)                                                         
         STC   R7,0(R8)                                                         
         LA    R7,4(R7)                                                         
         LA    R8,4(R8)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SVOVSYS,FAOVSYS     2=SPOT,3=NET                                 
         MVI   SVSTEREO,C'N'                                                    
         TM    FATSTAT6,X'80'                                                   
         BZ    *+8                                                              
         MVI   SVSTEREO,C'Y'                                                    
         DROP  RF                                                               
         DROP  R1                                                               
*                                                                               
         MVI   DMCB+7,QGETBRD      GET A(GETBROAD)                              
         BAS   RE,FLCALLOV                                                      
         MVC   VGETBRD,0(R1)                                                    
         MVI   DMCB+7,QSTAPACK     GET A(MSPACK) AND A(MSUNPK)                  
         BAS   RE,FLCALLOV                                                      
         MVC   VSTAPACK,0(R1)                                                   
         MVI   DMCB+7,QPSTVAL      GET A(PSTVAL)                                
         BAS   RE,FLCALLOV                                                      
         MVC   VPSTVAL,0(R1)                                                    
         LA    RE,GOMSPACK                                                      
         ST    RE,VMSPACK                                                       
         LA    RE,GOMSUNPK                                                      
         ST    RE,VMSUNPK                                                       
         B     FL0                                                              
*                                                                               
FLCALLOV LR    R0,RE                                                            
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
FL0      L     RF,=V(GETPROF)                                                   
         A     RF,RELO             RELOCATE                                     
         ST    RF,VGETPROF                                                      
         L     RF,=V(BRDWK)                                                     
         A     RF,RELO             RELOCATE                                     
         ST    RF,VBRDWK                                                        
         L     RF,=V(DPTRD)                                                     
         A     RF,RELO             RELOCATE                                     
         ST    RF,VDPTRD                                                        
         XC    LFMMSG,LFMMSG                                                    
         FOUT  LFMMSGH                                                          
*                                                                               
         OC    SVSAGN,SVSAGN       TEST NEW SECURITY                            
         BZ    FL0A                NO                                           
         L     RE,ASECBLK                                                       
         OC    0(16,RE),0(RE)                                                   
         BNZ   FL0A                                                             
         L     RF,VCOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
FL0A     XC    WORK,WORK           READ F0 PROFILE                              
         MVC   WORK+16(4),=C'S0F0'                                              
         MVC   WORK+20(2),AGYALPHA                                              
         MVC   WORK+22(1),SVEBCMED                                              
         MVC   WORK+23(3),SVEBCCLT                                              
         GOTO1 VGETPROF,DMCB,WORK+16,WORK,VDATAMGR                              
         MVC   SVF0PROF,WORK       SAVE F0 PROFILE                              
*                                                                               
         LA    R2,LFMRECH                                                       
         TM    4(R2),X'20'                                                      
         BO    FL1                                                              
         CLC   8(3,R2),=C'PFM'    ARE WE DOING AN AUTO PFM?                     
         BE    FL1                                                              
         CLC   8(3,R2),=C'INF'    ARE WE DOING AN AUTO INFO ?                   
         BE    FL1                                                              
*                                                                               
         XC    LFMREXP,LFMREXP                                                  
         FOUT  LFMREXPH                                                         
         BAS   RE,CLRKEXP                                                       
* SET TO EDIT ACTION AND KEY                                                    
         NI    LFMACTH+4,X'DF'                                                  
         NI    LFMKEYH+4,X'DF'                                                  
         XC    SVDATA,SVDATA                                                    
         MVI   ERRCD,INVERR                                                     
* VALIDATE RECORD                                                               
FL1      DS    0H                                                               
         GOTO1 =A(TESTXFR),RR=RELO  TEST A RETURN CALL                          
         BE    EXIT                                                             
*                                                                               
         CLI   PFKEY,12            IS IT A RETURN REQUEST                       
         BNE   FL1B                NO                                           
         CLI   SVXFRSY,0           DO WE HAVE SOMEPLACE TO GO BACK TO           
         BE    FL1B                NO - IGNORE                                  
         MVI   BYTE,C'R'           SET TO BUILD RETURN CALL                     
         GOTO1 =A(CALLXFR),RR=RELO                                              
         B     EXIT                                                             
*                                                                               
FL1B     MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(NEEDAKEY)                                            
         MVI   BYTE,C'I'             SET FLAG FOR PFM                           
         CLC   8(3,R2),=C'INF'       ARE WE DOING AN AUTO INFO ?                
         BE    FL1C                                                             
         CLC   8(3,R2),=C'PFM'       ARE WE DOING AN AUTO PFM?                  
         BNE   FL1D                                                             
         CLI   T219FFD+1,C'*'        TEST DDS TERMINAL                          
         BNE   FLERR                 NO                                         
         MVI   BYTE,C'P'             SET FLAG FOR PFM                           
*                                                                               
FL1C     OC    SVKEY+14(4),SVKEY+14  MAKE SURE HAVE SOMETHING                   
         BZ    FLERR                                                            
         GOTO1 =A(CALLXFR),RR=RELO                                              
         B     EXIT                POS'N CURSOR AND EXIT                        
*                                                                               
FL1D     MVI   ERRCD,INVERR                                                     
         CLI   5(R2),3                                                          
         BL    FLERR                                                            
         CLI   5(R2),8                                                          
         BH    FLERR                                                            
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
*                                                                               
         L     R7,=A(RECLIST)                                                   
         A     R7,RELO                                                          
         LH    R8,0(R7)                                                         
         L     R9,2(R7)                                                         
         A     R9,RELO                                                          
         LA    R7,6(R7)                                                         
*                                                                               
         EX    RE,FLCLC                                                         
         BE    FL2                                                              
         BXLE  R7,R8,*-8                                                        
         B     FLERR                                                            
FLCLC    CLC   8(0,R2),0(R7)                                                    
*                                                                               
FL2      MVC   SVREC,8(R7)                                                      
         MVC   SVOVLY,9(R7)                                                     
         MVC   BYTE2,10(R7)        SAVE SCREEN NUMBER                           
         MVC   SVLOCK,11(R7)       AND RECORD LOCKOUT CONTROL                   
         NI    SVLOCK+1,X'FF'-X'04'  DROP THE BIT THAT'S OR'D                   
*                                                                               
         CLI   13(R7),X'FD'        MOVED TO NET/SFM                             
         BE    FL2AA                                                            
*                                                                               
         CLI   13(R7),X'FE'        TEST MOVED TO SFM                            
         BL    FL2B                NO                                           
         BH    FL2A                OK IF SYSTEM = NET                           
         CLI   SVOVSYS,3           X'FE' OK IF SYS=NET                          
         BE    FL2B                                                             
*                                                                               
FL2A     MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(NOWINSFM)                                            
         CLI   SVOVSYS,3           SYS = NET?                                   
         BNE   *+10                                                             
FL2AA    MVC   NERRCD,=AL2(NOWINNET)                                            
         LA    R2,LFMRECH                                                       
         B     FLERR                                                            
*                                                                               
FL2B     CLI   SVREC,X'11'         TEST CLT REC                                 
         BE    FL2C                FORCE READ AGY                               
         CLI   SVREC,X'31'         TEST CL2 REC                                 
         BE    FL2C                FORCE READ AGY                               
         OC    SVAPROF,SVAPROF                                                  
         BNZ   FL2X                                                             
         SPACE 1                                                                
* DO NOT READ AGYHDR FOR SFM RECORDS (32-35)                                    
         SPACE 1                                                                
         CLI   SVREC,X'32'         TEST SFM REC                                 
         BL    *+12                                                             
         CLI   SVREC,X'35'                                                      
         BNH   FL2X                                                             
FL2C     GOTO1 =A(READAGY),RR=RELO   ELSE READ AGYHDR                           
*                                                                               
FL2X     DS    0H                                                               
*                                                                               
         MVI   ERRCD,ACCSERR                                                    
         CLI   T219FFD+1,C'*'      TEST DDS TERM                                
         BE    FL3                                                              
         TM    SVLOCK,X'01'        TEST RESTRICTED REC                          
         BO    FLERR               YES - ERROR                                  
*                                                                               
FL3      CLI   SVAPROF+7,C'C'      CANADIAN AGY                                 
         BE    FL4                                                              
         CLC   AGYALPHA,=C'T1'                                                  
         BE    FL4                                                              
         MVI   ERRCD,INVERR                                                     
         CLI   SVREC,X'2B'                                                      
         BE    FLERR                                                            
         CLI   SVREC,X'2C'                                                      
         BE    FLERR                                                            
         CLI   SVREC,X'2D'                                                      
         BE    FLERR                                                            
         CLI   SVREC,X'2E'                                                      
         BE    FLERR                                                            
*                                                                               
FL4      OI    4(R2),X'20'         SET REC VALID                                
         MVC   LFMREXP(6),=C'KEY IS'                                            
*                                                                               
         L     R7,=A(KEYLIST)                                                   
         A     R7,RELO                                                          
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
         MVC   LFMREXP+7(0),2(R7)  *EXECUTED*                                   
*                                                                               
         CLI   T219FFD+1,C'*'      TEST DDS TERM                                
         BE    FL10                                                             
         MVC   HALF2(1),SVLOCK+1   MOVE SAVED LOCKOUT BITS                      
         NC    HALF2(1),T219FFD+12 'AND' RESTRICTED ACCESS BITS                 
         BZ    FL10                IF OFF,OK                                    
         NI    4(R2),X'DF'         FORCE REC EDIT NEXT TIME                     
         CLC   =C'DIS',LFMACT      AND ALLOW DISPLAY ONLY                       
         BE    FL10                                                             
         CLI   LFMACTH+5,0         IF NO ACTION GIVE NORMAL ERROR               
         BE    FL10                                                             
         MVI   ERRCD,ACCSERR                                                    
         B     FLERR                                                            
         EJECT                                                                  
* VALIDATE ACTION                                                               
FL10     LA    R2,LFMACTH                                                       
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    FL20                                                             
*                                                                               
         CLI   SVACT,C'D'          WAS LAST ACTION DISPLAY                      
         BNE   FL14                                                             
         BAS   RE,FLACT            EDIT ACTION                                  
         CLI   SVACT,C'C'          IS NEW ACTION CHANGE                         
         BE    FL20                YES - NO KEY EDIT REQUIRED                   
FL12     NI    LFMKEYH+4,X'DF'     ELSE SET TO EDIT KEY                         
         BAS   RE,CLRKEXP                                                       
         B     FL20                                                             
*                                                                               
* OLD ACTION NOT DISPLAY                                                        
*                                                                               
FL14     CLI   SVACT,C'A'          WAS LAST ACTION ADD                          
         BNE   FL16                NO                                           
         BAS   RE,FLACT            EDIT ACTION                                  
         CLI   SVACT,C'C'          IS NEW ACTION CHANGE                         
         BNE   FL12                NO - MUST EDIT KEY                           
         CLI   SVKEY,C' '          SEE IF STATION FILE                          
         BH    FL20                                                             
         OC    SVKEY+14(4),SVKEY+14    WILL HAVE DISK ADDRESS IF ADD            
         BZ    FL12                WAS SUCCESSFUL                               
*                                  IF NOT MUST EDIT KEY                         
         B     FL20                                                             
*                                                                               
FL16     NI    LFMKEYH+4,X'DF'     SET TO EDIT KEY                              
         BAS   RE,CLRKEXP                                                       
         BAS   RE,FLACT                                                         
         B     FL20                                                             
         EJECT                                                                  
* THIS SUBROUTINE EDITS ACTION.                                                 
*                                                                               
FLACT    MVI   ERRCD,INVERR                                                     
         CLI   5(R2),3                                                          
         BL    FLERR                                                            
         CLI   5(R2),8                                                          
         BH    FLERR                                                            
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         BCTR  R5,0                                                             
*                                                                               
         L     R7,=A(ACTLIST)                                                   
         A     R7,RELO                                                          
         LH    R8,0(R7)                                                         
         L     R9,2(R7)                                                         
         A     R9,RELO                                                          
         LA    R7,6(R7)                                                         
*                                                                               
         EX    R5,FLCLC            CLC  8(0,R2),0(R7)                           
         BE    FLACTX                                                           
         BXLE  R7,R8,*-8                                                        
         B     FLERR                                                            
*                                                                               
FLACTX   MVC   SVACT,8(R7)         SAVE ACTION CODE                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
FL20     CLI   SVACT,C'A'          TEST ADD                                     
         BE    FL30                                                             
         CLI   SVACT,C'C'          TEST CHANGE                                  
         BE    FL30                                                             
         CLI   SVACT,C'D'          TEST DISPLAY                                 
         BE    FL30                                                             
* CHECK SPECIAL ACTION TABLE                                                    
         L     R4,=A(SPCLACTS)                                                  
         A     R4,RELO                                                          
FL22     CLC   0(1,R4),SVREC       MATCH REC                                    
         BNE   FL24                                                             
         CLC   1(1,R4),SVACT       MATCH ACT                                    
         BNE   FL24                                                             
*                                                                               
         MVC   SVOVLY,2(R4)        SET OVERLAY                                  
         MVC   BYTE2,3(R4)          AND SCREEN                                  
         CLC   2(2,R4),=X'1CFC'    CHK FOR EST COPY                             
         BNE   FL22D                                                            
         CLI   SVAPROF+7,C'C'      CHK FOR CANADIAN                             
         BNE   FL30                                                             
         CLI   LFMKEY,C'T'         CHK MEDIA IN FIELD HEADER                    
         BE    FL30                                                             
         CLI   LFMKEY,C'R'         CHK MEDIA IN FIELD HEADER                    
         BE    FL30                                                             
         B     FL25   ONLY MEDIA T OR R ALLOWED FOR CANADIAN EST COPY           
*                                                                               
FL22D    CLC   2(2,R4),=X'18F8'    CHK FOR PRD DELETE                           
         BNE   FL30                                                             
         CLI   SVOVSYS,3           2=SPOT,3=NET                                 
         BNE   FL30                                                             
         B     FL25                NO PRD DELETE FOR NETPAK                     
*                                                                               
FL24     LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   FL22                                                             
FL25     LA    R2,LFMACTH                                                       
         MVI   ERRCD,INVERR                                                     
         B     FLERR                                                            
         EJECT                                                                  
*----------------------------------------------------------*                    
* TEST NEW SECURITY IF ACTIVE                              *                    
*----------------------------------------------------------*                    
         SPACE 1                                                                
FL30     DS    0H                                                               
         OC    SVSAGN,SVSAGN       TEST NEW SECURITY ACTIVE                     
         BZ    FL30A               NO                                           
* TEST RECORD/ACTION COMBINATION VALID                                          
         L     RF,VCOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         XC    DUB,DUB                                                          
         MVC   DUB(1),SVACT                                                     
         LA    R0,DUB           POINT TO ACTION CODE                            
         ICM   R0,8,SVREC          RECORD TYPE IN HOB                           
         GOTO1 (RF),DMCB,('SECPRACT',ASECBLK),(R0)                              
         CLI   0(R1),SECPYES                                                    
         BE    FL30A                                                            
         LA    R2,LFMRECH                                                       
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(NOAUTHOR)                                            
         B     FLERR                                                            
         SPACE 1                                                                
*----------------------------------------------------------*                    
* FETCH NEW SCREEN IF REQUIRED                             *                    
*----------------------------------------------------------*                    
         SPACE 1                                                                
FL30A    CLC   SVSCRN,BYTE2                                                     
         BE    FL31                                                             
* FETCH SCREEN                                                                  
         MVC   SVSCRN,BYTE2       SAVE NEW SCREEN                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(3),=X'D90219'                                             
         MVC   DMCB+7(1),SVSCRN                                                 
         GOTO1 VCALLOV,DMCB,LFMTABH                                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'        SCREEN NOT FOUND                                     
*                                                                               
         CLI   SVREC,X'19'         TEST MEDIA COMMENT RECORD                    
         BNE   FL30X                                                            
         SR    R0,R0               YES-REMOVE LINES 11-14 FROM                  
         LA    R2,COML10H              COMMENT SCREEN                           
         LA    RF,4                                                             
FL30B    IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(2,R2),=C'  '                                                   
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,FL30B                                                         
*                                                                               
FL30X    LA    R2,LFMTABH                                                       
         BAS   RE,FNDUF         FIND FIRST UNP FIELD                            
         MVI   5(R2),0                                                          
* ADD PFKEY DESCRIPTION ON LINE 24 IF NOT STEREO                                
         CLI   SVSTEREO,C'Y'                                                    
         BE    FL31                                                             
         LA    R2,LFMTABH          FIND EOS                                     
FL30X2   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    FL30X4                                                           
         CLC   2(2,R2),=AL2(23*80)   PAST ROW 24/COL 1                          
         BNH   FL30X2                NO                                         
* IF IN LINE 24, THINK ABOUT WHERE THE FIELD ENDS !                             
         SR    RE,RE                                                            
         IC    RE,0(R2)            LENGTH OF FLDHDR                             
         SH    RE,=H'8'                                                         
         TM    1(R2),X'02'         TEST EXTENDED FLDHDR                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         SR    RF,RF                                                            
         ICM   RF,3,2(R2)          GET FIELD START                              
         AR    RE,RF               GIVES END + 1                                
         CLM   RE,3,STRFLDH+2      COMPARE TO MY START POSN                     
         BL    FL30X2                                                           
         B     FL31                                                             
*                                                                               
FL30X4   MVC   0(STRFLDX-STRFLDH+3,R2),STRFLDH  (INCLUDE X'000101')             
         CLI   SVXFRSY,0           TEST RETURN POSSIBLE                         
         BNE   *+14                                                             
         LA    RE,STRPF12-STRFLDH(R2)                                           
         XC    0(STRFLDX-STRPF12,RE),0(RE) CLEAR PF12 MESSAGE                   
         B     FL31                                                             
*                                                                               
STRFLDH  DC    AL1(STRFLDX-STRFLDH)                                             
         DC    X'20'               PROTECTED                                    
         DC    AL2(23*80+55)       ROW 24/COL 66                                
         DC    X'00'               INDS                                         
         DC    X'00'               INPUT LENGTH                                 
         DC    X'80'               TRANSMIT                                     
         DC    AL1(STRFLDX-STRFLDH-8)  DATA LENGTH                              
         DC    C'PF2=',X'C9958696409389A2A3' (INFO LIST)                        
STRPF12  DC    C' PF12=',X'D985A3A49995'      (RETURN)                          
STRFLDX  EQU   *                                                                
         DC    X'000101'           NEED NEW EOS FIELD MORON                     
*                                                                               
         EJECT                                                                  
*=================================================================*             
* KEY VALIDATION                                                  *             
*=================================================================*             
         SPACE 1                                                                
FL31     CLI   PFKEY,2             TEST PF2 ENTERED                             
         BNE   FL31B               NO                                           
         TM    LFMKEYH+4,X'20'     TEST KEY VALIDATED                           
         BO    FL31A               YES                                          
         CLI   LFMKEYH+5,0         TEST ANY KEY DATA                            
         BNE   FL31B               YES - VALIDATE IT                            
*                                                                               
FL31A    MVI   BYTE,C'I'           SET FOR INFO CALL                            
         GOTO1 =A(CALLXFR),RR=RELO                                              
         B     EXXMOD                                                           
*                                                                               
FL31B    LA    R2,LFMKEYH                                                       
         CLI   SVACT,C'A'          ALWAYS REVALIDATE KEY ON ADD                 
         BE    FL32                                                             
         TM    4(R2),X'20'         TEST VALID                                   
         BZ    FL32                                                             
         MVI   SVFMTSW,1           SET FOR EDIT                                 
         CLI   SVACT,C'S'          SPECIAL DDS DISPLAY                          
         BNE   FL40                NO                                           
         MVI   SVFMTSW,0           ALWAYS RESET TO FORMAT                       
         B     FL40                                                             
*                                                                               
* KEY NOT VALIDATED                                                             
*                                                                               
FL32     NI    4(R2),X'DF'         SET NOT VALID                                
         BAS   RE,CLRKEXP                                                       
         MVI   ERRCD,MSSNGERR                                                   
         XC    SVKEY,SVKEY                                                      
         CLI   5(R2),0                                                          
         BE    FLERR                                                            
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
         CLI   PFKEY,2                                                          
         BNE   FL34                                                             
         MVI   BYTE,C'I'           SET FOR INFO CALL                            
         GOTO1 =A(CALLXFR),RR=RELO                                              
         B     EXXMOD                                                           
*                                                                               
FL34     CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         EJECT                                                                  
FL40     DS    0H                                                               
         CLI   SVACT,C'A'          TEST ADD                                     
         BNE   FL42                                                             
         MVC   LFMMSG(23),=C'** ENTER RECORD DATA **'                           
*                                                                               
         LA    R2,LFMTABH                                                       
         BAS   RE,FNDUF            FIND FIRST UNP FIELD                         
*                                                                               
FL40A    CLI   5(R2),0             ANY INPUT                                    
         BNE   FL40C               YES                                          
*                                                                               
         CLI   SVREC,X'48'         TEST EXTENDED EST DATA                       
         BNE   FL40B               YES - SOME HAVE OPTIONAL FIELD 1             
         CLC   =C'PG',LFMREC                                                    
         BNE   FL40B                                                            
* FOR PGEST RECORD, LOOK THROUGH ALL UNP FIELDS                                 
         BAS   RE,NEXTUF                                                        
         BE    FL40A                                                            
         LA    R2,LFMTABH          NEED TO PUT CURSOR BACK TO FIRST UNP         
         BAS   RE,FNDUF                                                         
         B     FL41                                                             
*                                                                               
FL40B    CLI   SVREC,X'41'         NEW NETWORK UNIV                             
         BL    FL41             OR NEW NETWORK PROG                             
         CLI   SVREC,X'44'      OR NEW NETWORK HUT                              
         BH    FL41             OR NEW NETWORK HUT - MONTHS                     
*                                                                               
FL40C    MVI   SVFMTSW,1                                                        
         B     FL50                                                             
*                                                                               
* TEST IF NEED CONTROL BEFORE ADDS                                              
*                                                                               
FL41     L     R7,=A(RECLIST)                                                   
         A     R7,RELO                                                          
         LH    R8,0(R7)                                                         
         L     R9,2(R7)                                                         
         A     R9,RELO                                                          
         LA    R7,6(R7)                                                         
         CLC   SVREC,8(R7)         FIND TABLE ENTRY                             
         BE    *+10                                                             
         BXLE  R7,R8,*-10                                                       
         DC    H'0'                                                             
         TM    11(R7),X'80'                                                     
         BZ    EXIT                                                             
*                                                                               
FL41A    MVI   SVFMTSW,0           SET FOR FORMAT                               
         B     FL50                                                             
*                                                                               
         EJECT                                                                  
*=======================================================*                       
* ACTION IS NOT ADD                                     *                       
*=======================================================*                       
         SPACE 1                                                                
FL42     CLI   SVACT,C'D'          TEST DISPLAY                                 
         BNE   FL50                                                             
         CLI   SVFMTSW,0           FOR DISPLAY SWITCH MUST BE FMT               
         BE    FL50                                                             
         MVI   ERRCD,CHNGERR                                                    
         TM    SVLOCK,X'20'        TEST ALLOW DATA CHG ON DISPLAY               
         BZ    FLERR                                                            
         MVI   SVFMTSW,0           RESET SWITCH                                 
         EJECT                                                                  
FL50     DS    0H                                                               
         CLI   SVACT,C'R'          SET SWITCH FOR RESTORE                       
         BNE   *+8                                                              
         MVI   SVFMTSW,1           SET FOR EDIT                                 
* CLEAR RECORD AREA                                                             
         LA    R0,8                                                             
         LA    R1,REC                                                           
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
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
         BNE   EXXMOD                                                           
         EJECT                                                                  
         CLI   SVACT,C'A'          TEST ADD                                     
         BNE   FL52                                                             
         CLI   SVFMTSW,0           TEST FMT BEFORE ADD                          
         BE    EXIT                YES - LEAVE 'ENTER DATA' MSG                 
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(18),=C'** RECORD ADDED **'                                
         LA    R2,LFMRECH                                                       
         B     EXIT                                                             
*                                                                               
FL52     CLI   SVACT,C'C'          TEST CHANGE                                  
         BNE   FL54                                                             
         CLI   SVFMTSW,0           WAS THIS A FORMAT                            
         BNE   FL53                NO                                           
         MVC   LFMMSG(24),=C'** ENTER AMENDED DATA **'                          
         LA    R2,LFMTABH                                                       
         BAS   RE,FNDUF            FIND FIRST UNP FIELD                         
         B     EXIT                                                             
*                                                                               
FL53     MVC   LFMMSG(29),=C'** RECORD HAS BEEN CHANGED **'                     
         LA    R2,LFMRECH                                                       
         B     EXIT                                                             
*                                                                               
FL54     CLI   SVACT,C'D'          TEST DISPLAY                                 
         BNE   FL56                                                             
         MVC   LFMMSG(40),=CL40'** REQUESTED RECORD DISPLAYED **'               
         LA    R2,LFMRECH                                                       
         B     EXIT                                                             
*                                                                               
FL56     CLI   SVACT,C'X'          TEST DELETE                                  
         BNE   FL58                                                             
         CLI   SVFMTSW,0           WAS THIS A FORMAT                            
         BNE   FL57                                                             
         MVC   LFMMSG(40),=C'** ENTER DELETE CODE TO DELETE RECORD **'          
         LA    R2,LFMTABH                                                       
         BAS   RE,FNDUF                                                         
         B     EXIT                                                             
*                                                                               
FL57     MVC   LFMMSG(20),=C'** RECORD DELETED **'                              
         LA    R2,LFMRECH                                                       
         B     EXIT                                                             
*                                                                               
FL58     CLI   SVACT,C'R'          TEST RESTORE                                 
         BNE   FL60                                                             
         MVC   LFMMSG(21),=C'** RECORD RESTORED **'                             
         LA    R2,LFMRECH                                                       
         B     EXIT                                                             
*                                                                               
FL60     MVC   LFMMSG(22),=C'** ACTION COMPLETED **'                            
         LA    R2,LFMRECH                                                       
         B     EXIT                                                             
         EJECT                                                                  
CLRKEXP  OC    LFMKEXP,LFMKEXP                                                  
         BZR   RE                                                               
         XC    LFMKEXP,LFMKEXP                                                  
         FOUT  LFMKEXPH                                                         
         BR    RE                                                               
*                                                                               
FLERR    GOTO1 ERROR                                                            
         B     EXXMOD                                                           
*                                                                               
EXIT     OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
FNDUF    TM    1(R2),X'20'         TEST PROTECTED                               
         BCR   8,RE                NO = EXIT WITH CC EQ                         
NEXTUF   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
         EJECT                                                                  
SPCOMMON NTR1  BASE=BASERB                                                      
*                                                                               
         SRL   RF,24                                                            
         B     SPCOMTAB(RF)                                                     
*                                                                               
SPCOMTAB B     SPERROR             X'00'                                        
         B     SPANY               X'04'                                        
         B     SPMOVE              X'08'                                        
         B     SPPACK              X'0C'                                        
         B     SPREAD              X'10'                                        
         B     SPSEQ               X'14'                                        
         B     SPHIGH              X'18'                                        
         B     SPADD               X'1C'                                        
         B     SPDIR               X'20'                                        
         B     SPRDSTA             X'24'                                        
         B     SPSTA               X'28'                                        
         B     SPGETREC            X'2C'                                        
         B     SPPUTREC            X'30'                                        
         B     SPADDREC            X'34'                                        
         B     SPFIL               X'3C'                                        
         DC    5AL4(0)             X'40/44/48/4C/50' RESERVED                   
SPCOMUSR DC    9AL4(0)   ** USER ROUTINES ORIGIN HERE WITH X'54' **             
SPCOMCNT EQU   (*-SPCOMTAB)/4      NUMBER OF ENTRIES                            
         SPACE 2                                                                
SPCOMXIT XIT1                                                                   
         EJECT                                                                  
SPERROR  L     R4,ERRAREA                                                       
         CLI   ERRAREA,X'FF'       TEST PRESET MESSAGE                          
         BE    SPERROR2                                                         
*                                                                               
         CLI   ERRCD,NEWERR        IS THIS A 2 CHAR ERROR                       
         BNE   SPERROR1                                                         
*                                                                               
         MVI   ERRAREA,X'FF'                                                    
         XC    WORK,WORK           DEFINE CONTROL BLOCK                         
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         MVC   GTMTYP,GTMERR       SET MESSAGE TYPE TO ERROR                    
         MVC   GTMSGNO,NERRCD      AND MESSAGE NUMBER                           
         MVI   GTMSYS,2            AND MESSAGE SYSTEM                           
         DROP  R1                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),WORK           PUT OUT SYSTEM MESSAGE                       
         B     SPERROR2                                                         
*                                                                               
SPERROR1 MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVI   DMCB+20,2           SET OVERRIDE SYSTEM NUMBER                   
         GOTO1 VGETMSG,DMCB+12,(ERRCD,8(R4)),(X'FF',DMCB)                       
*                                                                               
SPERROR2 FOUT  (R4)                                                             
*                                                                               
         OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
         L     RD,BASERD           RETURN TO * BASE *                           
         B     SPCOMXIT                                                         
         SPACE 2                                                                
SPANY    CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         MVI   ERRCD,1                                                          
         B     SPERROR                                                          
*                                                                               
ANY2     TM    4(R2),X'10'                                                      
         BZ    SPCOMXIT                                                         
         MVI   ERRCD,3                                                          
         B     SPERROR                                                          
         EJECT                                                                  
SPPACK   SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1               EXIT ON ZERO LENGTH                          
         BZ    PACKX                                                            
         TM    4(R2),X'08'         OR NON-NUMERIC                               
         BZ    PACKX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+12                                                          
         CVB   R0,DUB                                                           
         B     PACKX                                                            
         PACK  DUB,8(0,R2)   * EXECUTED *                                       
*                                                                               
PACKX    XIT1  REGS=(R0,R1)                                                     
         SPACE 2                                                                
SPMOVE   MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    SPCOMXIT                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     PACKX                                                            
*                                                                               
         MVC   WORK(0),8(R2) * EXECUTED *                                       
         EJECT                                                                  
SPREAD   MVC   COMMAND,=C'DMREAD'                                               
         B     SPDIR                                                            
SPSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         B     SPDIR                                                            
SPHIGH   MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     SPDIR                                                            
SPADD    MVC   COMMAND,=C'DMADD'                                                
         B     SPDIR                                                            
SPWRITE  MVC   COMMAND,=C'DMWRT'                                                
*                                                                               
SPDIR    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTDIR',KEY,KEY               
*                                                                               
SPDIRX   TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         BZ    SPDIRX2             NO ERROR                                     
         CLI   COMMAND+2,C'R'      TEST READ COMMAND                            
         BE    SPDIRX2                                                          
         CLI   8(R1),X'20'         EXCEPT ON ADD ERROR TO STATION FILE,         
         BNE   *+18                                                             
         L     RF,4(R1)                                                         
         CLC   =C'STATION',0(RF)                                                
         BE    SPCOMXIT                                                         
         DC    H'0'                FORCE RECOVERY ON ADD/WRITE ERROR            
*                                                                               
SPDIRX2  MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,8(R1)                                                       
         BZ    SPCOMXIT                                                         
* DATAMGR ERROR HAS OCCURRED                                                    
         MVI   ERRCD,0                                                          
         B     SPERROR                                                          
         EJECT                                                                  
SPRDSTA  MVC   COMMAND,=C'DMREAD'                                               
*                                                                               
SPSTA    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'STATION',KEY,AREC             
*                                                                               
         B     SPDIRX                                                           
         EJECT                                                                  
SPGETREC MVC   COMMAND,=C'GETREC'                                               
         B     SPFIL                                                            
SPPUTREC MVC   COMMAND,=C'PUTREC'                                               
         B     SPFIL                                                            
SPADDREC MVC   COMMAND,=C'ADDREC'                                               
*                                                                               
SPFIL    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTFILE',KEY+14,AREC,X        
               DMWORK                                                           
*                                                                               
         CLI   COMMAND,C'G'        TEST GETREC                                  
         BE    SPDIRX2                                                          
         TM    8(R1),X'D0'         TEST EOF OR ERROR                            
         BZ    SPDIRX2                                                          
         DC    H'0'                FORCE RECOVERY ON ADD/WRITE ERRORS           
         EJECT                                                                  
*                                                                               
         ORG   SPCOMUSR                                                         
         B     SPCNADD                                                          
         B     SPCNCHA                                                          
         B     SPCNADDS                                                         
         B     SPCNCHAS                                                         
         ORG                                                                    
*                                                                               
SPCNADD  MVC   DMCB+4(4),=C'ADD '                                               
         B     GOSPCN                                                           
*                                                                               
SPCNCHA  MVC   DMCB+4(4),=C'CHA '                                               
         B     GOSPCN                                                           
*                                                                               
SPCNADDS MVC   DMCB+4(4),=C'ADDS'                                               
         B     GOSPCN                                                           
*                                                                               
SPCNCHAS MVC   DMCB+4(4),=C'CHAS'                                               
         B     GOSPCN                                                           
*                                                                               
GOSPCN   DS    0H                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY                              
         BNE   SPCOMXIT                                                         
         CLI   SVEBCMED,C'T'       TV ONLY                                      
         BNE   SPCOMXIT                                                         
         GOTO1 =A(SPCN),DMCB,RR=RELO                                            
         B     SPCOMXIT                                                         
         EJECT                                                                  
*=============================================================*                 
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
*=============================================================*                 
         SPACE 1                                                                
GOMSPACK NTR1  BASE=BASERB                                                      
         LR    R5,R1               SAVE CALLERS R1                              
         L     RA,VTWA                                                          
*                                                                               
         LA    R4,STAWRK                                                        
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,SVEBCMED                                                 
         MVC   STAPACOM,VCOMFACS                                                
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
GOMSUNPK NTR1  BASE=BASERB                                                      
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         LA    R4,STAWRK                                                        
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,SVEBCMED                                                 
         MVC   STAPACOM,VCOMFACS                                                
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SPCN     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'ADD ',4(R1)                                                   
         BE    CNADD                                                            
         CLC   =C'CHA ',4(R1)                                                   
         BE    CNCHA                                                            
         CLC   =C'ADDS',4(R1)                                                   
         BE    CNADDS                                                           
         CLC   =C'CHAS',4(R1)                                                   
         BE    CNCHAS                                                           
         DC    H'0'                                                             
*                                                                               
CNADD    LA    R5,REC+2                                                         
         CLI   REC,X'0D'        MGRPS + PGRPS HAVE AGY/MED IN REC+2             
         BE    CNADD2                                                           
         LA    R5,REC+1                                                         
CNADD2   NI    0(R5),X'F0'                                                      
         OI    0(R5),X'03'                                                      
         GOTO1 ADDREC                                                           
CNADD4   NI    0(R5),X'F0'                                                      
         OI    0(R5),X'08'                                                      
         GOTO1 ADDREC                                                           
         MVC   REC(13),SVKEY                                                    
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     SPCNX                                                            
         EJECT                                                                  
CNCHA    MVC   KEY,SVKEY                                                        
         LA    R5,KEY+2                                                         
         CLI   KEY,X'0D'                                                        
         BE    CNCHA1                                                           
         LA    R5,KEY+1                                                         
*                                                                               
CNCHA1   DS    0H                                                               
         NI    0(R5),X'F0'                                                      
         OI    0(R5),X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CNCHA2                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         B     CNADD               NOT FOUND SO GO ADD BOTH                     
*                                                                               
CNCHA2   LA    RE,REC2                                                          
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         MVC   REC(13),REC2                                                     
*                                  SEE IF ESTHDR                                
*                                  MUST PRESERVE BUCKETS                        
         CLI   REC,0                                                            
         BNE   CHCHA3                                                           
         CLI   REC+7,0                                                          
         BE    CHCHA3                                                           
         OC    REC+8(5),REC+8                                                   
         BNZ   CHCHA3                                                           
         MVC   REC+20(4),REC2+20       NET PAID TODAY                           
         MVC   REC+96(208),REC2+96     EORDN,EPAIDN                             
         MVC   REC+500(52),REC2+500    EAUTHN                                   
*                                                                               
CHCHA3   DS    0H                                                               
         GOTO1 PUTREC                                                           
         MVC   KEY,SVKEY                                                        
         NI    0(R5),X'F0'                                                      
         OI    0(R5),X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CNCHA4                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         B     CNADD               NOT FOUND - GO ADD IT                        
*                                                                               
CNCHA4   LA    RE,REC2                                                          
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         MVC   REC(13),REC2                                                     
*                                  SEE IF ESTHDR                                
*                                  MUST PRESERVE BUCKETS                        
         CLI   REC,0                                                            
         BNE   CHCHA5                                                           
         CLI   REC+7,0                                                          
         BE    CHCHA5                                                           
         OC    REC+8(5),REC+8                                                   
         BNZ   CHCHA5                                                           
         MVC   REC+20(4),REC2+20       NET PAID TODAY                           
         MVC   REC+96(208),REC2+96     EORDN,EPAIDN                             
         MVC   REC+500(52),REC2+500    EAUTHN                                   
*                                                                               
CHCHA5   DS    0H                                                               
         GOTO1 PUTREC                                                           
         MVC   REC(13),SVKEY                                                    
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     SPCNX                                                            
         EJECT                                                                  
CNADDS   LA    R5,REC+1                                                         
         MVI   0(R5),C'N'                                                       
         CLI   REC,C'S'            STA MASTERS                                  
         BE    CNADDS4                                                          
         CLI   REC,C'A'            ADDR RECS                                    
         BE    CNADDS4                                                          
         B     CNADDS5                                                          
*                                                                               
CNADDS4  MVI   REC+6,C'N'                                                       
*                                                                               
CNADDS5  MVC   COMMAND,=C'DMADD'                                                
         GOTO1 STA                                                              
         MVI   0(R5),C'C'                                                       
         CLI   REC,C'A'                                                         
         BE    CNADDS6                                                          
         CLI   REC,C'S'                                                         
         BE    CNADDS6                                                          
         B     CNADDS8                                                          
*                                                                               
CNADDS6  MVI   REC+6,C'C'                                                       
*                                                                               
CNADDS8  MVC   COMMAND,=C'DMADD'                                                
         GOTO1 STA                                                              
         MVC   REC(17),SVKEY                                                    
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     SPCNX                                                            
         EJECT                                                                  
CNCHAS   MVC   KEY,SVKEY                                                        
         LA    R5,KEY+1                                                         
         MVI   0(R5),C'N'          NETWORK                                      
         CLI   REC,C'S'            STA MASTERS                                  
         BE    CNCHAS4                                                          
         CLI   REC,C'A'            ADDR RECS                                    
         BE    CNCHAS4                                                          
         B     CNCHAS5                                                          
*                                  ALSO HAVE MEDIA IN REC+6                     
CNCHAS4  MVI   KEY+6,C'N'                                                       
*                                                                               
CNCHAS5  LA    RE,REC2                                                          
         ST    RE,AREC                                                          
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         CLC   KEY(15),REC2                                                     
         BE    CNCHAS6                                                          
         MVC   REC(17),KEY                                                      
         MVC   COMMAND,=C'DMADD'                                                
         GOTO1 STA                                                              
         B     CNCHAS6B                                                         
*                                                                               
CNCHAS6  MVC   REC(17),REC2        CHA KEYS                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 STA                                                              
*                                                                               
CNCHAS6B DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         MVI   0(R5),C'C'          COMBINED                                     
         CLI   KEY,C'A'                                                         
         BE    CNCHAS7                                                          
         CLI   KEY,C'S'                                                         
         BE    CNCHAS7                                                          
         B     CNCHAS8                                                          
*                                                                               
*                                  ADDR AND STA MASTER RECS                     
*                                  ALSO HAVE MEDIA IN KEY+6                     
CNCHAS7  MVI   KEY+6,C'C'                                                       
CNCHAS8  LA    RE,REC2                                                          
         ST    RE,AREC                                                          
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 STA                                                              
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         CLC   KEY(15),REC2                                                     
         BE    CNCHAS10            FOUND                                        
*                                                                               
         MVC   REC(17),KEY                                                      
         MVC   COMMAND,=C'DMADD'                                                
         GOTO1 STA                                                              
         B     CNCHASX                                                          
*                                                                               
CNCHAS10 MVC   REC(17),REC2                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 STA                                                              
*                                                                               
CNCHASX  MVC   REC(17),SVKEY                                                    
         MVC   KEY,SVKEY           RESTORE KEY                                  
*                                                                               
SPCNX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
READAGY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),AGYALPHA                                                
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BE    READAG2                                                          
         MVI   ERRCD,NEWERR        NO AGYHDR                                    
         MVC   NERRCD,=AL2(NOAGYREC)                                            
         GOTO1 ERROR                                                            
READAG2  DS    0H                                                               
         GOTO1 GETREC                                                           
         USING AGYHDRD,R8                                                       
         MVC   SVAPROF,AGYPROF                                                  
         MVC   SVAALPHA,AGYALPHA                                                
         MVC   SVACCOFC,AGYOFC2                                                 
         MVC   SVAGYFL1,AGYFLAG1                                                
         MVC   SVAGYFL2,AGYFLAG2                                                
         MVC   SVCTAGY,AGYCTAGY    CO-ORD AGENCY CODE                           
*                                                                               
         LA    R1,AGYEL                                                         
         SR    R0,R0                                                            
READAG4  ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    READAGX                                                          
         CLI   0(R1),3             FIND ACC AGENCY ELEMENT                      
         BNE   READAG4                                                          
         MVC   SVACCAGY,2(R1)      SAVE ACC AGENCY LIST                         
*                                                                               
READAGX  LA    R8,REC                                                           
         ST    R8,AREC                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==========================================================*                    
* INITIALIZATION ROUTINES                                  *                    
*==========================================================*                    
         SPACE 1                                                                
INITL    NTR1  BASE=*,LABEL=*                                                   
* CLEAR WORK AREA                                                               
         LA    R4,8(RC)            GET START OF AREA TO CLEAR                   
         L     R5,4(RD)            GET PREVIOUS RD VALUE                        
         SR    R5,R4               GIVES LENGTH TO CLEAR                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         LM    R2,R4,0(R1)                                                      
         ST    R3,VTWA                                                          
         LR    RA,R3               POINT RA TO TWA                              
         OI    LFMSRVH+6,X'81'     FORCE SRVREQ TO XMT/MOD                      
*                                                                               
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD          A(FIRST INPUT FIELD HDR)                     
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD          A(LAST INPUT FIELD HDR)                      
         MVC   NUMFLD,4(R2)        NUMBER OF INPUT FIELDS                       
*                                                                               
         L     R0,0(R1)                                                         
         ST    R0,VTIOB            A(TRANSLATOR IO BLOCK)                       
         L     R0,12(R1)                                                        
         ST    R0,VTIA             A(TIA)                                       
*                                                                               
         L     RF,VTIOB                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID                                                       
         CH    R0,=H'12'                                                        
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY                          
         DROP  RF                                                               
*                                                                               
         L     RE,VTWA                                                          
         AH    RE,=Y(SVSECBLK-T219FFD)                                          
         ST    RE,ASECBLK                                                       
*                                                                               
         MVC   VDATAMGR(80),0(R4)  FACILITY LIST                                
         MVC   VCOMFACS,16(R1)     ASSUMES R1 UNCHANGED                         
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   VGLOBBER,CGLOBBER                                                
         DROP  RF                                                               
         MVC   AGYALPHA,14(R3)                                                  
         LA    R3,64(R3)           PRESET ERROR MSG ADDRESS                     
         ST    R3,ERRAREA                                                       
         MVI   DMINBTS,X'C0'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==================================================================*            
* TEST XFR CONTROL FROM INFO OR ELSEWHERE                          *            
*==================================================================*            
         SPACE 1                                                                
TESTXFR  NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,VGLOBBER                                                   
         BZ    XFRNEQ                                                           
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',WORK,24,GLVXCTL                               
         CLI   8(R1),0                                                          
         BNE   XFRNEQ                                                           
* GET RID OF IT QUICKLY !                                                       
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         TM    GLVXFLG1,GLV1RETN   TEST THIS IS A RETURN CALL                   
         BZ    XFR10                                                            
* ON RETURN NEED TO XMT ALL                                                     
         LA    R2,LFMRECH                                                       
         SR    R0,R0                                                            
XFR2     IC    R0,0(R2)                                                         
         CLI   0(R2),0                                                          
         BE    XFR4                                                             
         AR    R2,R0                                                            
         B     XFR2                                                             
*                                                                               
XFR4     MVC   0(3,R2),=X'000101'  FORCE XMT ALL                                
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(23),=C'** BACK TO SPOT FILE **'                           
         LA    R2,LFMRECH                                                       
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     XFREQ                                                            
*                                                                               
XFR10    LA    R1,WORK             SAVE CALLING SYS/PRG FOR OVERLAYS            
         USING GLVXFRSY,R1                                                      
         MVC   SVXFRSY,GLVXFRSY                                                 
         MVC   SVXFRPR,GLVXFRPR                                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 (RF),(R1),=C'GETF',LFMRECH,3,GLVXREC                             
         CLI   8(R1),0                                                          
         BNE   XFRNEQ              FORGET ABOUT IT THEN                         
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         MVC   LFMACT(3),=C'DIS'                                                
         MVI   LFMACTH+5,3                                                      
*                                                                               
         GOTO1 (RF),(R1),=C'GETF',LFMKEYH,24,GLVPFM                             
         CLI   8(R1),0                                                          
         BNE   XFRNEQ              FORGET ABOUT IT THEN                         
         GOTO1 (RF),(R1),=C'DELE'                                               
         B     XFRNEQ                                                           
*                                                                               
XFREQ    CR    RB,RB               SET CC EQUAL                                 
         B     *+6                                                              
XFRNEQ   LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==================================================================*            
* CALL PFM OR INF THROUGH GLOBBER. USER RETURNS WITH =SW           *            
*==================================================================*            
         SPACE 1                                                                
CALLXFR  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
*                                                                               
         CLI   BYTE,C'R'           TEST RETURN                                  
         BNE   CALLXFR2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         CLI   SVOVSYS,2           2=SPOT,3=NET                                 
         BNE   *+10                                                             
         MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXFRPR,=C'FIL'                                                 
         MVC   GLVXTOSY,SVXFRSY                                                 
         MVC   GLVXTOPR,SVXFRPR                                                 
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         B     CALXFR30                                                         
*                                                                               
CALLXFR2 CLI   BYTE,C'P'           TEST PFM CALL                                
         BNE   CALXFR10                                                         
         USING GLPFMFIL,R1                                                      
         MVC   GLPFMFIL(6),=C'SPTFIL'                                           
         MVC   GLPFMDA,SVKEY+14                                                 
         MVC   GLPFMKEY(4),=C'*   '                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,54,GLPFMCDQ                          
         B     CALXFR20                                                         
*                                                                               
CALXFR10 CLI   BYTE,C'I'           TEST INFO CALL                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTF',LFMRECH,,GLVXREC                          
         GOTO1 (RF),(R1),,LFMKEYH,,GLVPFM                                       
*                                                                               
CALXFR20 XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         CLI   SVOVSYS,3           2=SPOT,3=NET                                 
         BE    CALXFR22                                                         
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         B     CALXFR24                                                         
*                                                                               
CALXFR22 MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXTOSY,=C'NET'                                                 
*                                                                               
CALXFR24 MVC   GLVXFRPR,=C'FIL'                                                 
         MVC   GLVXTOPR,=C'PFM'                                                 
         CLI   BYTE,C'P'           TEST PFM CALL                                
         BE    *+10                                                             
         MVC   GLVXTOPR,=C'INF'    SET FOR INFO                                 
         OI    GLVXFLG1,GLV1SEPS                                                
*                                                                               
CALXFR30 GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,24,GLVXCTL                           
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(23),=C'** BACK TO SPOT FILE **'                           
         OI    LFMMSG+6,X'80'      TRANSMIT                                     
         LA    R2,LFMRECH                                                       
         OI    6(R2),X'40'         POSITION CURSOR                              
CALXFRX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* RECLIST ENTRIES ARE  00-07 RECORD NAME                                        
*                      08    RECORD NUMBER                                      
*                      09    OVERLAY NUMBER                                     
*                      10    SCREEN NUMBER                                      
*                      11    X'80' = PASS CONTROL BEFORE ADDS                   
*                            X'40' = CANAD MEDIA C/N ALLOWED                    
*                            X'20' = ALLOW DATA CHANGE ON DISPLAY               
*  ***** NEW *****           X'01' = DDS ONLY ACCESS                            
*                      12    LIMITED ACCESS BITS FOR ADD/CHA/DEL                
*                            'AND' WITH TWA ACCESS BITS. IF BITS                
*                            MATCH, FUNCTION NOT ALLOWED.                       
*                            X'80' = NO STATION FILE                            
*                            X'40' = NO CLIENTS                                 
*                            X'20' = NO PRODUCTS                                
*                            X'10' = NO ESTIMATES                               
*                            X'04' = ALLOW CLT FRZ (WI ONLY)                    
*                                    WATCH IT - IT'S BACKWARDS                  
*                            X'01' = NO MKTGROUPS                               
*  AS OF 8/7/97        13    X'FF' = SUPPORT MOVED TO SFM                       
*                            X'FE' = NETWORK STILL IN FILE                      
*                            X'FD' = MOVE TO NET/SFM ONLY                       
         CNOP  2,4                                                              
RECLIST  DC    H'14'                                                            
         DC    A(RECLISTX-1)                                                    
*                                                                               
         DC    CL8'CLIENT  ',X'1111F100',X'4400'                                
         DC    CL8'CLTHDR  ',X'1111F100',X'4400'                                
         DC    CL8'CL2     ',X'3131D100',X'4400'                                
         DC    CL8'CGR     ',X'5454B400',X'4000'                                
         DC    CL8'PRODUCT ',X'1212F200',X'2000'                                
         DC    CL8'PRDHDR  ',X'1212F200',X'2000'                                
         DC    CL8'ESTIMATE',X'1313F380',X'10FD' << NETWORK ONLY                
         DC    CL8'ESTHDR  ',X'1313F380',X'1000'                                
         DC    CL8'ESTDOLS ',X'5353B300',X'1000'                                
         DC    CL8'EST$    ',X'5353B300',X'1000'                                
         DC    CL8'AUTHDOLS',X'5353B300',X'1000'                                
         DC    CL8'SUBEST  ',X'1A1AFA80',X'1000'                                
         DC    CL8'BRDSUB  ',X'1B1BFB80',X'1000'                                
         DC    CL8'USERDEM ',X'1E1EFE20',X'0000'                                
         DC    CL8'USRDEM  ',X'1E1EFE20',X'0000'                                
***************************************************************                 
*********      BE SURE NOT TO USE SAME FIRST LETTER  **********                 
*********         FOR TWO DIFFERENT COMMENT TYPES    **********                 
***************************************************************                 
         DC    CL8'A2COM   ',X'1D1DFD40',X'00FF'  << SFM                        
         DC    CL8'A3COM   ',X'1D1DFD40',X'00FF'  << SFM                        
         DC    CL8'BCOM    ',X'1D1DFD40',X'00FF'  << SFM                        
         DC    CL8'RSCOM   ',X'1D1DFD40',X'00FF'  << SFM                        
         DC    CL8'NVTEXT  ',X'1C1DFD40',X'00FF'  << SFM                        
         DC    CL8'MCOM    ',X'191DFD40',X'00FF'  << SFM                        
         DC    CL8'SDR     ',X'1F1DFD40',X'00FF'  << SFM                        
*                                                                               
         DC    CL8'PGROUP  ',X'1515F580',X'00FF'  << SFM                        
         DC    CL8'PGRDEF  ',X'1414F400',X'00FF'  << SFM                        
         DC    CL8'MGROUP  ',X'1717F780',X'01FF'  << SFM                        
         DC    CL8'MGRDEF  ',X'1616F600',X'01FF'  << SFM                        
*                                                                               
         DC    CL8'MASTER  ',X'2121E100',X'80FF'  << NET IN SFM TOO             
         DC    CL8'ADDRESS ',X'2222E200',X'80FD'  << NETWORK ONLY               
         DC    CL8'REPREC  ',X'2323E300',X'80FD'  << NETWORK ONLY               
         DC    CL8'MARKET  ',X'2424E440',X'00FD'  << NETWORK ONLY               
         DC    CL8'SPREP   ',X'2525E500',X'0000'                                
         DC    CL8'MENU    ',X'2626E600',X'0000'                                
         DC    CL8'FLIGHT  ',X'2727E700',X'0000'                                
         DC    CL8'SPLIT   ',X'2828E8C0',X'0000'                                
         DC    CL8'DEMOOVER',X'2C2CECC0',X'00FF'  << SFM                        
         DC    CL8'DEMOVER ',X'2C2CECC0',X'00FF'  << SFM                        
         DC    CL8'DEMODEF ',X'2B2BEB40',X'00FF'  << SFM                        
         DC    CL8'DEMDEF  ',X'2B2BEB40',X'00FF'  << SFM                        
         DC    CL8'NETDEF  ',X'2D2DED80',X'00FF'  << SFM                        
         DC    CL8'NETWORK ',X'2D2DED80',X'00FF'  << SFM                        
         DC    CL8'NTWKDEF ',X'2D2DED80',X'00FF'  << SFM                        
         DC    CL8'SHOWDEF ',X'2E2EEE00',X'00FF'  << SFM                        
         DC    CL8'SPLDEF  ',X'2F2FEF00',X'0000'                                
         DC    CL8'SPILLDEF',X'2F2FEF00',X'0000'                                
*                                                                               
         DC    CL8'AGYHDR  ',X'3232D201',X'0000'                                
         DC    CL8'DPTHDR  ',X'3434D441',X'0000'                                
         DC    CL8'EQUHDR  ',X'3535D541',X'0000'                                
         DC    CL8'TALFAC  ',X'3636D600',X'0000'                                
         DC    CL8'EBDREC  ',X'3838D800',X'0000'                                
* FOLLOWING RECS FOR NETPAK                                                     
         DC    CL8'WHUT    ',X'4343C320',X'0000' NEW HUT                        
         DC    CL8'MHUT    ',X'4444C420',X'0000' NEW HUT MONTH                  
         DC    CL8'HOLIHUT ',X'4545C500',X'0000' NEW HOLI-HUT                   
* FOR SPOTPAK FILE MAINTENANCE                                                  
         DC    CL8'PXCREC  ',X'4646C640',X'0000' PRD EXCLUSION REC              
         DC    CL8'SHRREC  ',X'4747C700',X'0000' SHARE RECORDS                  
         DC    CL8'PGEST   ',X'4848C800',X'0000' P$G PGEST REC                  
         DC    CL8'ATEST   ',X'4848C900',X'0000' AT&T PGEST REC                 
         DC    CL8'GFEST   ',X'4848CA40',X'0000' GEN FOOD PGEST REC             
RECLISTX EQU   *                                                                
         SPACE 2                                                                
* ENTRIES BELOW ARE REC/ACT/OVLY/SCREEN (4 BYTES/ENTRY)                         
*                                                                               
SPCLACTS DS    0H                                                               
         DC    X'12',C'X',X'18F8'  PRDHDR DELETE                                
         DC    X'12',C'L',X'19F9'  PRDHDR LOCK                                  
         DC    X'12',C'U',X'19F9'  PRDHDR UNLOCK                                
         DC    X'13',C'Y',X'1CFC'  ESTHDR COPY                                  
         DC    X'13',C'L',X'19F9'  ESTHDR LOCK                                  
         DC    X'13',C'U',X'19F9'  ESTHDR UNLK                                  
         DC    X'41',C'S',X'41C1'  UNIV DDS DISPLAY                             
         DC    X'43',C'S',X'43C3'  HUT DDS DISPLAY                              
         DC    X'44',C'S',X'44C4'  HUT DDS MONTH DISPLAY                        
         DC    X'48',C'X',X'48C8'  PGEST DELETE                                 
         DC    X'48',C'X',X'48C9'  ATT PGEST DEL                                
         DC    X'48',C'X',X'48CA'  GF  PGEST DEL                                
         DC    X'2F',C'R',X'2FEF'  SPILL RESTORE                                
         DC    X'FF'                                                            
         EJECT                                                                  
KEYLIST  DS    0H                                                               
         DC    X'11',AL1(L'K11)                                                 
K11      DC    C'MEDIA,CLIENT'                                                  
         DC    X'12',AL1(L'K12)                                                 
K12      DC    C'MEDIA,CLIENT,PRODUCT'                                          
         DC    X'13',AL1(L'K13)                                                 
K13      DC    C'MEDIA,CLIENT,PRODUCT,ESTIMATE'                                 
         DC    X'14',AL1(L'K14)                                                 
K14      DC    C'MEDIA,CLIENT,PGRPID'                                           
         DC    X'15',AL1(L'K15)                                                 
K15      DC    C'MEDIA,CLIENT,PRDGRP'                                           
         DC    X'16',AL1(L'K16)                                                 
K16      DC    C'MEDIA,CLIENT,PRDGRP,MGRPID'                                    
         DC    X'17',AL1(L'K17)                                                 
K17      DC    C'MED,CLT,PRDGRP,MKTGRP,(MKT)'                                   
         DC    X'18',AL1(L'K18)                                                 
K18      DC    C'MED,CLT,PRODUCT,MKTGRP,(MKT)'                                  
         DC    X'19',AL1(L'K19)                                                 
K19      DC    C'SUPPORTED IN SPOT/SFM'                                         
         DC    X'1A',AL1(L'K1A)                                                 
K1A      DC    C'MED,CLT,''POL'',MASTER-EST'                                    
         DC    X'1B',AL1(L'K1B)                                                 
K1B      DC    C'MED,CLT,PRD,POL-MASTER-EST'                                    
         DC    X'1C',AL1(L'K1C)                                                 
K1C      DC    C'SUPPORTED IN SPOT/SFM'                                         
         DC    X'1D',AL1(L'K1D)                                                 
K1D      DC    C'SUPPORTED IN SPOT/SFM'                                         
         DC    X'1E',AL1(L'K1E)                                                 
K1E      DC    C'MEDIA,CLIENT'                                                  
         DC    X'1F',AL1(L'K1F)                                                 
K1F      DC    C'SUPPORTED IN SPOT/SFM'                                         
*                                                                               
         DC    X'21',AL1(L'K21)                                                 
K21      DC    C'STATION-BAND,(CLIENT)'                                         
         DC    X'22',AL1(L'K22)                                                 
K22      DC    C'STATION'                                                       
         DC    X'23',AL1(L'K23)                                                 
K23      DC    C'MEDIA,REP'                                                     
         DC    X'24',AL1(L'K24)                                                 
K24      DC    C'MEDIA,MARKET'                                                  
         DC    X'25',AL1(L'K25)                                                 
K25      DC    C'MEDIA,SPECIAL REP'                                             
         DC    X'26',AL1(L'K26)                                                 
K26      DC    C'MEDIA,MENU'                                                    
         DC    X'27',AL1(L'K27)                                                 
K27      DC    C'MEDIA,CLIENT,PRODUCT,YEAR'                                     
         DC    X'28',AL1(L'K28)                                                 
K28      DC    C'MEDIA,CLIENT,PRODUCT,ESTIMATE'                                 
         DC    X'29',AL1(L'K29)                                                 
K29      DC    C'CODE (1-4 DIGITS) OR END DATE'                                 
         DC    X'2A',AL1(L'K2A)                                                 
K2A      DC    C'NTWK,PROGRAM,END DATE'                                         
         DC    X'2B',AL1(L'K2B)                                                 
K2B      DC    C'NTWK,SHOW,RSVC,(CLT),(SQ)'                                     
         DC    X'2C',AL1(L'K2C)                                                 
K2C      DC    C'NTWK,SHOW,RSVC,DEM,(CLT),(SQ)'                                 
         DC    X'2D',AL1(L'K2D)                                                 
K2D      DC    C'NTWK,(CLT),(EST)'                                              
         DC    X'2E',AL1(L'K2E)                                                 
K2E      DC    C'NTWK,SHOW'                                                     
         DC    X'2F',AL1(L'K2F)                                                 
K2F      DC    C'RTGSVC,STATION,(CLT)'                                          
         DC    X'30',AL1(L'K30)                                                 
K30      DC    C'RTGSVC,STATION,(CLT)'                                          
*                                                                               
         DC    X'31',AL1(L'K31)                                                 
K31      DC    C'MEDIA,CLIENT'                                                  
*        DC    X'31',AL1(L'K31)                                                 
*31      DC    C'CLIENT'                                                        
         DC    X'32',AL1(L'K32)                                                 
K32      DC    C'AGENCY'                                                        
         DC    X'33',AL1(L'K33)                                                 
K33      DC    C'REPORT,AGENCY,(MEDIA),(CLT)'                                   
         DC    X'34',AL1(L'K34)                                                 
K34      DC    C'AGENCY,MEDIA,MENU'                                             
         DC    X'35',AL1(L'K35)                                                 
K35      DC    C'AGENCY,MEDIA,(CLIENT)'                                         
         DC    X'36',AL1(L'K36)                                                 
K36      DC    C'MEDIA,CLIENT,TALFGRP'                                          
         DC    X'38',AL1(L'K38)                                                 
K38      DC    C'MEDIA,RATING SERVICE'                                          
*                                                                               
         DC    X'41',AL1(L'K41)                                                 
K41      DC    C'CODE (1-4 DIGITS) OR END DATE'                                 
         DC    X'42',AL1(L'K42)                                                 
K42      DC    C'NTWK,PROGRAM,END DATE'                                         
         DC    X'43',AL1(L'K43)                                                 
K43      DC    C'SCHEME,DAY,TIME,YEAR'                                          
         DC    X'44',AL1(L'K44)                                                 
K44      DC    C'SCHEME,DAY,TIME,YEAR'                                          
         DC    X'45',AL1(L'K45)                                                 
K45      DC    C'DATE,(SCHEME)'                                                 
*                                                                               
         DC    X'46',AL1(L'K46)                                                 
K46      DC    C'MEDIA,CLT,PRD,EST,STA'                                         
*                                                                               
         DC    X'47',AL1(L'K47)                                                 
K47      DC    C'MEDIA,CLT,PRD,EST'                                             
*                                                                               
         DC    X'48',AL1(L'K48)                                                 
K48      DC    C'MEDIA,CLT,PRD,EST'                                             
*                                                                               
         DC    X'53',AL1(L'K53)                                                 
K53      DC    C'MEDIA,CLIENT,PRODUCT,ESTIMATE'                                 
*                                                                               
         DC    X'54',AL1(L'K54)                                                 
K54      DC    C'MEDIA,CLIENT'                                                  
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         CNOP  2,4                                                              
ACTLIST  DC    H'9'                                                             
         DC    A(ACTLISTX-1)                                                    
         DC    CL8'ADD     ',C'A'                                               
         DC    CL8'CHANGE  ',C'C'                                               
         DC    CL8'DISPLAY ',C'D'                                               
         DC    CL8'DELETE  ',C'X'                                               
         DC    CL8'LOCK    ',C'L'                                               
         DC    CL8'UNLOCK  ',C'U'                                               
         DC    CL8'COPY    ',C'Y'                                               
         DC    CL8'RESTORE ',C'R'                                               
         DC    CL8'DDS     ',C'S'  DDS STANDARD DISPLAY                         
ACTLISTX EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMFDD                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
* DDGLOBEQUS                                                                    
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLPFMD                                                       
* FAGETTXTD                                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'150SCHTLF00A 05/01/02'                                      
         END                                                                    
