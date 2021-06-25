*          DATA SET SRSYS00S   AT LEVEL 008 AS OF 05/01/02                      
*PHASE T14000A                                                                  
*INCLUDE SQUASHER                                                               
         TITLE '$PS/$AS/$START/$STOP - START/STOP SYSTEM(S)'                    
SYS      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**$SYS**,RA,RR=R4                                      
         USING WRKD,RC             RC=A(W/S)                                    
         USING SRPARMD,R1                                                       
         ST    R1,APARMS                                                        
         ST    R4,RELO                                                          
         L     R9,SRPARM6          R9=A(TWA)                                    
         USING SRSYSFFD,R9                                                      
         L     R8,SRPARM1          R8=A(SYSFACS)                                
         USING SYSFACD,R8                                                       
*                                                                               
         L     RE,SRPARM4          EXTRACT ROUTINES FROM COMFACS                
         ST    RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         DROP  RE                                                               
         L     RE,=V(SQUASHER)                                                  
         A     RE,RELO                                                          
         ST    RE,ASQUASH                                                       
*                                                                               
         XC    MSG,MSG             SET FACPAK SYSTEM ID NAME                    
         MVC   MSG+00(15),=C'ED/9999 (XXXX) '                                   
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         MVC   SYSID,SSBSYSID                                                   
         MVC   SYSCH,SSBSYSCH                                                   
         MVC   SYSNAM1,SSBSYSN1                                                 
         MVC   SYSNAM3,SSBSYSNA                                                 
         MVC   SYSNAM4,SSBSYSN4                                                 
         MVC   MSG+02(1),SYSCH                                                  
         MVC   MSG+09(4),SYSNAM4                                                
         DROP  RE                                                               
*                                                                               
         L     RE,SRPARM3          EXTRACT LUID FROM UTL                        
         MVC   LUID,TSYM-UTLD(RE)                                               
         TBIN  SECS                                                             
         ST    R1,TIME             GET TIME IN SECONDS                          
         DROP  R1                                                               
         MVI   STATFILT,X'FF'      INITIALISE STATUS FILTERS                    
         EJECT                                                                  
***********************************************************************         
* $SYS TO DISPLAY SYSTEM STATUS                                       *         
* $PS  TO DISPLAY SYSTEM STATUS AND CHANGE TO $AS FOR ALTER NEXT TIME *         
***********************************************************************         
         SPACE 1                                                                
SYS1     MVC   PROFSID,SPACES      P3=PROFSID(,PASSWORD)                        
         MVC   PASSWORD,SPACES                                                  
         LA    R2,SRVP3H                                                        
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    SYS2                                                             
         MVI   MSGNUM,3                                                         
         GOTO1 ASCANNER,DMCB,(R2),(2,WORK)                                      
         CLI   4(R1),0                                                          
         BE    MESSAGE                                                          
         LA    RE,WORK                                                          
         CLI   1(RE),0                                                          
         BNE   MESSAGE                                                          
         CLI   0(RE),8                                                          
         BH    MESSAGE                                                          
         MVC   PROFSID,12(RE)                                                   
         CLI   4(R1),2                                                          
         BNE   SYS2                                                             
         LA    RE,32(RE)                                                        
         CLI   1(RE),0                                                          
         BNE   MESSAGE                                                          
         CLI   0(RE),8                                                          
         BH    MESSAGE                                                          
         MVC   PASSWORD,12(RE)                                                  
*                                                                               
SYS2     MVC   REASON,SPACES       P4=REASON CODE                               
         LA    R2,SRVP4H                                                        
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    SYS3                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   REASON(0),8(R2)                                                  
*                                                                               
SYS3     CLC   SRVSRV+1(3),=C'SYS' $PS OR $SYS                                  
         BE    *+12                                                             
         CLI   SRVSRV+1,C'P'                                                    
         BNE   SYS4                                                             
         BAS   RE,ANYSTAT          CHECK IF +/-/R FILTERS INPUT                 
         BNE   MESSAGE                                                          
         MVI   FORMAT,X'03'        DISP STATUS/DATA                             
         BAS   RE,FORMSYS                                                       
         LA    R2,SRVP1H                                                        
         CLI   SRVSRV+1,C'P'       CHANGE $PS TO $AS                            
         BNE   *+12                                                             
         MVI   SRVSRV+1,C'A'                                                    
         LA    R2,SRVL1H                                                        
         MVI   MSGNUM,1                                                         
         B     MESSAGE                                                          
*                                                                               
SYS4     CLI   SRVSRV+1,C'A'       $AS                                          
         BNE   SYS10                                                            
         BAS   RE,ANYSTAT          CHECK IF +/-/R FILTERS INPUT                 
         BNE   MESSAGE                                                          
         MVI   FORMAT,X'02'        DISP DATA                                    
         BAS   RE,FORMSYS                                                       
         LA    R2,SRVL1H           R2=A(FIRST TWA LINE)                         
         USING LINED,R2                                                         
         LA    R3,STATLST          R3=A(SE STATUS LIST)                         
*                                                                               
SYS5     OC    LINSYS,LINSYS       END OF SE STATUS LIST                        
         BZ    SYS8                                                             
         CLI   LINSTATH+5,0        ANY INPUT                                    
         BNE   *+14                                                             
         MVC   LINSTAT,1(R3)       NO - FILL IN CURRENT STATUS                  
         B     SYS7                                                             
         CLC   LINSTAT,1(R3)       YES - IS IT DIFFERENT                        
         BE    SYS7                                                             
         MVC   SENUMB,0(R3)        SET SE NUMBER                                
         BAS   RE,GETSYS                                                        
         L     R5,ASENTRY          POINT TO SELIST ENTRY                        
         USING SELISTD,R5                                                       
*                                                                               
SYS6     CLI   LINSTAT,C'+'        START                                        
         BNE   SYS6A                                                            
         BAS   RE,OKTOUP           CHECK IF OK TO START                         
         BNZ   MESSAGE                                                          
         LA    R1,SPACES                                                        
         LA    RF,UPSYS                                                         
         B     SYS6X                                                            
SYS6A    CLI   LINSTAT,C'-'        STOP                                         
         BNE   SYS6B                                                            
         BAS   RE,OKTODN           CHECK IF OK TO STOP                          
         BNZ   MESSAGE                                                          
         LA    RF,DNSYS                                                         
         B     SYS6X                                                            
SYS6B    CLI   LINSTAT,C'R'        READONLY                                     
         BNE   SYS6C                                                            
         BAS   RE,OKTORO           CHECK IF OK TO SET READ ONLY                 
         BNZ   MESSAGE                                                          
         LA    RF,ROSYS                                                         
         B     SYS6X                                                            
SYS6C    MVI   MSGNUM,3            INVALID INPUT IN STATUS                      
         B     MESSAGE                                                          
SYS6X    BASR  RE,RF               GO TO UP/DN/RO RTN                           
         BNZ   MESSAGE                                                          
*                                                                               
SYS7     LA    R2,LINNEXT          BUMP TO NEXT LINE                            
         LA    R3,L'STATLST(R3)    STATUS                                       
         B     SYS5                                                             
*                                                                               
SYS8     CLI   STATFILT,X'FF'      RE-DISPLAY STATUS/DATA                       
         BNE   SYS9                                                             
         MVI   FORMAT,X'03'                                                     
         BAS   RE,FORMSYS                                                       
SYS9     XC    SRVSRV,SRVSRV       CLEAR SRV                                    
         LA    R2,SRVSRVH                                                       
         MVI   MSGNUM,2                                                         
         B     MESSAGE                                                          
         EJECT                                                                  
***********************************************************************         
* $START SYSTEM IN P1                                                 *         
* $STOP SYSTEM IN P1                                                  *         
***********************************************************************         
         SPACE 1                                                                
SYS10    CLC   SRVSRV+1(3),=C'START'                                            
         BE    SYS11                                                            
         CLC   SRVSRV+1(3),=C'STOP'                                             
         BE    SYS11                                                            
         DC    H'0'                                                             
*                                                                               
SYS11    MVI   FORMAT,X'03'        DISPLAY SYSTEMS IN CASE OF ERROR             
         BAS   RE,FORMSYS                                                       
*                                                                               
SYS12    LA    R2,SRVP1H           P1=SYSTEM NAME                               
         MVI   MSGNUM,4                                                         
         CLI   5(R2),0             MUST BE INPUT                                
         BE    MESSAGE                                                          
         SR    R1,R1               EXTRACT & SPACE FILL INPUT FIELD             
         IC    R1,5(R2)                                                         
         MVC   WORK(16),SPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
*                                                                               
         L     R5,VSELIST          SEARCH SELIST FOR NAME                       
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R5                                                       
         MVI   MSGNUM,5                                                         
         CLC   SENAME,WORK                                                      
         BE    *+12                                                             
         BXLE  R5,R6,*-10                                                       
         B     MESSAGE                                                          
         MVC   SENUMB,SESYS        SAVE SESYS                                   
*                                                                               
SYS13    CLC   SRVSRV+1(3),=C'START'                                            
         BNE   SYS14                                                            
         MVI   MSGNUM,3                                                         
         LA    R2,SRVP2H                                                        
         CLI   5(R2),0                                                          
         BE    SYS13A                                                           
         CLC   8(11,R2),=C'RECOVERY=NO'                                         
         BE    SYS13A                                                           
         CLC   8(8,R2),=C'READONLY'                                             
         BNE   MESSAGE                                                          
         BAS   RE,OKTORO           CHECK IF OK TO SET READ ONLY                 
         BNZ   MESSAGE                                                          
         BAS   RE,ROSYS            SET SYSTEM  READ ONLY                        
         BNZ   MESSAGE                                                          
         B     SYS16                                                            
SYS13A   LA    R2,SRVP1H                                                        
         MVI   MSGNUM,6            SYSTEM ALREADY STARTED                       
         TM    SEIND,SEISETRO+SEISTRT                                           
         BO    *+12                OK TO START READONLY SYSTEM                  
         TM    SEIND,X'80'                                                      
         BO    MESSAGE                                                          
         LA    R2,SRVP3H                                                        
         BAS   RE,OKTOUP           CHECK IF OK TO START                         
         BNZ   MESSAGE                                                          
         LA    R2,SRVP1H                                                        
         LA    R1,SPACES                                                        
         BAS   RE,UPSYS            GO TO UP RTN                                 
         BNZ   MESSAGE                                                          
         B     SYS16                                                            
*                                                                               
SYS14    MVI   MSGNUM,7            SYSTEM ALREADY STOPPED                       
         TM    SEIND,X'02'         TEST FORCE STOP BIT                          
         BO    *+12                IF ON CAN BE CLOSED                          
         TM    SEIND,X'80'                                                      
         BZ    MESSAGE                                                          
         LA    R2,SRVP3H                                                        
         BAS   RE,OKTODN           CHECK IF OK TO STOP                          
         BNZ   MESSAGE                                                          
         LA    R2,SRVP1H                                                        
         BAS   RE,DNSYS            GO TO DN RTN                                 
         BNZ   MESSAGE                                                          
*                                                                               
SYS16    MVI   FORMAT,X'03'        DISPLAY ALL SELIST DATA                      
         BAS   RE,FORMSYS                                                       
         XC    SRVSRV,SRVSRV       CLEAR SRV                                    
         LA    R2,SRVSRVH                                                       
         MVI   MSGNUM,2                                                         
         B     MESSAGE                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO TEST IF START/STOP/READONLY ALLOWED FROM THIS TERMINAL  *         
* NEED TO INPUT PASSWORD FOR START IF SET BY STOP                     *         
***********************************************************************         
         SPACE 1                                                                
OKTODN   ST    RE,SAVERE           CHECK IF OK TO TAKE SYSTEM DOWN              
         MVI   MSGNUM,0                                                         
OKTODNX  CLI   MSGNUM,0                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
OKTORO   ST    RE,SAVERE           CHECK IF OK TO SET SYSTEM R/O                
         MVI   MSGNUM,0                                                         
         CLI   MSGNUM,0                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
OKTOUP   ST    RE,SAVERE           CHECK IF OK TO BRING SYSTEM UP               
         MVI   MSGNUM,0                                                         
         CLC   SESSPSWD,SPACES     WAS SYSTEM STOPPED WITH A PASSWORD           
         BNH   OKTOUPX             NO                                           
         CLC   SESSPSWD,PASSWORD   YES MUST MATCH PASSWORD INPUT                
         BE    OKTOUPX                                                          
         MVI   MSGNUM,15           SYSTEM IS PASSWORD PROTECTED                 
OKTOUPX  CLI   MSGNUM,0                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* TEST IF ANY STATUS FILTERS INPUT IN S/R FIELD OR P1                 *         
* FORMAT IS $S/R,... WITH ANY COMBO OF +,U,-,D,*,N OR R               *         
* OR THE STRING CAN BE INPUT IN P1                                    *         
***********************************************************************         
         SPACE 1                                                                
ANYSTAT  NTR1                                                                   
         MVI   MSGNUM,0                                                         
ANYS1    LA    R2,SRVSRVH          TEST IF $SYS,.... INPUT                      
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    ANYS2                                                            
         LR    R0,R1                                                            
         LA    RF,8(R2)                                                         
ANYS1A   CLI   0(RF),C','                                                       
         BE    ANYS1B                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,ANYS1A                                                        
         B     ANYS2                                                            
ANYS1B   LA    RF,1(RF)            RF=1ST BYTE OF STATUS STRING                 
         LR    R0,RF                                                            
         LA    RE,8(R2)                                                         
         SR    R0,RE                                                            
         SR    R1,R0               R1=LENGTH OF STATUS STRING                   
         BNP   ANYS2                                                            
         CLI   SRVP1H+5,0                                                       
         BE    ANYS3                                                            
         LA    R2,SRVP1H                                                        
         MVI   MSGNUM,17           STATUS IN S/R OR P1 ONLY                     
         B     ANYSX                                                            
*                                                                               
ANYS2    LA    R2,SRVP1H           STATUS=... OR S=... IN P1                    
         SR    R1,R1               TEST IF ANY INPUT                            
         ICM   R1,1,5(R2)                                                       
         BZ    ANYSX                                                            
         LA    RF,8(R2)            R1=LENGTH OF STRING                          
*                                                                               
ANYS3    MVI   STATFILT,0          INIT STATUS FLAGS                            
ANYS4    CLI   0(RF),C'+'          VALID CHRS ARE +/U,-/D,*/N,R                 
         BNE   *+12                                                             
         OI    STATFILT,X'80'                                                   
         B     ANYS5                                                            
         CLI   0(RF),C'U'                                                       
         BNE   *+12                                                             
         OI    STATFILT,X'80'                                                   
         B     ANYS5                                                            
         CLI   0(RF),C'-'                                                       
         BNE   *+12                                                             
         OI    STATFILT,X'40'                                                   
         B     ANYS5                                                            
         CLI   0(RF),C'D'                                                       
         BNE   *+12                                                             
         OI    STATFILT,X'40'                                                   
         B     ANYS5                                                            
         CLI   0(RF),C'R'                                                       
         BNE   *+12                                                             
         OI    STATFILT,X'20'                                                   
         B     ANYS5                                                            
         CLI   0(RF),C'*'                                                       
         BNE   *+12                                                             
         OI    STATFILT,X'10'                                                   
         B     ANYS5                                                            
         CLI   0(RF),C'N'                                                       
         BNE   *+12                                                             
         OI    STATFILT,X'10'                                                   
         B     ANYS5                                                            
         MVI   MSGNUM,16           INVALID SYSTEM STATUS CHR                    
         B     ANYSX                                                            
*                                                                               
ANYS5    LA    RF,1(RF)                                                         
         BCT   R1,ANYS4                                                         
*                                                                               
ANYSX    CLI   MSGNUM,0            EXIT WITH CC EQL IF ALL OK                   
         BNE   ANYSXX                                                           
         XIT1                                                                   
ANYSXX   XIT1  REGS=(R2)           RETURN A(ERROR FIELD) IF CC NEQ              
         EJECT                                                                  
***********************************************************************         
* FORMAT SELIST ENTRIES IN TWA                                        *         
* FORMAT BYTE SET TO X'01'=STATUS REQUIRED,X'02'=DATA REQUIRED        *         
* PASSES BACK STATLST CONTAINING MULTIPLE 4 BYTE ENTRIES              *         
* BYTE 1 = SESYS                                                      *         
* BYTE 2 = STATUS (+/-/*/R)                                           *         
* BYTE 3 = UPDATIVE FACPAK ID                                         *         
* BYTE 4 = SPARE                                                      *         
***********************************************************************         
         SPACE 1                                                                
FORMSYS  NTR1                                                                   
         LA    RE,STATLST          CLEAR SE STATUS LIST                         
         LA    RF,STATLSTL                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LA    RE,4095(,R9)        A(TWA)                                       
         LA    RE,1(,RE)           2ND BASE FOR TWA (TEMPORARY)                 
         USING SRSYSFFD+4096,RE                                                 
         LA    RE,SRVT1H                                                        
         DROP  RE                                                               
         ST    RE,ASYSMSGH         SET A(NEXT SYSTEM MESSAGE HEADER)            
         LA    R3,STATLST                                                       
         LA    R2,SRVL1H                                                        
         USING LINED,R2                                                         
         L     R5,VSELIST                                                       
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R5                                                       
*                                                                               
FORM2    MVC   0(1,R3),SESYS       SET SE & STAT IN STATLST                     
         MVI   1(R3),C'+'                                                       
         TM    SEIND,X'80'                                                      
         BO    *+8                                                              
         MVI   1(R3),C'-'                                                       
         TM    SEIND,X'02'                                                      
         BZ    *+8                                                              
         MVI   1(R3),C'*'                                                       
         TM    SEIND,X'84'         TEST STARTED AND READ ONLY                   
         BNO   *+8                                                              
         MVI   1(R3),C'R'                                                       
         TM    SEIND,X'90'         TEST STARTED AND SET READ ONLY               
         BNO   *+8                                                              
         MVI   1(R3),C'R'                                                       
*                                                                               
FORM2A   CLI   STATFILT,X'FF'      ANY STATUS FILTERS INPUT                     
         BE    FORM2X                                                           
         CLI   1(R3),C'+'          TEST TO SHOW UP SYSTEMS                      
         BNE   *+12                                                             
         TM    STATFILT,X'80'                                                   
         BZ    FORM2B                                                           
         CLI   1(R3),C'-'          TEST TO SHOW DOWN SYSTEMS                    
         BNE   *+12                                                             
         TM    STATFILT,X'40'                                                   
         BZ    FORM2B                                                           
         CLI   1(R3),C'R'          TEST TO SHOW READONLY SYSTEMS                
         BNE   *+12                                                             
         TM    STATFILT,X'20'                                                   
         BZ    FORM2B                                                           
         CLI   1(R3),C'*'          TEST TO SHOW NOP SYSTEMS                     
         BNE   *+12                                                             
         TM    STATFILT,X'10'                                                   
         BZ    FORM2B                                                           
         B     FORM2X                                                           
FORM2B   XC    0(L'STATLST,R3),0(R3)                                            
         B     FORMH               NOT DISPLAYING THIS SYSTEM                   
*                                                                               
FORM2X   TM    FORMAT,X'01'                                                     
         BZ    *+10                                                             
         MVC   LINSTAT,1(R3)                                                    
         OI    LINSTATH+6,X'80'                                                 
         TM    FORMAT,X'02'                                                     
         BZ    FORM3                                                            
*                                                                               
         GOTO1 AHEXOUT,DMCB,SESYS,LINSYS,1,=C'TOG'                              
         MVC   LINNAM,SENAME                                                    
*                                                                               
FORM3    NI    LINSYSH+1,X'F3'     SET NORMAL INTENSITY                         
         NI    LINSTATH+1,X'F3'                                                 
FORM3A   CLI   LINSTAT,C'+'        SET COLOURS FOR UP                           
         BNE   FORM3B                                                           
         OI    LINSYSH+1,X'08'     SET HIGH INTENSITY                           
         MVI   LINSTATX+5,X'04'    GREEN                                        
         MVI   LINSYSH+4,X'0C'                                                  
         MVI   LINSYSH+5,X'04'                                                  
         B     FORMG                                                            
FORM3B   CLI   LINSTAT,C'-'        SET COLOURS FOR DOWN                         
         BNE   FORM3C                                                           
         MVI   LINSTATX+5,X'02'    RED                                          
         MVI   LINSYSH+4,X'0C'                                                  
         MVI   LINSYSH+5,X'02'                                                  
         CLC   SESSLUID,SPACES     HAS SOMEONE TAKEN THIS SYSTEM DOWN           
         BNH   FORMG               NO                                           
         OI    LINSYSH+5,X'80'     YES SET REVERSE VIDEO                        
         MVI   LINNAM-1,C'-'                                                    
         BAS   RE,OUTMSG                                                        
         B     FORMG                                                            
FORM3C   CLI   LINSTAT,C'*'        SET COLOURS FOR NOP                          
         BNE   FORM3D                                                           
         MVI   LINSTATX+5,X'06'    YELLOW                                       
         MVI   LINSYSH+4,X'0C'                                                  
         MVI   LINSYSH+5,X'06'                                                  
         B     FORMG                                                            
FORM3D   CLI   LINSTAT,C'R'        SET COLOURS FOR READONLY                     
         BNE   FORM3E                                                           
         MVI   LINSTATX+5,X'06'    YELLOW                                       
         MVI   LINSYSH+4,X'0C'                                                  
         MVI   LINSYSH+5,X'06'                                                  
         CLC   SESSLUID,SPACES     HAS SOMEONE SET THIS SYSTEM READONLY         
         BNH   FORMG               NO                                           
         OI    LINSYSH+5,X'80'     YES SET REVERSE VIDEO                        
         MVI   LINNAM-1,C'-'                                                    
*NOP*    BAS   RE,OUTMSG                                                        
         B     FORMG                                                            
FORM3E   EQU   *                                                                
*                                                                               
FORMG    LA    R2,LINNEXT          BUMP TO NEXT LINE &                          
         LA    R3,L'STATLST(R3)    STATUS LIST ENTRY                            
FORMH    BXLE  R5,R6,FORM2                                                      
         XC    LINSYS,LINSYS       SET END OF DISPLAY                           
         XC    LINSTAT,LINSTAT                                                  
         B     EXIT                                                             
         SPACE 2                                                                
OUTMSG   NTR1                                                                   
         MVC   SYSMSG,SPACES                                                    
         MVC   SYSSYS,SENAME       SYSTEM NAME                                  
         MVI   SYSS1,C'/'                                                       
         SR    R0,R0                                                            
         ICM   R1,15,SESSSTOP      TIME TAKEN DOWN (SECONDS)                    
         CLI   LINSTAT,C'-'                                                     
         BE    OUTMSG1                                                          
         CLI   LINSTAT,C'R'                                                     
         BE    OUTMSG1                                                          
         ICM   R1,15,SESSSTRT      TIME BROUGHT UP (SECONDS)                    
OUTMSG1  LTR   R1,R1                                                            
         BZ    OUTMSG2                                                          
         D     R0,=F'60'                                                        
         SR    R0,R0                                                            
         D     R0,=F'60'           R0=MINS,R1=HOURS                             
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SYSTIME(2),DUB+6(2)                                              
         MVI   SYSTIME+2,C':'                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SYSTIME+3(2),DUB+6(2)                                            
OUTMSG2  MVI   SYSS2,C'/'                                                       
         MVC   SYSPID,SESSPID      BY WHO                                       
         MVI   SYSS3,C'/'                                                       
         MVC   SYSWHY,SESSWHY      FOR WHAT REASON                              
*                                                                               
         LA    RF,L'SYSMSG                                                      
         GOTO1 ASQUASH,DMCB,SYSMSG,(RF)                                         
*                                                                               
         L     RF,ASYSMSGH         GET NEXT MESSAGE AREA                        
         CLI   0(RF),0                                                          
         BE    SYSMSGX             EXIT IF END OF SCREEN                        
         MVI   4(RF),X'0C'                                                      
         MVI   5(RF),X'07'         WHITE                                        
*                                                                               
         CLI   LINSTAT,C'-'                                                     
         BNE   *+8                                                              
         MVI   5(RF),X'02'         DOWN=RED                                     
         CLI   LINSTAT,C'R'                                                     
         BNE   *+8                                                              
         MVI   5(RF),X'06'         READONLY=YELLOW                              
         CLI   LINSTAT,C'+'                                                     
         BNE   *+8                                                              
         MVI   5(RF),X'04'         UP=GREEN                                     
         OI    5(RF),X'80'         REVERSE VIDEO                                
*                                                                               
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),SYSMSG                                                   
         LA    RF,9(R1,RF)                                                      
         ST    RF,ASYSMSGH          BUMP TO NEXT MESSAGE HEADER FIELD           
*                                                                               
SYSMSGX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* START THE SE IN SENUMB.                                             *         
* EXIT WITH CC=NEQ ON ERROR WITH MSGNUM SET                           *         
***********************************************************************         
         SPACE 1                                                                
UPSYS    NTR1                                                                   
         CLC   0(8,R1),=C'READONLY'                                             
         BE    *+8                                                              
         BAS   RE,RWSYS            MAKE SURE WRITES ENABLED                     
         MVI   MSGNUM,0                                                         
         BAS   RE,GETSYS                                                        
         L     R5,ASENTRY                                                       
         USING SELISTD,R5                                                       
         OC    SEFILES,SEFILES     ANY FILES THIS SE                            
         BZ    UPOK                NO - EXIT                                    
         TM    SEIND,SEISTRT       MAKE SYRE SYSTEM IS DOWN                     
         BZ    UPS0                                                             
         BAS   RE,DNSYS            IF NOT TAKE IT DOWN                          
*                                                                               
UPS0     BAS   RE,SETSYS           FRIG TCB ENTRY FOR SENUMB                    
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         GOTO1 VDMOD000,DMCB,VFINDSYS,(SESYS,0)                                 
         L     R1,4(R1)            R1=A(SYSFILES LIST)                          
         LH    R0,2(R1)                                                         
         LA    R1,4(R1)                                                         
         XC    AREQUEST,AREQUEST                                                
*                                  CLEAR ISCILAST FOR ALL I/S FILES             
UPS1     L     RE,4(R1)                                                         
         TM    0(R1),X'01'         I/S FILE ?                                   
         BZ    *+10                                                             
         USING ISDTF,RE                                                         
         XC    ISCILAST,ISCILAST                                                
         TM    0(R1),X'20'         TEST REQUEST FILE                            
         BZ    *+8                                                              
         ST    RE,AREQUEST                                                      
*        TM    0(R1),X'40'         TEST RECOVERY FILE                           
*        BZ    UPS1A                                                            
*        USING DTFPHD,RE                                                        
*        L     R7,DBLK                                                          
*        LTR   R7,R7                                                            
*        BZ    UPS1A               TEST BLOCKED                                 
*        XC    0(8,R7),0(R7)       ERASE ANY LEFTOVER DATA                      
UPS1A    LA    R1,8(R1)                                                         
         BCT   R0,UPS1                                                          
         DROP  RE                                                               
*                                                                               
         L     R7,VSSB             CALL V(OPENSYS)                              
         USING SSBD,R7                                                          
         MVI   SSBMTIND,0          *** DISABLE MULTI-TASKING WAITS ***          
         GOTO1 VTICTOC,PLIST,C'SSET'   SUSPEND TIMERS                           
         GOTO1 VDMOD000,DMCB,VOPENSYS,(SESYS,BIGWRK)                            
         GOTO1 VTICTOC,PLIST,C'RSET'   RESET TIMERS                             
         MVI   SSBMTIND,C'M'       *** ENABLE MULTI-TASKING WAITS  ***          
         OC    DMCB+8(2),DMCB+8                                                 
         BZ    UPS2                                                             
         MVC   DUB(2),DMCB+8       SAVE ERROR BYTES                             
         MVC   DMCB(4),VCLSESYS                                                 
         MVI   SSBMTIND,0          *** DISABLE MULTI-TASKING WAITS ***          
         GOTO1 VTICTOC,PLIST,C'SSET'   SUSPEND TIMERS                           
         GOTO1 VDMOD000,DMCB           CLOSE SYSTEM FILES                       
         GOTO1 VTICTOC,PLIST,C'RSET'   RESET TIMERS                             
         MVI   SSBMTIND,C'M'       *** ENABLE MULTI-TASKING WAITS  ***          
         DROP  R7                                                               
*                                                                               
         MVC   DMCB+8(2),DUB                                                    
         BAS   RE,RELSYS           RESET TCB ENTRY                              
         MVI   MSGNUM,9                                                         
         TM    DMCB+9,X'80'        INVALID CPU ID                               
         BO    UPX                                                              
         MVI   MSGNUM,10                                                        
         TM    DMCB+9,X'40'        MISSING CPU ID                               
         BO    UPX                                                              
         MVI   MSGNUM,11                                                        
         TM    DMCB+9,X'20'        ACTIVE IN ANOTHER PARTITION                  
         BO    UPX                                                              
         DC    H'0'                                                             
*                                  LOAD & GO TO RECOVERY RESTORE                
UPS2     CLC   SRVP2(11),=C'RECOVERY=NO'                                        
         BE    UPS3                NO MUCKING ABOUT                             
         GOTO1 VCALLOV,DMCB,BIGWRK,X'D9010100'                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF CAN'T LOAD                            
         LA    RF,BIGWRK                                                        
         XC    DMCB(12),DMCB                                                    
         GOTO1 (RF),(R1),,(SESYS,(R8)),0                                        
         CLI   8(R1),0                                                          
         BE    UPS3                                                             
         XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(15),MSG                                                   
         MVC   SRVMSG+15(40),0(RE)                                              
         MVI   MSGNUM,X'FF'                                                     
*                                                                               
UPS3     ICM   R1,15,AREQUEST      TEST SYSTEM REQUEST FILE ERASED              
         BZ    UPS4                                                             
         CLC   DNEXT-DTFPHD(L'DNEXT,R1),=X'00010000'                            
         BNE   UPS4                                                             
         SH    R1,=H'4'                                                         
         ICM   R1,15,0(R1)         YES - POINT TO REQUEST ADDRESS LIST          
         BZ    UPS4                                                             
*                                                                               
         LH    RE,0(R1)            SET UP FOR REQUEST LIST BXLE                 
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         XC    2(6,R1),2(R1)       AND CLEAR REQUEST POINTERS                   
         BXLE  R1,RE,*-6                                                        
*                                                                               
UPS4     BAS   RE,RELSYS           RESET TCB ENTRY                              
*                                                                               
UPOK     NI    SEIND,X'FC'         SET SYSTEM UP FLAGS                          
         OI    SEIND,X'80'                                                      
         MVC   SESSSTRT,TIME       SET START TIME                               
         MVC   SESSLUID,LUID       SET START LUID                               
         MVC   SESSPSWD,SPACES     CLEAR PASSWORD                               
         MVC   SESSPID,PROFSID     SET START PERSON ID                          
         MVC   SESSWHY,REASON      SET START REASON TEXT                        
*                                                                               
UPOK1    GOTO1 VTICTOC,DUB,C'SSET' SEND MESSAGE TO OPERATOR                     
         MVC   UPDNMSG+4(3),SYSNAM3                                             
         MVC   UPDNMSG+9(L'SENAME),SENAME                                       
         MVC   UPDNMSG+17(7),=C'STARTED'                                        
         TM    SEIND,SEIRONLY                                                   
         BO    UPOK2                                                            
         GOTO1 VDMOD000,DMCB,VWCTYPE,UPDNMSG,LUPDNMSG,C'LVL1'                   
         GOTO1 VTICTOC,DUB,C'RSET'                                              
         B     UPX                                                              
*                                                                               
UPOK2    MVC   UPDNXTR(9),=C' READONLY'                                         
         GOTO1 VDMOD000,DMCB,VWCTYPE,UPDNMSG,LUPDNXTR,C'LVL1'                   
         GOTO1 VTICTOC,DUB,C'RSET'                                              
*                                                                               
UPX      CLI   MSGNUM,0            TEST FOR ERRORS                              
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE ENABLE THE SYSTEM IN SENUMB                                   *         
* EXIT WITH CC=NEQ ON ERROR WITH MSGNUM SET                           *         
***********************************************************************         
         SPACE 1                                                                
RWSYS    NTR1                                                                   
         MVI   MSGNUM,0                                                         
         BAS   RE,GETSYS                                                        
         L     R5,ASENTRY                                                       
         USING SELISTD,R5                                                       
         OC    SEFILES,SEFILES     ANY FILES THIS SE                            
         BZ    RWX                 NO - EXIT                                    
         TM    SEIND,SEISETRO      IS IT SET READ-ONLY                          
         BNO   RWX                                                              
UPENABLE NI    SEIND,255-SEISETRO                                               
         L     R3,SEFILES                                                       
         L     R3,0(R3)            POINT TO SYSTEM FILE LIST                    
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         ICM   RF,3,2(R3)          RF = NUMBER OF FILES                         
         LA    R3,4(R3)                                                         
         MVC   FIRSTFIL,0(R3)      EXT NUMBER OF 1ST FILE                       
         NI    FIRSTFIL,X'F0'      REMOVE LS BITS                               
UPEN1    MVC   FULL(1),0(R3)       EXT NUMBER OF THIS FILE                      
         NI    FULL,X'F0'          REMOVE LS BITS                               
         CLC   FULL(1),FIRSTFIL    ARE THEY FROM SAME SYSTEM                    
         BNE   UPEN2                                                            
         ICM   R1,7,5(R3)                                                       
         NI    36(R1),X'FF'-X'80'  CLR READ-ONLY IN DTF                         
UPEN2    LA    R3,8(R3)            NEXT DTF                                     
         BCT   RF,UPEN1                                                         
         LA    R1,2                SYSTEM WRITE ENABLED                         
         BAS   RE,BRDCAST                                                       
         B     RWOK                                                             
*                                                                               
RWOK     GOTO1 VTICTOC,DUB,C'SSET'                                              
         MVC   UPDNMSG+4(3),SYSNAM3                                             
         MVC   UPDNMSG+9(L'SENAME),SENAME                                       
         MVC   UPDNMSG+17(7),=C'+WRITE '                                        
         GOTO1 VDMOD000,DMCB,VWCTYPE,UPDNMSG,LUPDNMSG,C'LVL1'                   
         GOTO1 VTICTOC,DUB,C'RSET'                                              
*                                                                               
RWX      CLI   MSGNUM,0            TEST FOR ERRORS                              
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* STOP SYSTEM IN SENUMB                                               *         
* EXIT WITH CC=NEQ ON ERROR WITH MSGNUM SET                           *         
***********************************************************************         
         SPACE 1                                                                
DNSYS    NTR1                                                                   
         MVI   MSGNUM,12           CANNOT STOP SERVICE SYSTEM                   
         CLI   SENUMB,X'01'                                                     
         BE    DNX                                                              
         MVI   MSGNUM,0                                                         
         BAS   RE,GETSYS                                                        
         L     R2,ASENTRY                                                       
         USING SELISTD,R2                                                       
         OI    SEIND,X'01'         SET SE NO-OP                                 
         NI    SEIND,X'7D'         TURN-OFF OP & STOP BITS                      
         OC    SEFILES,SEFILES     ANY FILES                                    
         BZ    DN5                 NO - EXIT                                    
         L     R3,=F'1000'         I/O LOOP COUNT                               
*                                                                               
DN2      L     R5,VTCB             CHECK SE NOT ACTIVE IN ANOTHER TASK          
         BAS   RE,SETBXLE                                                       
         USING TCBD,R5                                                          
*                                                                               
         CLC   TCBSYS,SESYS                                                     
         BE    *+12                                                             
         BXLE  R5,R6,*-10                                                       
         B     DN4                                                              
*                                  IF IT IS LOOP UNTIL NOT ACTIVE               
         GOTO1 VCALLOV,DMCB,BIGWRK,X'D9010100'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         BCT   R3,DN2                                                           
         MVI   MSGNUM,8            ACTIVE - TRY AGAIN                           
         OI    SEIND,X'80'         RESET SE TO OP                               
         NI    SEIND,X'FE'                                                      
         B     DNX                                                              
*                                                                               
DN4      BAS   RE,SETSYS           CLOSE SE FILES                               
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         L     RE,VSSB                                                          
         MVI   SSBMTIND-SSBD(RE),0 *** DISABLE MULTI-TASKING WAITS ***          
         GOTO1 VTICTOC,PLIST,C'SSET'   SUSPEND TIMERS                           
         GOTO1 VDMOD000,DMCB,VCLSESYS,(SENUMB,BIGWRK)                           
         GOTO1 VTICTOC,PLIST,C'RSET'   RESET TIMERS                             
         L     RE,VSSB             *** ENABLE MULTI-TASKING WAITS  ***          
         MVI   SSBMTIND-SSBD(RE),C'M'                                           
         BAS   RE,RELSYS                                                        
         L     R2,ASENTRY                                                       
*                                                                               
DN5      MVC   SESSSTOP,TIME       SET STOP TIME                                
         MVC   SESSLUID,LUID       SET LUID OF TERMINAL                         
         MVC   SESSPSWD,PASSWORD   SET STOP PASSWORD                            
         MVC   SESSPID,PROFSID     SET STOP PROFS ID                            
         MVC   SESSWHY,REASON      SET STOP REASON TEXT                         
*                                                                               
DNOK     GOTO1 VTICTOC,DUB,C'SSET' SEND SYSTEM DOWN MSG TO OPERATOR             
         MVC   UPDNMSG+4(3),SYSNAM3                                             
         L     RE,ASENTRY                                                       
         MVC   UPDNMSG+9(L'SENAME),SENAME-SELISTD(RE)                           
         MVC   UPDNMSG+17(7),=C'STOPPED'                                        
         GOTO1 VDMOD000,DMCB,VWCTYPE,UPDNMSG,LUPDNMSG,C'LVL1'                   
         GOTO1 VTICTOC,DUB,C'RSET'                                              
*                                                                               
DNX      CLI   MSGNUM,0                                                         
         B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* START/READONLY THE SYSTEM IN SENUMB                                 *         
* EXIT WITH CC=NEQ ON ERROR WITH MSGNUM SET                           *         
***********************************************************************         
         SPACE 1                                                                
ROSYS    NTR1                                                                   
         MVI   MSGNUM,12           CANNOT ALTER SERVICE/CONTROL                 
         CLI   SENUMB,X'01'                                                     
         BE    ROX                                                              
         MVI   MSGNUM,0                                                         
         BAS   RE,GETSYS                                                        
         L     R2,ASENTRY                                                       
         USING SELISTD,R2                                                       
         OC    SEFILES,SEFILES     ANY FILES                                    
         BZ    ROOK                NO - EXIT                                    
         MVI   MSGNUM,13           SYSTEM ALREADY READ-ONLY                     
         TM    SEIND,SEIRONLY+SEISETRO                                          
         BNZ   ROX                                                              
         MVI   MSGNUM,0                                                         
         L     R3,=F'1000'         I/O LOOP COUNT                               
*                                                                               
RO2      L     R5,VTCB             CHECK SE NOT ACTIVE IN ANOTHER TASK          
         BAS   RE,SETBXLE                                                       
         USING TCBD,R5                                                          
*                                                                               
RO2A     CLC   TCBSYS,SESYS                                                     
         BE    RO3                                                              
         LA    R1,TCBSWTAB         CHECK TASKS SWITCH TABLES                    
         SR    RF,RF                                                            
         ICM   RF,1,TCBSWNUM                                                    
         BZ    RO2C                                                             
RO2B     CLC   SESYS,TCBSWSYS-TCBSWTAB(R1)   IF WE FIND THIS SYSTEM             
         BNE   *+14                          WITH RECOVERY RECORDS              
         OC    TCBSWRVF-TCBSWTAB(4,R1),TCBSWRVF-TCBSWTAB(R1)                    
         BNZ   RO3                           MUST NOT MAKE IT READ ONLY         
         LA    R1,TCBSWLEN(R1)                                                  
         BCT   RF,RO2B                                                          
RO2C     BXLE  R5,R6,RO2A          TRY NEXT TASK                                
         B     RO4                                                              
*                                  IF IT IS LOOP UNTIL NOT ACTIVE               
RO3      GOTO1 VCALLOV,DMCB,BIGWRK,X'D9010100'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         BCT   R3,RO2                                                           
         MVI   MSGNUM,8            ACTIVE - TRY AGAIN                           
         B     ROX                                                              
*                                  SET SE FILES TO READ-ONLY                    
RO4      OI    SEIND,SEISETRO                                                   
         L     R3,SEFILES                                                       
         L     R3,0(R3)            POINT TO SYSTEM FILE LIST                    
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         MVI   MSGNUM,14                                                        
         ICM   RF,3,2(R3)          RF = NUMBER OF FILES                         
         LA    R3,4(R3)                                                         
         MVC   FIRSTFIL,0(R3)      EXT NUMBER OF 1ST FILE                       
         NI    FIRSTFIL,X'F0'      REMOVE LS BITS                               
RO4A     MVC   FULL(1),0(R3)       EXT NUMBER OF THIS FILE                      
         NI    FULL,X'F0'          REMOVE LS BITS                               
         CLC   FULL(1),FIRSTFIL    ARE THEY FROM SAME SYSTEM                    
         BNE   RO4B                                                             
         ICM   R1,7,5(R3)                                                       
         TM    36(R1),X'80'        MUST NOT BE READ-ONLY ALREADY                
         B     *+12                                                             
         NI    SEIND,255-SEISETRO                                               
         B     ROX                                                              
         OI    36(R1),X'80'        SET READ-ONLY IN DTF                         
RO4B     LA    R3,8(R3)            NEXT DTF                                     
         BCT   RF,RO4A                                                          
*                                                                               
ROOK     LA    R1,1                MSG 1 SYSTEM GONE READ-ONLY                  
         BAS   RE,BRDCAST                                                       
         MVI   MSGNUM,0                                                         
         GOTO1 VTICTOC,DUB,C'SSET'                                              
         MVC   UPDNMSG+4(3),SYSNAM3                                             
         L     RE,ASENTRY                                                       
         MVC   UPDNMSG+9(L'SENAME),SENAME-SELISTD(RE)                           
         MVC   UPDNMSG+17(7),=C'RD-ONLY'                                        
         GOTO1 VDMOD000,DMCB,VWCTYPE,UPDNMSG,LUPDNMSG,C'LVL1'                   
         GOTO1 VTICTOC,DUB,C'RSET'                                              
*                                                                               
         TM    SEIND,SEISTRT       IS SYSTEM UP AND RUNNING                     
         BNZ   ROX                                                              
         GOTO1 UPSYS,=C'READONLY'  NO SO START IT                               
*                                                                               
ROX      CLI   MSGNUM,0                                                         
         B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
BRDCAST  NTR1  ,                   SET DIRECT BROADCAST BYTE                    
         L     R3,VUTL                                                          
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING UTLD,R3                                                          
BC010    CLC   TSYS,SENUMB                                                      
         BE    *+12                                                             
BC020    BXLE  R3,R4,BC010                                                      
         B     EXIT                                                             
         CLM   R1,1,=X'02'         ARE WE GOING READ ENABLED                    
         BNE   BC030                                                            
         CLI   TBRSYS,1            IS BC 1 STILL PENDING                        
         BNE   BC030                                                            
         MVI   TBRSYS,0            CLR DIRECT BC                                
         NI    TSTAT2,255-TSTATBCP                                              
         B     BC020               NEXT TRM                                     
BC030    OI    TSTAT2,TSTATBCP                                                  
         STC   R1,TBRSYS           SET DIRECT BC                                
         B     BC020               NEXT TRM                                     
         DROP  R3                                                               
         EJECT                                                                  
GETSYS   NTR1  ,                   GET SELIST ENTRY FOR SYS=SENUMB              
         L     R5,VSELIST                                                       
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R5                                                       
         CLC   SESYS,SENUMB                                                     
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
         ST    R5,ASENTRY                                                       
         B     EXIT                                                             
         SPACE 1                                                                
SETSYS   NTR1  ,                   SET TCB ENTRY FOR SENUMB                     
         L     R5,ASENTRY                                                       
         USING SELISTD,R5                                                       
         L     R7,VSSB                                                          
         USING SSBD,R7                                                          
         L     R7,SSBTKADR                                                      
         USING TCBD,R7                                                          
         MVC   SAVETCB(8),TCBDTFS                                               
         MVC   SAVETCB+8(1),TCBSYS                                              
         MVC   TCBDTFS(8),SEFILES                                               
         MVC   TCBSYS,SESYS                                                     
         GOTO1 VDTFIOA,DMCB,(C'I',(R7))                                         
         B     EXIT                                                             
         SPACE 1                                                                
RELSYS   NTR1  ,                   RESET TCB ENTRY                              
         L     R7,VSSB                                                          
         USING SSBD,R7                                                          
         L     R7,SSBTKADR                                                      
         USING TCBD,R7                                                          
         MVC   TCBDTFS(8),SAVETCB                                               
         MVC   TCBSYS,SAVETCB+8                                                 
         GOTO1 VDTFIOA,DMCB,(C'I',(R7))                                         
         B     EXIT                                                             
         DROP  R5,R7                                                            
         SPACE 2                                                                
SETBXLE  LH    R6,0(R5)            SET BXLE REGS                                
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         BR    RE                                                               
         EJECT                                                                  
MESSAGE  CLI   MSGNUM,X'FF'        SET MESSAGE & EXIT                           
         BE    MESSAGE2                                                         
         SR    R1,R1                                                            
         IC    R1,MSGNUM                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'45'                                                        
         LA    R1,MSGLIST(R1)                                                   
         MVC   MSG+15(45),0(R1)                                                 
         MVC   SRVMSG,MSG                                                       
         CLI   MSGNUM,2            TEST IF ERROR OR OK MESSAGE                  
         BH    MESSAGE2                                                         
         MVC   SRVMSG(2),=C'OK'                                                 
         MVC   SRVMSG+3(4),=C'0001'                                             
MESSAGE2 NI    SRVSRVH+6,X'BF'                                                  
         OI    6(R2),X'40'                                                      
         SPACE 1                                                                
EXIT     XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
DOTS     DC    8CL1'.'                                                          
ZEROES   DC    8CL1'0'                                                          
SPACES   DC    CL80' '                                                          
UPDNMSG  DC    C'*FACXXX* SSSSSSS XXXXXXX'                                      
LUPDNMSG EQU   *-UPDNMSG                                                        
UPDNXTR  DC    C'                        '                                      
LUPDNXTR EQU   *-UPDNMSG                                                        
         EJECT                                                                  
MSGLIST  DS    0CL45                                                            
         DC    CL45'SYSTEM STATUS DISPLAYED. ALTER ?'                           
         DC    CL45'SYSTEM STATUS CHANGED. ENTER NEXT REQUEST'                  
         DC    CL45'INVALID INPUT FIELD'                                        
         DC    CL45'MISSING INPUT FIELD'                                        
         DC    CL45'INVALID SENAME'                                             
         DC    CL45'SYSTEM IS ALREADY STARTED'                                  
         DC    CL45'SYSTEM IS STOPPED'                                          
         DC    CL45'SYSTEM IS IN PROCESS - TRY AGAIN'                           
         DC    CL45'INVALID CPU ID'                                             
         DC    CL45'MISSING CPU ID'                                             
         DC    CL45'SYSTEM IS ACTIVE IN ANOTHER ADR SPACE'                      
         DC    CL45'CANNOT ALTER SERVICE/CONTROL SYSTEMS'                       
         DC    CL45'SYSTEM IS ALREADY READ ONLY'                                
         DC    CL45'READ ONLY FILES EXIST WITHIN SYSTEM'                        
         DC    CL45'SYSTEM IS PASSWORD PROTECTED'                               
         DC    CL45'STATUS CHRS MUST BE +/U,-/D,*/N OR R'                       
         DC    CL45'STATUS CHRS IN ONE FIELD ONLY'                              
         EJECT                                                                  
LINED    DSECT                     ** DSECT TO COVER SCREEN LINE **             
LINSTATH DS    CL8                                                              
LINSTAT  DS    CL1                                                              
LINSTATX DS    CL8                                                              
LINSYSH  DS    CL8                                                              
LINSYS   DS    CL2                                                              
LINFLAG  DS    CL1                                                              
LINNAM   DS    CL7                                                              
LINNEXT  EQU   *                                                                
         SPACE 1                                                                
WRKD     DSECT                     DSECT TO COVER $SYS W/S                      
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
APARMS   DS    A                                                                
RELO     DS    A                                                                
ACOMFACS DS    A                                                                
AHEXOUT  DS    A                                                                
ASCANNER DS    A                                                                
ASQUASH  DS    A                                                                
ASENTRY  DS    A                                                                
AREQUEST DS    A                                                                
ASYSMSGH DS    A                                                                
SAVERE   DS    A                                                                
TIME     DS    XL4                                                              
LUID     DS    CL8                                                              
PASSWORD DS    CL8                                                              
PROFSID  DS    CL8                                                              
REASON   DS    CL16                                                             
FORMAT   DS    C                                                                
MSGNUM   DS    C                                                                
FIRSTFIL DS    X                                                                
SENUMB   DS    C                                                                
STATFILT DS    X                                                                
SYSID    DS    X                                                                
SYSCH    DS    C                                                                
SYSNAM1  DS    CL1                                                              
SYSNAM3  DS    CL3                                                              
SYSNAM4  DS    CL4                                                              
SAVETCB  DS    XL10                                                             
MSG      DS    CL60                                                             
WORK     DS    CL128                                                            
*                                                                               
STATLST  DS    100XL4                                                           
STATLSTL EQU   *-STATLST                                                        
*                                                                               
SYSMSGH  DS    CL8                 SYSTEM MESSAGE FIELD HEADER                  
SYSMSG   DS    0CL39                                                            
SYSSYS   DS    CL7                                                              
SYSS1    DS    C                                                                
SYSTIME  DS    CL5                                                              
SYSS2    DS    C                                                                
SYSPID   DS    CL8                                                              
SYSS3    DS    C                                                                
SYSWHY   DS    CL16                                                             
SYSMSGL  EQU   *                                                                
*                                                                               
BIGWRK   DS    600D                                                             
WRKX     EQU   *                                                                
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
         EJECT                                                                  
SRSYSFFD DSECT                                                                  
         DS    CL64                                                             
* SRSYSFFD                                                                      
       ++INCLUDE SRSYSFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SRSYS00S  05/01/02'                                      
         END                                                                    
