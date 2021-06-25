*          DATA SET SRSYS00    AT LEVEL 004 AS OF 04/07/15                      
*PHASE T14000A                                                                  
*INCLUDE SQUASHER                                                               
         TITLE '$PS/$AS/$START/$STOP - START/STOP SYSTEM(S)'                    
SYS      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,**$SYS**,RA,RR=RE                                          
         USING WORKD,RC            RC=A(W/S)                                    
         MVC   IPARMS,0(R1)                                                     
         ST    R1,SAVER1                                                        
         ST    RD,SAVERD                                                        
         ST    RE,RELO                                                          
         L     R9,ATWA             R9=A(TWA)                                    
         USING SRSYSFFD,R9                                                      
         L     R8,ASYSFACS         R8=A(SYSFACS)                                
         USING SYSFACD,R8                                                       
         BRAS  RE,INIT                                                          
*                                                                               
         BRAS  RE,VALP3            P3=USERID/PASSWORD                           
         BNE   MESSAGE                                                          
         BRAS  RE,VALP4            P4=REASON CODE                               
         BNE   MESSAGE                                                          
         EJECT                                                                  
***********************************************************************         
* $SYS TO DISPLAY SYSTEM STATUS                                       *         
* $PS  TO DISPLAY SYSTEM STATUS AND CHANGE TO $AS FOR ALTER NEXT TIME *         
***********************************************************************         
SYS3     CLC   SRVSRV+1(3),=C'SYS' $PS OR $SYS                                  
         BE    *+12                                                             
         CLI   SRVSRV+1,C'P'                                                    
         BNE   SYS4                                                             
*&&UK                                                                           
         L     RE,VSSB                                                          
         TM    SSBSYSFL-SSBD(RE),X'80'                                          
         BO    SYS3A                                                            
         MVC   SRVSRV+1(3),=C'SYS'                                              
*&&                                                                             
SYS3A    BRAS  RE,ANYSTAT          CHECK IF +/-/R FILTERS INPUT                 
         BNE   MESSAGE                                                          
         MVI   FORMAT,X'03'        DISP STATUS/DATA                             
         BRAS  RE,FORMSYS                                                       
         LA    R2,SRVP1H                                                        
         CLI   SRVSRV+1,C'P'       CHANGE $PS TO $AS                            
         BNE   *+12                                                             
         MVI   SRVSRV+1,C'A'                                                    
         LA    R2,SRVL1H                                                        
         ST    R2,FADRH                                                         
         MVI   MSGNUM,1                                                         
         B     MESSAGE                                                          
*                                                                               
SYS4     CLI   SRVSRV+1,C'A'       $AS                                          
         BNE   SYS10                                                            
         BRAS  RE,ANYSTAT          CHECK IF +/-/R FILTERS INPUT                 
         BNE   MESSAGE                                                          
         MVI   FORMAT,X'02'        DISP DATA                                    
         BRAS  RE,FORMSYS                                                       
         LA    R2,SRVL1H           R2=A(FIRST TWA LINE)                         
         USING LINED,R2                                                         
         LA    R3,STATLST          R3=A(SE STATUS LIST)                         
*                                                                               
SYS5     OC    LINSYS,LINSYS       END OF SE STATUS LIST                        
         BZ    SYS8                                                             
         CLI   LINSTATH+5,0        ANY INPUT                                    
         BNE   *+14                                                             
         MVC   LINSTAT,1(R3)       NO-FILL IN CURRENT STATUS                    
         B     SYS7                                                             
         CLC   LINSTAT,1(R3)       YES-IS IT DIFFERENT                          
         BE    SYS7                                                             
         MVC   SENUMB,0(R3)        SET SE NUMBER                                
         BRAS  RE,GETSYS                                                        
         L     R5,ASENTRY          POINT TO SELIST ENTRY                        
         USING SELISTD,R5                                                       
*                                                                               
SYS6     CLI   LINSTAT,C'+'        START                                        
         BNE   SYS6A                                                            
         BRAS  RE,OKTOUP           CHECK IF OK TO START                         
         BNZ   MESSAGE                                                          
         LA    R1,SPACES                                                        
         LA    RF,UPSYS                                                         
         B     SYS6X                                                            
*                                                                               
SYS6A    CLI   LINSTAT,C'-'        STOP                                         
         BNE   SYS6B                                                            
         BRAS  RE,OKTODN           CHECK IF OK TO STOP                          
         BNZ   MESSAGE                                                          
         LA    RF,DNSYS                                                         
         B     SYS6X                                                            
*                                                                               
SYS6B    CLI   LINSTAT,C'R'        READONLY                                     
         BNE   SYS6C                                                            
         BRAS  RE,OKTORO           CHECK IF OK TO SET READ ONLY                 
         BNZ   MESSAGE                                                          
         LA    RF,ROSYS                                                         
         B     SYS6X                                                            
*                                                                               
SYS6C    MVI   MSGNUM,3            INVALID INPUT IN STATUS                      
         B     MESSAGE                                                          
SYS6X    BASR  RE,RF               GO TO UP/DN/RO RTN                           
         BNZ   MESSAGE                                                          
*                                                                               
SYS7     LA    R2,LINNEXT          BUMP TO NEXT LINE                            
         LHI   R0,SRVT1H-SRSYSFFD                                               
         A     R0,ATWA                                                          
         CR    R2,R0               WATCH OVERFLOW                               
         BNL   SYS8                                                             
         LA    R3,L'STATLST(R3)    STATUS                                       
         B     SYS5                                                             
*                                                                               
SYS8     CLI   STATFILT,X'FF'      RE-DISPLAY STATUS/DATA                       
         BNE   SYS9                                                             
         MVI   FORMAT,X'03'                                                     
         BRAS  RE,FORMSYS                                                       
SYS9     XC    SRVSRV,SRVSRV       CLEAR SRV                                    
         LA    R2,SRVSRVH                                                       
         MVI   MSGNUM,2                                                         
         B     MESSAGE                                                          
         EJECT                                                                  
***********************************************************************         
* $START SYSTEM IN P1                                                 *         
* $STOP  SYSTEM IN P1                                                 *         
***********************************************************************         
SYS10    L     RE,VSSB                                                          
         TM    SSBSYSFL-SSBD(RE),X'80' IS THIS A TEST SYSTEM                    
         BO    SYS11                                                            
         MVI   MSGNUM,22           ONLY VALID FOR TEST SYSTEMS                  
         B     MESSAGE                                                          
*                                                                               
SYS11    CLC   SRVSRV+1(3),START                                                
         BE    SSX12                                                            
         CLC   SRVSRV+1(3),STOP                                                 
         BE    SSX12                                                            
         DC    H'0'                                                             
*                                                                               
SSX12    MVI   FORMAT,X'03'        DISPLAY SYSTEMS IN CASE OF ERROR             
         BRAS  RE,FORMSYS                                                       
*                                                                               
SYS12    LA    R2,SRVP1H           P1=SYSTEM NAME                               
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0              MUST BE INPUT                                
         BNE   *+12                                                             
         MVI   MSGNUM,4                                                         
         B     MESSAGE                                                          
*                                                                               
         MVC   WORK(16),SPACES                                                  
         LLC   R1,FHIL             EXTRACT & SPACE FILL INPUT FIELD             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),FHDA                                                     
*                                                                               
         L     R5,VSELIST          SEARCH SELIST FOR NAME                       
         LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         AHI   R5,6                                                             
         USING SELISTD,R5                                                       
         CLC   SENAME,WORK                                                      
         BE    SSX16                                                            
         BXLE  R5,RE,*-10                                                       
         MVI   MSGNUM,5                                                         
         B     MESSAGE                                                          
*                                                                               
SSX16    MVC   SENUMB,SESYS        SAVE SESYS                                   
*                                                                               
SYS13    CLC   SRVSRV+1(3),START                                                
         BNE   SYS14                                                            
         MVI   MSGNUM,3                                                         
         LA    R2,SRVP2H                                                        
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    SYS13A                                                           
         CLC   FHDA(11),NORCV                                                   
         BE    SYS13A                                                           
         CLC   FHDA(8),READONLY                                                 
         BE    *+12                                                             
         MVI   MSGNUM,3                                                         
         B     MESSAGE                                                          
*                                                                               
         BRAS  RE,OKTORO           CHECK IF OK TO SET READ ONLY                 
         BNZ   MESSAGE                                                          
         BRAS  RE,ROSYS            SET SYSTEM READ ONLY                         
         BNZ   MESSAGE                                                          
         B     SYS16                                                            
*                                                                               
SYS13A   LA    R2,SRVP1H                                                        
         MVI   MSGNUM,6            SYSTEM ALREADY STARTED                       
         TM    SEIND,SEISETRO+SEISTRT                                           
         BO    *+12                OK TO START READONLY SYSTEM                  
         TM    SEIND,X'80'                                                      
         BO    MESSAGE                                                          
*                                                                               
         LA    R2,SRVP3H                                                        
         ST    R2,FADRH                                                         
         BRAS  RE,OKTOUP           CHECK IF OK TO START                         
         BNE   MESSAGE                                                          
         LA    R2,SRVP1H                                                        
         ST    R2,FADRH                                                         
         LA    R1,SPACES                                                        
         BRAS  RE,UPSYS            GO TO UP RTN                                 
         BNZ   MESSAGE                                                          
         B     SYS16                                                            
*                                                                               
SYS14    MVI   MSGNUM,7            SYSTEM ALREADY STOPPED                       
         TM    SEIND,X'02'         TEST FORCE STOP BIT                          
         BO    *+12                IF ON CAN BE CLOSED                          
         TM    SEIND,X'80'                                                      
         BZ    MESSAGE                                                          
         LA    R2,SRVP3H                                                        
         BRAS  RE,OKTODN           CHECK IF OK TO STOP                          
         BNZ   MESSAGE                                                          
         LA    R2,SRVP1H                                                        
         BRAS  RE,DNSYS            GO TO DN RTN                                 
         BNZ   MESSAGE                                                          
*                                                                               
SYS16    MVI   FORMAT,X'03'        DISPLAY ALL SELIST DATA                      
         BRAS  RE,FORMSYS                                                       
         XC    SRVSRV,SRVSRV       CLEAR SRV                                    
         LA    R2,SRVSRVH                                                       
         MVI   MSGNUM,2                                                         
         B     MESSAGE                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO TEST IF START/STOP/READONLY ALLOWED FROM THIS TERMINAL  *         
* NEED TO INPUT PASSWORD FOR START IF SET BY STOP                     *         
***********************************************************************         
OKTODN   NTR1                      CHECK IF OK TO TAKE SYSTEM DOWN              
         MVI   MSGNUM,0                                                         
         B     EXITOK                                                           
*                                                                               
OKTORO   NTR1                      CHECK IF OK TO SET SYSTEM R/O                
         MVI   MSGNUM,0                                                         
         B     EXITOK                                                           
*                                                                               
OKTOUP   NTR1                      CHECK IF OK TO BRING SYSTEM UP               
         MVI   MSGNUM,0                                                         
         CLC   SESSPSWD,SPACES     WAS SYSTEM STOPPED WITH A PASSWORD           
         BNH   EXITOK              NO                                           
         CLC   SESSPSWD,PASSWORD   YES MUST MATCH PASSWORD INPUT                
         BE    EXITOK                                                           
         MVI   MSGNUM,15           SYSTEM IS PASSWORD PROTECTED                 
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* TEST IF ANY STATUS FILTERS INPUT IN S/R FIELD OR P1                 *         
* FORMAT IS $S/R,... WITH ANY COMBO OF +,U,-,D,*,N OR R               *         
* OR THE STRING CAN BE INPUT IN P1                                    *         
***********************************************************************         
ANYSTAT  NTR1                                                                   
         LA    R2,SRVSRVH          TEST IF $SYS,.... INPUT                      
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BE    ANYS02                                                           
         ICM   RF,15,=CL4',=, '                                                 
         IC    RF,=X'FF'                                                        
         GOTO1 ASCANNER,DMCB,FHD,(3,WORK),(RF),0                                
         CLI   DMCB+4,2                                                         
         BL    ANYS02                                                           
*                                                                               
ANYS01   LA    R3,WORK             R3=A(SCAN BLOCK)                             
         USING SCANBLKD,R3                                                      
         AHI   R3,SCBLKLQ                                                       
         LLC   R0,SC1STLEN         R0=LENGTH OF INPUT FILTER                    
         LA    RF,SC1STFLD         RF=A(INPUT FILTER)                           
         CLI   0(RF),C'A'          TEST TO SHOW ADV IDS                         
         BNE   *+8                                                              
         OI    FORMAT1,X'01'       SET SHOW ADV IDS FLAG                        
         B     ANYS02                                                           
         MVI   STATFILT,0          INIT STATUS FLAGS                            
*&&DO                                                                           
         CLI   SRVP1H+(FHIL-FHD),0 MUST BE NOTHING IN P1 IF IN S/R FLD          
         BE    ANYS04                                                           
         LA    RF,SRVP1H                                                        
         ST    RF,FADRH                                                         
         MVI   MSGNUM,17           STATUS IN S/R OR P1 ONLY                     
         B     EXITL                                                            
*&&                                                                             
ANYS02   LA    R2,SRVP1H           TEST IF $SYS,.... INPUT                      
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    EXITOK                                                           
*                                                                               
         LLC   R0,FHIL             R0=LENGTH OF INPUT FILTER                    
         LA    RF,FHDA             RF=A(INPUT FILTER)                           
         CLI   0(RF),C'A'          TEST TO SHOW ADV IDS                         
         BNE   *+12                                                             
         OI    FORMAT1,X'01'       SET SHOW ADV IDS FLAG                        
         B     *+8                                                              
         MVI   STATFILT,0          INIT STATUS FLAGS                            
*                                                                               
ANYS04   LA    R1,SFILTS           R1=LIST OF VALID FILTERS                     
*                                                                               
ANYS06   CLC   0(1,R1),0(RF)       IF FILTER MATCHES TURN ON FLAG BIT           
         BNE   *+14                                                             
         OC    STATFILT,1(R1)                                                   
         B     ANYS08                                                           
*                                                                               
         LA    R1,2(R1)            NEXT VALID FILTER                            
         CLI   0(R1),X'FF'                                                      
         BNE   ANYS06                                                           
         MVI   MSGNUM,16                                                        
         B     EXITL                                                            
*                                                                               
ANYS08   LA    RF,1(RF)            NEXT FILTER IN INPUT STRING                  
         BCT   R0,ANYS04                                                        
         B     EXITOK                                                           
*                                                                               
SFILTS   DC    C'+',X'80'                                                       
         DC    C'U',X'80'                                                       
         DC    C'-',X'40'                                                       
         DC    C'D',X'40'                                                       
         DC    C'R',X'20'                                                       
         DC    C'*',X'10'                                                       
         DC    C'N',X'10'                                                       
         DC    C'2',X'08'                                                       
         DC    C'A',X'00'                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT SELIST ENTRIES IN TWA                                        *         
* FORMAT BYTE SET TO X'01'=STATUS REQUIRED,X'02'=DATA REQUIRED        *         
* PASSES BACK STATLST CONTAINING MULTIPLE 4 BYTE ENTRIES              *         
* BYTE 1=SESYS                                                        *         
* BYTE 2=STATUS (+/-/*/R)                                             *         
* BYTE 3=UPDATIVE FACPAK ID                                           *         
* BYTE 4=SPARE                                                        *         
***********************************************************************         
FORMSYS  NTR1                                                                   
         LA    R0,STATLST          CLEAR SE STATUS LIST                         
         LHI   R1,STATLSTL                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LHI   RE,SRVT1H-SRSYSFFD                                               
         A     RE,ATWA                                                          
         ST    RE,ASYSMSGH         SET A(NEXT SYSTEM MESSAGE HEADER)            
*                                                                               
         LA    R3,STATLST                                                       
         LA    R2,SRVL1H                                                        
         USING LINED,R2                                                         
         L     R5,VSELIST                                                       
         LH    R6,0(R5)            SET BXLE REGS                                
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         MVI   SLOT,0                                                           
*                                                                               
FRM02    LLC   R0,SLOT             SCREEN SLOT NUMBER                           
         AHI   R0,1                                                             
         STC   R0,SLOT                                                          
*                                                                               
         MVC   0(1,R3),SESYS       SET SE & STAT IN STATLST                     
         MVI   1(R3),C'+'                                                       
         TM    SEIND,SEISTRT       TEST SYSTEM STARTED                          
         BO    *+8                                                              
         MVI   1(R3),C'-'                                                       
         TM    SEIND,SEIRESA       TEST RESTRICTED ACCESS                       
         BZ    *+8                                                              
         MVI   1(R3),C'*'                                                       
         TM    SEIND,SEISTRT+SEIRONLY  STARTED & READ-ONLY (FASTART)            
         BNO   *+8                                                              
         MVI   1(R3),C'R'                                                       
         TM    SEIND,SEISTRT+SEISETRO  STARTED & SET TO READ-ONLY               
         BNO   *+8                                                              
         MVI   1(R3),C'R'                                                       
*                                                                               
FRM03    ICM   RE,15,ASYSSTAB      SET UPDATIVE FACPAK ID                       
         BZ    FRM04                                                            
         LLC   RF,SESYS                                                         
         AR    RE,RF               TABLE IS INDEXED BY SENUM                    
         CLI   0(RE),15            ENTRY CONTAINS FACPAK ID NUM                 
         BH    FRM04                                                            
         MVC   2(1,R3),0(RE)                                                    
*                                                                               
FRM04    CLI   STATFILT,X'FF'      ANY STATUS FILTERS INPUT                     
         BE    FRM08                                                            
*                                                                               
         TM    STATFILT,X'08'      DO WE WANT SECOND SCREEN                     
         BZ    FRM05                                                            
         CLI   SLOT,120            YES-THEN SKIP FIRST SCREEN'S WORTH           
         BNH   FRM06                                                            
         B     FRM08                                                            
*                                                                               
FRM05    CLI   1(R3),C'+'          TEST TO SHOW UP SYSTEMS                      
         BNE   *+12                                                             
         TM    STATFILT,X'80'                                                   
         BZ    FRM06                                                            
*                                                                               
         CLI   1(R3),C'-'          TEST TO SHOW DOWN SYSTEMS                    
         BNE   *+12                                                             
         TM    STATFILT,X'40'                                                   
         BZ    FRM06                                                            
*                                                                               
         CLI   1(R3),C'R'          TEST TO SHOW READONLY SYSTEMS                
         BNE   *+12                                                             
         TM    STATFILT,X'20'                                                   
         BZ    FRM06                                                            
*                                                                               
         CLI   1(R3),C'*'          TEST TO SHOW NOP SYSTEMS                     
         BNE   *+12                                                             
         TM    STATFILT,X'10'                                                   
         BZ    FRM06                                                            
*                                                                               
         B     FRM08                                                            
*                                                                               
FRM06    XC    0(L'STATLST,R3),0(R3)                                            
         B     FORMH               NOT DISPLAYING THIS SYSTEM                   
*                                                                               
FRM08    TM    FORMAT,X'01'                                                     
         BZ    *+10                                                             
         MVC   LINSTAT,1(R3)                                                    
         OI    LINSTATH+(FHOI-FHD),FHOITR                                       
*                                                                               
         TM    FORMAT,X'02'                                                     
         BZ    FORM3                                                            
         GOTO1 AHEXOUT,DMCB,SESYS,LINSYS,1,=C'TOG'                              
         MVC   LINNAM,SENAME                                                    
*                                                                               
         TM    FORMAT1,X'01'       TEST TO SHOW UPDATIVE FACPAK ID              
         BZ    FORM3                                                            
         LLC   RE,2(R3)            GET ADV SYSTEN NUMBER 01-15                  
         SLL   RE,1                                                             
         LA    RE,ADVIDS(RE)       GET ADV SYSTEM TWO CHR ID                    
         CLI   2(R3),X'FF'                                                      
         BNE   *+12                                                             
         LA    RE,ADVALL           SET ALL ADVS FOR GLOBAL SYSTEM               
         B     FRM10                                                            
         CLI   2(R3),15                                                         
         BNH   FRM10                                                            
         LA    RE,ADVUNK           SET UNKNOWN ADV                              
FRM10    MVC   LINSYS,0(RE)                                                     
         CLI   SESYS,X'01'         SERVICE UP IN ALL ADVS                       
         BNE   *+10                                                             
         MVC   LINSYS,ADVALL                                                    
         CLI   SESYS,X'0A'         CONTROL UP IN ALL ADVS                       
         BNE   *+10                                                             
         MVC   LINSYS,ADVALL                                                    
         CLI   SESYS,X'0B'         GAMES UP IN ALL ADVS                         
         BNE   *+10                                                             
         MVC   LINSYS,ADVALL                                                    
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
         BRAS  RE,OUTMSG                                                        
         B     FORMG                                                            
*                                                                               
FORM3C   CLI   LINSTAT,C'*'        SET COLOURS FOR NOP                          
         BNE   FORM3D                                                           
         MVI   LINSTATX+5,X'06'    YELLOW                                       
         MVI   LINSYSH+4,X'0C'                                                  
         MVI   LINSYSH+5,X'06'                                                  
         B     FORMG                                                            
*                                                                               
FORM3D   CLI   LINSTAT,C'R'        SET COLOURS FOR READONLY                     
         BNE   FORM3E                                                           
         MVI   LINSTATX+5,X'06'    YELLOW                                       
         MVI   LINSYSH+4,X'0C'                                                  
         MVI   LINSYSH+5,X'06'                                                  
         CLC   SESSLUID,SPACES     HAS SOMEONE SET THIS SYSTEM READONLY         
         BNH   FORMG               NO                                           
         OI    LINSYSH+5,X'80'     YES SET REVERSE VIDEO                        
         MVI   LINNAM-1,C'-'                                                    
*NOP*    BRAS  RE,OUTMSG                                                        
         B     FORMG                                                            
FORM3E   EQU   *                                                                
*                                                                               
FORMG    LA    R2,LINNEXT          BUMP TO NEXT LINE &                          
         LHI   R0,SRVT1H-SRSYSFFD                                               
         A     R0,ATWA                                                          
         CR    R2,R0                                                            
         BNL   FORMJ                                                            
         LA    R3,L'STATLST(R3)    STATUS LIST ENTRY                            
*                                                                               
FORMH    BXLE  R5,R6,FRM02                                                      
*                                                                               
FORMJ    XC    LINSYS,LINSYS       SET END OF DISPLAY                           
         XC    LINSTAT,LINSTAT                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT SYSTEM STAUS CHANGE MESSAGE                                  *         
***********************************************************************         
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
         LLC   R1,0(RF)                                                         
         AHI   R1,-9                                                            
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
UPSYS    NTR1                                                                   
         CLC   READONLY,0(R1)                                                   
         BE    *+8                                                              
         BRAS  RE,RWSYS            MAKE SURE WRITES ENABLED                     
         MVI   MSGNUM,0                                                         
         BRAS  RE,GETSYS                                                        
*                                                                               
         L     R5,ASENTRY                                                       
         USING SELISTD,R5                                                       
         OC    SEFILES,SEFILES     ANY FILES THIS SE                            
         BZ    UPOK                NO-EXIT                                      
         TM    SEIND,SEISTRT       MAKE SYRE SYSTEM IS DOWN                     
         BZ    UPS0                                                             
         BRAS  RE,DNSYS            IF NOT TAKE IT DOWN                          
*                                                                               
UPS0     BRAS  RE,SETSYS           FRIG TCB ENTRY FOR SENUMB                    
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         GOTO1 VDMOD000,DMCB,VFINDSYS,(SESYS,0)                                 
                                                                                
         L     R1,4(R1)            R1=A(SYSFILES LIST)                          
         USING SYSFLSTD,R1                                                      
         LH    R0,SYSF#FLS                                                      
         LA    R1,SYSFLIST                                                      
         XC    AREQUEST,AREQUEST                                                
*                                  CLEAR ISCILAST FOR ALL I/S FILES             
         USING ISDTF,RE                                                         
UPS1     L     RE,SYSFADTF-1       A(DTF)                                       
         TM    SYSFIND1,SFISF      I/S FILE ?                                   
         BZ    UPS1A                                                            
         TM    SYSFIND2,SFALIAS                                                 
         BO    UPS1A               DONT CLEAR IF NOT NATIVE                     
         XC    ISCILAST,ISCILAST                                                
         DROP  RE                                                               
                                                                                
UPS1A    TM    SYSFIND1,SFREQ      TEST REQUEST FILE                            
         BZ    *+8                                                              
         ST    RE,AREQUEST                                                      
*                                                                               
*NOP*    USING DTFPHD,RE                                                        
*NOP*    TM    SYSFIND1,SFRCV      TEST RECOVERY FILE                           
*NOP*    BZ    UPS1B                                                            
*NOP*    L     R7,DBLK                                                          
*NOP*    LTR   R7,R7                                                            
*NOP*    BZ    UPS1B               TEST BLOCKED                                 
*NOP*    XC    0(8,R7),0(R7)       ERASE ANY LEFTOVER DATA                      
*NOP*    DROP  RE                                                               
*                                                                               
UPS1B    LA    R1,SYSFLNQ(R1)      BUMP TO NEXT LIST ENTRY                      
         BCT   R0,UPS1                                                          
         DROP  R1                                                               
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
         BRAS  RE,RELSYS           RESET TCB ENTRY                              
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
         BE    UPS3                                                             
         GOTO1 VCALLOV,DMCB,BIGWRK,X'D9010100'                                  
         CLI   4(R1),X'FF'                                                      
         JE    *+2                 DIE IF CANT LOAD                             
*                                                                               
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
         AHI   R1,-4                                                            
         ICM   R1,15,0(R1)         YES-POINT TO REQUEST ADDRESS LIST            
         BZ    UPS4                                                             
*                                                                               
         LH    RE,0(R1)            SET UP FOR REQUEST LIST BXLE                 
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         XC    2(6,R1),2(R1)       AND CLEAR REQUEST POINTERS                   
         BXLE  R1,RE,*-6                                                        
*                                                                               
UPS4     BRAS  RE,RELSYS           RESET TCB ENTRY                              
*                                                                               
UPOK     NI    SEIND,X'FC'         SET SYSTEM UP FLAGS                          
         OI    SEIND,X'80'                                                      
         MVC   SESSSTRT,TIME       SET START TIME                               
         MVC   SESSLUID,LUID       SET START LUID                               
         MVC   SESSPSWD,SPACES     CLEAR PASSWORD                               
         MVC   SESSPID,USERID      SET START PERSON ID                          
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
UPX      ICM   RF,15,AMQIO                                                      
         BZ    UPXX                                                             
         GOTO1 (RF),DMCB,=CL8'SYSOPEN ',0,0,0                                   
*                                                                               
UPXX     CLI   MSGNUM,0            TEST FOR ERRORS                              
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE ENABLE THE SYSTEM IN SENUMB                                   *         
* EXIT WITH CC=NEQ ON ERROR WITH MSGNUM SET                           *         
***********************************************************************         
RWSYS    NTR1                                                                   
         MVI   MSGNUM,0                                                         
         BRAS  RE,GETSYS                                                        
         L     R5,ASENTRY                                                       
         USING SELISTD,R5                                                       
         OC    SEFILES,SEFILES     ANY FILES THIS SE                            
         BZ    RWX                 NO-EXIT                                      
         TM    SEIND,SEISETRO      IS IT SET READ-ONLY                          
         BNO   RWX                                                              
*                                                                               
         XC    DUB,DUB             SET US TO UPDATIVE IN DSPACE                 
         MVC   DUB+3(1),SESYS                                                   
         MVI   DUB+1,8                                                          
         GOTO1 VLOCKSPC,DUB                                                     
         CLI   DUB+4,X'80'         TEST FOR UNAVAILABLE                         
         BO    RWX                                                              
*                                                                               
UPENABLE NI    SEIND,255-SEISETRO                                               
         L     R3,SEFILES          A(DTF SYSTEM LIST)                           
         USING SYSFLSTD,R3                                                      
         L     R3,0(R3)            A(SYSTEM FILE LIST)                          
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         ICM   RF,3,SYSF#FLS       RF=NUMBER OF FILES                           
         LA    R3,SYSFLIST                                                      
         MVC   FIRSTFIL,SYSFILE#   EXT NUMBER OF 1ST FILE                       
         NI    FIRSTFIL,X'F0'      REMOVE LS BITS                               
                                                                                
UPEN1    MVC   FULL(1),SYSFILE#    EXT NUMBER OF THIS FILE                      
         NI    FULL,X'F0'          REMOVE LS BITS                               
         CLC   FULL(1),FIRSTFIL    ARE THEY FROM SAME SYSTEM                    
         BNE   UPEN2                                                            
         ICM   R1,7,5(R3)                                                       
         USING DTFPHD,R1                                                        
         NI    DTFOPEN,X'FF'-DTF_RO  CLEAR READ-ONLY BIT IN DTF                 
         DROP  R1                                                               
                                                                                
UPEN2    LA    R3,SYSFLNQ(R3)      NEXT DTF                                     
         BCT   RF,UPEN1                                                         
         DROP  R3                                                               
                                                                                
         LHI   R1,2                SYSTEM WRITE ENABLED                         
         BRAS  RE,BRDCAST                                                       
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
DNSYS    NTR1                                                                   
         MVI   MSGNUM,12           CAN NOT STOP SERVICE SYSTEM                  
         CLI   SENUMB,X'01'                                                     
         BE    DNX                                                              
         MVI   MSGNUM,21           CAN NOT STOP DEMO SYSTEM                     
         CLI   SENUMB,X'0C'                                                     
         BE    DNX                                                              
         MVI   MSGNUM,0                                                         
         BRAS  RE,GETSYS                                                        
*                                                                               
         L     R2,ASENTRY                                                       
         USING SELISTD,R2                                                       
         OI    SEIND,SEINOP        SET SE NO-OP                                 
         NI    SEIND,X'FF'-SEISTRT-SEIRESA                                      
         OC    SEFILES,SEFILES                                                  
         BZ    DN5                 EXIT IF NO FILES                             
         LA    R3,60               NUMBER OF TIME TO TRY                        
*                                                                               
DN2      L     R5,VTCB             CHECK SE NOT ACTIVE IN ANOTHER TASK          
         LH    R6,0(R5)            SET BXLE REGS                                
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING TCBD,R5                                                          
*                                                                               
         CLC   TCBSEN,SESYS                                                     
         BE    *+12                                                             
         BXLE  R5,R6,*-10                                                       
         B     DN4                                                              
*                                  IF IT IS LOOP UNTIL NOT ACTIVE               
         GOTOR AGETFACT,DMCB,(X'80',=F'38400'),F#WAIT                           
         BCT   R3,DN2                                                           
*                                                                               
         MVI   MSGNUM,8            ACTIVE - TRY AGAIN                           
         OI    SEIND,SEISTRT       RESET SE TO OP                               
         NI    SEIND,X'FF'-SEINOP                                               
         B     DNX                                                              
*                                                                               
DN4      BRAS  RE,SETSYS           CLOSE SE FILES                               
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         L     RE,VSSB                                                          
         MVI   SSBMTIND-SSBD(RE),0 *** DISABLE MULTI-TASKING WAITS ***          
         GOTO1 VTICTOC,PLIST,C'SSET'   SUSPEND TIMERS                           
         GOTO1 VDMOD000,DMCB,VCLSESYS,(SENUMB,BIGWRK)                           
         GOTO1 VTICTOC,PLIST,C'RSET'   RESET TIMERS                             
         L     RE,VSSB             *** ENABLE MULTI-TASKING WAITS  ***          
         MVI   SSBMTIND-SSBD(RE),C'M'                                           
         BRAS  RE,RELSYS                                                        
         L     R2,ASENTRY                                                       
*                                                                               
DN5      MVC   SESSSTOP,TIME       SET STOP TIME                                
         MVC   SESSLUID,LUID       SET LUID OF TERMINAL                         
         MVC   SESSPSWD,PASSWORD   SET STOP PASSWORD                            
         MVC   SESSPID,USERID      SET STOP PROFS ID                            
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
ROSYS    NTR1                                                                   
         MVI   MSGNUM,12           CANNOT ALTER SERVICE/CONTROL                 
         CLI   SENUMB,X'01'                                                     
         BE    ROX                                                              
         MVI   MSGNUM,0                                                         
         BRAS  RE,GETSYS                                                        
         L     R2,ASENTRY                                                       
         USING SELISTD,R2                                                       
         OC    SEFILES,SEFILES     ANY FILES                                    
         BZ    ROOK                NO-EXIT                                      
*NOP     MVI   MSGNUM,13           SYSTEM ALREADY READ-ONLY                     
*NOP     TM    SEIND,SEIRONLY+SEISETRO                                          
*NOP     BNZ   ROX                                                              
         MVI   MSGNUM,0                                                         
         LA    R3,60               I/O LOOP COUNT                               
*                                                                               
RO2      L     R5,VTCB             CHECK SE NOT ACTIVE IN ANOTHER TASK          
         LH    R6,0(R5)            SET BXLE REGS                                
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING TCBD,R5                                                          
*                                                                               
RO2A     CLC   TCBSEN,SESYS                                                     
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
RO3      GOTOR AGETFACT,DMCB,(X'80',=F'38400'),F#WAIT                           
         BCT   R3,RO2                                                           
         MVI   MSGNUM,8            ACTIVE - TRY AGAIN                           
         B     ROX                                                              
*                                                                               
RO4      OI    SEIND,SEISETRO      SET SE FILES TO READ-ONLY                    
         L     R3,SEFILES          A(DTF FILE LIST)                             
         L     R3,0(R3)            A(SYSTEM FILE LIST)                          
         USING SYSFLSTD,R3                                                      
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         MVI   MSGNUM,14                                                        
         ICM   RF,3,SYSF#FLS       RF=NUMBER OF FILES IN LIST                   
         LA    R3,SYSFLIST         A(START OF LIST)                             
         MVC   FIRSTFIL,SYSFILE#   EXT NUMBER OF 1ST FILE                       
         NI    FIRSTFIL,X'F0'      REMOVE LS BITS                               
*                                                                               
RO4A     MVC   FULL(1),SYSFILE#    EXT NUMBER OF THIS FILE                      
         NI    FULL,X'F0'          REMOVE LS BITS                               
         CLC   FULL(1),FIRSTFIL    ARE THEY FROM SAME SYSTEM                    
         BNE   RO4B                NO                                           
         ICM   R1,7,SYSFADTF                                                    
         USING DTFPHD,R1                                                        
         TM    DTFOPEN,DTF_RO      MUST NOT BE READ-ONLY ALREADY                
         B     *+12                                                             
         NI    SEIND,255-SEISETRO                                               
         B     ROX                                                              
         OI    DTFOPEN,DTF_RO      SET TO READ-ONLY IN DTF                      
         DROP  R1                                                               
                                                                                
RO4B     LA    R3,SYSFLNQ(R3)      NEXT DTF                                     
         BCT   RF,RO4A                                                          
         DROP  R3                                                               
*                                                                               
ROOK     LHI   R1,1                MSG 1 SYSTEM GONE READ-ONLY                  
         BRAS  RE,BRDCAST                                                       
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
         BNZ   RODEQ                                                            
         GOTO1 UPSYS,=C'READONLY'  NO SO START IT                               
         B     ROX                                                              
*                                                                               
RODEQ    LLC   RE,SESYS                                                         
         CVD   RE,DUB              NO BUILD ENQUEUE ID                          
         UNPK  DUB(4),DUB+6(2)                                                  
         OI    DUB+3,X'F0'                                                      
         MVC   UPDMIN+4(4),DUB                                                  
         XC    DUB(12),DUB                                                      
         LA    R2,UPDMAJ                                                        
         LA    RE,UPDMIN                                                        
         DEQ   ((R2),(RE),8,SYSTEM),RET=HAVE                                    
*                                                                               
         L     R2,ASENTRY                                                       
         XC    DUB,DUB             SET READ ONLY IN DSPACE                      
         MVC   DUB+3(1),SESYS                                                   
         MVI   DUB+1,9                                                          
         GOTO1 VLOCKSPC,DUB                                                     
*                                                                               
ROX      CLI   MSGNUM,0                                                         
         B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* SET DIRECT BROADCAST BYTE IN UTL LIST FOR AN SE                     *         
* ENTRY: R1=READ/WRITE STATUS (01=READ, 02=WRITE)                     *         
***********************************************************************         
BRDCAST  NTR1                                                                   
         SAM31                                                                  
         L     R3,VUTL                                                          
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING UTLD,R3                                                          
BCS02    CLC   TSYS,SENUMB         CONNECTED TO THIS SYSTEM?                    
         BNE   BCS06                                                            
         CHI   R1,2                ARE WE GOING READ ENABLED                    
         BNE   BCS04                                                            
*                                                                               
         CLI   TBRSYS,1            IS BROADCAST 1 STILL PENDING                 
         BNE   BCS04               NO                                           
         MVI   TBRSYS,0            CLEAR BROADCAST 1                            
         NI    TSTAT2,255-TSTATBCP CLEAR PENDING                                
         B     BCS06                                                            
*                                                                               
BCS04    EQU   *                                                                
*&&UK*&& B     BCS06               *NOP* BROADCAST                              
         OI    TSTAT2,TSTATBCP     SET PENDING                                  
         STC   R1,TBRSYS           SET MESSAGE                                  
*                                                                               
BCS06    BXLE  R3,RE,BCS02         NEXT TERMINAL                                
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET SELIST ENTRY FOR SYS=SENUMB                                     *         
***********************************************************************         
GETSYS   NTR1                                                                   
         L     R2,VSELIST                                                       
         LH    RE,0(R2)                                                         
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING SELISTD,R2                                                       
         CLC   SESYS,SENUMB                                                     
         BE    *+10                                                             
         BXLE  R2,RE,*-10                                                       
         DC    H'0'                                                             
         ST    R2,ASENTRY                                                       
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SET TCB ENTRY FOR SE NUMBER                                         *         
***********************************************************************         
SETSYS   NTR1                      SET TCB ENTRY FOR SENUMB                     
         L     R2,ASENTRY                                                       
         USING SELISTD,R2                                                       
         L     RF,VSSB                                                          
         ICM   R3,15,SSBTKADR-SSBD(RF)                                          
         USING TCBD,R3                                                          
         MVC   SAVETCB(8),TCBDTFS                                               
         MVC   SAVETCB+8(1),TCBSEN                                              
         MVC   TCBDTFS(8),SEFILES                                               
         MVC   TCBSEN,SESYS                                                     
         GOTO1 VDTFIOA,DMCB,(C'I',TCBD)                                         
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* RESET TCB ENTRY                                                     *         
***********************************************************************         
RELSYS   NTR1                      RESET TCB ENTRY                              
         L     RF,VSSB                                                          
         L     R3,SSBTKADR-SSBD(RF)                                             
         USING TCBD,R3                                                          
         MVC   TCBDTFS(8),SAVETCB                                               
         MVC   TCBSEN,SAVETCB+8                                                 
         GOTO1 VDTFIOA,DMCB,(C'I',TCBD)                                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT MESSAGE TO SCREEN                                            *         
***********************************************************************         
MESSAGE  CLI   MSGNUM,X'FF'        SET MESSAGE & EXIT                           
         BE    MESSAGE2                                                         
         LLC   R1,MSGNUM                                                        
         BCTR  R1,0                                                             
         MHI   R1,MSGL                                                          
         LA    R1,MSGLIST(R1)                                                   
         MVC   MSG+15(MSGL),0(R1)                                               
         MVC   SRVMSG,MSG                                                       
         CLI   MSGNUM,2            TEST IF ERROR OR OK MESSAGE                  
         BH    MESSAGE2                                                         
         MVC   SRVMSG(2),=C'OK'                                                 
         MVC   SRVMSG+3(4),=C'0001'                                             
*                                                                               
MESSAGE2 ICM   RF,15,FADRH                                                      
         BZ    XMOD                                                             
         NI    SRVSRVH+(FHOI-FHD),255-FHOICU                                    
         OI    (FHOI-FHD)(RF),FHOICU                                            
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE USERID/PASSWORD IN P3                                      *         
***********************************************************************         
VALP3    NTR1                                                                   
         MVC   USERID,SPACES       P3=USERID(,PASSWORD)                         
         MVC   PASSWORD,SPACES                                                  
         LA    R2,SRVP3H                                                        
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BE    EXITOK                                                           
*                                                                               
         GOTO1 ASCANNER,DMCB,(R2),(2,WORK)                                      
         CLI   DMCB+4,0                                                         
         BNE   *+12                                                             
         MVI   MSGNUM,18                                                        
         B     EXITL                                                            
*                                                                               
         LA    R3,WORK                                                          
         USING SCANBLKD,R3                                                      
         CLI   SC1STLEN,0          FORMAT IS USERID,(PASSWORD)                  
         BNE   *+12                                                             
         MVI   MSGNUM,18                                                        
         B     EXITL                                                            
         CLI   SC2NDLEN,0                                                       
         BE    *+12                                                             
         MVI   MSGNUM,18                                                        
         B     EXITL                                                            
*                                                                               
         CLI   SC1STLEN,8          MAX 8 CHARACTERS                             
         BNH   *+12                                                             
         MVI   MSGNUM,19                                                        
         B     EXITL                                                            
*                                                                               
         MVC   USERID,SC1STFLD     SET USERID                                   
*                                                                               
         CLI   DMCB+4,2            OPTIONAL PASSWORD ENTERED?                   
         BNE   EXITOK              NO                                           
*                                                                               
         AHI   R3,SCBLKLQ                                                       
         CLI   SC1STLEN,0          FORMAT IS USERID,(PASSWORD)                  
         BNE   *+12                                                             
         MVI   MSGNUM,18                                                        
         B     EXITL                                                            
         CLI   SC2NDLEN,0                                                       
         BE    *+12                                                             
         MVI   MSGNUM,18                                                        
         B     EXITL                                                            
*                                                                               
         CLI   SC1STLEN,8          MAX 8 CHARACTERS FOR PASSWORD                
         BNH   *+12                                                             
         MVI   MSGNUM,20                                                        
         B     EXITL                                                            
*                                                                               
         MVC   PASSWORD,SC1STFLD                                                
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE P4 (REASON CODE)                                           *         
***********************************************************************         
VALP4    NTR1                                                                   
         MVC   REASON,SPACES       P4=REASON CODE                               
         LA    R2,SRVP4H                                                        
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         XR    RF,RF                                                            
         ICM   RF,1,FHIL                                                        
         BZ    EXITOK                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   REASON(0),FHDA                                                   
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1                                                                   
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   AMQIO,CMQIO                                                      
         MVC   AGETFACT,CGETFACT                                                
*                                                                               
         L     RF,=V(SQUASHER)                                                  
         A     RF,RELO                                                          
         ST    RF,ASQUASH                                                       
*                                                                               
         MVC   MSG,SPACES          SET FACPAK SYSTEM ID NAME                    
         MVC   MSG+00(15),=CL15'ED/9999 (XXXX) '                                
*                                                                               
         L     RF,VSSB                                                          
         USING SSBD,RF                                                          
         MVC   SYSID,SSBSYSID                                                   
         MVC   SYSCH,SSBSYSCH                                                   
         MVC   SYSNAM1,SSBSYSN1                                                 
         MVC   SYSNAM3,SSBSYSNA                                                 
         MVC   SYSNAM4,SSBSYSN4                                                 
         MVC   MSG+02(1),SYSCH                                                  
         MVC   MSG+09(4),SYSNAM4                                                
         CLI   MSG+12,C' '                                                      
         BNE   *+10                                                             
         MVC   MSG+12(2),=CL2') '  CLOSE UP BRACKET IF NECESSARY                
         DROP  RF                                                               
*                                                                               
         L     RE,AUTL             EXTRACT LUID FROM UTL                        
         MVC   LUID,TSYM-UTLD(RE)                                               
*                                                                               
         TBIN  SECS                                                             
         ST    R1,TIME             GET TIME IN SECONDS                          
*                                                                               
         MVI   STATFILT,X'FF'      INITIALISE STATUS FILTERS                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,UPDID                                              
         L     R1,12(R1)                                                        
         MVC   MYUPDID,0(R1)       GET UPDID FOR SYSTEM                         
*                                                                               
         GOTO1 VDMOD000,DMCB,VFINDSYS,(1,0)                                     
         L     R1,4(R1)            R1=A(SYSFILES LIST)                          
         AHI   R1,-4                                                            
         L     R1,0(R1)            EXTRACT SYSSTAB (FACPAK ID TAB)              
         ST    R1,ASYSSTAB                                                      
         MVI   FORMAT1,X'00'       FLAG TO SHOW FACPAK ID                       
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* HANDY ROUTINES AND EXIT POINTS                                      *         
***********************************************************************         
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
XMOD     L     RD,SAVERD                                                        
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
UPDID    DC    CL08'UPDID   '                                                   
START    DC    CL08'START   '                                                   
STOP     DC    CL08'STOP    '                                                   
READONLY DC    CL08'READONLY'                                                   
NORCV    DC    CL12'RECOVERY=NO'                                                
DOTS     DC    8CL1'.'                                                          
ZEROES   DC    8CL1'0'                                                          
SPACES   DC    CL80' '                                                          
*                                                                               
UPDNMSG  DC    C'*FACXXX* SSSSSSS XXXXXXX'                                      
LUPDNMSG EQU   *-UPDNMSG                                                        
*                                                                               
UPDNXTR  DC    C'                        '                                      
LUPDNXTR EQU   *-UPDNMSG                                                        
*                                                                               
         CNOP  0,8                                                              
MYUPDID  DS    0CL16                                                            
UPDMAJ   DC    C'DMGR    '                                                      
UPDMIN   DC    C'SNUM0000'                                                      
*                                                                               
ADVALL   DC    C'..'                                                            
ADVUNK   DC    C'?.'                                                            
ADVIDS   DS    0CL32                                                            
*&&UK*&& DC    C'????a1??a2??a3??a4??aA??????????'                              
*&&US*&& DC    C'????a1a5rAa2??a3a4rCa6??a7??rB??'                              
         EJECT                                                                  
***********************************************************************         
* MESSAGE TABLE                                                       *         
***********************************************************************         
MSGL     EQU   45                                                               
MSGLIST  DS    0CL(MSGL)                                                        
MSG01    DC    CL(MSGL)'System status displayed. Alter?'                        
MSG02    DC    CL(MSGL)'System status changed. Enter next request'              
*                                                                               
MSG03    DC    CL(MSGL)'Invalid input field'                                    
MSG04    DC    CL(MSGL)'Missing input field'                                    
MSG05    DC    CL(MSGL)'Invalid SENAME'                                         
MSG06    DC    CL(MSGL)'System is already started'                              
MSG07    DC    CL(MSGL)'System is stopped'                                      
MSG08    DC    CL(MSGL)'System is in process - Try again'                       
MSG09    DC    CL(MSGL)'Invalid CPU Id'                                         
MSG10    DC    CL(MSGL)'Missing CPU Id'                                         
MSG11    DC    CL(MSGL)'System is active in another address space'              
MSG12    DC    CL(MSGL)'Cannot alter Service/Control systems'                   
MSG13    DC    CL(MSGL)'System is already read only'                            
MSG14    DC    CL(MSGL)'Read only files exist within system'                    
MSG15    DC    CL(MSGL)'System is password protected'                           
MSG16    DC    CL(MSGL)'Status chrs must be A,+/U,-/D,*/N or R'                 
MSG17    DC    CL(MSGL)'Status chrs can''t be in more than 1 field'             
MSG18    DC    CL(MSGL)'Invalid format - Need Userid,(Password)'                
MSG19    DC    CL(MSGL)'Userid can only be 8 characters maximum'                
MSG20    DC    CL(MSGL)'Password can only be 8 characters maximum'              
MSG21    DC    CL(MSGL)'Can not stop DEMO system'                               
MSG22    DC    CL(MSGL)'This action is only valid for test systems'             
         EJECT                                                                  
***********************************************************************         
* SCREEN LINE DSECT                                                   *         
***********************************************************************         
LINED    DSECT                                                                  
LINSTATH DS    CL8                                                              
LINSTAT  DS    CL1                                                              
LINSTATX DS    CL8                                                              
LINSYSH  DS    CL8                                                              
LINSYS   DS    CL2                                                              
LINFLAG  DS    CL1                                                              
LINNAM   DS    CL7                                                              
LINNEXT  EQU   *                                                                
                                                                                
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WORKD    DSECT                     DSECT TO COVER $SYS W/S                      
DUB      DS    D                                                                
SAVER1   DS    A                                                                
SAVERD   DS    A                                                                
*                                                                               
IPARMS   DS    0XL32                                                            
ASYSFACS DS    A                   A(SYSTEM FACILITIES TABLE)                   
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL ENTRY)                                 
ACOMFACS DS    A                   A(COMMON FACILITIES TABLE)                   
ASELIST  DS    A                   A(SELIST ENTRY)                              
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR INPUT OUTPUT BLOCK)             
*                                                                               
FULL     DS    F                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
RELO     DS    A                                                                
FADRH    DS    A                                                                
*                                                                               
AHEXOUT  DS    A                                                                
ASCANNER DS    A                                                                
ASQUASH  DS    A                                                                
AMQIO    DS    A                                                                
ASENTRY  DS    A                                                                
AREQUEST DS    A                                                                
ASYSSTAB DS    A                                                                
AGETFACT DS    A                                                                
*                                                                               
ASYSMSGH DS    A                                                                
SAVERE   DS    A                                                                
TIME     DS    XL4                                                              
LUID     DS    CL8                                                              
PASSWORD DS    CL8                                                              
USERID   DS    CL8                                                              
REASON   DS    CL16                                                             
FORMAT   DS    X                                                                
FORMAT1  DS    X                                                                
MSGNUM   DS    C                                                                
FIRSTFIL DS    X                                                                
SENUMB   DS    C                                                                
STATFILT DS    X                                                                
SLOT     DS    X                                                                
SYSID    DS    X                                                                
SYSCH    DS    C                                                                
         DS    XL3                                                              
SYSNAM1  DS    CL1                                                              
SYSNAM3  DS    CL3                                                              
SYSNAM4  DS    CL4                                                              
SAVETCB  DS    XL10                                                             
MSG      DS    CL60                                                             
WORK     DS    CL128                                                            
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
STATLST  DS    200XL4                                                           
STATLSTL EQU   *-STATLST                                                        
*                                                                               
BIGWRK   DS    600D                                                             
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* DDSCANBLKD                                                                    
       ++INCLUDE DDSCANBLKD                                                     
* DMSYSFD                                                                       
       ++INCLUDE DMSYSFD                                                        
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
* DMDTFPH                                                                       
       ++INCLUDE DMDTFPH                                                        
* DDFH                                                                          
       ++INCLUDE DDFH                                                           
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
SRSYSFFD DSECT                                                                  
         DS    CL64                                                             
* SRSYSFFD                                                                      
       ++INCLUDE SRSYSFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRSYS00   04/07/15'                                      
         END                                                                    
