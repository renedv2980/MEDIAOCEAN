*          DATA SET NENAV08    AT LEVEL 060 AS OF 03/05/18                      
*PHASE T31808B                                                                  
*&&ONLIN SET   Y                    ONLINE ONLY PROGRAM                         
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31808   TITLE 'F NAV08 - STEWARD - SPLIT UNIT OVERLAY'                         
T31808   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV08**,RA                                                    
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     R7,ATWA                                                          
         USING TWAD,R7                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSAREC,ANETBLK                                                   
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
*  GET SECURITY AGENCY AND PASSWORD                                             
*                                                                               
         L     R4,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,R4)                                        
         GOTO1 (RF),DMCB,0                                                      
*                                                                               
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         MVC   SECAGY,FATAGYSC     SAVE SECURITY AGENCY                         
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVPASSWD,FAPASSWD   SAVE PERSONAL ID                             
         DROP  RE                                                               
*                                                                               
         MVI   TSACTN,TSAGET        GET FIRST RECORD FROM TSAR                  
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                 MUST BE ONE RECORD                          
         BAS   RE,CHKSPTAB          CHECK THE SPLIT TABLE                       
         B     CHAPR050                                                         
*                                                                               
*  PROCESS RETURNED BUY INFORMATION                                             
*  SEND NEXT TSAR RECORD TO THE BUY                                             
*                                                                               
CHAPR020 MVI   TSACTN,TSANXT        READ NEXT TSAR RECORD                       
         BAS   RE,CALLTSAR                                                      
         BNE   EXIT                                                             
         BAS   RE,CHKSPTAB           CHECK THE SPLIT TABLE                      
*                                                                               
*  GET BUY VALIDATE SPLIT WRITE BUYS BACK                                       
*                                                                               
CHAPR050 L     R4,AIO1              POINTS TO BUY RECORD                        
         ZIC   RE,SEQNUM                                                        
         LA    RE,1(RE)                                                         
         STCM  RE,1,SEQNUM                                                      
         BAS   RE,GETPAK                                                        
         OC    ERROR,ERROR                                                      
         BNZ   CHAPR100                                                         
         BAS   RE,GETBUY                                                        
         OC    ERROR,ERROR                                                      
         BNZ   CHAPR100                                                         
*        BAS   RE,READPROF                                                      
         BAS   RE,GETUSER                                                       
         CLI   SPLITCNT,0                                                       
         BE    *+12                                                             
         BAS   RE,SPLTMULT          NEW VERSION OF SPLIT                        
         B     CHAPR020                                                         
*                                                                               
         BAS   RE,SPLTUNIT          SPLIT THE UNIT                              
CHAPR100 BAS   RE,SNDSPLT                                                       
         B     CHAPR020                                                         
         SPACE 3                                                                
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  READ THE PKG RECORD MOVE INTO AIO1                                           
*                                                                               
GETPAK   NTR1                                                                   
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
         CLC   RUPCLI(13),PAKCLI                                                
         BE    GETPAKEX                                                         
         L     R4,AIO1                                                          
         USING NPRECD,R4                                                        
         XC    ERROR,ERROR                                                      
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,BAGYMD                                                     
         GOTO1 VCLPACK,DMCB,RUPCLI,NPKCLT                                       
         MVC   NPKNET,RUPNET                                                    
         LA    R6,RUPEST                                                        
         MVI   BYTE,3                                                           
         BRAS  RE,GETBINRY                                                      
         MVC   NPKEST,BYTE                                                      
         LA    R6,RUPPACK                                                       
         MVI   BYTE,3                                                           
         BRAS  RE,GETBINRY                                                      
         MVC   NPKPACK,BYTE                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(19),KEYSAVE                                                  
         BE    GETPAK50                                                         
         DC    H'0'                                                             
*                                                                               
GETPAK50 GOTO1 AIOCALL,DMCB,UNT+FIL+GET,AIO                                     
         L     R4,AIO                                                           
         MVC   PAKDPT,NPAKDP                                                    
         MVC   PAKCNTL,NPAKCNTL                                                 
         MVC   PAKSTAT,NPAKSTAT                                                 
GETPAKEX B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
*  READ THE BUY RECORD MOVE INTO AIO1                                           
*                                                                               
GETBUY   NTR1                                                                   
         L     R4,AIO1                                                          
         USING NURECD,R4                                                        
         XC    ERROR,ERROR                                                      
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,BAGYMD                                                    
         GOTO1 VCLPACK,DMCB,RUPCLI,BINCLT                                       
         MVC   NUKPCLT,BINCLT                                                   
         MVC   NUKPNET,RUPNET                                                   
         MVC   NUKPPROG,RUPPCODE                                                
         GOTO1 VDATCON,DMCB,(4,RUPBDATE),(2,NUKPDATE)                           
         LA    R6,RUPEST                                                        
         MVI   BYTE,3                                                           
         BRAS  RE,GETBINRY                                                      
         MVC   NUKPEST,BYTE                                                     
         MVI   BYTE,1               DEFAULT LINE NUMBER                         
         OC    RUPSLINE,RUPSLINE                                                
         BZ    GETBUY30                                                         
         LA    R6,RUPSLINE                                                      
         MVI   BYTE,3                                                           
         BRAS  RE,GETBINRY                                                      
GETBUY30 MVC   NUKPSUB,BYTE                                                     
         MVC   NUKPDP,PAKDPT                                                    
*                                                                               
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(19),KEYSAVE                                                  
         BE    GETBUY50                                                         
         MVI   ERROR,NOTFOUND                                                   
         B     SPLERR                                                           
*                                                                               
GETBUY50 GOTO1 AIOCALL,DMCB,UNT+FIL+GET,AIO                                     
         L     R4,AIO                                                           
*                                                                               
*  CHECK THE STATUS TO SEE IF CHANGES ARE ALLOWED TO THE UNIT                   
*                                                                               
         CLI   RUPREAS,X'40'        WAS REASON CODE INPUTTED                    
         BH    GETBUY70                                                         
         TM    NUPACKST,X'02'       CHECK IF REASON CODE REQUIRED               
         BZ    GETBUY70                                                         
         MVI   ERROR,AUDITERR                                                   
         B     SPLERR                                                           
*                                                                               
GETBUY70 TM    PAKCNTL,X'08'        CHECK IF CABLE LOCKED                       
         BZ    *+12                                                             
         MVI   ERROR,UCBLKERR                                                   
         B     SPLERR                                                           
*                                                                               
         TM    PAKSTAT,X'20'        CHECK IF PACKAGE LOCKED                     
         BZ    *+12                                                             
         MVI   ERROR,PAKLERR                                                    
         B     SPLERR                                                           
*                                                                               
*        TM    CLIOPTN2,X'08'       CHECK IF CLIENT FROZEN                      
*        BZ    *+12                                                             
*        MVI   ERROR,CLIFRERR                                                   
*        B     SPLERR                                                           
*                                                                               
         BAS   RE,GETCORPR          GET THE CORPORATE PRODUCT                   
*                                                                               
         MVC   STARTACT,NUACTUAL                                                
         MVC   STARTASS,NUASSIGN                                                
*                                                                               
GETBUYEX B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
SPLERR   LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
         MVI   BUYERRSW,C'Y'                                                    
         MVC   ERNUMBR,ERROR                                                    
         XC    ERMXFLD(60),ERMXFLD                                              
         MVC   ERMXFLD(7),=CL7'PRODUCT'                                         
         B     EXIT                                                             
         SPACE                                                                  
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*  ROUTINE CHEVCKS TO SEE IF A SPLIT REQUEST WAS INPUTTED                       
*  IF NO REQUEST WAS INPUTTED PUT DEFAULT VALUES IN TABLE                       
*                                                                               
CHKSPTAB NTR1                                                                   
         L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         CLI   RUPSPLTB,0           IS NEW SPLIT ACTIVE                         
         BE    CHKSPTEX                                                         
*                                                                               
         XC    SPLITCNT(SPLTACCM),SPLITCNT                                      
*                                                                               
         MVI   LOOPSW,C'F'          SET SWITCH TO FIRST PASS                    
*                                                                               
CHKSPT75 LA    RF,RUPSPLTB                                                      
         SR    RE,RE                                                            
CHKSPT80 CLI   0(RF),X'FF'                                                      
         BE    CHKSPT85                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     CHKSPT80                                                         
CHKSPT85 STCM  RE,1,SPLITCNT                                                    
*                                                                               
CHKSPTEX B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
*                                                                               
* SPLIT THE UNIT                                                                
* THIS IS THE UPGRADED SPLIT UNIT WHICH                                         
* WILL SPLIT THE UNIT MULTIPLE WAYS                                             
* BASED ON THE LENGTH TABLE (RUPSPLTAB)                                         
*                                                                               
SPLTMULT NTR1                                                                   
*                                                                               
         XC    ERROR,ERROR                                                      
         L     R2,AIO1                                                          
         USING NURECD,R2                                                        
*                                                                               
         MVC   SAVSTAT,NURSTAT                                                  
* CHECK INPUT LENGTH TO UNIT LENGTH                                             
*                                                                               
         L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         LA    R6,RUPSPLTB                                                      
         MVC   STARTLEN,RUPNLEN                                                 
         CLC   NULEN,RUPNLEN                                                    
         BNE   LENERR                                                           
         DROP  RE                                                               
*                                                                               
* - SPLIT UNIT LENGTH AND WRITE UNIT IN NBAIO BACK WITH NEW LENGTH              
         MVI   COPYSPSW,C'N'        SET COPYSWITCH TO NO                        
*                                                                               
* - GET NEXT VALID LINE USING X'84'KEY                                          
MSPLT15  LA    R4,KEY                                                           
         XC    KEY(20),KEY                                                      
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(3),NUKAM                                                   
         MVC   KEY+4(4),NUKNET     NETWORK                                      
         MVC   KEY+8(6),NUKPROG    PROGRAM                                      
         MVC   KEY+14(2),NUKDATE   DATE                                         
         MVC   KEY+16(1),NUKEST    ESTIMATE                                     
         MVC   KEY+17(1),NUKSUB    SUBLINE                                      
         MVC   KEY+18(1),NUKDP     DAYPART                                      
                                                                                
         GOTO1 AIOCALL,DMCB,PASSDEL+UNT+DIR+HIGH                                
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
MSPLT20  CLC   KEY(17),KEYSAVE                                                  
         BNE   MSPLT22                                                          
         MVC   FULL(1),KEY+17      SAVE SUB-LINE                                
MSPLTSEQ GOTO1 AIOCALL,DMCB,PASSDEL+UNT+DIR+SEQ                                 
         B     MSPLT20                                                          
MSPLT22  ZIC   RE,FULL                                                          
         ZIC   RF,SPLITCNT                                                      
         AR    RE,RF                                                            
         C     RE,=F'192'         HAVE WE REACHED THE LINE NUMB LIMIT?          
         BH    LINERR              YES/DON'T SPLIT THIS UNIT                    
*                                                                               
*  GETREC BEFORE PUTREC                                                         
MSPLT23  MVC   KEY(20),NUKEY                                                    
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,UNT+FIL+GET+UPDATE,AIO1                             
*                                                                               
*  INITIALIZE LINE NUMBER                                                       
         CLI   LOOPSW,C'F'                                                      
         BNE   *+10                                                             
         MVC   NEWLINE,NUKSUB      SAVE LINE NUMBER                             
* SET NEW VALUES IN  CURRENT UNIT                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RF,12(R1)                                                        
         USING NUSDREL,RF                                                       
         MVC   SVPOSTYP,NUPOSTYP                                                
         MVC   SVSTATYP,NUSTATYP                                                
         OI    NUSDST4,X'40'       SET UNIT SPLIT BY UTILS/STEWARD              
*******  NI    NUSDST3,X'7F'       RESET FROZEN ASSIGNED COST BIT               
*                                                                               
         TM    NUSDST3,X'40'       TEST FOR COPYSPLIT                           
         BZ    *+8                                                              
         MVI   COPYSPSW,C'Y'       SET COPYSPLIT SWITCH                         
         DROP  RF                                                               
*                                                                               
MSPLT24  L     R2,AIO1                                                          
*                                                                               
         MVC   NULEN,0(R6)         MOVE OUT CURRENT LENGTH                      
         CLI   NULEN,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   LOOPSW,C'L'                                                      
         BNE   MSPLT26                                                          
         ZIC   RE,STARTLEN          TOTAL UNIT LENGTH                           
         ZIC   RF,LENSUM            CURRENT RUNNIING LENGTH                     
         SR    RE,RF                                                            
         STCM  RE,1,NULEN           MOVE OUT REMAINING LENGTH                   
         CLI   NULEN,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
* CALCULATE THE COST                                                            
MSPLT26  SR    R0,R0                                                            
         ICM   R1,15,STARTACT                                                   
         ZIC   RE,0(R6)             CURRENT LENGTH                              
         MR    R0,RE                                                            
         ZIC   RE,STARTLEN          TOTAL LENGTH                                
         DR    R0,RE                                                            
         SR    R0,R0                                                            
         A     R1,=F'50'                                                        
         D     R0,=F'100'                                                       
         SR    R0,R0                                                            
         M     R0,=F'100'                                                       
         STCM  R1,15,NUACTUAL      REPLACE IT                                   
         STCM  R1,15,OUACTUAL      REPLACE IT                                   
*                                                                               
         CLI   LOOPSW,C'L'                                                      
         BNE   MSPLT28                                                          
         ICM   RE,15,STARTACT          ORIGINAL UNIT ACTUAL COST                
         ICM   RF,15,ACTSUM            CURRENT RUNNIING ACTUAL COST             
         SR    RE,RF                                                            
         STCM  RE,15,NUACTUAL       MOVE OUT REMAINING LENGTH                   
         STCM  RE,15,OUACTUAL       MOVE OUT REMAINING LENGTH                   
                                                                                
*                                                                               
MSPLT28  SR    R0,R0                                                            
         ICM   R1,15,STARTASS                                                   
         ZIC   RE,0(R6)             CURRENT LENGTH                              
         MR    R0,RE                                                            
         ZIC   RE,STARTLEN          TOTAL LENGTH                                
         DR    R0,RE                                                            
         SR    R0,R0                                                            
         A     R1,=F'50'                                                        
         D     R0,=F'100'                                                       
         SR    R0,R0                                                            
         M     R0,=F'100'                                                       
         STCM  R1,15,NUASSIGN      REPLACE IT                                   
         STCM  R1,15,OUASSIGN      REPLACE IT                                   
*                                                                               
         CLI   LOOPSW,C'L'                                                      
         BNE   MSPLT32                                                          
         ICM   RE,15,STARTASS          ORIGINAL UNIT ASSIGNED COST              
         ICM   RF,15,ASSSUM            CURRENT RUNNIING ASSIGNED COST           
         SR    RE,RF                                                            
         STCM  RE,15,NUASSIGN       MOVE OUT REMAINING LENGTH                   
         STCM  RE,15,OUASSIGN       MOVE OUT REMAINING LENGTH                   
*                                                                               
MSPLT32  GOTO1 VCALCSHP,DMCB,AIO1,ACOMFACS,CLICPRPD                             
*                                                                               
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
         CLC   RUPTYPE,=CL2'CH'     IS THIS A DRAFT CHECK                       
         BNE   MSPLT260             YES DONT UPDATE UNITS                       
*                                                                               
* HANDLE ACTIVITY ELEMENT                                                       
* 99 ELEMENT                                                                    
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'99',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   MSPLT40                                                          
         L     R3,12(R1)                                                        
         USING NUACTEL,R3                                                       
         GOTO1 VDATCON,DMCB,(5,0),(3,THREE)                                     
         MVC   NUACTCDT,THREE      LAST ACTIVITY DATE                           
         CLI   NUACTLEN,22                                                      
         BL    MSPLT40                                                          
         MVC   NUACTCID,SVPASSWD   LAST PERSONAL ID                             
         MVC   NUACTAGD,SECAGY     SECURITY AGENCY                              
         MVC   NUACTRSN(3),RUPREAS    REASON CODE                               
         MVI   NUACTRSN+3,X'40'    BLANK FILL                                   
         DROP  R3                                                               
*                                                                               
* HANDLE TRAFFIC ELEMENTS                                                       
* 21 ELEMENT                                                                    
*                                                                               
*  THIS IS ONLY DONE FOR FIRST UNIT                                             
*                                                                               
MSPLT40  CLI   LOOPSW,C'F'                                                      
         BNE   MSPLT70                                                          
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   MSPLT50                                                          
         L     R3,12(R1)                                                        
         USING NUCMLEL,R3                                                       
         OI    NUCMLFLG,X'80'       SET LENGTH CHANGE INDICATOR                 
         DROP  R3                                                               
* 23 ELEMENT                                                                    
MSPLT50  GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'23',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   MSPLT70                                                          
         L     R3,12(R1)                                                        
         USING NUFDCEL,R3                                                       
MSPLT60  OI    NUFDCFLG,X'80'       SET LENGTH CHANGE INDICATOR                 
         ZIC   RE,NUFDCLEN                                                      
         AR    R3,RE                                                            
         CLI   0(R3),X'23'                                                      
         BE    MSPLT60                                                          
         DROP  R3                                                               
* 04 COMMENT ELEMENT (LINE 1)                                                   
MSPLT70  XC    HALF,HALF                                                        
         MVI   HALF,C'C'                                                        
         MVI   HALF+1,1                                                         
         OC    RUPCOMT1,RUPCOMT1                                                
         BZ    MSPLT75                                                          
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'04',HALF),0                        
         CLI   RUPCOMT1,X'FF'                                                   
         BE    MSPLT75                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING NUCOMD,R3                                                        
         MVI   NUCOMD,X'04'                                                     
         MVC   NUCOMTYP(2),HALF                                                 
         MVC   FLD(60),RUPCOMT1                                                 
         BAS   RE,GETCOMM                                                       
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'04',(R2)),ELEM,0                   
* 04 COMMENT ELEMENT (LINE 2)                                                   
MSPLT75  XC    HALF,HALF                                                        
         MVI   HALF,C'C'                                                        
         MVI   HALF+1,2                                                         
         OC    RUPCOMT2,RUPCOMT2                                                
         BZ    MSPLT90                                                          
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'04',HALF),0                        
         CLI   RUPCOMT2,X'FF'                                                   
         BE    MSPLT90                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING NUCOMD,R3                                                        
         MVI   NUCOMD,X'04'                                                     
         MVC   NUCOMTYP(2),HALF                                                 
         MVC   FLD(60),RUPCOMT2                                                 
         BAS   RE,GETCOMM                                                       
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'04',(R2)),ELEM,0                   
         DROP  R3                                                               
*                                                                               
MSPLT90  CLI   LOOPSW,C'F'                                                      
         BNE   MSPLT92                                                          
         GOTO1 AIOCALL,DMCB,UNT+FIL+PUT,AIO1                                    
         B     MSPLT260                                                         
*                                                                               
****     BAS   RE,SETBLOCK                                                      
***      L     R3,AIO2                                                          
****     LA    R3,1500(R3)          ADDRESS OF DEMO BLOCK 1ST UNIT              
*******  BAS   RE,GETDEMOS          GET DEMOS                                   
*                                                                               
*  RESET THE POINTERS AFTER THE GETDEMO CALL                                    
*                                                                               
****     MVC   KEY(20),NUKEY                                                    
******   GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
******   CLC   KEY(20),KEYSAVE                                                  
*****8   BE    *+6                                                              
****     DC    H'0'                                                             
*******  GOTO1 AIOCALL,DMCB,UNT+FIL+GET,AIO1                                    
*                                                                               
* - CREATE SPLIT UNIT NEW LINE NUMBER                                           
*                                                                               
MSPLT92  ZIC   R1,FULL                                                          
         LA    R1,1(R1)                                                         
         STC   R1,NUKSUB           *** NEW LINE NUMBER                          
         MVC   NEWLINE,NUKSUB      SAVE NEW LINE NUMBER                         
*                                                                               
         L     R2,AIO1                                                          
* DELETE ALL BILLING ELEMENTS FROM NEW RECORD                                   
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'10',(R2)),0                        
*                                                                               
* HANDLE TRAFFIC ELEMENTS                                                       
* 21 ELEMENT                                                                    
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   MSPLT140                                                         
         L     R3,12(R1)                                                        
         USING NUCMLEID,R3                                                      
         MVC   DUB(1),NUCMLPRD                                                  
         MVC   DUB+1(3),NUCMPROD                                                
         ZIC   R1,NUCMLELN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    NUCML1(0),NUCML1     KEEP FEED, CLEAR REST OF ELEMENT            
         CLI   COPYSPSW,C'Y'                                                    
         BNE   *+16                                                             
         MVC   NUCMLPRD,DUB                                                     
         MVC   NUCMPROD,DUB+1                                                   
         DROP  R3                                                               
* 23 ELEMENT                                                                    
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'23',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   MSPLT140                                                         
         L     R3,12(R1)                                                        
         USING NUFDCEL,R3                                                       
MSPLT120 MVC   DUB(1),NUFDCPRD                                                  
         MVC   DUB+1(3),NUFDPROD                                                
         ZIC   R1,NUFDCLEN          SET LENGTH CHANGE INDICATOR                 
         SH    R1,=H'7'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    NUFDCML1(0),NUFDCML1   KEEP FEED, CLEAR REST OF ELEMENT          
         CLI   COPYSPSW,C'Y'                                                    
         BNE   *+16                                                             
         MVC   NUFDCPRD,DUB                                                     
         MVC   NUFDPROD,DUB+1                                                   
         ZIC   RE,NUFDCLEN          GET NEXT ELEMENT                            
         AR    R3,RE                                                            
         CLI   0(R3),X'23'                                                      
         BE    MSPLT120                                                         
         DROP  R3                                                               
*                                                                               
* SET CREATION DATE                                                             
MSPLT140 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'99',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   MSPLT200                                                         
         L     R3,12(R1)                                                        
         USING NUACTEL,R3                                                       
         GOTO1 VDATCON,DMCB,(5,0),(3,THREE)                                     
         MVC   NUACTADT,THREE      ADD DATE                                     
         MVC   NUACTCDT,THREE      LAST ACTIVITY DATE                           
         CLI   NUACTLEN,22                                                      
         BL    MSPLT200                                                         
         MVC   NUACTAID,SVPASSWD   ADD PERSONAL ID                              
         MVC   NUACTCID,SVPASSWD   LAST PERSONAL ID                             
         MVC   NUACTAGD,SECAGY     SECURITY AGENCY                              
         MVC   NUACTRSN(3),RUPREAS    REASON CODE                               
         MVI   NUACTRSN+3,X'40'    BLANK FILL                                   
         DROP  R3                                                               
*                                                                               
MSPLT200 GOTO1 AIOCALL,DMCB,UNT+FIL+ADDREC,AIO1                                 
         MVC   DSKADDR,NDXDA                                                    
*                                                                               
* CREATE PASSIVE POINTERS WITH NEW SUB-LINE NUMBER                              
         LA    R4,KEY             CREATE X'84' PASSIVE                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(3),NUKAM                                                   
         MVC   KEY+4(4),NUKNET                                                  
         MVC   KEY+8(6),NUKPROG                                                 
         MVC   KEY+14(2),NUKDATE                                                
         MVC   KEY+16(1),NUKEST                                                 
         MVC   KEY+17(1),NEWLINE      SAVED NEW LINE NUMBER                     
         MVC   KEY+18(1),NUKDP                                                  
         MVC   KEY+20(1),SAVSTAT       SAVE POSTING TYPE                        
         MVC   KEY+21(4),DSKADDR                                                
         GOTO1 AIOCALL,DMCB,UNT+DIR+ADD                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'94'                                                        
         MVC   KEY+1(3),NUKAM                                                   
         MVC   KEY+4(1),NUKEST                                                  
         MVC   KEY+5(4),NUKNET                                                  
         BAS   RE,DAYLOOK           PUTS ONE BYTE DAY CODE IN NUKDDAY           
         MVC   KEY+10(1),NUKTIME                                                
         MVC   KEY+11(6),NUKPROG                                                
         MVC   KEY+17(2),NUKDATE                                                
         MVC   KEY+19(1),NEWLINE      NEW SAVED LINE NUMBER                     
         MVC   KEY+20(1),SAVSTAT      SAVE POSTING TYPE                         
         MVC   KEY+21(4),DSKADDR                                                
         GOTO1 AIOCALL,DMCB,UNT+DIR+ADD                                         
*                                                                               
*  IF UNIT IS MAKEGOOD SEED THE INFO ON ALL THE MISSED UNITS                    
*                                                                               
*  BUILD 07 ELEMENT                                                             
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING NUMGD,R3                                                         
         MVC   NUMGEL(2),=XL2'0720'                                             
         MVC   NUMGPCOD,NUKPROG                                                 
         MVC   NUMGPNM,NUPROGNM                                                 
         MVC   NUMGDATE,NUKDATE                                                 
         MVC   NUMGSUB,NUKSUB                                                   
         MVC   NUMGSTAT,NUUNITST                                                
*                                                                               
* GET MISSED INFORMATION                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'06',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   MSPLT260                                                         
         L     R3,12(R1)                                                        
* GET MISSED UNIT/MOVE NEW MISSED INFO OUT                                      
MSPLT240 XC    KEY,KEY                                                          
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(3),NUKAM                                                   
         MVC   KEY+4(4),NUKNET                                                  
         MVC   KEY+16(1),NUKEST                                                 
         MVC   KEY+18(1),NUKDP                                                  
         MVC   KEY+8(6),NUMGPCOD                                                
         MVC   KEY+14(2),NUMGDATE                                               
         MVC   KEY+17(1),NUMGSUB                                                
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,UNT+FIL+GET+UPDATE,AIO2                             
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'07',AIO2),ELEM,0                   
         GOTO1 AIOCALL,DMCB,UNT+FIL+PUT,AIO2                                    
* GET NEXT MISSED ELEMENT                                                       
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'06'                                                      
         BE    MSPLT240                                                         
*                                                                               
MSPLT260 L     R3,AIO2                                                          
         USING NDDEMBLK,R3                                                      
         BAS   RE,SETBLOCK                                                      
*                                                                               
* CHECK FOR SIDE BY SIDE DEMO REQUEST IF                                        
* ON SET UP NETBLOCK TO GET 2ND DEMO SET                                        
*                                                                               
         L     RE,AIO2                                                          
         USING NETBLOCK,RE                                                      
         MVI   DEMO2COL,C'N'                                                    
         TM    RUPSTAT2,X'07'       CHECK 2 SETS OF DEMOS                       
         BZ    MSPLT450                                                         
         MVI   DEMO2COL,C'Y'                                                    
*                                                                               
         TM    RUPSTAT2,X'01'       CHECK ACTUALS                               
         BZ    *+20                                                             
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,C'N'                                                    
         MVI   NBDEMRAW,C'N'                                                    
         B     MSPLT280                                                         
*                                                                               
         TM    RUPSTAT2,X'02'       ESTIMATED RAW                               
         BZ    *+20                                                             
         MVI   NBACTOPT,C'N'                                                    
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBDEMRAW,C'Y'                                                    
         B     MSPLT280                                                         
*                                                                               
         TM    RUPSTAT2,X'04'       ESTIMATED GUARANTEES                        
         BZ    *+16                                                             
         MVI   NBACTOPT,C'N'                                                    
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBDEMRAW,C'N'                                                    
         DROP  RE                                                               
*                                                                               
MSPLT280 L     R3,AIO2                                                          
         LA    R3,1500(R3)                                                      
         BAS   RE,GETDEMOS          GET DEMOS                                   
*                                                                               
*  FOLLOWING CODE IS TO STORE FIRST COLUMN DEMOS                                
*  JUST IN CASE SIDE BY SIDE DEMOS ARE REQUSTED                                 
*                                                                               
         LA    RE,2000(R3)          STORE IN AIO2+3500                          
         MVC   0(200,RE),NDACTDEM                                               
         TM    RUPSTAT,X'10'        CHECK FOR ACTUAL DEMO REQUEST               
         BO    MSPLT450                                                         
         MVC   0(200,RE),NDESTDEM                                               
         DROP  R3                                                               
*                                                                               
MSPLT450 L     R3,AIO2                                                          
         LA    R3,2500(R3)                                                      
         BAS   RE,SETBLOCK                                                      
         L     R3,AIO2                                                          
         LA    R3,1500(R3)                                                      
         BAS   RE,GETDEMOS          GET DEMOS                                   
*                                                                               
*                                                                               
* ADD TO ACCUMULATORS AND SET THE SWITCHES                                      
*                                                                               
MSPLT600 LA    R6,1(R6)             BUMP TO NEXT SPLIT                          
         CLI   0(R6),X'FF'                                                      
         BE    MSPLTEX                                                          
*                                                                               
         MVI   LOOPSW,0                                                         
         CLI   1(R6),X'FF'                                                      
         BNE   *+8                                                              
         MVI   LOOPSW,C'L'        SET LOOPSW TO LAST LENGTH TO PROCESS          
*                                                                               
         ICM   RE,15,NUACTUAL                                                   
         ICM   RF,15,ACTSUM                                                     
         AR    RE,RF                                                            
         STCM  RE,15,ACTSUM                                                     
*                                                                               
         ICM   RE,15,NUASSIGN                                                   
         ICM   RF,15,ASSSUM                                                     
         AR    RE,RF                                                            
         STCM  RE,15,ASSSUM                                                     
*                                                                               
         ZIC   RE,NULEN                                                         
         ZIC   RF,LENSUM                                                        
         AR    RE,RF                                                            
         STCM  RE,1,LENSUM                                                      
*                                                                               
         BAS   RE,SNDSPLT                                                       
*                                                                               
         B     MSPLT15                                                          
*                                                                               
MSPLTEX  BAS   RE,SNDSPLT           SEND LAST LENGTH                            
         B     EXIT                                                             
         DROP  R5                                                               
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
* ORIGINAL SPLIT THE UNIT                                                       
* THIS IS THE ORIGINAL SPLIT UNIT WHICJ IS CAPABLE OF SPLITTING                 
* A SINGLE UNIT INTO TWO.                                                       
*                                                                               
SPLTUNIT NTR1                                                                   
         XC    ERROR,ERROR                                                      
         L     R2,AIO1                                                          
         USING NURECD,R2                                                        
*                                                                               
         MVC   SAVSTAT,NURSTAT                                                  
* CHECK INPUT LENGTH TO UNIT LENGTH                                             
*                                                                               
         L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         CLC   NULEN,RUPNLEN                                                    
         BNE   LENERR                                                           
         DROP  RE                                                               
*                                                                               
* - SPLIT UNIT LENGTH AND WRITE UNIT IN NBAIO BACK WITH NEW LENGTH              
         MVI   COPYSPSW,C'N'                                                    
         MVI   SPLT45SW,C'N'                                                    
         CLI   NULEN,45                                                         
         BNE   SPLT10                                                           
         MVI   SPLT45SW,C'Y'        SET 45 SECOND SPLIT SWITCH                  
         MVI   OULEN,15             FIRST UNIT 15 SEC                           
         MVI   NEWLEN,30            NEW UNIT 30 SEC                             
         MVC   DIVIDER,=F'300'      DIVIDE AMOUNTS BY 3                         
         B     SPLT15                                                           
*                                                                               
SPLT10   SR    R0,R0               SPLIT UNIT LENGTH                            
         ZIC   R1,NULEN                                                         
         LR    R3,R1                                                            
         D     R0,=F'2'                                                         
         STC   R1,OULEN            SAVE NEW LENGTH OF CURRENT UNIT              
         SR    R3,R1               GET LENGTH OF FUTURE CREATED UNIT            
         STC   R3,NEWLEN           AND SAVE LENGTH IN NEWLEN                    
         MVC   DIVIDER,=F'200'     DIVIDE AMOUNTS BY 3                          
                                                                                
* - GET NEXT VALID LINE USING X'84'KEY                                          
SPLT15   LA    R4,KEY                                                           
         XC    KEY(20),KEY                                                      
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(3),NUKAM                                                   
         MVC   KEY+4(4),NUKNET     NETWORK                                      
         MVC   KEY+8(6),NUKPROG    PROGRAM                                      
         MVC   KEY+14(2),NUKDATE   DATE                                         
         MVC   KEY+16(1),NUKEST    ESTIMATE                                     
         MVC   KEY+17(1),NUKSUB    SUBLINE                                      
         MVC   KEY+18(1),NUKDP     DAYPART                                      
                                                                                
         GOTO1 AIOCALL,DMCB,PASSDEL+UNT+DIR+HIGH                                
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
SPLT20   CLC   KEY(17),KEYSAVE                                                  
         BNE   SPLT22                                                           
         MVC   FULL(1),KEY+17      SAVE SUB-LINE                                
SPLTSEQ  GOTO1 AIOCALL,DMCB,PASSDEL+UNT+DIR+SEQ                                 
         B     SPLT20                                                           
SPLT22   CLI   FULL,191           HAVE WE REACHED THE LINE NUMB LIMIT?          
         BH    LINERR              YES/DON'T SPLIT THIS UNIT                    
*                                                                               
*  GETREC BEFORE PUTREC                                                         
SPLT23   MVC   KEY(20),NUKEY                                                    
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,UNT+FIL+GET+UPDATE,AIO1                             
                                                                                
* SET NEW VALUES IN  CURRENT UNIT                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RF,12(R1)                                                        
         USING NUSDREL,RF                                                       
         MVC   SVPOSTYP,NUPOSTYP                                                
         MVC   SVSTATYP,NUSTATYP                                                
         OI    NUSDST4,X'40'       SET UNIT SPLIT BY UTILS/STEWARD              
*******  NI    NUSDST3,X'7F'       RESET FROZEN ASSIGNED COST BIT               
*                                                                               
         TM    NUSDST3,X'40'       TEST FOR COPYSPLIT                           
         BZ    *+8                                                              
         MVI   COPYSPSW,C'Y'       SET COPYSPLIT SWITCH                         
         DROP  RF                                                               
*                                                                               
SPLT24   L     R2,AIO1                                                          
         MVC   NULEN,OULEN         SET NEW LENGTH                               
         CLI   NULEN,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
* HALVE COST                                                                    
         SR    R0,R0                                                            
         ICM   R1,15,NUASSIGN                                                   
         D     R0,DIVIDER                                                       
         SR    R0,R0                                                            
         M     R0,=F'100'                                                       
         ICM   R0,15,NUASSIGN      SAVE IT                                      
         STCM  R1,15,NUASSIGN      REPLACE IT                                   
         SR    R0,R1                                                            
         MVC   OUASSIGN,NUASSIGN                                                
         STCM  R0,15,NEWASSGN                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R1,15,NUACTUAL                                                   
         D     R0,DIVIDER                                                       
         SR    R0,R0                                                            
         M     R0,=F'100'                                                       
         ICM   R0,15,NUACTUAL      SAVE IT                                      
         STCM  R1,15,NUACTUAL      REPLACE WITH HALF                            
         SR    R0,R1                                                            
         MVC   OUACTUAL,NUACTUAL                                                
         STCM  R0,15,NEWACTUL                                                   
*                                                                               
         GOTO1 VCALCSHP,DMCB,AIO1,ACOMFACS,CLICPRPD                             
*                                                                               
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
         CLC   RUPTYPE,=CL2'CH'     IS THIS A DRAFT CHECK                       
         BNE   SPLT300              YES DONT UPDATE UNITS                       
*                                                                               
* HANDLE ACTIVITY ELEMENT                                                       
* 99 ELEMENT                                                                    
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'99',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SPLT40                                                           
         L     R3,12(R1)                                                        
         USING NUACTEL,R3                                                       
         GOTO1 VDATCON,DMCB,(5,0),(3,THREE)                                     
         MVC   NUACTCDT,THREE      LAST ACTIVITY DATE                           
         CLI   NUACTLEN,22                                                      
         BL    SPLT40                                                           
         MVC   NUACTCID,SVPASSWD   LAST PERSONAL ID                             
         MVC   NUACTAGD,SECAGY     SECURITY AGENCY                              
         MVC   NUACTRSN(3),RUPREAS    REASON CODE                               
         MVI   NUACTRSN+3,X'40'    BLANK FILL                                   
         DROP  R3                                                               
*                                                                               
* HANDLE TRAFFIC ELEMENTS                                                       
* 21 ELEMENT                                                                    
SPLT40   GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SPLT50                                                           
         L     R3,12(R1)                                                        
         USING NUCMLEL,R3                                                       
         OI    NUCMLFLG,X'80'       SET LENGTH CHANGE INDICATOR                 
         DROP  R3                                                               
* 23 ELEMENT                                                                    
SPLT50   GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'23',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SPLT70                                                           
         L     R3,12(R1)                                                        
         USING NUFDCEL,R3                                                       
SPLT60   OI    NUFDCFLG,X'80'       SET LENGTH CHANGE INDICATOR                 
         ZIC   RE,NUFDCLEN                                                      
         AR    R3,RE                                                            
         CLI   0(R3),X'23'                                                      
         BE    SPLT60                                                           
         DROP  R3                                                               
* 04 COMMENT ELEMENT (LINE 1)                                                   
SPLT70   XC    HALF,HALF                                                        
         MVI   HALF,C'C'                                                        
         MVI   HALF+1,1                                                         
         OC    RUPCOMT1,RUPCOMT1                                                
         BZ    SPLT75                                                           
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'04',HALF),0                        
         CLI   RUPCOMT1,X'FF'                                                   
         BE    SPLT75                                                           
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING NUCOMD,R3                                                        
         MVI   NUCOMD,X'04'                                                     
         MVC   NUCOMTYP(2),HALF                                                 
         MVC   FLD(60),RUPCOMT1                                                 
         BAS   RE,GETCOMM                                                       
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'04',(R2)),ELEM,0                   
* 04 COMMENT ELEMENT (LINE 2)                                                   
SPLT75   XC    HALF,HALF                                                        
         MVI   HALF,C'C'                                                        
         MVI   HALF+1,2                                                         
         OC    RUPCOMT2,RUPCOMT2                                                
         BZ    SPLT90                                                           
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'04',HALF),0                        
         CLI   RUPCOMT2,X'FF'                                                   
         BE    SPLT90                                                           
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING NUCOMD,R3                                                        
         MVI   NUCOMD,X'04'                                                     
         MVC   NUCOMTYP(2),HALF                                                 
         MVC   FLD(60),RUPCOMT2                                                 
         BAS   RE,GETCOMM                                                       
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'04',(R2)),ELEM,0                   
         DROP  R3                                                               
*                                                                               
SPLT90   GOTO1 AIOCALL,DMCB,UNT+FIL+PUT,AIO1                                    
****     BAS   RE,SETBLOCK                                                      
***      L     R3,AIO2                                                          
****     LA    R3,1500(R3)          ADDRESS OF DEMO BLOCK 1ST UNIT              
*******  BAS   RE,GETDEMOS          GET DEMOS                                   
*                                                                               
*  RESET THE POINTERS AFTER THE GETDEMO CALL                                    
*                                                                               
****     MVC   KEY(20),NUKEY                                                    
******   GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
******   CLC   KEY(20),KEYSAVE                                                  
*****8   BE    *+6                                                              
****     DC    H'0'                                                             
*******  GOTO1 AIOCALL,DMCB,UNT+FIL+GET,AIO1                                    
*                                                                               
* - CREATE PARTNER UNIT WITH NEW LENGTH AND LINE NUMBER                         
*                                                                               
         MVC   NULEN,NEWLEN        *** NEW LENGTH                               
         CLI   NULEN,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,FULL                                                          
         LA    R1,1(R1)                                                         
         STC   R1,NUKSUB           *** NEW LINE NUMBER                          
         MVC   NEWLINE,NUKSUB      SAVE NEW LINE NUMBER                         
         MVC   NUASSIGN,NEWASSGN   *** NEW ASSIGNED COST                        
         MVC   NUACTUAL,NEWACTUL   *** NEW ACTUAL COST                          
*                                                                               
         GOTO1 VCALCSHP,DMCB,AIO1,ACOMFACS,CLICPRPD                             
*                                                                               
         L     R2,AIO1                                                          
* DELETE ALL BILLING ELEMENTS FROM NEW RECORD                                   
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'10',(R2)),0                        
*                                                                               
* HANDLE TRAFFIC ELEMENTS                                                       
* 21 ELEMENT                                                                    
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SPLT140                                                          
         L     R3,12(R1)                                                        
         USING NUCMLEID,R3                                                      
         MVC   DUB(1),NUCMLPRD                                                  
         MVC   DUB+1(3),NUCMPROD                                                
         ZIC   R1,NUCMLELN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    NUCML1(0),NUCML1     KEEP FEED, CLEAR REST OF ELEMENT            
         CLI   COPYSPSW,C'Y'                                                    
         BNE   *+16                                                             
         MVC   NUCMLPRD,DUB                                                     
         MVC   NUCMPROD,DUB+1                                                   
         DROP  R3                                                               
* 23 ELEMENT                                                                    
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'23',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SPLT140                                                          
         L     R3,12(R1)                                                        
         USING NUFDCEL,R3                                                       
SPLT120  MVC   DUB(1),NUFDCPRD                                                  
         MVC   DUB+1(3),NUFDPROD                                                
         ZIC   R1,NUFDCLEN          SET LENGTH CHANGE INDICATOR                 
         SH    R1,=H'7'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    NUFDCML1(0),NUFDCML1   KEEP FEED, CLEAR REST OF ELEMENT          
         CLI   COPYSPSW,C'Y'                                                    
         BNE   *+16                                                             
         MVC   NUFDCPRD,DUB                                                     
         MVC   NUFDPROD,DUB+1                                                   
         ZIC   RE,NUFDCLEN          GET NEXT ELEMENT                            
         AR    R3,RE                                                            
         CLI   0(R3),X'23'                                                      
         BE    SPLT120                                                          
         DROP  R3                                                               
*                                                                               
* SET CREATION DATE                                                             
SPLT140  GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'99',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SPLT200                                                          
         L     R3,12(R1)                                                        
         USING NUACTEL,R3                                                       
         GOTO1 VDATCON,DMCB,(5,0),(3,THREE)                                     
         MVC   NUACTADT,THREE      ADD DATE                                     
         MVC   NUACTCDT,THREE      LAST ACTIVITY DATE                           
         CLI   NUACTLEN,22                                                      
         BL    SPLT200                                                          
         MVC   NUACTAID,SVPASSWD   ADD PERSONAL ID                              
         MVC   NUACTCID,SVPASSWD   LAST PERSONAL ID                             
         MVC   NUACTAGD,SECAGY     SECURITY AGENCY                              
         MVC   NUACTRSN(3),RUPREAS    REASON CODE                               
         MVI   NUACTRSN+3,X'40'    BLANK FILL                                   
         DROP  R3                                                               
*                                                                               
SPLT200  GOTO1 AIOCALL,DMCB,UNT+FIL+ADDREC,AIO1                                 
         MVC   DSKADDR,NDXDA                                                    
*                                                                               
* CREATE PASSIVE POINTERS WITH NEW SUB-LINE NUMBER                              
         LA    R4,KEY             CREATE X'84' PASSIVE                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(3),NUKAM                                                   
         MVC   KEY+4(4),NUKNET                                                  
         MVC   KEY+8(6),NUKPROG                                                 
         MVC   KEY+14(2),NUKDATE                                                
         MVC   KEY+16(1),NUKEST                                                 
         MVC   KEY+17(1),NEWLINE      SAVED NEW LINE NUMBER                     
         MVC   KEY+18(1),NUKDP                                                  
         MVC   KEY+20(1),SAVSTAT       SAVE POSTING TYPE                        
         MVC   KEY+21(4),DSKADDR                                                
         GOTO1 AIOCALL,DMCB,UNT+DIR+ADD                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'94'                                                        
         MVC   KEY+1(3),NUKAM                                                   
         MVC   KEY+4(1),NUKEST                                                  
         MVC   KEY+5(4),NUKNET                                                  
         BAS   RE,DAYLOOK           PUTS ONE BYTE DAY CODE IN NUKDDAY           
         MVC   KEY+10(1),NUKTIME                                                
         MVC   KEY+11(6),NUKPROG                                                
         MVC   KEY+17(2),NUKDATE                                                
         MVC   KEY+19(1),NEWLINE      NEW SAVED LINE NUMBER                     
         MVC   KEY+20(1),SAVSTAT      SAVE POSTING TYPE                         
         MVC   KEY+21(4),DSKADDR                                                
         GOTO1 AIOCALL,DMCB,UNT+DIR+ADD                                         
*                                                                               
*  IF UNIT IS MAKEGOOD SEED THE INFO ON ALL THE MISSED UNITS                    
*                                                                               
*  BUILD 07 ELEMENT                                                             
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING NUMGD,R3                                                         
         MVC   NUMGEL(2),=XL2'0720'                                             
         MVC   NUMGPCOD,NUKPROG                                                 
         MVC   NUMGPNM,NUPROGNM                                                 
         MVC   NUMGDATE,NUKDATE                                                 
         MVC   NUMGSUB,NUKSUB                                                   
         MVC   NUMGSTAT,NUUNITST                                                
*                                                                               
* GET MISSED INFORMATION                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'06',(R2)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SPLT300                                                          
         L     R3,12(R1)                                                        
* GET MISSED UNIT/MOVE NEW MISSED INFO OUT                                      
SPLT240  XC    KEY,KEY                                                          
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(3),NUKAM                                                   
         MVC   KEY+4(4),NUKNET                                                  
         MVC   KEY+16(1),NUKEST                                                 
         MVC   KEY+18(1),NUKDP                                                  
         MVC   KEY+8(6),NUMGPCOD                                                
         MVC   KEY+14(2),NUMGDATE                                               
         MVC   KEY+17(1),NUMGSUB                                                
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,UNT+FIL+GET+UPDATE,AIO2                             
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'07',AIO2),ELEM,0                   
         GOTO1 AIOCALL,DMCB,UNT+FIL+PUT,AIO2                                    
* GET NEXT MISSED ELEMENT                                                       
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'06'                                                      
         BE    SPLT240                                                          
*                                                                               
SPLT300  L     R3,AIO2                                                          
         LA    R3,2500(R3)          ADDRESS OF DEMO BLOCK 2ND UNIT              
         USING NDDEMBLK,R3                                                      
         BAS   RE,SETBLOCK                                                      
*                                                                               
* CHECK FOR SIDE BY SIDE DEMO REQUEST IF                                        
* ON SET UP NETBLOCK TO GET 2ND DEMO SET                                        
*                                                                               
         L     RE,AIO2                                                          
         USING NETBLOCK,RE                                                      
         MVI   DEMO2COL,C'N'                                                    
         TM    RUPSTAT2,X'07'       CHECK 2 SETS OF DEMOS                       
         BZ    SPLT450                                                          
         MVI   DEMO2COL,C'Y'                                                    
*                                                                               
         TM    RUPSTAT2,X'01'       CHECK ACTUALS                               
         BZ    *+20                                                             
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,C'N'                                                    
         MVI   NBDEMRAW,C'N'                                                    
         B     SPLT280                                                          
*                                                                               
         TM    RUPSTAT2,X'02'       ESTIMATED RAW                               
         BZ    *+20                                                             
         MVI   NBACTOPT,C'N'                                                    
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBDEMRAW,C'Y'                                                    
         B     SPLT280                                                          
*                                                                               
         TM    RUPSTAT2,X'04'       ESTIMATED GUARANTEES                        
         BZ    *+16                                                             
         MVI   NBACTOPT,C'N'                                                    
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBDEMRAW,C'N'                                                    
         DROP  RE                                                               
*                                                                               
SPLT280  CLI   SPLT45SW,C'Y'        CHECK FOR 45 SECOND UNIT                    
         BNE   SPLT350              DON'T DO SECOND DEMO LOOKUP                 
         MVC   NULEN,NEWLEN         SET LOOKUP TO NEW UNIT LENGTH               
         CLI   NULEN,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETDEMOS          GET DEMOS FOR NEW UNIT                      
*                                                                               
*  FOLLOWING CODE IS TO STORE FIRST COLUMN DEMOS                                
*  JUST IN CASE SIDE BY SIDE DEMOS ARE REQUSTED                                 
*                                                                               
         LA    RE,1200(R3)          STORE IN AIO2+3700                          
         MVC   0(200,RE),NDACTDEM                                               
         TM    RUPSTAT,X'10'        CHECK FOR ACTUAL DEMO REQUEST               
         BO    SPLT350                                                          
         MVC   0(200,RE),NDESTDEM                                               
*                                                                               
SPLT350  MVC   NULEN,OULEN                                                      
         CLI   NULEN,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO2                                                          
         LA    R3,1500(R3)          ADDRESS OF DEMO BLOCK 2ND UNIT              
         BAS   RE,GETDEMOS          GET DEMOS FOR OLD UNIT                      
*                                                                               
*  FOLLOWING CODE IS TO STORE FIRST COLUMN DEMOS                                
*  JUST IN CASE SIDE BY SIDE DEMOS ARE REQUSTED                                 
*                                                                               
         LA    RE,2000(R3)          STORE IN AIO2+3500                          
         MVC   0(200,RE),NDACTDEM                                               
         TM    RUPSTAT,X'10'        CHECK FOR ACTUAL DEMO REQUEST               
         BO    SPLT450                                                          
         MVC   0(200,RE),NDESTDEM                                               
         DROP  R3                                                               
*                                                                               
SPLT450  L     R3,AIO2                                                          
         LA    R3,2500(R3)          ADDRESS OF DEMO BLOCK 2ND UNIT              
         BAS   RE,SETBLOCK                                                      
         CLI   SPLT45SW,C'Y'        CHECK FOR 45 SECOND UNIT                    
         BNE   SPLT500              DON'T DO SECOND DEMO LOOKUP                 
         MVC   NULEN,NEWLEN         SET LOOKUP TO NEW UNIT LENGTH               
         CLI   NULEN,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETDEMOS          GET DEMOS FOR NEW UNIT                      
*                                                                               
SPLT500  MVC   NULEN,OULEN                                                      
         CLI   NULEN,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO2                                                          
         LA    R3,1500(R3)          ADDRESS OF DEMO BLOCK 2ND UNIT              
         BAS   RE,GETDEMOS          GET DEMOS FOR OLD UNIT                      
*                                                                               
SPLTEX   B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
LINERR   LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
         MVI   ERROR,SUBERR                                                     
         XC    ERMXFLD,ERMXFLD                                                  
         MVC   ERMXFLD(6),=CL6'SPLIT '                                          
         CLI   SPLITCNT,0           IF NEW, SPLIT ERROR IS DONE HERE            
         BE    *+8                                                              
         BAS   RE,SNDSPLT                                                       
         B     SPLTEX                                                           
         DROP  R6                                                               
LENERR   LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
         MVI   ERROR,STCHAERR                                                   
         XC    ERMXFLD,ERMXFLD                                                  
         MVC   ERMXFLD(6),=CL6'LENGTH'                                          
         CLI   SPLITCNT,0           IF NEW, SPLIT ERROR IS DONE HERE            
         BE    *+8                                                              
         BAS   RE,SNDSPLT                                                       
         B     SPLTEX                                                           
         DROP  R6                                                               
* - R2 MUST POINT TO SECOND PASSIVE KEY                                         
*                                                                               
DAYLOOK  DS    0H                                                               
         MVI   KEY+9,X'0A'         PRE-SET DAY CODE TO VAR                      
         LA    RF,DAYTAB                                                        
         LA    R0,DAYLEN                                                        
         CLC   NUDAY,0(RF)                                                      
         BE    *+16                                                             
         LA    RF,L'DAYTAB(RF)                                                  
         BCT   R0,*-14                                                          
         B     *+10                NOT IN TABLE                                 
         MVC   KEY+9(1),1(RF)      EXTRACT DAY VALUE FROM TABLE                 
         BR    RE                                                               
*                                                                               
DAYTAB   DS    0CL2                                                             
         DC    X'4001'              MONDAY                                      
         DC    X'2002'              TUESDAY                                     
         DC    X'1003'              WEDNESDAY                                   
         DC    X'0804'              THURSDAY                                    
         DC    X'0405'              FRIDAY                                      
         DC    X'0206'              SATURDAY                                    
         DC    X'0107'              SUNDAY                                      
         DC    X'7C08'              M-F                                         
         DC    X'7F09'              M-SU                                        
DAYLEN   EQU   (*-DAYTAB)/L'DAYTAB                                              
         EJECT                                                                  
*                                                                               
* GET THE LENGTH OF THE COMMENT AND MOVE INTO THE ELEMENT                       
*                                                                               
GETCOMM  NTR1                                                                   
         LA    R5,ELEM                                                          
         USING NUCOMD,R5                                                        
         LA    R3,59                                                            
GETCOM30 LR    RE,R3                                                            
         LA    R4,FLD                                                           
         AR    RE,R4                                                            
         CLI   0(RE),X'40'                                                      
         BH    GETCOM40                                                         
GETCOM35 BCT   R3,GETCOM30                                                      
*                                                                               
GETCOM40 EX    R3,*+4                                                           
         MVC   NUCOMMNT(0),FLD                                                  
*                                                                               
         LA    R3,5(R3)             ELEMENT LENGTH                              
         STCM  R3,1,NUCOMLEN                                                    
         B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
*-- GET DEFAULT CORP PRODUCT CODE                                               
GETCORPR NTR1                                                                   
         L     R4,AIO1                                                          
         USING NURECD,R4                                                        
         CLC   HOLDCLT,NUKCLT       CHECK CLIENT BREAK                          
         BE    CLIPREX                                                          
         MVC   HOLDCLT,NUKCLT                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),NUKPCLT                                                 
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 CLIENT RECORD MUST EXIST                    
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO2                                    
         L     RE,AIO2                                                          
         USING CLTHDR,RE                                                        
*                                                                               
         MVC   CLICPRPD,CPRPRD                                                  
GETCOREX B     EXIT                                                             
         DROP  R4,RE                                                            
         EJECT                                                                  
*                                                                               
* EXTRACT THE DEMOS USING NETVALUE                                              
*                                                                               
SETBLOCK NTR1                                                                   
         L     R2,AIO2                                                          
         USING NETBLOCK,R2                                                      
         XCEF  (R2),3500                                                        
*                                                                               
         MVC   NBAIO,AIO1                                                       
         MVI   NBACTOPT,C'N'                                                    
         MVI   NBESTOPT,C'M'                                                    
         MVC   NBACTAM,BAGYMD                                                   
         MVC   NBACOM,ACOMFACS                                                  
         MVC   NBDTADSP,=X'001B'                                                
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF         RF=A(COMFACS)                                
         MVC   NBDM,CDATAMGR                                                    
         MVC   NBCALLOV,CCALLOV                                                 
         MVC   NBDATCON,CDATCON                                                 
         MVC   NBGETDAY,CGETDAY                                                 
         MVC   NBADDAY,CADDAY                                                   
         MVC   NBHEXOUT,CHEXOUT                                                 
         MVC   NBHELLO,CHELLO                                                   
         MVC   NBDEMADR,CDEMADDR                                                
         MVC   NBDEMAIN,CDEMAINT                                                
         MVC   NBDEMAND,CDEMAND                                                 
         MVC   NBDEMEL,CDEMEL                                                   
         MVC   NBDEMMTH,CDEMOMTH                                                
         MVC   NBDEMOUT,CDEMOUT                                                 
         MVC   NBGTPROF,CGETPROF                                                
         DROP  RF                                                               
*                                                                               
* GET PROFILES                                                                  
*                                                                               
         LA    R4,KEY                                                           
         USING CLTHDR,R4                                                        
         XC    CKEY,CKEY                                                        
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BINCLT                                                   
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO3                                    
*                                                                               
         L     R4,AIO3                                                          
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
*  CHECK DEMOS NEEDED FOR RETURN                                                
         TM    RUPSTAT,X'20'        CHECK FOR NO GUARANTEES                     
         BZ    *+8                                                              
         MVI   NBDEMRAW,C'Y'        RETURN RAW DEMOS                            
         TM    RUPSTAT,X'10'        CHECK FOR ACTUAL DEMOS                      
         BZ    *+8                                                              
         MVI   NBACTOPT,C'Y'        RETURN ACTUAL DEMOS                         
*                                                                               
         XC    KEY,KEY             GET USER PROFILE INTO NBUSER                 
         MVC   KEY(4),=C'S0N0'     GET N0 PROFILE INTO NBUSER                   
         MVC   KEY+4(2),QAGY                                                    
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),RUPCLI                                                  
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),COFFICE                                                
         GOTOR NBGTPROF,DMCB,KEY,NBUSER,NBDM                                    
         MVI   KEY+3,C'1'          GET N1 PROFILE INTO NBUSER1                  
         GOTOR NBGTPROF,DMCB,KEY,NBUSER1,NBDM                                   
         MVI   KEY+3,C'2'          GET N2 PROFILE INTO NBUSER2                  
         GOTOR NBGTPROF,DMCB,KEY,NBUSER2,NBDM                                   
         MVC   NBUSER,NBUSER                                                    
         MVC   NBUSER1,NBUSER1                                                  
         MVC   NBUSER2,NBUSER2                                                  
*  CHECK OVERRIDE EQUIVALENCING OPTIONS                                         
         TM    RUPSTAT,X'08'                                                    
         BZ    *+8                                                              
         MVI   NBUSER2,30                                                       
         TM    RUPSTAT,X'04'                                                    
         BZ    *+8                                                              
         MVI   NBUSER2,0                                                        
         TM    RUPSTAT,X'02'                                                    
         BZ    *+8                                                              
         MVI   NBUSER+1,30                                                      
         TM    RUPSTAT,X'01'                                                    
         BZ    *+8                                                              
         MVI   NBUSER+1,0                                                       
         DROP  R4,R5                                                            
*  CHECK 2 DECIMAL AGENCY                                                       
         TM    SVAGYFL2,X'30'                                                   
         BZ    *+8                                                              
         OI    NBINDS3,X'40'        2 DECIMAL AGENCY                            
*                                                                               
         MVC   NBSTATYP,SVSTATYP                                                
         MVC   NBSTSTAT,SVSTATYP                                                
         MVC   NBPOSTYP,SVPOSTYP                                                
         MVC   NBSTPSTT,SVPOSTYP                                                
         MVC   NBSURVEY,SVPOSTYP                                                
*                                                                               
         MVI   NBPREOPT,C'N'                                                    
         CLI   NBPOSTYP,C'C'                                                    
         BE    SETBL100                                                         
         CLI   NBPOSTYP,C'D'                                                    
         BE    SETBL100                                                         
         CLI   NBPOSTYP,C'O'                                                    
         BNE   *+8                                                              
SETBL100 MVI   NBPREOPT,C'Y'                                                    
*                                                                               
         XC    NBHUNOPT,NBHUNOPT                                                
         CLI   SVPOSTYP,C'S'                                                    
         BE    SETBLEX                                                          
         CLI   SVPOSTYP,C'H'                                                    
         BE    SETBLEX                                                          
         CLI   SVPOSTYP,C'N'                                                    
         BE    SETBLEX                                                          
         MVI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
*                                                                               
SETBLEX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* EXTRACT THE DEMOS USING NETVALUE                                              
* R3=ADDRESS OF DEMO BLOCK                                                      
GETDEMOS NTR1                                                                   
         L     R2,AIO2                                                          
         USING NETBLOCK,R2                                                      
         USING NDDEMBLK,R3                                                      
*                                                                               
         LA    RF,NBXBLK                                                        
         STCM  RF,15,NBEXTEND                                                   
         USING NBXTNDD,RF                                                       
         L     RE,ATWA                                                          
         AHI   RE,QNTDMS-TWAD                                                   
         STCM  RE,15,NBXCDNL       A(COMSCORE DEMO NAME LIST)                   
         DROP  RF                                                               
*                                                                               
         ST    R3,NBADEM                                                        
*                                                                               
         MVC   NDUSRNMS,SVUSRNMS    USER DEMO NAMES                             
*                                                                               
* REMOVE END OF TABLE MARKER                                                    
         MVC   NDDEMOS,QDEMOS                                                   
         LA    RE,25                                                            
         LA    RF,NDDEMOS                                                       
GTDEM050 CLI   0(RF),X'FF'                                                      
         BE    GTDEM080                                                         
         LA    RF,3(RF)                                                         
         BCT   RE,GTDEM050                                                      
         B     GTDEM100                                                         
GTDEM080 MVI   0(RF),0                                                          
*                                                                               
*  CALL NETVALUE                                                                
GTDEM100 GOTO1 VNETVAL,DMCB,NETBLOCK                                            
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
* SUB-ROUTINE TO REMOVE A LEADING CHARACTER FROM FLD                            
* ON EXIT, CC=EQ FOR NOTHING LEFT IN FIELD, NEQ FOR SOMETHING THERE             
*                                                                               
REMOVE   ZIC   R1,FLDH+5                                                        
         SH    R1,=H'1'                                                         
         BZR   RE                                                               
         STC   R1,FLDH+5                                                        
         EX    R1,MOVEFLD          SHIFT FLD OVER TO THE LEFT                   
         LA    RF,FLD(R1)          POINT TO LAST CHAR POSITION                  
         MVI   0(RF),C' '                                                       
         CLI   FLDH+5,0                                                         
         BR    RE                                                               
*                                                                               
MOVEFLD  MVC   FLD(0),FLD+1                                                     
         EJECT                                                                  
*-- READ ESTIMATE IF NEEDED AND GET THE USER DEMO NAMES                         
GETUSER  NTR1                                                                   
         L     R4,AIO1              POINT R4 TO UNIT RECORD                     
         USING NURECD,R4                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),NUKCLT                                                  
         MVC   KEY+4(3),=CL3'POL'                                               
         MVC   KEY+7(1),NUKEST                                                  
*                                                                               
         CLC   KEY+1(7),SVESTKEY                                                
         BE    GETUSREX                                                         
         MVC   SVESTKEY,KEY+1                                                   
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 CLIENT RECORD MUST EXIST                    
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO2                                    
         L     RE,AIO2                                                          
         USING ESTHDR,RE                                                        
*                                                                               
         MVC   SVUSRNMS,EUSRNMS                                                 
GETUSREX B     EXIT                                                             
         DROP  R4,RE                                                            
         EJECT                                                                  
*-- REDA NEW CLIENT RECORD EXTRACT OFFICE CODE READ N2 PROFILE                  
READPROF NTR1                                                                   
         L     R4,AIO1              POINT R4 TO UNIT RECORD                     
         USING NURECD,R4                                                        
         CLC   HOLDCLT,NUKCLT       CHECK CLIENT BREAK                          
         BE    CLIPREX                                                          
         MVC   HOLDCLT,NUKCLT                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),NUKPCLT                                                 
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),NUKPCLT                                                 
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 CLIENT RECORD MUST EXIST                    
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO2                                    
         L     RE,AIO2                                                          
         USING CLTHDR,RE                                                        
*                                                                               
         MVC   CLIOPTN2,COPT2                                                   
         MVC   CLICPRPD,CPRPRD                                                  
*                                                                               
*  READ THE PROFILE                                                             
*                                                                               
CLIPROF  XC    KEY,KEY             GET USER PROFILE INTO NBUSER                 
         MVC   KEY(4),=C'S0N2'                                                  
         MVC   KEY+4(2),QAGY                                                    
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),NUKPCLT                                                 
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),COFFICE                                                
         GOTO1 VGETPROF,DMCB,KEY,N2PROFLE,VDATAMGR                              
CLIPREX  B     EXIT                                                             
         DROP  R4,RE                                                            
         EJECT                                                                  
*=================================================================*             
* SEND SPLIT INFO TO THE PC                                       *             
*=================================================================*             
         SPACE 1                                                                
SNDSPLT  NTR1                                                                   
*                                                                               
         LHI   R1,X'27'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  CHECK IF ERROR RETURNED                                                      
*                                                                               
         OC    ERROR,ERROR                                                      
         BZ    SNP040                                                           
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
*                                                                               
         LA    R4,SEQNUM                                                        
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERROR                                                         
         LHI   R1,X'39'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VGETMSG,DMCB+12,(ERROR,WORK),(X'FF',DMCB),(7,0)                  
         LA    R4,WORK+8                                                        
         LHI   R1,X'3A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERMXFLD                                                       
         LHI   R1,X'3B'                                                         
         BAS   RE,SENDD                                                         
         MVI   BUYERRSW,C'N'                                                    
         B     SNP280                                                           
         DROP  R6                                                               
*                                                                               
SNP040   LA    R4,SEQNUM            SEQUENCE NUMBER                             
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,OUACTUAL          UNIT 1 ACTUAL                               
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,OUASSIGN          UNIT 1 ASSUGNED                             
         LHI   R1,X'03'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         CLC   RUPTYPE,=CL2'CH'     IS THIS A DRAFT CHECK                       
         BNE   SNP050               YES DONT PASS LINE NUMBER                   
         DROP  RE                                                               
*                                                                               
         LA    R4,NEWLINE           UNIT 2 LINE NUMBER                          
         LHI   R1,X'04'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNP050   CLI   SPLITCNT,0           IF NEW SPLIT METHOD                         
         BNE   SNP070                                                           
         LA    R4,NEWACTUL          UNIT 2 ACTUAL                               
         LHI   R1,X'05'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NEWASSGN          UNIT 2 ASSIGNED                             
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE DEMOS ORIGINAL UNIT                                               
*                                                                               
SNP070   L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         L     RF,AIO2                                                          
         LA    RF,1500(RF)                                                      
         USING NDDEMBLK,RF                                                      
         LA    R3,NDESTDEM                                                      
         TM    RUPSTAT,X'10'        CHECK IF ACTUAL REQUESTED                   
         BZ    *+8                                                              
         LA    R3,NDACTDEM                                                      
         LA    R2,QDEMOS                                                        
         LA    R6,25                                                            
         LA    R4,FULL                                                          
         DROP  RE,RF                                                            
*                                                                               
SNP080   CLI   0(R2),X'FF'                                                      
         BE    SNP100                                                           
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R3)                                                    
         LHI   R1,X'07'                                                         
         BAS   RE,SENDD                                                         
         MVC   FULL(2),2(R3)                                                    
         LHI   R1,X'08'                                                         
         BAS   RE,SENDD                                                         
         MVC   FULL,4(R3)                                                       
         LHI   R1,X'09'                                                         
         BAS   RE,SENDD                                                         
         LA    R2,3(R2)             GET NEXT CATEGORY                           
         LA    R3,8(R3)             GET NEXT SET OF VALUES                      
         BCT   R6,SNP080                                                        
*                                                                               
*  OUTPUT THE DEMOS SECOND UNIT                                                 
*  THIS FILED IS NOT TRANSMITTED WHEN THE NEW SPLIT FEATURE IS USED             
*                                                                               
                                                                                
SNP100   CLI   SPLITCNT,0           IF NEW SPLITMOTHOD                          
         BNE   SNP140                                                           
         L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         CLI   SPLT45SW,C'Y'        45 SECOND SPLIT                             
         BNE   SNP140               NO,DON'T PASS SECOND SET OF DEMOS           
         L     RF,AIO2                                                          
         LA    RF,2500(RF)                                                      
         USING NDDEMBLK,RF                                                      
         LA    R3,NDESTDEM                                                      
         TM    RUPSTAT,X'10'        CHECK IF ACTUAL REQUESTED                   
         BZ    *+8                                                              
         LA    R3,NDACTDEM                                                      
         LA    R2,QDEMOS                                                        
         LA    R6,25                                                            
         LA    R4,FULL                                                          
         DROP  RE,RF                                                            
*                                                                               
SNP120   CLI   0(R2),X'FF'                                                      
         BE    SNP140                                                           
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R3)                                                    
         LHI   R1,X'0A'                                                         
         BAS   RE,SENDD                                                         
         MVC   FULL(2),2(R3)                                                    
         LHI   R1,X'0B'                                                         
         BAS   RE,SENDD                                                         
         MVC   FULL,4(R3)                                                       
         LHI   R1,X'0C'                                                         
         BAS   RE,SENDD                                                         
         LA    R2,3(R2)             GET NEXT CATEGORY                           
         LA    R3,8(R3)             GET NEXT SET OF VALUES                      
         BCT   R6,SNP120                                                        
*                                                                               
*  OUTPUT THE DEMOS ORIGINAL UNIT COLUMN2                                       
*                                                                               
SNP140   CLI   DEMO2COL,C'Y'                                                    
         BNE   SNP280                                                           
         L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         L     RF,AIO2                                                          
         LA    RF,1500(RF)                                                      
         LA    R3,2000(RF)          DEMOS HELD AT AIO2+3500                     
         LA    R2,QDEMOS                                                        
         LA    R6,25                                                            
         LA    R4,FULL                                                          
         DROP  RE                                                               
*                                                                               
SNP160   CLI   0(R2),X'FF'                                                      
         BE    SNP180                                                           
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R3)                                                    
         LHI   R1,X'0E'                                                         
         BAS   RE,SENDD                                                         
         MVC   FULL(2),2(R3)                                                    
         LHI   R1,X'0F'                                                         
         BAS   RE,SENDD                                                         
         MVC   FULL,4(R3)                                                       
         LHI   R1,X'10'                                                         
         BAS   RE,SENDD                                                         
         LA    R2,3(R2)             GET NEXT CATEGORY                           
         LA    R3,8(R3)             GET NEXT SET OF VALUES                      
         BCT   R6,SNP160                                                        
*                                                                               
*  OUTPUT THE DEMOS SECOND UNIT COLUMN 2                                        
*  THIS FILED IS NOT TRANSMITTED WHEN THE NEW SPLIT FEATURE IS USED             
*                                                                               
                                                                                
SNP180   CLI   SPLITCNT,0           IF NEW SPLITMOTHOD                          
         BNE   SNP280                                                           
         L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         CLI   SPLT45SW,C'Y'        45 SECOND SPLIT                             
         BNE   SNP280               NO,DON'T PASS SECOND SET OF DEMOS           
         L     RF,AIO2                                                          
         LA    RF,2500(RF)                                                      
         LA    R3,1200(RF)          DEMOS HELD AT AIO2+3500                     
         LA    R2,QDEMOS                                                        
         LA    R6,25                                                            
         LA    R4,FULL                                                          
         DROP  RE                                                               
*                                                                               
SNP200   CLI   0(R2),X'FF'                                                      
         BE    SNP280                                                           
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R3)                                                    
         LHI   R1,X'11'                                                         
         BAS   RE,SENDD                                                         
         MVC   FULL(2),2(R3)                                                    
         LHI   R1,X'12'                                                         
         BAS   RE,SENDD                                                         
         MVC   FULL,4(R3)                                                       
         LHI   R1,X'13'                                                         
         BAS   RE,SENDD                                                         
         LA    R2,3(R2)             GET NEXT CATEGORY                           
         LA    R3,8(R3)             GET NEXT SET OF VALUES                      
         BCT   R6,SNP200                                                        
*                                                                               
*  OUTPUT THE RESULT CODE IF ACTUALS REQUESTED                                  
*                                                                               
SNP280   L     RE,ANETBLK                                                       
         USING BUYUPLDD,RE                                                      
         L     RF,AIO2                                                          
         USING NETBLOCK,RF                                                      
         TM    RUPSTAT,X'10'        CHECK IF ACTUAL REQUESTED                   
         BZ    SNPEX                                                            
         CLI   NBRESULT,0                                                       
         BE    *+10                                                             
         MVC   HALF(1),NBRESULT                                                 
*--MOVE CABLE LEVEL INFO                                                        
         CLI   NBPOSTYP,C'C'                                                    
         BNE   SNP300                                                           
         CLI   NBUSER2+5,C'Y'                                                   
         BE    SNP300                                                           
         CLI   NBUSER2+5,C'N'                                                   
         BE    SNP300                                                           
         CLI   NBUSER2+5,X'40'                                                  
         BNH   SNP300                                                           
         MVC   HALF+1(1),NBUSER2+5                                              
SNP300   LA    R4,HALF              RESULT CODE                                 
         LHI   R1,X'0D'                                                         
         BAS   RE,SENDD                                                         
         DROP  RE,RF                                                            
*                                                                               
SNPEX    B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR TEST                                        *             
*=================================================================*             
         SPACE 1                                                                
CALLTSR2 LR    R0,RE                                                            
         PRINT GEN                                                              
         GOTO1 VTSAR,TSARBLK                                                    
         PRINT NOGEN                                                            
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
CALBUYEX XIT1                                                                   
         EJECT                                                                  
UNTFILE  DC    CL8'UNTFILE'                                                     
NEBLOCKL EQU   NBBLKEND-NETBLOCK+1                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
*  ROUTINE CONVERTS A VARIABLE LENGTH NUMERIC                                   
*  FIELD INTO BINARY                                                            
*  R6 - ADDRESS OF INPUT FIELD                                                  
*  BYTE - LENGTH OF THE FIELD                                                   
*                                                                               
*  OUTPUT                                                                       
*  BYTE - BINARY NUMBER                                                         
*                                                                               
GETBINRY NTR1  BASE=*,LABEL=*                                                   
         SR    RF,RF                                                            
         ZIC   RE,BYTE                                                          
         LR    R1,R6                                                            
*                                                                               
GTBI040  CLI   0(R1),X'40'                                                      
         BNH   GTBI080                                                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,GTBI040                                                       
*                                                                               
GTBI080  BCTR  RF,0                                                             
         EX    RF,FLDPACK                                                       
         CVB   R0,DUB                                                           
         STCM  R0,1,BYTE                                                        
         J     EXIT                                                             
         SPACE 1                                                                
FLDPACK  PACK  DUB,0(0,R6)                                                      
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
       ++INCLUDE NENAVWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
DIVIDER  DS    F                                                                
SPLITCNT DS    XL1                  NUMBER OF SPLITS                            
LOOPSW   DS    CL1                  LOOP SWITCH F=FIRST PASS                    
*                                               L=LAST PASS                     
LENSUM   DS    XL1                  RUNNNING SUM OF LENGTHS                     
ACTSUM   DS    XL4                  RUNNING SUM OF ACT DOLLARS                  
ASSSUM   DS    XL4                  RUNNING SUM OF ASS DOLLARS                  
*                                                                               
STARTLEN DS    XL1                  INITIAL LENGTH                              
STARTACT DS    XL4                  INITIAL ACTUAL DOLLARS                      
STARTASS DS    XL4                  INITIAL ASSIGNED DOLLARS                    
SPLTACCM EQU   *-SPLITCNT                                                       
*                                                                               
UNPAYSW  DS    CL1                                                              
COPYSPSW DS    CL1                  Y=COPYSPLIT UNIT                            
UNBILLSW DS    CL1                                                              
SPLT45SW DS    CL1                                                              
SVPOSTYP DS    CL1                                                              
SVSTATYP DS    CL1                                                              
HOLDCLT  DS    CL2                                                              
N2PROFLE DS    CL16                                                             
THREE    DS    CL3                                                              
SEQNUM   DS    CL1                                                              
SAVSTAT  DS    CL1                                                              
DEMO2COL DS    CL1                                                              
DSKADDR  DS    XL4                                                              
*                                                                               
NEWLINE  DS    XL1                                                              
NEWASSGN DS    XL4                                                              
NEWACTUL DS    XL4                                                              
NEWLEN   DS    XL1                                                              
OUASSIGN DS    XL4                                                              
OUACTUAL DS    XL4                                                              
OULEN    DS    XL1                                                              
*                                                                               
CLIOPTN2 DS    CL1                                                              
CLICPRPD DS    CL3                                                              
BINCLT   DS    XL2                                                              
*                                                                               
PAKCLI   DS    CL3                                                              
PAKDPT   DS    CL1                                                              
PAKCNTL  DS    CL1                                                              
PAKSTAT  DS    CL1                                                              
*                                                                               
SECAGY   DS    CL2                                                              
SVPASSWD DS    CL2                                                              
*                                                                               
SVESTKEY DS    CL28                                                             
SVUSRNMS DS    CL28                                                             
*                                                                               
ELEM     DS    CL80                                                             
NBXBLK   DS    XL1600              NETBLKXTND BLOCK                             
         DS    0D                                                               
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAFACTS                                                        
       PRINT ON                                                                 
       ++INCLUDE NAVDSECTS                                                      
       ++INCLUDE NETBLKXTND                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060NENAV08   03/05/18'                                      
         END                                                                    
