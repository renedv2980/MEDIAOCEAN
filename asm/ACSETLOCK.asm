*          DATA SET ACSETLOCK  AT LEVEL 032 AS OF 08/09/11                      
*PHASE T00A66A                                                                  
         SPACE 2                                                                
*===============================================================*               
* LOCK TABLE RESIDES IN CHKPT1 SO IT IS AVAILABLE ON A SYSTEM   *               
* RESTART. THERE ARE TWO LOCK BYTES THAT ARE USED --            *               
* 1)  ALOCKBYT  -  ADDRESS OF LOCK BYTE - RESIDES IN CHKPT1     *               
*     (CORE) AT TABLE-2, CONTROLS THE INHIBIT LOCKING FUNCTION  *               
*     AND IS RECOVERED AFTER A RESTART                          *               
*                                                               *               
* 2) LDGLOCK                                                    *               
*     THIS IS LOCAL TO THIS OVERLAY AND CONTROLS WHETHER THE    *               
*     LOCK TABLE IS BEING UPDATED CURRENTLY - YOU COULD         *               
*     ENCOUNTER A PROBLEM IF SYSTEMS GOES DOWN MID-PROCESS      *               
*     WHILE THIS BIT IS SET (UNLESS PROGRAMS LIBRARY IS ALSO    *               
*     RELOADED WHEN SYSTEM COMES BACK UP) - BIT WOULD NOW BE    *               
*     ON AND YOU WOULD NEVER GET BACK IN TO PROCESS TABLE SINCE *               
*     OVERLAY WOULD THINK IT IS LOCKED, BUT THERE WOULD BE      *               
*     NOTHING PROCESSING TO EVENTUALLY UNLOCK IT - SOLUTION     *               
*     IS TO JUST RELOAD PHASE OR PATCH LDGLOCK TO X'00'         *               
*===============================================================*               
SETLOCK  TITLE 'TEST AND SET LEDGER LOCK'                                       
SETLOCK  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**SETL**,RR=RE,CLEAR=YES                             
         USING WORKD,RC                                                         
         ST    RE,RELO                                                          
         LR    R2,R1                                                            
         USING PARMD,R2            R2=A(PARAMETER LIST)                         
         MVI   PARMRETN,0          RESET RETURN CODE                            
         L     R9,PARMACOM                                                      
         USING COMFACSD,R9         R9=A(COMFACS)                                
         MVC   PROTON,CPROTON                                                   
         MVC   PROTOFF,CPROTOFF                                                 
* CALL FASWITCH TO GET SYSFAC ADDRESS                                           
         L     RF,CSWITCH                                                       
         LA    R1,DMCB                                                          
         MVC   0(4,R1),=X'FEFFFFFF'  SET SYSTEM = FE                            
         BASR  RE,RF                                                            
*                                                                               
         L     RE,0(R1)            POINT TO SYSFACS                             
         USING SYSFACD,RE                                                       
         L     R0,VSSB                                                          
         ST    R0,ASSB             SAVE SSB ADDRESS                             
         L     RE,VSYSFAC3                                                      
         DROP  RE                                                               
         L     R8,(AACCLTB1-ACCFACSD)(RE)  POINT TO TABLE                       
         USING LDGTABD,R8          R8=A(LEDGER TABLE)                           
*                                                                               
         LR    R0,R8                                                            
         SH    R0,=H'2'            LOCK BYTE PRECEDES TABLE                     
         ST    R0,ALOCKBYT         SAVE LOCK BYTE ADDRESS                       
*                                                                               
         LA    R8,6(R8)            POINT TO FIRST TABLE ENTRY                   
         ST    R8,ALDGTAB          SAVE TABLE ADDRESS                           
*                                                                               
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1           EXTRACT SYSTEM VALUES                        
         MVC   SEN,FASYS                                                        
         MVC   DATE,FADATEB                                                     
         MVC   TIME,FATIME                                                      
         MVC   LUID,FASYM                                                       
*                                                                               
         L     R1,PARMAKEY         BUILD TABLE LOOKUP KEY                       
         MVC   CUL,0(R1)                                                        
*                                                                               
         LA    R1,ACTTAB           LOCATE ACTION TABLE ENTRY                    
         LA    R0,ACTTABN                                                       
         CLC   PARMACTN,0(R1)                                                   
         BE    *+14                                                             
         LA    R1,L'ACTTAB(R1)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         A     RF,RELO                                                          
         BR    RF                  GO TO ACTION ROUTINE                         
*                                                                               
SETLOCKX XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* CREATE A LOCK ENTRY                                                 *         
***********************************************************************         
         SPACE 1                                                                
LOCK     L     RE,ALOCKBYT                                                      
         TM    0(RE),INHLOCKQ      TEST TABLE UPDATE INHIBITED                  
         BZ    *+12                                                             
         MVI   PARMRETN,PARMERR1                                                
         B     SETLOCKX                                                         
         BAS   RE,LOCKTAB          LOCK THE LEDGER TABLE                        
*                                                                               
         LA    R0,LDGTMAXN                                                      
LOCK02   CLC   LDGTKEY,SEN         LOOK-UP LEDGER IN TABLE                      
         BE    LOCK04                                                           
         LA    R8,LDGTABL(R8)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,LOCK02                                                        
         B     LOCK06                                                           
*                                                                               
LOCK04   TM    LDGTSTAT,LDGTSLOK   TEST ENTRY LOCKED                            
         BZ    *+12                                                             
         MVI   PARMRETN,PARMERR5                                                
         B     LOCK28                                                           
         ST    R8,TABA             SAVE A(LEDGER TABLE ENTRY)                   
*                                                                               
LOCK06   LA    R3,IO                                                            
         USING LDGRECD,R3          READ LEDGER RECORD                           
         MVI   LDGKEY,C' '                                                      
         MVC   LDGKEY+1(L'LDGKEY-1),LDGKEY                                      
         MVC   LDGKCPY(L'CUL),CUL                                               
         GOTO1 CDATAMGR,DMCB,(X'80',DMREAD),ACCFIL,LDGRECD,LDGRECD              
         BE    LOCK08                                                           
         MVI   PARMRETN,PARMERR3                                                
         B     LOCK28                                                           
*                                                                               
LOCK08   LA    R4,LDGRECD+ACCORFST                                              
         SR    R0,R0                                                            
         USING LGLELD,R4           LOCATE LEDGER LOCK ELEMENT                   
LOCK10   IC    R0,LGLLN                                                         
         AR    R4,R0               BUMP TO NEXT ELEMENT                         
         CLI   LGLEL,LGLELQ        TEST LEDGER LOCK ELEMENT                     
         BE    LOCK12                                                           
         CLI   LGLEL,0             TEST END OF RECORD                           
         BNE   LOCK10                                                           
*                                                                               
         MVI   LGLEL,LGLELQ        BUILD A LEDGER LOCK ELEMENT                  
         MVI   LGLLN,LGLLNQ                                                     
         MVI   LGLSTAT,0                                                        
         MVI   LGLELD+LGLLNQ,0                                                  
         LA    R1,LGLELD+LGLLNQ+1                                               
         LA    R0,LDGRECD          SET NEW RECORD LENGTH                        
         SR    R1,R0                                                            
         STCM  R1,3,LDGRECD+ACCORLEN                                            
         B     LOCK14                                                           
*                                                                               
LOCK12   DS    0H                                                               
         TM    LGLSTAT,LGLSLOCK    IS IT LOCKED                                 
         BNZ   LOCK16                                                           
*                                                                               
LOCK14   MVC   LGLDATE,DATE        SET LOCK PARAMETERS                          
         MVC   LGLTIME,TIME                                                     
         MVC   LGLLUID,LUID                                                     
         MVI   LGLSTAT,0                                                        
*                                                                               
LOCK16   GOTO1 PROTOFF             TURN PROT OFF FOR ACCESS TO LDGTAB           
         ICM   R8,15,TABA          GET/TEST A(LEDGER TABLE ENTRY)               
         BNZ   LOCK22                                                           
         L     R8,ALDGTAB          LOCATE A FREE ENTRY                          
         SR    RE,RE                                                            
         LA    R0,LDGTMAXN                                                      
LOCK18   CLC   LDGTABD(LDGTABL),SPACES                                          
         BNH   LOCK22                                                           
         TM    LDGTSTAT,LDGTSLOK   SAVE A(FIRST UNLOCKED ENTRY) IN RE           
         BNZ   LOCK20                                                           
         LTR   RE,RE                                                            
         BNZ   LOCK20                                                           
         LR    RE,R8                                                            
LOCK20   LA    R8,LDGTABL(R8)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,LOCK18                                                        
         LTR   R8,RE               GET/TEST A(FIRST UNLOCKED ENTRY)             
         BNZ   LOCK22                                                           
         MVI   PARMRETN,PARMERR4                                                
         B     LOCK26                                                           
*                                                                               
LOCK22   MVC   LDGTKEY,SEN         BUILD A TABLE ENTRY                          
         MVC   LDGTTIME,LGLTIME    SET TIME                                     
         MVC   LDGTLUID,LGLLUID    SET LUID                                     
         MVC   LDGTSTAT,LGLSTAT    SET STATUS                                   
         TM    LDGTSTAT,LDGTSLOK   TEST ALREADY LOCKED                          
         BZ    LOCK24                                                           
         MVI   PARMRETN,PARMERR5                                                
         B     LOCK26                                                           
*                                                                               
LOCK24   OI    LDGTSTAT,LDGTSLOK   SET TABLE ENTRY LOCKED                       
         OI    LGLSTAT,LGLSLOCK    SET RECORD LOCKED                            
         GOTO1 CDATAMGR,DMCB,DMWRT,ACCFIL,LDGRECD,LDGRECD                       
         NI    LDGLOCK,255-LDGLPROC                                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T WRITE LEDGER RECORD                    
         GOTO1 PROTON                TURN PROTECTION ON - NO ERROR              
         B     SETLOCKX                                                         
*                                                                               
LOCK26   NI    LDGLOCK,255-LDGLPROC  ERROR - PROTECTION IS OFF                  
         L     RE,ASSB                                                          
         OI    SSBSTAT1-SSBD(RE),SSBSCHK1  SET CHKPT1 UPDATED                   
         GOTO1 PROTON                TURN PROTECTION BACK ON                    
         B     SETLOCKX                                                         
*                                                                               
LOCK28   GOTO1 PROTOFF               ERROR - TURN PROTECTION OFF                
         B     LOCK26                                                           
         EJECT                                                                  
***********************************************************************         
* TEST LOCK                                                           *         
***********************************************************************         
         SPACE 1                                                                
TEST     LA    R0,LDGTMAXN                                                      
TEST02   CLC   LDGTKEY,SEN         LOOK-UP LEDGER IN TABLE                      
         BNE   *+14                                                             
         MVC   PARMRETN,LDGTSTAT   RETURN STATUS BYTE                           
         B     SETLOCKX                                                         
         LA    R8,LDGTABL(R8)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,TEST02                                                        
*                                                                               
         BAS   RE,LOCKTAB          LOCK THE LEDGER TABLE                        
         LA    R3,IO                                                            
         USING LDGRECD,R3          READ LEDGER RECORD                           
         MVI   LDGKEY,C' '                                                      
         MVC   LDGKEY+1(L'LDGKEY-1),LDGKEY                                      
         MVC   LDGKCPY(L'CUL),CUL                                               
         GOTO1 CDATAMGR,DMCB,DMREAD,ACCFIL,LDGRECD,LDGRECD                      
         BE    TEST08                                                           
         MVI   PARMRETN,PARMERR3                                                
         B     TESTN                                                            
*                                                                               
TEST08   LA    R4,LDGRECD+ACCORFST                                              
         SR    R0,R0                                                            
         USING LGLELD,R4           LOCATE LEDGER LOCK ELEMENT                   
TEST10   IC    R0,LGLLN                                                         
         AR    R4,R0               BUMP TO NEXT ELEMENT                         
         CLI   LGLEL,0             TEST END OF RECORD                           
         BE    TESTN                                                            
         CLI   LGLEL,LGLELQ        TEST LEDGER LOCK ELEMENT                     
         BNE   TEST10                                                           
*                                                                               
*        CLC   LGLDATE,DATE        TEST LOCKED TODAY                            
*        BNE   TESTN                                                            
         TM    LGLSTAT,LGLSLOCK    TEST LOCKED                                  
         BZ    TESTN                                                            
         MVC   PARMRETN,LGLSTAT    RETURN LEDGER STATUS                         
*                                                                               
         GOTO1 PROTOFF             TURN PROT OFF FOR ACCESS TO LDGTAB           
         L     R8,ALDGTAB          LOCATE A FREE ENTRY                          
         SR    RE,RE                                                            
         LA    R0,LDGTMAXN                                                      
TEST12   OC    LDGTABD(LDGTABL),LDGTABD                                         
         BZ    TEST16                                                           
         TM    LDGTSTAT,LDGTSLOK   SAVE A(FIRST UNLOCKED ENTRY) IN RE           
         BNZ   TEST14                                                           
         LTR   RE,RE                                                            
         BNZ   TEST14                                                           
         LR    RE,R8                                                            
TEST14   LA    R8,LDGTABL(R8)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,TEST12                                                        
         LTR   R8,RE               GET/TEST A(FIRST UNLOCKED ENTRY)             
         BZ    TESTN                                                            
*                                                                               
TEST16   MVC   LDGTKEY,SEN         BUILD A TABLE ENTRY                          
         MVC   LDGTTIME,LGLTIME    SET TIME                                     
         MVC   LDGTLUID,LGLLUID    SET LUID                                     
         MVC   LDGTSTAT,LGLSTAT    SET STATUS                                   
*                                                                               
TESTN    GOTO1 PROTON              TURN PROTECTION BACK ON                      
         NI    LDGLOCK,255-LDGLPROC                                             
         B     SETLOCKX                                                         
         EJECT                                                                  
***********************************************************************         
* UNLOCK A LEDGER RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
UNLK     BAS   RE,LOCKTAB          LOCK THE LEDGER TABLE                        
*                                                                               
         LA    R0,LDGTMAXN                                                      
UNLK02   CLC   LDGTKEY,SEN         LOOK-UP LEDGER IN TABLE                      
         BE    UNLK04                                                           
         LA    R8,LDGTABL(R8)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UNLK02                                                        
         B     UNLK06                                                           
*                                                                               
UNLK04   TM    LDGTSTAT,LDGTSLOK   TEST ENTRY LOCKED                            
         BZ    UNLKN                                                            
         NI    LDGTSTAT,255-LDGTSLOK                                            
*                                                                               
UNLK06   LA    R3,IO                                                            
         USING LDGRECD,R3          READ LEDGER RECORD                           
         MVI   LDGKEY,C' '                                                      
         MVC   LDGKEY+1(L'LDGKEY-1),LDGKEY                                      
         MVC   LDGKCPY(L'CUL),CUL                                               
         GOTO1 CDATAMGR,DMCB,(X'80',DMREAD),ACCFIL,LDGRECD,LDGRECD              
         NI    LDGLOCK,255-LDGLPROC                                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T WRITE LEDGER RECORD                    
         B     SETLOCKX                                                         
*                                                                               
UNLK08   LA    R4,LDGRECD+ACCORFST                                              
         SR    R0,R0                                                            
         USING LGLELD,R4           LOCATE LEDGER LOCK ELEMENT                   
UNLK10   IC    R0,LGLLN                                                         
         AR    R4,R0               BUMP TO NEXT ELEMENT                         
         CLI   LGLEL,0             TEST END OF RECORD                           
         BE    UNLKN                                                            
         CLI   LGLEL,LGLELQ        TEST LEDGER LOCK ELEMENT                     
         BNE   UNLK10                                                           
*                                                                               
*        CLC   LGLDATE,DATE        TEST LOCKED TODAY                            
*        BNE   UNLKN                                                            
         TM    LGLSTAT,LGLSLOCK    IS IT LOCKED                                 
         BZ    UNLKN                                                            
         NI    LGLSTAT,255-LGLSLOCK                                             
*                                                                               
         GOTO1 CDATAMGR,DMCB,DMWRT,ACCFIL,LDGRECD,LDGRECD                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T WRITE LEDGER RECORD                    
*                                                                               
UNLKN    NI    LDGLOCK,255-LDGLPROC                                             
         L     RE,ASSB                                                          
         OI    SSBSTAT1-SSBD(RE),SSBSCHK1  SET CHKPT1 UPDATED                   
         B     SETLOCKX                                                         
         EJECT                                                                  
***********************************************************************         
* RETURN ADDRESS OF TABLE AND TABLE LOCK WORD                         *         
***********************************************************************         
         SPACE 1                                                                
ATAB     LA    R0,LDGTABD                                                       
         ST    R0,PARMAKEY                                                      
         MVC   PARMRETN,LDGLOCK                                                 
         L     RE,ALOCKBYT                                                      
         OC    PARMRETN,0(RE)                                                   
         B     SETLOCKX                                                         
         SPACE 2                                                                
***********************************************************************         
* INHIBIT FURTHER LOCKING                                             *         
***********************************************************************         
         SPACE 1                                                                
STOP     L     RE,ALOCKBYT                                                      
         OI    0(RE),INHLOCKQ      INHIBIT FURTHER LOCKING                      
         B     SETLOCKX                                                         
         SPACE 2                                                                
***********************************************************************         
* ALLOW LOCKING                                                       *         
***********************************************************************         
         SPACE 1                                                                
FREE     L     RE,ALOCKBYT                                                      
         NI    0(RE),255-INHLOCKQ                                               
         MVC   PARMRETN,0(RE)                                                   
         B     SETLOCKX                                                         
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO LOCK THE TABLE                                           *         
***********************************************************************         
         SPACE 1                                                                
LOCKTAB  XC    IO(64),IO                                                        
         LA    R3,10                                                            
LOCKTAB2 TM    LDGLOCK,LDGLPROC    TEST TABLE LOCKED                            
         BNZ   *+10                                                             
         OI    LDGLOCK,LDGLPROC    SET TABLE LOCKED                             
         BR    RE                                                               
         LR    R0,RE               ISSUE I/O AND WAIT TO AWAIT UNLOCK           
         IC    R1,IO                                                            
         LA    R1,1(R1)                                                         
         STC   R1,IO                                                            
         GOTO1 CDATAMGR,DMCB,DMRDHI,CTFILE,IO,IO                                
         LR    RE,R0                                                            
         BCT   R3,LOCKTAB2                                                      
         MVI   PARMRETN,PARMERR2                                                
         B     SETLOCKX                                                         
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
DMRDHI   DC    C'DMRDHI '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMWRT    DC    C'DMWRT  '                                                       
CTFILE   DC    C'CTFILE '                                                       
ACCFIL   DC    C'ACCOUNT'                                                       
SPACES   DC    CL132' '                                                         
         SPACE 1                                                                
ACTTAB   DS    0XL4                ** ACTION TABLE **                           
         DC    AL1(PARMLOCK),AL3(LOCK)                                          
         DC    AL1(PARMTEST),AL3(TEST)                                          
         DC    AL1(PARMATAB),AL3(ATAB)                                          
         DC    AL1(PARMUNLK),AL3(UNLK)                                          
         DC    AL1(PARMSTOP),AL3(STOP)                                          
         DC    AL1(PARMFREE),AL3(FREE)                                          
ACTTABN  EQU   (*-ACTTAB)/L'ACTTAB                                              
         SPACE 1                                                                
         DC    (X'7FF'-(*-SETLOCK))X'00'                                        
         SPACE 1                                                                
         EJECT                                                                  
WORKD    DSECT                                                                  
PROTON   DS    V                   PROTECTION OFF                               
PROTOFF  DS    V                   PROCTECTION OFF                              
RELO     DS    A                                                                
DMCB     DS    6F                                                               
ALDGTAB  DS    A                                                                
ALOCKBYT DS    A                   ADDRESS OF LOCK BYTE                         
INHLOCKQ EQU   X'80'               INHIBIT LOCKS                                
ASSB     DS    A                                                                
TABA     DS    A                                                                
DATE     DS    XL3                                                              
TIME     DS    PL4                                                              
LUID     DS    CL8                                                              
SEN      DS    XL1                                                              
CUL      DS    XL3                                                              
LDGLOCK  DS    XL1                 LDGTAB LOCK WORD                             
LDGLPROC EQU   X'40'               LOCK/UNLOCK IN PROCESS                       
IO       DS    2048X                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE ACSETLOCKD                                                     
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDACCFACS                                                                     
       ++INCLUDE DDACCFACS                                                      
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032ACSETLOCK 08/09/11'                                      
         END                                                                    
