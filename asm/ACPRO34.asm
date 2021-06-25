*          DATA SET ACPRO34    AT LEVEL 003 AS OF 09/12/02                      
*PHASE T60B34A,*                                                                
         TITLE 'T60B34 - PANEL MAINTENANCE'                                     
T60B34   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B34**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BE    PANEL2                                                           
         CLI   MODE,VALREC                                                      
         BE    PANEL4                                                           
         CLI   MODE,DISPREC                                                     
         BE    PANEL10                                                          
         B     XIT                                                              
*                                                                               
* VALKEY LOGIC                                                                  
*                                                                               
PANEL2   LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LA    RF,PRECOPY                                                       
         CLI   ACTNUM,ACTNCOPY     TEST FOR COPY                                
         BE    PANEL3                                                           
         CLI   ACTNUM,ACTNREN      TEST FOR RENAME                              
         BE    PANEL3                                                           
         LA    RF,PREMT            EDIT MAINT SCREEN KEYS                       
*                                                                               
PANEL3   BASR  RE,RF                                                            
         CLI   ACTNUM,ACTNADD      TEST FOR ADD                                 
         BNE   *+8                                                              
         BAS   RE,PROCPF           YES-INSPECT PF KEYS                          
         B     XIT                                                              
*                                                                               
* VALREC LOGIC                                                                  
*                                                                               
PANEL4   TM    WHENOK,X'01'        TEST FOR ADD OR CHANGE                       
         BO    PANEL6                                                           
         CLI   ACTNUM,ACTNCHA      TEST FOR CHANGE                              
         BNE   *+12                                                             
         BAS   RE,PREMT            YES-RE-EDIT THE KEY FIELDS                   
         BAS   RE,PROCPF           INSPECT PF KEYS                              
*                                                                               
         BAS   RE,VREC             LET GENCON DEAL WITH ADD/CHANGE              
         B     PANELX                                                           
*                                                                               
PANEL6   LA    RF,COPY             THESE ACTIONS ARE HANDLED MANUALLY           
         CLI   ACTNUM,ACTNCOPY                                                  
         BE    PANEL8                                                           
         LA    RF,RENAME                                                        
         CLI   ACTNUM,ACTNREN                                                   
         BE    PANEL8                                                           
         LA    RF,DELETE                                                        
         CLI   ACTNUM,ACTNDEL                                                   
         BE    PANEL8                                                           
         LA    RF,RESTORE                                                       
         CLI   ACTNUM,ACTNRES                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PANEL8   BASR  RE,RF               CALL SPECIFIC ROUTINE                        
         B     PANELX                                                           
*                                                                               
PANEL10  BAS   RE,PROCPF                                                        
         BAS   RE,DISP                                                          
*                                                                               
PANELX   XMOD1 1                                                                
         SPACE 2                                                                
* SUB-ROUTINE TO INSPECT THE PF KEYS                                            
*                                                                               
PROCPF   CLI   PFKEY,PF2           TEST PF2=FIELD MAINT                         
         BNER  RE                  NO                                           
         MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'FIELD',=C'MAINT',(L'QPANEL,QPANEL),0               
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO VALIDATE THE MAINT SCREEN'S KEY FIELDS       *                 
***************************************************************                 
         SPACE 1                                                                
PREMT    NTR1  ,                                                                
         LA    R2,PROCODEH                                                      
         GOTO1 ANY                                                              
         MVC   QPANEL,WORK                                                      
         CLI   ACTNUM,ACTNADD      TEST FOR ADD                                 
         BNE   PREMT1              NO                                           
         ZIC   R0,5(R2)            GET CODE LENGTH                              
         GOTO1 TSTAN,QPANEL        CHECK FOR ALPHA-NUMERIC CODE                 
         MVI   ERROR,INVALID                                                    
         CLC   QPANEL,DEFPANEL     TEST IF DEFAULT PANEL INPUT                  
         BE    ERREND              YES-ITS AN ERROR                             
*                                                                               
PREMT1   LA    R4,KEY                                                           
         USING ACPNKEY,R4                                                       
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,QPANEL                                                  
         CLI   ACTNUM,ACTNDEL      TEST FOR DELETE                              
         BE    *+12                                                             
         CLI   ACTNUM,ACTNRES      TEST FOR RESTORE                             
         BNE   PREMTX                                                           
*                                                                               
PREMT2   OI    WHENOK,X'01'        NO-OP GENCON                                 
         CLI   ACTNUM,ACTNDEL                                                   
         BE    *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETES FOR A RESTORE                   
         GOTO1 READ                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
*                                                                               
PREMTX   B     XIT                                                              
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO VALIDATE THE COPY/RENAME SCREEN'S KEY FIELDS *                 
***************************************************************                 
         SPACE 1                                                                
PRECOPY  NTR1  ,                                                                
         LA    R2,CPYFROMH                                                      
         GOTO1 ANY                                                              
         MVC   FROMCODE,WORK                                                    
         LA    R4,KEY                                                           
         USING ACPNKEY,R4                                                       
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,FROMCODE                                                
         GOTO1 READ                                                             
*                                                                               
PRECOPY2 LA    R2,CPYTOH                                                        
         GOTO1 ANY                                                              
         MVC   TOCODE,WORK                                                      
         MVC   ACPNCODE,TOCODE                                                  
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         CLC   ACPNKEY,KEYSAVE     TEST IF PANEL FOUND                          
         BNE   PRECOPYX            NO-OK                                        
         CLI   ACTNUM,ACTNREN      TEST FOR RENAME                              
         BE    PRECOPY4            YES                                          
*                                                                               
         MVI   ERROR,RECISDEL      FOR COPY ITS ALWAYS AN ERROR                 
         TM    ACSTATUS,X'80'      TEST IF RECORD DELETED                       
         BO    *+8                                                              
         MVI   ERROR,RECEXIST                                                   
         B     ERREND                                                           
*                                                                               
PRECOPY4 MVI   ERROR,RECEXIST      FOR RE-NAME, ALLOW A DELETE                  
         TM    ACSTATUS,X'80'                                                   
         BZ    ERREND                                                           
*                                                                               
PRECOPYX B     XIT                                                              
         EJECT                                                                  
***************************************************                             
* SUB-ROUTINE TO VALIDATE THE PANEL RECORD DATA   *                             
***************************************************                             
         SPACE 1                                                                
VREC     NTR1  ,                                                                
         LA    R2,PRONAMEH                                                      
         GOTO1 ANY                                                              
         CLI   ACTNUM,ACTNCHA                                                   
         BE    VREC2                                                            
*                                                                               
         LA    R6,ELEM                                                          
         USING ACPHD,R6                                                         
         MVI   ACPHEL,ACPHELQ                                                   
         MVI   ACPHLEN,ACPHLENQ                                                 
         MVC   ACPHNAME,WORK                                                    
         MVC   ACPHADD,TODAYP      DATA ADDED=TODAY                             
         MVC   ACPHPERS,TWAALIAS                                                
         MVC   ACPHLAST,TODAYP                                                  
         MVI   ACPHWHAT,C'A'       ADDED                                        
         GOTO1 ADDELEM                                                          
         B     VRECX                                                            
*                                                                               
VREC2    MVI   ELCODE,ACPHELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ACPHNAME,WORK                                                    
*                                                                               
VRECX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***************************************************                             
* SUB-ROUTINE TO DISPLAY THE PANEL HEADER DATA    *                             
***************************************************                             
         SPACE 1                                                                
DISP     NTR1  ,                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,ACPHELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACPHD,R6                                                         
         MVC   PRONAME,ACPHNAME                                                 
         OI    PRONAMEH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
******************************************************                          
* SUB-ROUTINE TO PERFORM THE DELETE                  *                          
******************************************************                          
         SPACE 1                                                                
DELETE   NTR1  ,                                                                
         LA    R4,KEY                                                           
         USING ACPNKEY,R4                                                       
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,QPANEL                                                  
         GOTO1 HIGH                                                             
         B     DELETE2                                                          
*                                                                               
DELETE1  LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
DELETE2  CLC   ACPNKEY(ACPNNUM-ACPNKEY),KEYSAVE                                 
         BNE   DELETE4                                                          
*                                                                               
         L     R4,AIO                                                           
         CLI   ACPNNUM,0           TEST FOR HEADER RECORD                       
         BNE   *+8                                                              
         BAS   RE,DISP             YES-DISPLAY DATA                             
         OI    ACSTATUS,X'80'      TURN ON DELETE BIT                           
         GOTO1 WRITE                                                            
         B     DELETE1                                                          
*                                                                               
DELETE4  MVC   CONHEAD(L'DELMSG),DELMSG                                         
         OI    GENSTAT2,USMYOK+STLCOK  USE LOWER CASE MESSAGE                   
         LA    R2,PROCODEH                                                      
         ST    R2,ACURFORC                                                      
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
DELETEX  B     XIT                                                              
         EJECT                                                                  
******************************************************                          
* SUB-ROUTINE TO PERFORM A RESTORE                   *                          
******************************************************                          
         SPACE 1                                                                
RESTORE  NTR1  ,                                                                
         LA    R4,KEY                                                           
         USING ACPNKEY,R4                                                       
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,QPANEL                                                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         B     RESTORE2                                                         
*                                                                               
RESTORE1 LA    R4,KEY                                                           
         OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
*                                                                               
RESTORE2 CLC   ACPNKEY(ACPNNUM-ACPNKEY),KEYSAVE                                 
         BNE   RESTORE4                                                         
*                                                                               
         L     R4,AIO                                                           
         CLI   ACPNNUM,0           TEST FOR HEADER                              
         BNE   *+8                                                              
         BAS   RE,DISP             DISPLAY THE RECORD                           
         NI    ACSTATUS,X'FF'-X'80' TURN OFF DELETE BIT                         
         GOTO1 WRITE                                                            
*                                  LOOK FOR TRAILER ELEMENT                     
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACFDELQ',AIO),(1,=X'FF')              
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BE    RESTORE4            ALL DONE                                     
         B     RESTORE1                                                         
*                                                                               
RESTORE4 MVC   CONHEAD(L'RESTMSG),RESTMSG                                       
         OI    GENSTAT2,USMYOK+STLCOK   USE LOWER CASE MESSAGE                  
         LA    R2,PROCODEH                                                      
         ST    R2,ACURFORC                                                      
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
RESTOREX B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* SUB-ROUTINE TO COPY A PANEL.  AT ENTRY, FROMCODE AND TOCODE     *             
* SPECIFY THE OLD AND NEW CODES IN THE COPY.                      *             
*******************************************************************             
         SPACE 1                                                                
COPY     NTR1  ,                                                                
         LA    R4,KEY                                                           
         USING ACPNKEY,R4                                                       
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,FROMCODE   FROM PANEL                                   
*                                                                               
COPY2    GOTO1 HIGH                                                             
*                                                                               
COPY4    CLC   ACPNKEY(ACPNNUM-ACPNKEY),KEYSAVE                                 
         BNE   COPY10              FINISHED COPY                                
*                                                                               
         MVC   FROMKEY,ACPNKEY     SAVE FROM PANEL RECORD'S KEY                 
         L     R4,AIO                                                           
         MVC   ACPNCODE,TOCODE     SET NEW PANEL CODE                           
         CLI   ACPNNUM,0           TEST FOR HEADER RECORD                       
         BNE   COPY6               NO                                           
*                                                                               
         MVI   ELCODE,ACPHELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACPHD,R6                                                         
         MVC   ACPHPERS,TWAALIAS                                                
         MVC   ACPHLAST,TODAYP                                                  
         MVI   ACPHWHAT,C'C'       NOTE COPY IS LAST ACTION                     
*                                                                               
COPY6    GOTO1 ADD                                                              
         LA    R4,KEY                                                           
         MVC   ACPNKEY,FROMKEY     RESTORE FROM KEY                             
         ZIC   R1,ACPNNUM          INCREMENT RECORD NUMBER                      
         LA    R1,1(R1)                                                         
         STC   R1,ACPNNUM                                                       
         B     COPY2               GET NEXT RECORD                              
*                                                                               
COPY10   MVC   CONHEAD(5),=C'PANEL'                                             
         MVC   CONHEAD+6(4),FROMCODE                                            
         MVC   CONHEAD+11(6),=C'COPIED'                                         
         OC    CONHEAD,SPACES                                                   
         GOTO1 SQUASHER,DMCB,CONHEAD,L'CONHEAD                                  
         LA    R2,CPYFROMH                                                      
         ST    R2,ACURFORC                                                      
*                                                                               
COPYX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO PERFORM A RENAME.  AT ENTRY, FROMCODE AND     *                
* TOCODE CONTAIN TO SOURCE AND DESTINATION CODES.  TOCODE MAY  *                
* BE DELETED FROM A PREVIOUS RENAME                            *                
****************************************************************                
         SPACE 1                                                                
RENAME   NTR1  ,                                                                
         LA    R4,KEY                                                           
         USING ACPNKEY,R4                                                       
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   FROMKEY,ACPNKEY     SAVE KEY SO FAR                              
         MVC   ACPNCODE,TOCODE     TO PANEL                                     
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   ACPNKEY,KEYSAVE     TEST IF TO KEY IS THERE                      
         BNE   *+8                                                              
         MVI   DELSW,C'Y'          YES-TOCODE MUST BE DELETED                   
*                                                                               
RENAME1  MVC   ACPNKEY,FROMKEY     RESTORE FROM KEY                             
         MVC   ACPNCODE,FROMCODE   SET FROM CODE                                
         MVC   FROMKEY,ACPNKEY     SAVE ENTIRE FROM KEY                         
*                                                                               
* FIRST COPY THE FROM PANEL DATA SET TO THE TO PANEL                            
*                                                                               
RENAME2  GOTO1 HIGH                                                             
*                                                                               
RENAME4  CLC   ACPNKEY(ACPNNUM-ACPNKEY),KEYSAVE                                 
         BNE   RENAME10            FINISHED COPY                                
*                                                                               
         MVC   FROMKEY,ACPNKEY     SAVE FROM KEY                                
         L     R4,AIO                                                           
         MVC   ACPNCODE,TOCODE     SET NEW PANEL CODE                           
         CLI   ACPNNUM,0           TEST FOR HEADER RECORD                       
         BNE   RENAME6             NO                                           
*                                                                               
         MVI   ELCODE,ACPHELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACPHD,R6                                                         
         MVC   ACPHPERS,TWAALIAS                                                
         MVC   ACPHLAST,TODAYP                                                  
         MVI   ACPHWHAT,C'R'       NOTE RENAME IS LAST ACTION                   
*                                                                               
RENAME6  CLI   DELSW,C'Y'          TEST TOCODE IS DELETED                       
         BE    RENAME8             YES                                          
         GOTO1 ADD                                                              
         B     RENAME9                                                          
*                                                                               
RENAME8  OI    DMINBTS,X'08'                                                    
         MVC   AIO,AIO2            READ INTO IO2                                
         MVC   KEY(L'ACPNKEY),ACPNKEY EXTRACT TO KEY                            
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1            RESTORE IO POINTER                           
         NI    DMINBTS,X'FF'-X'08' TURN OFF DELETE BIT                          
         L     RF,WRITE            RF=A(WRITE ROUTINE)                          
         CLC   KEY(L'ACPNKEY),KEYSAVE TEST IF KEY FOUND                         
         BE    *+8                 YES                                          
         L     RF,ADD              RF=A(ADD ROUTINE)                            
         BASR  RE,RF                                                            
*                                                                               
RENAME9  LA    R4,KEY                                                           
         MVC   ACPNKEY,FROMKEY     GET LAST FROM KEY                            
         ZIC   R1,ACPNNUM          INCREMENT RECORD NUMBER                      
         LA    R1,1(R1)                                                         
         STC   R1,ACPNNUM                                                       
         B     RENAME2             GET NEXT RECORD                              
*                                                                               
* NOW WRITE BACK THE FROM PANEL DATA SET AS DELETED                             
*                                                                               
RENAME10 LA    R4,KEY                                                           
         MVC   ACPNKEY,FROMKEY                                                  
         MVI   ACPNNUM,0           START AT THE FROM HEADER                     
         GOTO1 HIGH                                                             
         B     RENAME12                                                         
*                                                                               
RENAME11 LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
RENAME12 CLC   ACPNKEY(ACPNNUM-ACPNKEY),KEYSAVE                                 
         BNE   RENAME15                                                         
*                                                                               
         L     R4,AIO                                                           
         OI    ACSTATUS,X'80'      TURN ON DELETE BIT                           
         GOTO1 WRITE                                                            
         B     RENAME11                                                         
*                                                                               
* SET OUTPUT MESSAGE                                                            
*                                                                               
RENAME15 MVC   CONHEAD(5),=C'PANEL'                                             
         MVC   CONHEAD+6(4),FROMCODE                                            
         MVC   CONHEAD+11(7),=C'RENAMED'                                        
         OC    CONHEAD,SPACES                                                   
         GOTO1 SQUASHER,DMCB,CONHEAD,L'CONHEAD                                  
         LA    R2,CPYFROMH                                                      
         ST    R2,ACURFORC                                                      
*                                                                               
RENAMEX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR ALPHANUMERIC FIELD                                   
*                                                                               
* AT ENTRY, R0=N'BYTES TO CHECK, R1=A(STRING TO CHECK)                          
*                                                                               
TSTAN    ST    RE,SAVERE                                                        
         MVI   ERROR,INVALID                                                    
*                                                                               
TSTAN1   CLI   0(R1),C'0'                                                       
         BL    *+16                                                             
         CLI   0(R1),C'9'                                                       
         BH    ERREND                                                           
         B     TSTAN2                                                           
         CLI   0(R1),C'A'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'I'                                                       
         BNH   TSTAN2              CHARACTER IS BETWEEN A-I                     
         CLI   0(R1),C'J'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'R'                                                       
         BNH   TSTAN2                                                           
         CLI   0(R1),C'S'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'Z'                                                       
         BH    ERREND                                                           
*                                                                               
TSTAN2   LA    R1,1(R1)            NEXT CHARACTER IN FIELD                      
         BCT   R0,TSTAN1                                                        
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
COPYMSG  DC    C'**PANEL COPIED**'                                              
DELMSG   DC    C'**PANEL DELETED**'                                             
RESTMSG  DC    C'**PANEL RESTORED**'                                            
DEFPANEL DC    C'9999'                                                          
         EJECT                                                                  
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0X                                                               
SAVERE   DS    A                                                                
*                                                                               
QPANEL   DS    CL(L'ACPNCODE)                                                   
FROMCODE DS    CL(L'ACPNCODE)                                                   
TOCODE   DS    CL(L'ACPNCODE)                                                   
DELSW    DS    C                   Y=TOCODE IS DELETED                          
FROMKEY  DS    XL(L'ACPNKEY)                                                    
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROC4D                                                       
         SPACE 2                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROC5D                                                       
         SPACE 2                                                                
         ORG   T60BFFD+2304                                                     
LSAVES   DS    0D                                                               
         DS    CL((SAVAREA-LSAVES)-(*-LSAVES))  SPARE                           
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACPRO34   09/12/02'                                      
         END                                                                    
