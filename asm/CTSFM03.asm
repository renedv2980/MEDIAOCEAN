*          DATA SET CTSFM03    AT LEVEL 054 AS OF 01/29/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE TA0A03A                                                                  
*                                                                               
*BOBY  MAT/02  ADDED A SECOND DISKOUT OPTION TO GIVE A COMMA DELINITED          
*              FILE CONTAINING COLUMN HEADERS. ACTIVATED IF DISK                
*              OPTION IS '2'. SHOULD ONLY BE RUN VIA TSO.                       
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM03 -- DRIVER DICTIONARY ENTRY MAINTENANCE/LIST  *         
*                                                                     *         
*  COMMENTS:     MAINTAINS THE DRIVER DICTIONARY ENTRIES FOR ALL      *         
*                SYSTEMS.  RECORDS ARE ON THE GENFILE.                *         
*                                                                     *         
*                IF REQUESTOR ID IS 'KEY' THEN REPORT IS SORTED       *         
*                   BY 'ENTRY' RATHER THAN 'DESCRIPTION'              *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                DDGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  CALLS TO:     DRIVAL (DRIVER VALIDATION ROUTINES)                  *         
*                DATAMGR                                              *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMF3 (MAINTENANCE)                        *         
*                        CTSFME3 (LIST)                               *         
*                        CTSFMD3 (REPORT)                             *         
*                                                                     *         
*  OUTPUTS:      UPDATED ENTRY RECORDS, LIST, OR REPORT.              *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER POINTER                    *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- THIRD ABASE                                    *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A03 - DRIVER DICTIONARY ENTRY MAINTENANCE/LIST'              
TA0A03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A03**,R7,R5,RR=R3                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO             RELOCATION FACTOR                            
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A36'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADRIVAL,DMCB        ADDRESS OF DRIVAL ROUTINES                   
*                                                                               
         OI    GENSTAT4,NODELLST   NO DELETE FROM LIST                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECPUT        AFTER CHANGE                                 
         BE    XRP                                                              
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       CLI   ACTEQU,ACTREP       TEST OFFLINE REPORT ACTION                   
         BE    VK10                                                             
*                                                                               
         XC    DICTNAME,DICTNAME   DICTIONARY NAME                              
         LA    R2,ENMDICTH                                                      
         CLI   5(R2),0             TEST ANY DATA                                
         BE    MISSERR             REQUIRED                                     
         GOTO1 ANY                                                              
         MVC   DICTNAME,WORK       SAVE DICTIONARY NAME                         
*                                                                               
         XC    ENTRNAME,ENTRNAME   ENTRY NAME                                   
         LA    R2,ENMENTH                                                       
         CLI   5(R2),0             TEST ANY DATA                                
         BNE   *+16                                                             
         CLI   ACTEQU,ACTLIST      TEST ONLINE LIST ACTION                      
         BNE   MISSERR             REQUIRED                                     
         B     VK20                                                             
*                                                                               
         GOTO1 ANY                                                              
         MVC   ENTRNAME,WORK       SAVE ENTRY NAME                              
         B     VK20                                                             
*                                                                               
VK10     XC    DICTNAME,DICTNAME   DICTIONARY NAME                              
         LA    R2,ENPDICTH                                                      
         CLI   5(R2),0             TEST ANY DATA                                
         BE    MISSERR             REQUIRED                                     
         GOTO1 ANY                 GET KEY                                      
*                                                                               
         CLC   =C'ALL     ',WORK   TEST 'ALL' DICTIONARIES                      
         BE    *+10                                                             
         MVC   DICTNAME,WORK                                                    
*                                                                               
         XC    ENTRNAME,ENTRNAME                                                
*                                                                               
VK20     LA    R4,KEY                                                           
         USING DICKEYD,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   DICSYS,DICSYSQ      SYSTEM                                       
         MVI   DICKTYP,DICKTYPQ    RECORD TYPE                                  
         MVC   DICCODE,DICTNAME    DICTIONARY NAME                              
         MVC   DICENTRY,ENTRNAME   ENTRY NAME                                   
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       L     R4,AIO                                                           
         USING DICKEY,R4                                                        
         MVC   DICTNAME,DICCODE                                                 
         MVC   ENTRNAME,DICENTRY                                                
*                                                                               
         CLI   ENMLANGH+5,0        LANGUAGE GIVEN?                              
         BNE   *+18                                                             
         MVC   ENMLANG,=C'ENG'     NO -- DEFAULT TO ENGLISH                     
         MVI   ENMLANGH+5,3        FUDGE INPUT LENGTH                           
         OI    ENMLANGH+6,X'80'    XMIT                                         
*                                                                               
         LA    R2,ENMLANGH                                                      
         CLI   ENMLANGH+5,3        3-CHAR LANGUAGE CODE?                        
         BE    *+12                                                             
         MVI   ERROR,INVLANG       NO                                           
         B     TRAPERR                                                          
*                                                                               
         CLC   =C'ALL',ENMLANG     'ALL' LANGUAGE?                              
         BNE   *+12                                                             
         MVI   DICLANG,X'FF'                                                    
         B     VR4                                                              
*                                                                               
         LA    R3,LANGTAB0         A(FIRST ENTRY IN LANGUAGE TABLE)             
         USING LANGTABD,R3                                                      
*                                                                               
VR3      CLC   ENMLANG,LANGSHR     MATCH ON SHORT LANGUAGE NAME?                
         BNE   *+14                                                             
         MVC   DICLANG,LANGCODE    YES -- SAVE LANGUAGE CODE                    
         B     VR4                                                              
         AH    R3,LANGTAB          LENGTH OF EACH ENTRY                         
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   VR3                                                              
         MVI   ERROR,INVLANG       YES -- INVALID LANGUAGE                      
         B     TRAPERR                                                          
         DROP  R3                                                               
*                                                                               
VR4      XC    DRVLBLK,DRVLBLK     CLEAR THE DRIVAL PARAMETER BLOCK             
         MVC   DBDICTNM,DICCODE    DICTIONARY NAME                              
         MVC   DBCOMADR,ACOMFACS   COMFACS                                      
         LA    R1,ELEM                                                          
         ST    R1,DBELOADR         ALWAYS PUT ELEMENT INTO ELEM                 
         LA    R1,L'ELEM(R1)       A(END OF ELEMENT BUFFER)                     
         ST    R1,DBENDADR                                                      
*                                                                               
         MVI   ELCODE,X'02'        DESCRIPTION ELEMENT                          
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING DENAMD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   DENAMEL,X'02'                                                    
         MVI   DENAMLEN,DENAMDXQ                                                
         LA    R2,ENMDESCH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR             THIS ELEMENT IS REQUIRED                     
         GOTO1 ANY                                                              
         MVC   DENAMEEX,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,X'22'        INPUT TYPE ELEMENT                           
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMITYPH                                                      
         CLI   5(R2),0                                                          
         BNE   VR5                                                              
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'04',AIO),(1,=X'22')              
         B     VR10                                                             
VR5      MVI   DBOPCODE,X'22'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
         MVI   ELTYPE,X'22'                                                     
         BAS   RE,INPUTEL                                                       
*                                                                               
VR10     MVI   ELCODE,X'23'        INPUT LENGTH ELEMENT                         
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMILENH                                                      
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         MVI   DBOPCODE,X'23'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
*                                                                               
VR20     MVI   ELCODE,X'24'        INPUT ROUTINE ELEMENT                        
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMIRTNH                                                      
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         MVI   DBOPCODE,X'24'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
*                                                                               
VR30     MVI   ELCODE,X'25'        INPUT ARGS ELEMENT                           
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMIARGH                                                      
         CLI   5(R2),0                                                          
         BNE   VR35                                                             
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'04',AIO),(1,=X'25')              
         B     VR40                                                             
VR35     MVI   DBOPCODE,X'25'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
         MVI   ELTYPE,X'25'                                                     
         BAS   RE,INPUTEL                                                       
*                                                                               
VR40     MVI   ELCODE,X'26'        INPUT OPTIONS ELEMENTS                       
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMIOPTH                                                      
         CLI   5(R2),0                                                          
         BNE   VR42                                                             
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'04',AIO),(1,=X'26')              
         B     VR50                                                             
VR42     MVI   DBOPCODE,X'26'                                                   
         LA    R1,SCANBLK                                                       
         LA    R0,8                                                             
         XC    0(74,R1),0(R1)                                                   
         LA    R1,74(R1)                                                        
         BCT   R0,*-10                                                          
         XC    SCANOFLO,SCANOFLO                                                
         GOTO1 SCANNER,DMCB,(52,(R2)),(9,SCANBLK),C',=,='                       
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   4(R1),8                                                          
         BH    MAX8OPT                                                          
         XC    SCANOFLO,SCANOFLO                                                
         MVC   DBSCANLN,4(R1)                                                   
         LA    R3,SCANBLK                                                       
VR45     ST    R3,DBSRCADR                                                      
         XC    ELEM,ELEM                                                        
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
         LA    R3,74(R3)                                                        
         CLI   0(R3),0                                                          
         BNE   VR45                                                             
         MVI   ELTYPE,X'26'                                                     
         BAS   RE,INPUTEL                                                       
*                                                                               
VR50     MVI   ELCODE,X'32'        OUTPUT TYPE ELEMENT                          
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMOTYPH                                                      
         CLI   5(R2),0                                                          
         BNE   VR55                                                             
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'04',AIO),(1,=X'32')              
         B     VR60                                                             
VR55     MVI   DBOPCODE,X'32'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
         MVI   ELTYPE,X'32'                                                     
         BAS   RE,INPUTEL                                                       
*                                                                               
VR60     MVI   ELCODE,X'33'        OUTPUT LENGTH ELEMENT                        
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMOLENH                                                      
         CLI   5(R2),0                                                          
         BE    VR70                                                             
         MVI   DBOPCODE,X'33'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
*                                                                               
VR70     MVI   ELCODE,X'34'        OUTPUT ROUTINE ELEMENT                       
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMORTNH                                                      
         CLI   5(R2),0                                                          
         BE    VR80                                                             
         MVI   DBOPCODE,X'34'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
*                                                                               
VR80     MVI   ELCODE,X'35'        OUTPUT ARGS ELEMENT                          
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMOARGH                                                      
         CLI   5(R2),0                                                          
         BNE   VR85                                                             
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'04',AIO),(1,=X'35')              
         B     VR90                                                             
VR85     MVI   DBOPCODE,X'35'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
         MVI   ELTYPE,X'35'                                                     
         BAS   RE,INPUTEL                                                       
*                                                                               
VR90     MVI   ELCODE,X'36'        OUTPUT OPTIONS ELEMENTS                      
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMOOPTH                                                      
         CLI   5(R2),0                                                          
         BNE   VR92                                                             
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'04',AIO),(1,=X'36')              
         B     VR100                                                            
VR92     MVI   DBOPCODE,X'36'                                                   
         LA    R1,SCANBLK                                                       
         LA    R0,8                                                             
         XC    0(74,R1),0(R1)                                                   
         LA    R1,74(R1)                                                        
         BCT   R0,*-10                                                          
         XC    SCANOFLO,SCANOFLO                                                
         GOTO1 SCANNER,DMCB,(52,(R2)),(9,SCANBLK),C',=,='                       
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   4(R1),8                                                          
         BH    MAX8OPT                                                          
         XC    SCANOFLO,SCANOFLO                                                
         MVC   DBSCANLN,4(R1)                                                   
         LA    R3,SCANBLK                                                       
VR95     ST    R3,DBSRCADR                                                      
         XC    ELEM,ELEM                                                        
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
         LA    R3,74(R3)                                                        
         CLI   0(R3),0                                                          
         BNE   VR95                                                             
         MVI   ELTYPE,X'36'                                                     
         BAS   RE,INPUTEL                                                       
*                                                                               
VR100    MVI   ELCODE,X'82'        HEADS ROUTINE ELEMENT                        
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMHRTNH                                                      
         CLI   5(R2),0                                                          
         BE    VR110                                                            
         MVI   DBOPCODE,X'82'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
*                                                                               
VR110    MVI   ELCODE,X'83'        HEADS ARGS ELEMENT                           
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMHARGH                                                      
         CLI   5(R2),0                                                          
         BNE   VR115                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'04',AIO),(1,=X'83')              
         B     VR120                                                            
VR115    MVI   DBOPCODE,X'83'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
         MVI   ELTYPE,X'83'                                                     
         BAS   RE,INPUTEL                                                       
*                                                                               
VR120    MVI   ELCODE,X'87'        HEAD1 LITERAL                                
         GOTO1 REMELEM                                                          
         MVI   DBOPCODE,X'87'                                                   
         LA    R2,ENMHL1H                                                       
         CLI   5(R2),0                                                          
         BE    VR130                                                            
         XC    ELEM,ELEM                                                        
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         MVI   ELEM+2,1                                                         
         GOTO1 ADDELEM                                                          
VR130    LA    R2,ENMHL2H          HEAD2 LITERAL                                
         CLI   5(R2),0                                                          
         BE    VR140                                                            
         XC    ELEM,ELEM                                                        
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         MVI   ELEM+2,2                                                         
         GOTO1 ADDELEM                                                          
VR140    LA    R2,ENMHL3H          HEAD3 LITERAL                                
         CLI   5(R2),0                                                          
         BE    VR150                                                            
         XC    ELEM,ELEM                                                        
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         MVI   ELEM+2,3                                                         
         GOTO1 ADDELEM                                                          
VR150    LA    R2,ENMHL4H          HEAD4 LITERAL                                
         CLI   5(R2),0                                                          
         BE    VR160                                                            
         XC    ELEM,ELEM                                                        
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         MVI   ELEM+2,4                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
VR160    MVI   ELCODE,X'08'        ATTRIBUTE ELEMENT                            
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R2,ENMATTRH                                                      
         CLI   5(R2),0                                                          
         BNE   VR170                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'04',AIO),(1,=X'08')              
         B     VR180                                                            
VR170    MVI   DBOPCODE,X'08'                                                   
         ST    R2,DBSRCADR                                                      
         GOTO1 ADRIVAL,DMCB,DRVLBLK                                             
         CLI   DBERRNUM,0                                                       
         BNE   ERR                                                              
         GOTO1 ADDELEM                                                          
         MVI   ELTYPE,X'08'                                                     
         BAS   RE,INPUTEL                                                       
*                                                                               
VR180    MVI   ELCODE,DESGRPLQ                                                  
         GOTO1 REMELEM                                                          
         LA    R2,ENMSECGH         SECURITY GROUP                               
         CLI   5(R2),0                                                          
         BE    VR200                                                            
         TM    4(R2),X'08'                                                      
         BZ    ERR                                                              
*        CLI   8(R2),C'0'          VALIDATE 0-9,A-Z (CLOSE ENOUGH)              
*        BL    *+12                                                             
*        CLI   8(R2),C'9'                                                       
*        BNH   VR182                                                            
*        CLI   8(R2),C'A'                                                       
*        BL    ERR                                                              
*        CLI   8(R2),C'Z'                                                       
*        BH    ERR                                                              
*                                                                               
VR182    LA    R6,ELEM                                                          
         USING DESGRPD,R6                                                       
         XC    ELEM,ELEM                                                        
         MVI   DESGRPEL,DESGRPLQ                                                
         MVI   DESGRPLN,X'03'                                                   
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
         STC   R3,DESECGRP         SET SECURITY GROUP (BINARY)                  
         DROP  R6                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR200    LA    R1,DICELDQ          LENGTH OF KEY                                
         LA    R3,DICFIRST         A(FIRST ELEMENT)                             
VR210    CLI   0(R3),0             TEST END OF RECORD                           
         BE    VR220                                                            
         ZIC   R0,1(R3)            ELEMENT LENGTH                               
         AR    R1,R0               TALLY RECORD LENGTH                          
         AR    R3,R0                                                            
         B     VR210                                                            
VR220    LA    R1,1(R1)            FINAL BYTE OF ZERO                           
         CLM   R1,3,DICLEN         RECORD LENGTHS MUST MATCH                    
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       BAS   RE,CLEAR            CLEAR ALL INPUT FIELDS                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        DESCRIPTION                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING DENAMD,R6                                                        
         CLI   DENAMLEN,DENAMDLQ   IS THIS A SHORT DESCRIPTION?                 
         BNE   *+14                                                             
         MVC   ENMDESC(L'DENAME),DENAME                                         
         B     *+10                                                             
         MVC   ENMDESC,DENAMEEX    NO -- IT'S LONG                              
         OI    ENMDESCH+6,X'80'                                                 
*                                                                               
         L     R6,AIO                                                           
         USING DICKEYD,R6                                                       
         CLI   DICLANG,X'FF'                                                    
         BNE   *+14                                                             
         MVC   ENMLANG,=C'ALL'     IT'S LANGUAGE 'ALL'                          
         B     DR7                                                              
*                                                                               
         LA    R3,LANGTAB0         A(FIRST ENTRY IN LANGUAGE TABLE)             
         USING LANGTABD,R3                                                      
DR5      CLC   DICLANG,LANGCODE    MATCH ON LANGUAGE CODE?                      
         BE    *+18                                                             
         AH    R3,LANGTAB          LENGTH OF EACH ENTRY                         
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   DR5                                                              
         DC    H'0'                BAD LANGUAGE CODE IN RECORD                  
*                                                                               
         MVC   ENMLANG,LANGSHR     SHORT LANGUAGE NAME                          
DR7      OI    ENMLANGH+6,X'80'    XMIT                                         
         OI    ENMLANGH+1,X'01'    MODIFY                                       
         MVI   ENMLANGH+5,3                                                     
         DROP  R3,R6                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'22'       INPUT TYPE                                   
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     DR10                                                             
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ENMITYP(0),DEINPTXT                                              
         OI    ENMITYPH+6,X'80'                                                 
*                                                                               
DR10     L     R6,AIO                                                           
         MVI   ELCODE,X'23'        INPUT LENGTH                                 
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
         USING DEILD,R6                                                         
         EDIT  (1,DEILEN),(3,ENMILEN),ALIGN=LEFT                                
         OI    ENMILENH+6,X'80'                                                 
*                                                                               
DR20     L     R6,AIO                                                           
         MVI   ELCODE,X'24'        INPUT ROUTINE                                
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
         USING DEIRD,R6                                                         
         MVC   ENMIRTN,DEIROUT                                                  
         OI    ENMIRTNH+6,X'80'                                                 
*                                                                               
DR30     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR40                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'25'       INPUT ARGS                                   
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     DR40                                                             
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ENMIARG(0),DEINPTXT                                              
         OI    ENMIARGH+6,X'80'                                                 
*                                                                               
DR40     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR50                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'26'       INPUT OPTIONS                                
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     DR50                                                             
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ENMIOPT(0),DEINPTXT                                              
         OI    ENMIOPTH+6,X'80'                                                 
*                                                                               
DR50     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR60                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'32'       OUTPUT TYPE                                  
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     DR60                                                             
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ENMOTYP(0),DEINPTXT                                              
         OI    ENMOTYPH+6,X'80'                                                 
*                                                                               
DR60     L     R6,AIO                                                           
         MVI   ELCODE,X'33'        OUTPUT LENGTH                                
         BAS   RE,GETEL                                                         
         BNE   DR70                                                             
         USING DEOLD,R6                                                         
         EDIT  (1,DEOLEN),(3,ENMOLEN),ALIGN=LEFT                                
         OI    ENMOLENH+6,X'80'                                                 
*                                                                               
DR70     L     R6,AIO                                                           
         MVI   ELCODE,X'34'        OUTPUT ROUTINE                               
         BAS   RE,GETEL                                                         
         BNE   DR80                                                             
         USING DEORD,R6                                                         
         MVC   ENMORTN,DEOROUT                                                  
         OI    ENMORTNH+6,X'80'                                                 
*                                                                               
DR80     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR90                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'35'       OUTPUT ARGS                                  
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     DR90                                                             
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ENMOARG(0),DEINPTXT                                              
         OI    ENMOARGH+6,X'80'                                                 
*                                                                               
DR90     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR100                                                            
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'36'       OUTPUT OPTIONS                               
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     DR100                                                            
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ENMOOPT(0),DEINPTXT                                              
         OI    ENMOOPTH+6,X'80'                                                 
*                                                                               
DR100    L     R6,AIO                                                           
         MVI   ELCODE,X'82'        HEADS ROUTINE                                
         BAS   RE,GETEL                                                         
         BNE   DR110                                                            
         USING DESRD,R6                                                         
         MVC   ENMHRTN,DESROUT                                                  
         OI    ENMHRTNH+6,X'80'                                                 
*                                                                               
DR110    L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR120                                                            
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'83'       HEADS ARGS                                   
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     DR120                                                            
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ENMHARG(0),DEINPTXT                                              
         OI    ENMHARGH+6,X'80'                                                 
*                                                                               
DR120    L     R6,AIO                                                           
         MVI   ELCODE,X'87'        HEAD LINES 1,2,3,4                           
         BAS   RE,GETEL                                                         
         BNE   DR130                                                            
         USING DELITD,R6                                                        
DR125    LA    R2,ENMHL1H                                                       
         CLI   DELITLIN,1                                                       
         BE    DR127                                                            
         LA    R2,ENMHL2H                                                       
         CLI   DELITLIN,2                                                       
         BE    DR127                                                            
         LA    R2,ENMHL3H                                                       
         CLI   DELITLIN,3                                                       
         BE    DR127                                                            
         LA    R2,ENMHL4H                                                       
DR127    ZIC   R1,DELITLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),DELITRAL                                                 
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTEL                                                        
         BE    DR125                                                            
*                                                                               
DR130    L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR140                                                            
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'08'       ATTRIBUTE ELEMENT                            
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     DR140                                                            
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ENMATTR(0),DEINPTXT                                              
         OI    ENMATTRH+6,X'80'                                                 
*                                                                               
DR140    L     R6,AIO                                                           
         MVI   ELCODE,DESGRPLQ     X'88'                                        
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         USING DESGRPD,R6                                                       
         EDIT  (1,DESECGRP),(2,ENMSECG),ALIGN=LEFT                              
*        MVC   ENMSECG,DESECGRP                                                 
         OI    ENMSECGH+6,X'80'                                                 
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* AFTER RECORD CHANGE                                                           
*                                                                               
XRP      L     R4,AIO              A(ENTRY RECORD)                              
         USING DICKEYD,R4                                                       
         LA    R1,KEY                                                           
         MVC   DICKLANG-DICKEY(1,R1),DICLANG                                    
         DROP  R4                                                               
*                                                                               
         GOTO1 WRITE               UPDATE KEY                                   
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              SELECTED RECORD                              
         USING DICKEYD,R4                                                       
*                                                                               
         MVC   ENLDICT,DICCODE     DICTIONARY NAME                              
         OI    ENLDICTH+6,X'80'                                                 
         MVC   ENLENT,DICENTRY     ENTRY NAME                                   
         OI    ENLENTH+6,X'80'                                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* ONLINE LIST                                                                   
*                                                                               
LR       LA    R4,KEY                                                           
         OC    KEY,KEY             TEST FIRST TIME THROUGH                      
         BNZ   LR5                                                              
*                                                                               
         MVI   DICSYS,DICSYSQ      SYSTEM                                       
         MVI   DICKTYP,DICKTYPQ    RECORD TYPE                                  
         MVC   DICCODE,DICTNAME    DICTIONARY NAME                              
         MVC   DICENTRY,ENTRNAME   ENTRY NAME                                   
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR5      CLI   ENLLANGH+5,0        LANGUAGE GIVEN?                              
         BE    LR15                NO                                           
*                                                                               
         LA    R2,ENLLANGH                                                      
         CLI   ENLLANGH+5,3        3-CHAR LANGUAGE CODE?                        
         BE    *+12                                                             
         MVI   ERROR,INVLANG       NO                                           
         B     TRAPERR                                                          
*                                                                               
         CLC   =C'ALL',ENLLANG     'ALL' LANGUAGE?                              
         BNE   *+12                                                             
         MVI   LANGFILT,X'FF'                                                   
         B     LR15                                                             
*                                                                               
         LA    R3,LANGTAB0         A(FIRST ENTRY IN LANGUAGE TABLE)             
         USING LANGTABD,R3                                                      
*                                                                               
LR10     CLC   ENLLANG,LANGSHR     MATCH ON SHORT LANGUAGE NAME?                
         BNE   *+14                                                             
         MVC   LANGFILT,LANGCODE   YES -- SAVE LANGUAGE CODE                    
         B     LR15                                                             
         AH    R3,LANGTAB          LENGTH OF EACH ENTRY                         
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   LR10                                                             
         MVI   ERROR,INVLANG       YES -- INVALID LANGUAGE                      
         B     TRAPERR                                                          
         DROP  R3                                                               
*                                                                               
LR15     GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(20),SAVEKEY     TEST SAME DICTIONARY                         
         BNE   LRX                 NO MORE ENTRIES TO LIST                      
*                                                                               
         CLI   ENLLANGH+5,0        LANGUAGE FILTER GIVEN?                       
         BE    *+14                                                             
         CLC   DICKLANG,LANGFILT   YES -- SAME LANGUAGE?                        
         BNE   LR20                NO -- SKIP THIS ENTRY                        
*                                                                               
         GOTO1 GETREC              GET THE DICTIONARY ENTRY RECORD              
         L     R4,AIO                                                           
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LSTENT,DICENTRY     PUT ENTRY NAME IN LIST LINE                  
*                                                                               
         CLI   DICLANG,X'FF'                                                    
         BNE   *+14                                                             
         MVC   LSTLANG,=C'ALL'     IT'S LANGUAGE 'ALL'                          
         B     LR35                                                             
*                                                                               
         LA    R3,LANGTAB0         A(FIRST ENTRY IN LANGUAGE TABLE)             
         USING LANGTABD,R3                                                      
LR33     CLC   DICLANG,LANGCODE    MATCH ON LANGUAGE CODE?                      
         BE    *+18                                                             
         AH    R3,LANGTAB          LENGTH OF EACH ENTRY                         
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   LR33                                                             
         DC    H'0'                BAD LANGUAGE CODE IN RECORD                  
         MVC   LSTLANG,LANGSHR     SHORT LANGUAGE NAME                          
         DROP  R3                                                               
*                                                                               
LR35     L     R6,AIO                                                           
         MVI   ELCODE,X'02'        DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING DENAMD,R6                                                        
         MVC   LSTDESC,DENAME      PUT DESCRIPTION IN LIST LINE                 
*                                                                               
         MVC   MYWORK,SPACES                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'23'        INPUT LENGTH                                 
         BAS   RE,GETEL                                                         
         BNE   LR40                                                             
         USING DEILD,R6                                                         
         EDIT  (1,DEILEN),(3,MYWORK),ALIGN=LEFT                                 
*                                                                               
LR40     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LR50                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'22'       INPUT TYPE                                   
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     LR50                                                             
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK+4(0),DEINPTXT                                             
*                                                                               
LR50     L     R6,AIO                                                           
         MVI   ELCODE,X'24'        INPUT ROUTINE                                
         BAS   RE,GETEL                                                         
         BNE   LR60                                                             
         USING DEIRD,R6                                                         
         MVC   MYWORK+10(8),DEIROUT                                             
*                                                                               
LR60     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LR65                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'25'       INPUT ARGS                                   
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     LR65                                                             
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK+19(0),DEINPTXT                                            
*                                                                               
LR65     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LR70                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'26'       INPUT OPTIONS                                
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     LR70                                                             
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK+40(0),DEINPTXT                                            
*                                                                               
LR70     GOTO1 SQUASHER,DMCB,MYWORK,L'MYWORK                                    
         MVC   LSTINP,MYWORK                                                    
*                                                                               
         MVC   MYWORK,SPACES                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'33'        OUTPUT LENGTH                                
         BAS   RE,GETEL                                                         
         BNE   LR80                                                             
         USING DEOLD,R6                                                         
         EDIT  (1,DEOLEN),(3,MYWORK),ALIGN=LEFT                                 
*                                                                               
LR80     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LR90                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'32'       OUTPUT TYPE                                  
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     LR90                                                             
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK+4(0),DEINPTXT                                             
*                                                                               
LR90     L     R6,AIO                                                           
         MVI   ELCODE,X'34'        OUTPUT ROUTINE                               
         BAS   RE,GETEL                                                         
         BNE   LR100                                                            
         USING DEORD,R6                                                         
         MVC   MYWORK+9(8),DEOROUT                                              
*                                                                               
LR100    L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LR105                                                            
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'35'       OUTPUT ARGS                                  
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     LR105                                                            
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK+18(0),DEINPTXT                                            
*                                                                               
LR105    L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LR110                                                            
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'36'       OUTPUT OPTIONS                               
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     LR110                                                            
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK+39(0),DEINPTXT                                            
*                                                                               
LR110    GOTO1 SQUASHER,DMCB,MYWORK,L'MYWORK                                    
         MVC   LSTOUT,MYWORK                                                    
*                                                                               
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* OFFLINE REPORT                                                                
*                                                                               
PR       CLI   ENPSUMM,C'Y'                                                     
         BE    PS                                                               
         STM   RE,RC,12(RD)        ACQUIRE SOME STORAGE                         
         LR    R1,RD                                                            
         A     R1,=A(((SECSORTL+7)/8)*8)   THIS LONG (D'WORD ALIGNED)           
         ST    RD,76(R1)                                                        
         LA    RF,72(RD)                                                        
         ST    RF,ASECSORT         SAVE ITS ADDRESS                             
         MVC   0(4,RD),=C'+PR+'                                                 
         LA    RF,72(R1)                                                        
         ST    RF,8(RD)                                                         
         LR    RD,RF                                                            
*                                                                               
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   SORTSW,0                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVI   DICSYS,DICSYSQ      SYSTEM                                       
         MVI   DICKTYP,DICKTYPQ    RECORD TYPE                                  
         MVC   DICCODE,DICTNAME    DICTIONARY NAME                              
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         CLI   ENPLANGH+5,0        LANGUAGE GIVEN?                              
         BE     PR15               NO                                           
*                                                                               
         LA    R2,ENPLANGH                                                      
         CLI   ENPLANGH+5,3        3-CHAR LANGUAGE CODE?                        
         BE    *+12                                                             
         MVI   ERROR,INVLANG       NO                                           
         B     TRAPERR                                                          
*                                                                               
         CLC   =C'ALL',ENPLANG     'ALL' LANGUAGE?                              
         BNE   *+12                                                             
         MVI   LANGFILT,X'FF'                                                   
         B     PR15                                                             
*                                                                               
         LA    R3,LANGTAB0         A(FIRST ENTRY IN LANGUAGE TABLE)             
         USING LANGTABD,R3                                                      
*                                                                               
PR10     CLC   ENPLANG,LANGSHR     MATCH ON SHORT LANGUAGE NAME?                
         BNE   *+14                                                             
         MVC   LANGFILT,LANGCODE   YES -- SAVE LANGUAGE CODE                    
         B     PR15                                                             
         AH    R3,LANGTAB          LENGTH OF EACH ENTRY                         
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   PR10                                                             
         MVI   ERROR,INVLANG       YES -- INVALID LANGUAGE                      
         B     TRAPERR                                                          
         DROP  R3                                                               
*                                                                               
PR15     GOTO1 HIGH                FIRST RECORD                                 
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(2),SAVEKEY      TEST ENTRY RECORD                            
         BE    PR32                                                             
         CLI   ENPLANGH+5,0                                                     
         BE    PR36                IF SORTING, NOW GET SORTED RECS              
         B     PRX                 NO MORE ENTRIES TO LIST                      
*                                                                               
PR32     OC    SAVEKEY+12(8),SAVEKEY+12    TEST DICTIONARY WAS ENTERED          
         BZ    PR34                IT WAS NOT                                   
         CLC   DICCODE,SAVEKEY+12  TEST DICTIONARY MATCH                        
         BE    PR34                                                             
         CLI   ENPLANGH+5,0                                                     
         BE    PR36                IF SORTING, NOW GET SORTED RECS              
         B     PRX                                                              
*                                                                               
PR34     CLI   ENPLANGH+5,0        LANGUAGE FILTER GIVEN?                       
         BE    *+14                                                             
         CLC   DICKLANG,LANGFILT   YES -- SAME LANGUAGE?                        
         BNE   PR20                NO -- SKIP THIS ENTRY                        
*                                                                               
         CLC   DICCODE,DICTNAME    TEST CHANGE OF DICTIONARY                    
         BE    *+14                                                             
         MVI   FORCEHED,C'Y'       EJECT PAGE ON CHANGE OF DICTIONARY           
         MVC   DICTNAME,DICCODE                                                 
*                                                                               
         GOTO1 GETREC                                                           
         CLI   ENPLANGH+5,0        TEST ALL LANGUAGES                           
         BNE   PR42                                                             
         CLI   SORTSW,0            TEST SORT INIT'D                             
         BNE   PR35                                                             
         LA    RF,SORTCARD         DEFAULT SORT SEQUENCE                        
         CLC   =C'KEY',REMUSER     IF USER IS KEY                               
         BNE   *+8                                                              
         LA    RF,SORTCRD1            SORT BY ENTRY                             
         GOTO1 SORTER,DMCB,(RF),SORTRECD,0                                      
         MVI   SORTSW,1            SET SORT INITIALISED                         
PR35     LA    R4,KEY                                                           
         MVC   SLANG,DICKLANG      BUILD SORT RECORD                            
         MVC   SENTRY,DICENTRY                                                  
         MVC   SDADR,DICDA                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SNAME,DENAME-DENAMD(R6)                                          
         GOTO1 SORTER,DMCB,SORTPUT,SREC                                         
         B     PR20                                                             
*                                                                               
PR36     CLI   SORTSW,0            NOTHING SORTED                               
         BE    PRX                                                              
         BAS   RE,RESORT           GET SORTED RECS BACK, DO 2ND SORT            
*                                                                               
PR38     L     RF,ASECSORT                                                      
         LA    RF,4(RF)                                                         
PR40     CLI   0(RF),255                                                        
         BE    PRX                 END OF SECSAREA                              
         OC    0(4,RF),0(RF)       TEST NO ADDRESS                              
         BZ    *+12                                                             
         CLI   0(RF),X'40'         TEST SECSENT                                 
         BL    *+12                NO                                           
         LA    RF,4(RF)                                                         
         B     PR40                                                             
         ST    RF,ASECSORT                                                      
         LA    R4,KEY                                                           
         MVC   DICDA,0(RF)         SET DISK ADDRESS FOR GETREC                  
         GOTO1 GETREC              GET THE DICTIONARY ENTRY RECORD              
*                                                                               
         L     R4,AIO                                                           
*&&UK                                                                           
         CLI   DICLANG,2           TEST LANG ENG/EUS ETC                        
         BNH   *+12                                                             
         CLI   DICLANG,255         OR 'ALL'                                     
         BL    PR42                                                             
         ICM   RF,15,ABOX          TEST BOXES IN USE                            
         BZ    PR42                                                             
         MVI   BOXREQ-BOXD(RF),C'B'  DO BOX LINE                                
*&&                                                                             
         GOTO1 SPOOL,DMCB,(R8)     OR BLANK (US)                                
*                                                                               
PR42     L     R4,AIO                                                           
         MVC   P,SPACES            CLEAR THE PRINT LINE                         
         MVC   PRTENT,DICENTRY     PUT ENTRY NAME IN PRINT LINE                 
*                                                                               
         CLI   DICLANG,X'FF'                                                    
         BNE   *+14                                                             
         MVC   PRTLANG,=C'ALL'     IT'S LANGUAGE 'ALL'                          
         B     PR46                                                             
*                                                                               
         LA    R3,LANGTAB0         A(FIRST ENTRY IN LANGUAGE TABLE)             
         USING LANGTABD,R3                                                      
PR44     CLC   DICLANG,LANGCODE    MATCH ON LANGUAGE CODE?                      
         BE    *+18                                                             
         AH    R3,LANGTAB          LENGTH OF EACH ENTRY                         
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   PR44                                                             
         DC    H'0'                BAD LANGUAGE CODE IN RECORD                  
         MVC   PRTLANG,LANGSHR     SHORT LANGUAGE NAME                          
         DROP  R3                                                               
*                                                                               
PR46     L     R6,AIO                                                           
         MVI   ELCODE,X'02'        DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING DENAMD,R6                                                        
         MVC   PRTDESC,DENAME      PUT DESCRIPTION IN PRINT LINE                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'23'        INPUT LENGTH                                 
         BAS   RE,GETEL                                                         
         BNE   PR48                                                             
         USING DEILD,R6                                                         
         EDIT  (1,DEILEN),(3,PRTILEN)                                           
*                                                                               
PR48     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   PR50                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'22'       INPUT TYPE                                   
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     PR50                                                             
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTITYP(0),DEINPTXT                                              
*                                                                               
PR50     L     R6,AIO                                                           
         MVI   ELCODE,X'24'        INPUT ROUTINE                                
         BAS   RE,GETEL                                                         
         BNE   PR60                                                             
         USING DEIRD,R6                                                         
         MVC   PRTIRTN,DEIROUT                                                  
*                                                                               
PR60     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   PR70                                                             
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'25'       INPUT ARGS                                   
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     PR70                                                             
         XC    WORK,WORK                                                        
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),DEINPTXT                                               
         LA    R1,1(R1)                                                         
         STC   R1,WORK+5                                                        
         AH    R1,=H'8'                                                         
         STC   R1,WORK                                                          
         CLI   WORK+8,C'L'                                                      
         BNE   *+14                                                             
         MVC   PRTIARG,WORK+8                                                   
         B     PR70                                                             
         LA    RF,SCANBLK                                                       
         LA    R0,8                                                             
         XC    0(74,RF),0(RF)                                                   
         LA    RF,74(RF)                                                        
         BCT   R0,*-10                                                          
         GOTO1 SCANNER,DMCB,(10,WORK),(4,SCANBLK),C',=/ '                       
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    RF,SCANBLK                                                       
         LA    RE,PRTIARG                                                       
PR65     ZIC   R1,0(RF)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),12(RF)                                                   
         LA    RF,32(RF)                                                        
         LA    RE,132(RE)                                                       
         CLI   0(RF),0                                                          
         BNE   PR65                                                             
*                                                                               
PR70     L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   PR170                                                            
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'26'       INPUT OPTIONS                                
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     PR170                                                            
         XC    WORK,WORK                                                        
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),DEINPTXT                                               
         LA    R1,1(R1)                                                         
         STC   R1,WORK+5                                                        
         AH    R1,=H'8'                                                         
         STC   R1,WORK                                                          
         LA    RF,SCANBLK                                                       
         LA    R0,8                                                             
         XC    0(74,RF),0(RF)                                                   
         LA    RF,74(RF)                                                        
         BCT   R0,*-10                                                          
         GOTO1 SCANNER,DMCB,(10,WORK),(4,SCANBLK),C',=, '                       
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    RF,SCANBLK                                                       
         LA    RE,PRTIOPT                                                       
PR75     MVC   0(L'PRTIOPT,RE),12(RF)                                           
         LA    RF,32(RF)                                                        
         LA    RE,132(RE)                                                       
         CLI   0(RF),0                                                          
         BNE   PR75                                                             
*                                                                               
PR170    L     R6,AIO                                                           
         MVI   ELCODE,X'33'        OUTPUT LENGTH                                
         BAS   RE,GETEL                                                         
         BNE   PR180                                                            
         USING DEOLD,R6                                                         
         EDIT  (1,DEOLEN),(3,PRTOLEN)                                           
*                                                                               
PR180    L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   PR190                                                            
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'32'       OUTPUT TYPE                                  
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     PR190                                                            
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTOTYP(0),DEINPTXT                                              
*                                                                               
PR190    L     R6,AIO                                                           
         MVI   ELCODE,X'34'        OUTPUT ROUTINE                               
         BAS   RE,GETEL                                                         
         BNE   PR200                                                            
         USING DEORD,R6                                                         
         MVC   PRTORTN,DEOROUT                                                  
*                                                                               
PR200    L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   PR210                                                            
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'35'       OUTPUT ARGS                                  
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     PR210                                                            
         XC    WORK,WORK                                                        
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),DEINPTXT                                               
         LA    R1,1(R1)                                                         
         STC   R1,WORK+5                                                        
         AH    R1,=H'8'                                                         
         STC   R1,WORK                                                          
         CLI   WORK+8,C'L'                                                      
         BNE   *+14                                                             
         MVC   PRTOARG,WORK+8                                                   
         B     PR210                                                            
         LA    RF,SCANBLK                                                       
         LA    R0,8                                                             
         XC    0(74,RF),0(RF)                                                   
         LA    RF,74(RF)                                                        
         BCT   R0,*-10                                                          
         GOTO1 SCANNER,DMCB,(10,WORK),(4,SCANBLK),C',=/ '                       
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    RF,SCANBLK                                                       
         LA    RE,PRTOARG                                                       
PR205    ZIC   R1,0(RF)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),12(RF)                                                   
         LA    RF,32(RF)                                                        
         LA    RE,132(RE)                                                       
         CLI   0(RF),0                                                          
         BNE   PR205                                                            
*                                                                               
PR210    L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   PR220                                                            
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'36'       OUTPUT OPTIONS                               
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     PR220                                                            
         XC    WORK,WORK                                                        
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),DEINPTXT                                               
         LA    R1,1(R1)                                                         
         STC   R1,WORK+5                                                        
         AH    R1,=H'8'                                                         
         STC   R1,WORK                                                          
         LA    RF,SCANBLK                                                       
         LA    R0,8                                                             
         XC    0(74,RF),0(RF)                                                   
         LA    RF,74(RF)                                                        
         BCT   R0,*-10                                                          
         GOTO1 SCANNER,DMCB,(10,WORK),(4,SCANBLK),C',=, '                       
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    RF,SCANBLK                                                       
         LA    RE,PRTOOPT                                                       
PR215    MVC   0(L'PRTOOPT,RE),12(RF)                                           
         LA    RF,32(RF)                                                        
         LA    RE,132(RE)                                                       
         CLI   0(RF),0                                                          
         BNE   PR215                                                            
*                                                                               
PR220    L     R6,AIO                                                           
         MVI   ELCODE,X'87'        HEAD LINES 1,2,3,4                           
         BAS   RE,GETEL                                                         
         BNE   PR240                                                            
         USING DELITD,R6                                                        
PR225    LA    R2,PRTHEAD                                                       
         CLI   DELITLIN,1                                                       
         BE    PR227                                                            
         LA    R2,PRTHEAD+132                                                   
         CLI   DELITLIN,2                                                       
         BE    PR227                                                            
         LA    R2,PRTHEAD+264                                                   
         CLI   DELITLIN,3                                                       
         BE    PR227                                                            
         LA    R2,PRTHEAD+396                                                   
PR227    MVC   WORK,SPACES                                                      
         ZIC   R1,DELITLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),DELITRAL                                                 
         MVC   0(24,R2),WORK                                                    
         BAS   RE,NEXTEL                                                        
         BE    PR225                                                            
*                                                                               
PR240    L     R6,AIO                                                           
         MVI   ELCODE,X'82'        HEADS ROUTINE                                
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   PRTHFLAG,C'*'                                                    
*                                                                               
PR250    L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INPUT FIELD ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   PR260                                                            
         USING DEINPD,R6                                                        
         CLI   DEINPOP,X'08'       ATTRIBUTE ELEMENT                            
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     PR260                                                            
         MVC   PRTDESC+132,=C'ATT='                                             
         XC    WORK,WORK                                                        
         ZIC   R1,DEINPLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),DEINPTXT                                                 
         LA    R2,1(R1)                                                         
         XC    DMCB,DMCB                                                        
         GOTO1 CHOPPER,DMCB,((R2),WORK),(16,PRTDESC+136),(C'P',3)               
*****    OC    DMCB+8(4),DMCB+8                                                 
*****    BNZ   *+6                                                              
*****    DC    H'0'                                                             
*                                                                               
PR260    CLI   ENPLANGH+5,0        DON'T DOUBLE SPACE IF ALL LANGS              
         BE    *+8                                                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)     SEND PRINT LINE(S) TO PRINTER                
         MVI   SPACING,1                                                        
         CLI   ENPLANGH+5,0                                                     
         BE    PR38                NEXT SORT RECORD                             
         B     PR20                NEXT ENTRY RECORD                            
*                                                                               
PRX      L     RD,4(RD)            RECOVER SAVE AREA                            
         LM    RE,RC,12(RD)                                                     
         B     EXIT                                                             
         EJECT                                                                  
*              SUMMARY REPORT                                                   
         SPACE 3                                                                
PS       LA    R1,SUMSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         CLI   ENPDISK,C'Y'                                                     
         BNE   PS2                                                              
         OPEN  (DISKOUT,(OUTPUT))                                               
*                                                                               
PS2      DS    0H                                                               
         CLI   ENPDISK,C'2'                                                     
         BNE   PS2A                                                             
         OPEN  (DISKOUT2,(OUTPUT))                                              
*                                                                               
PS2A     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVI   DICSYS,DICSYSQ      SYSTEM                                       
         MVI   DICKTYP,DICKTYPQ    RECORD TYPE                                  
         MVC   DICCODE,DICTNAME    DICTIONARY NAME                              
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                FIRST RECORD                                 
         B     PS30                                                             
*                                                                               
PS20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
PS30     CLC   KEY(2),SAVEKEY      TEST ENTRY RECORD                            
         BNE   PSX                 NO MORE ENTRIES TO LIST                      
*                                                                               
         OC    SAVEKEY+12(8),SAVEKEY+12    TEST DICTIONARY WAS ENTERED          
         BZ    *+14                IT WAS NOT                                   
         CLC   DICCODE,SAVEKEY+12  TEST DICTIONARY MATCH                        
         BNE   PSX                                                              
*                                                                               
         CLC   DICCODE,DICTNAME    TEST CHANGE OF DICTIONARY                    
         BE    *+14                                                             
         MVI   FORCEHED,C'Y'       EJECT PAGE ON CHANGE OF DICTIONARY           
         MVC   DICTNAME,DICCODE                                                 
*                                                                               
         GOTO1 GETREC              GET THE DICTIONARY ENTRY RECORD              
         L     R4,AIO                                                           
*                                                                               
         MVC   P,SPACES            CLEAR THE PRINT LINE                         
         XC    FILEOUT,FILEOUT     INIT FILEOUT AREA                            
         MVC   PRSENT,DICENTRY     PUT ENTRY NAME IN PRINT LINE                 
*                                                                               
         CLI   ENPDISK,C'2'        IF SECOND DISK OPTION                        
         BNE   *+18                                                             
         LA    R1,FILEOUT          ESTABLISH FILE OUTPUT AREA                   
         USING FILEOUTD,R1                                                      
         MVC   FILENT,DICENTRY        PUT ENTRY NAME TO OUTPUT                  
         MVI   FILENTC,C'\'           SET DELIMITER                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
*                                                                               
         USING DENAMD,R6                                                        
         CLI   DENAMLEN,DENAMDLQ   IS THIS A SHORT DESCRIPTION?                 
         BNE   *+14                                                             
         MVC   PRSDESC(L'DENAME),DENAME                                         
         B     *+10                                                             
         MVC   PRSDESC,DENAMEEX    NO -- IT'S LONG                              
*                                                                               
         CLI   ENPDISK,C'2'        IF SECOND DISK OPTION                        
         BNE   PS31                                                             
*                                                                               
         LA    R1,FILEOUT          ESTABLISH FILE OUTPUT AREA                   
         USING FILEOUTD,R1                                                      
*                                                                               
         CLI   DENAMLEN,DENAMDLQ   IS THIS A SHORT DESCRIPTION?                 
         BNE   *+14                                                             
         MVC   FILDESC(L'DENAME),DENAME                                         
         B     *+10                                                             
         MVC   FILDESC,DENAMEEX    NO -- IT'S LONG                              
*                                                                               
         MVI   FILDESCC,C'\'       SET DELIMITER                                
*                                                                               
PS31     DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'33'        OUTPUT LENGTH                                
         BAS   RE,GETEL                                                         
         BNE   PS40                                                             
*                                                                               
         USING DEOLD,R6                                                         
         EDIT  (1,DEOLEN),(4,PRSOLEN)                                           
*                                                                               
         CLI   ENPDISK,C'2'        IF SECOND DISK OPTION                        
         BNE   PS40                                                             
*                                                                               
         LA    R1,FILEOUT          ESTABLISH FILE OUTPUT AREA                   
         USING FILEOUTD,R1                                                      
*                                                                               
         EDIT  (1,DEOLEN),(4,FILOLEN)                                           
         MVI   FILOLENC,C'\'           SET DELIMITER                            
*                                                                               
PS40     DS    0H                                                               
*                                                                               
         CLI   ENPDISK,C'2'        IF SECOND DISK OPTION                        
         BNE   PS100                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'87'        COLUMN HEADLINE                              
         BAS   RE,GETEL                                                         
*                                                                               
         LA    R1,FILEOUT          ESTABLISH FILE OUTPUT AREA                   
         USING FILEOUTD,R1                                                      
*                                                                               
         SR    RF,RF                                                            
*                                                                               
PSHDLOOP DS    0H                                                               
*                                                                               
         BNE   PSHDDONE            END OF HEADLINES                             
*                                                                               
         USING DELITD,R6           ESTABLISH LITERAL ELEMENT                    
*                                                                               
         IC    RF,DELITLEN         GET ELEMENT LENGTH                           
         AHI   RF,-(DELITRAL-DELITD+1) GET LITERAL EXECUTE LENGTH               
         BM    PSHDCONT            NO HEADLINE AVAILABLE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FILHD1(0),DELITRAL  MOVE LITERAL TO OUTPUT                       
*                                                                               
         MVI   FILHD1C,C'\'        SET DELIMITER                                
         LA    R1,L'FILHD1+1(R1)   BUMP TO NEXT HEADLINE AREA                   
*                                                                               
PSHDCONT DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           GET NEXT HEADLINE                            
         B     PSHDLOOP                                                         
*                                                                               
PSHDDONE DS    0H                                                               
*                                                                               
PS100    DS    0H                                                               
*                                                                               
PS180    CLI   ENPDISK,C'Y'        OPEN TO OUTPUT A DISK FILE                   
         BNE   PS200                                                            
         PUT   DISKOUT,P                                                        
         SPACE 1                                                                
PS200    DS    0H                                                               
         CLI   ENPDISK,C'2'        IF SECOND DISK OPTION                        
         BNE   PS250                                                            
*                                  PUT OUT RECORD                               
*                                  COLLAPSE RECORD                              
         LA    R0,L'FILEOUT        OUTPUT RECORD MAX LENGTH                     
         LA    RE,FILEOUT          START OF OUTPUT RECORD                       
         LA    RF,FILEOUT                                                       
*                                                                               
PSCOLLP  DS    0H                                                               
*                                                                               
         CLI   0(RF),0             SKIP NULLS                                   
         BNE   *+12                                                             
         MVI   0(RF),C' '             ERASE NULL                                
         B     PSCOLCN                                                          
*                                                                               
         MVC   0(1,RE),0(RF)         KEEP NON-NULLS                             
*                                                                               
         CR    RE,RF               IF RF AHEAD OF RE                            
         BE    *+8                                                              
         MVI   0(RF),C' '             ERASE MOVED CHARACTER                     
*                                                                               
         AHI   RE,1                BUMP POINTER                                 
*                                                                               
PSCOLCN  DS    0H                                                               
*                                                                               
         LA    RF,1(RF)            BUMP POINTER                                 
         BCT   R0,PSCOLLP          NEXT CHARACTER                               
*                                                                               
PSCOLDN  DS    0H                                                               
*                                                                               
         PUT   DISKOUT2,FILEOUT    WRITE RECORD                                 
*                                                                               
PS250    DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)     SEND PRINT LINE(S) TO PRINTER                
         B     PS20                NEXT ENTRY RECORD                            
*                                                                               
PSX      CLI   ENPDISK,C'Y'                                                     
         BNE   PSX1                                                             
         CLOSE (DISKOUT)                                                        
PSX1     DS    0H                                                               
         CLI   ENPDISK,C'2'        IF SECOND DISK OPTION                        
         BNE   PSX2                                                             
PSX2     DS    0H                                                               
         CLOSE (DISKOUT2)                                                       
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* HANDLE INPUT FIELD ELEMENTS                                                   
*                                                                               
INPUTEL  NTR1                                                                   
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'04',AIO),(1,ELTYPE)              
*                                                                               
         XC    ELEM,ELEM                                                        
         USING DEINPD,R6                                                        
         MVI   DEINPEL,X'04'                                                    
         ZIC   R1,5(R2)                                                         
         AH    R1,=H'3'                                                         
         STC   R1,DEINPLEN                                                      
         MVC   DEINPOP,ELTYPE                                                   
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DEINPTXT(0),8(R2)                                                
         GOTO1 ADDELEM                                                          
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* BLANK OUT ALL INPUT FIELDS                                                    
*                                                                               
CLEAR    NTR1                                                                   
*                                                                               
         LA    RF,ENMTAGH          LAST FIELD ON SCREEN                         
         LA    R2,ENMDESCH         FIRST FIELD HEADER                           
*                                                                               
CLEAR10  ZIC   R3,0(R2)            LENGTH OF FIELD + HEADER                     
         SH    R3,=H'9'            MINUS HEADER LENGTH AND 1 FOR EX             
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R3,=H'8'            MINUS LENGTH OF EXTENSION                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
CLEAR20  ZIC   R3,0(R2)            RESTORE LENGTH                               
         LA    R2,0(R2,R3)         NEXT SCREEN FIELD                            
         CR    R2,RF               TEST END OF SCREEN                           
         BE    CLEARX                                                           
*                                                                               
         TM    1(R2),X'20'         TEST FIELD IS PROTECTED                      
         BZ    CLEAR10             IT IS NOT -- CLEAR IT                        
         B     CLEAR20             IT IS -- TRY NEXT FIELD                      
*                                                                               
CLEARX   B     EXIT                                                             
         EJECT                                                                  
ERR      XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(10),=C'* ERROR * '                                       
         MVC   CONHEAD+10(40),DBERRMSG                                          
         MVC   CONHEAD+55(2),=C' *'                                             
         GOTO1 SQUASHER,DMCB,CONHEAD,60                                         
         GOTO1 ERREX2                                                           
*                                                                               
MAX8OPT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MAX8OPTM),MAX8OPTM                                     
         GOTO1 ERREX2                                                           
MAX8OPTM DC    C'* ERROR * MAXIMUM OF 8 OPTIONS *'                              
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
*        GET RECORDS FROM SORTER, WRITE INTO SECSAREA, AND SORT AGAIN           
*        SECONDARY SORT RECORD FORMAT: DICENTRY,DICDA(1),...,DICDA(N)           
*                                                                               
RESORT   NTR1                                                                   
         L     RE,ASECSORT         CLEAR SECONDARY SORT AREA                    
         L     RF,=A(SECSORTL-4)                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         JO    *+2                 DIE ON OVERLAPPING MVCL                      
         BCTR  RE,0                                                             
         MVI   0(RE),255           SET END MARKER                               
*                                                                               
         L     R3,ASECSORT                                                      
SEC      USING SECSRECD,R3                                                      
RES2     GOTO1 SORTER,DMCB,SORTGET                                              
         ICM   R1,15,4(R1)                                                      
         BZ    RES6                END OF PRIMARY SORTED RECS                   
         MVC   SREC,0(R1)                                                       
         CLC   SAVSNAME,SNAME      TEST DIFFERENT ENTRY                         
         BE    RES4                NO - USE EXISTING RECORD IF POSSIBLE         
RES3     LA    R3,SECSRECL(R3)     YES - START A NEW RECORD                     
         CLI   SEC.SECSENT,255     END OF AVAILABLE SORT AREA?                  
         BE    RES6                YES - SORT AND EXIT                          
         MVC   SEC.SECSENT,SENTRY  NEW RECORD - SET DICENTRY                    
         MVC   SEC.SECSDADS,SDADR  AND FIRST DISK ADDRESS                       
         MVC   SAVSNAME,SNAME      SAVE NAME                                    
         B     RES2                                                             
*                                                                               
RES4     LA    RF,SEC.SECSDADS+4                                                
         LA    R0,ACTLANGS-1                                                    
         OC    0(4,RF),0(RF)       FIND NEXT FREE ADDR SLOT                     
         BZ    *+16                                                             
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
         B     RES3                ENTRY WITHOUT ENG/EUS EQUIVALENT             
         MVC   0(4,RF),SDADR       STORE DISK ADDRESS                           
         B     RES2                                                             
         DROP  SEC                                                              
*                                                                               
RES6     SR    R0,R0               CALC NO. OF ENTRIES USED                     
         LR    R1,R3                                                            
         L     R3,ASECSORT                                                      
         SR    R1,R3                                                            
         LA    R3,SECSRECL                                                      
         DR    R0,R3                                                            
         LA    R0,1(R1)                                                         
         C     R0,=A(MAX_ENTRY_RECS)                                            
         BNH   *+6                                                              
         DC    H'0'                INCREASE MAX_ENTRY_RECS                      
         GOTO1 XSORT,DMCB,ASECSORT,(R0),SECSRECL,L'SECSENT,0                    
         B     EXIT                                                             
         EJECT                                                                  
* HEADLINE ROUTINES                                                             
*                                                                               
HOOK     NTR1                                                                   
*                                                                               
         MVC   H4+13(8),DICTNAME                                                
*                                                                               
         L     R4,ABOX             BOXES, IF AROUND                             
         LTR   R4,R4                                                            
         BZ    EXIT                                                             
         USING BOXD,R4                                                          
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         LA    R2,BOXCOLS                                                       
         CLI   ENPSUMM,C'Y'                                                     
         BE    HOOK2                                                            
         MVI   BOXROWS+9,C'M'                                                   
         MVI   0(R2),C'L'                                                       
         MVI   9(R2),C'C'                                                       
         MVI   13(R2),C'C'                                                      
         MVI   34(R2),C'C'                                                      
         MVI   69(R2),C'C'                                                      
         MVI   105(R2),C'C'                                                     
         MVI   131(R2),C'R'                                                     
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
HOOK2    MVI   BOXROWS+8,C'M'                                                   
         MVI   24(R2),C'L'                                                      
         MVI   33(R2),C'C'                                                      
         MVI   94(R2),C'C'                                                      
         MVI   100(R2),C'R'                                                     
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
HEDSPECS SSPEC H1,1,C'DRIVER SYSTEM'                                            
         SSPEC H2,1,C'-------------'                                            
         SSPEC H1,51,C'DICTIONARY REPORT'                                       
         SSPEC H2,51,C'-----------------'                                       
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H4,1,C'DICTIONARY -'                                             
         SSPEC H8,2,C'ENTRY    LNG DESCRIPTION'                                 
         SSPEC H8,50,C'INPUT'                                                   
         SSPEC H8,85,C'OUTPUT'                                                  
         SSPEC H8,107,C'HEADINGS'                                               
         SSPEC H9,36,C'LEN TYPE  ROUTINE  ARGS  OPTIONS    '                    
         SSPEC H9,71,C'LEN TYPE ROUTINE  ARGS  OPTIONS      '                   
         DC    X'00'                                                            
*                                                                               
SUMSPECS SSPEC H1,1,C'DRIVER SYSTEM'                                            
         SSPEC H2,1,C'-------------'                                            
         SSPEC H1,51,C'DICTIONARY REPORT'                                       
         SSPEC H2,51,C'-----------------'                                       
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H4,1,C'DICTIONARY -'                                             
         SSPEC H8,26,C'ENTRY    DESCRIPTION'                                    
         SSPEC H8,96,C'WIDTH'                                                   
         DC    X'00'                                                            
         SPACE 1                                                                
DISKOUT  DCB   DDNAME=DISKOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FB,BLKSIZE=2640,LRECL=132                                  
DISKOUT2 DCB   DDNAME=DISKOUT2,DSORG=PS,MACRF=(PM),                    X        
               RECFM=FB,BLKSIZE=2550,LRECL=255                                  
         SPACE 1                                                                
RELO     DS    F                   RELOCATION FACTOR                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
EXIT     XIT1                                                                   
         SPACE 3                                                                
SORTCARD DC    C'SORT FIELDS=(1,21,A),FORMAT=BI,WORK=1 '                        
SORTCRD1 DC    C'SORT FIELDS=(26,8,A,1,21,A),FORMAT=BI,WORK=1 '                 
SORTRECD DC    C'RECORD TYPE=F,LENGTH=33 '                                      
SORTPUT  DC    C'PUT'                                                           
SORTGET  DC    C'GET'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DRIVALBLKD                                                     
         EJECT                                                                  
       ++INCLUDE FALANGTAB                                                      
         EJECT                                                                  
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE CTGENDIC                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMF3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFME3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMD3D                                                       
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
SYSSAVL  EQU   6144                L'SAVED STORAGE                              
         ORG   SYSSPARE                                                         
ADRIVAL  DS    A                   A(DRIVAL)                                    
DICTNAME DS    CL8                 DICTIONARY NAME                              
ENTRNAME DS    CL8                 ENTRY NAME                                   
SAVEKEY  DS    XL32                GENFILE KEY                                  
ELTYPE   DS    XL1                                                              
MYWORK   DS    CL90                                                             
SCANBLK  DS    8XL74               SCANNER BLOCK                                
SCANOFLO DS    XL74                SCANNER BLOCK OVERFLOW                       
LANGFILT DS    X                   LANGUAGE CODE FILTER                         
SREC     DS    0XL33               PRIMARY SORT RECORD (FOR PRINT)              
SNAME    DS    CL(L'DENAME)                                                     
SLANG    DS    XL1                                                              
SDADR    DS    XL4                                                              
SENTRY   DS    CL(L'DICENTRY)                                                   
*                                                                               
SAVSNAME DS    CL(L'DENAME)                                                     
SORTSW   DS    XL1                                                              
*                                                                               
ASECSORT DS    A                   A(SECONDARY SORT AREA)                       
*                                                                               
SECSRECD DS    0X                  SECONDARY SORT RECORD                        
SECSENT  DS    CL(L'DICENTRY)                                                   
SECSDADS DS    (ACTLANGS)XL4                                                    
SECSRECL EQU   *-SECSRECD                                                       
ACTLANGS EQU   4                   NO. OF LANGUAGES ACTIVE IN DICTS +1          
MAX_ENTRY_RECS EQU 2000                                                         
SECSORTL EQU   (MAX_ENTRY_RECS)*SECSRECL+1     L'SECONDARY SORT AREA            
FILEOUT  DS    CL255               DISKOUT2 RECORD AREA                         
         SPACE 5                                                                
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTENT   DS    CL8                                                              
         DS    C                                                                
LSTLANG  DS    CL3                                                              
         DS    CL2                                                              
LSTDESC  DS    CL20                                                             
         DS    C                                                                
LSTINP   DS    CL19                                                             
         DS    C                                                                
LSTOUT   DS    CL19                                                             
         SPACE 5                                                                
* OFFLINE REPORT DETAIL LINE                                                    
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL1                                                              
PRTENT   DS    CL8                                                              
         DS    CL1                                                              
PRTLANG  DS    CL3                                                              
         DS    CL1                                                              
PRTDESC  DS    CL20                                                             
         DS    CL1                                                              
PRTILEN  DS    CL3                                                              
         DS    CL1                                                              
PRTITYP  DS    CL5                                                              
         DS    CL1                                                              
PRTIRTN  DS    CL8                                                              
         DS    CL1                                                              
PRTIARG  DS    CL5                                                              
         DS    CL1                                                              
PRTIOPT  DS    CL9                                                              
         DS    CL1                                                              
PRTOLEN  DS    CL3                                                              
         DS    CL1                                                              
PRTOTYP  DS    CL4                                                              
         DS    CL1                                                              
PRTORTN  DS    CL8                                                              
         DS    CL1                                                              
PRTOARG  DS    CL5                                                              
         DS    CL1                                                              
PRTOOPT  DS    CL11                                                             
         DS    CL1                                                              
PRTHEAD  DS    CL24                                                             
PRTHFLAG DS    CL1                                                              
         SPACE 2                                                                
         ORG   P                                                                
         DS    CL25                                                             
PRSENT   DS    CL8                                                              
         DS    CL1                                                              
PRSDESC  DS    CL60                                                             
         DS    CL1                                                              
PRSOLEN  DS    CL5                                                              
         DS    CL1                                                              
PRSATTR  DS    CL20                                                             
*                                                                               
FILEOUTD DSECT LAYOUT OF SECOND DISK OUTPUT                                     
FILENT   DS    CL8                 ENTRY KEYWORD                                
FILENTC  DS    CL1                 DELIMITER                                    
FILDESC  DS    CL60                DESCRIPTION                                  
FILDESCC DS    CL1                 DELIMITER                                    
FILOLEN  DS    CL5                 OUTPUT LENGTH                                
FILOLENC DS    CL1                 DELIMITER                                    
FILHD1   DS    CL25                HEADLINE 1                                   
FILHD1C  DS    CL1                 DELIMITER                                    
FILHD2   DS    CL25                HEADLINE 2                                   
FILHD2C  DS    CL1                 DELIMITER                                    
FILHD3   DS    CL25                HEADLINE 3                                   
FILHD3C  DS    CL1                 DELIMITER                                    
FILHD4   DS    CL25                HEADLINE 4                                   
FILHD4C  DS    CL1                 DELIMITER                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054CTSFM03   01/29/15'                                      
         END                                                                    
