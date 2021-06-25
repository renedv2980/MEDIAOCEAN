*          DATA SET REATHNACR  AT LEVEL 209 AS OF 02/05/07                      
*PHASE REATNCRA,*                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE BUFFAHI                                                                
*INCLUDE CARDS                                                                  
*INCLUDE COVAIL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE REPVALMN                                                               
*INCLUDE DMDMGRL                                                                
         TITLE 'REATHNACR - REPPAK ATHENA FILE CREATE'                          
**********************************************************************          
*  HISTORY OF CHANGES                                                *          
**********************************************************************          
*  18FEB/93 (BU ) --- UPGRADE UTL ACCESS FOR > 1 REP SYSTEM #        *          
*                                                                    *          
*  16NOV95  (BU ) --- UPGRADE FOR 2K CONTRACTS.                      *          
*                                                                    *          
*  03MAR98  (BU ) --- UPGRADE FOR 6K CONTRACTS.                      *          
*                                                                    *          
*  10AUG98  (BU ) --- DON'T DUMP ON BAD RECORD                       *          
*                                                                    *          
*  12APR99  (BU ) --- SKIP X'03' ELTS BEYOND CONTRACT FLIGHT DATES   *          
*                                                                    *          
*  23JAN00  (BU ) --- SKIP ORDER WITH INEXPLICABLE DATES             *          
*                                                                    *          
*  18SEP00  (BU ) --- SPEED UP ACCESS                                *          
*                                                                    *          
*  21MAR05  (BU ) --- REPLACE RCONDATE W/REVISED DATES, IF PRESENT   *          
*                                                                    *          
*   NOV29/06 (BU ) --- DMOPEN FOR UPDATE                             *          
*                                                                    *          
*                     ***  END TOMBSTONE  ***                        *          
**********************************************************************          
REATNCR  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE 0,REATNCR,VREGSAVE,R9,RA,R7                                      
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     R4,=V(STXITER)                                                   
         ST    R4,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
         GOTO1 =V(DATCON),(R1),(5,WORK),(2,TODAY2)                              
         BAS   R5,GETMONDT                                                      
*                                                                               
IN100    GOTO1 =V(CARDS),DMCB,WORK,=C'RE00'                                     
         CLC   =C'/*',WORK                                                      
         BE    IN200                                                            
         CLC   =C'WRITE=YES',WORK                                               
         BNE   *+16                                                             
         MVI   WRITEFLG,C'Y'                                                    
         MVI   FLIST,C'U'                                                       
         B     IN100                                                            
*                                                                               
         CLC   =C'MED=',WORK                                                    
         BNE   *+14                                                             
         MVC   MED,WORK+4                                                       
         B     IN100                                                            
*                                                                               
         CLC   =C'REP=',WORK                                                    
         BNE   PURGEALL                                                         
         MVC   REP,WORK+4                                                       
         B     IN100                                                            
*                                                                               
PURGEALL EQU   *                                                                
         CLC   =C'PURGE=ALL',WORK                                               
         BNE   *+12                                                             
         MVI   PRGALL,C'Y'                                                      
         B     IN100                                                            
*                                                                               
         CLC   =C'RECREATE=YES',WORK                                            
         BNE   *+12                                                             
         MVI   RECRTE,C'Y'                                                      
         B     IN100                                                            
*                                                                               
         CLC   =C'UPDATE=YES',WORK                                              
         BNE   *+12                                                             
         MVI   WUPDTE,C'Y'                                                      
         B     IN100                                                            
*                                                                               
         CLC   =C'DELETE=YES',WORK                                              
         BNE   *+12                                                             
         MVI   DELFLG,C'Y'                                                      
         B     IN100                                                            
*                                                                               
         CLC   =C'DATE=',WORK                                                   
         BNE   IN120                                                            
         GOTO1 =V(DATVAL),DMCB,(0,WORK+5),DUB                                   
         OC    DMCB(4),DMCB                                                     
         BZ    INCANCEL                                                         
         GOTO1 =V(DATCON),(R1),(0,DUB),(2,TODAY2)                               
         BAS   R5,GETMONDT         GET MONDAY DATE                              
         B     IN100                                                            
*                                                                               
IN120    EQU   *                                                                
****     CLC   =C'STE=',WORK                                                    
****     BNE   IN140                                                            
****     MVC   STAGEFIL,WORK+4                                                  
****     CLI   STAGEFIL+6,C'T'                                                  
****     BNE   IN100                                                            
****     MVI   STAGEFIL+6,C' '                                                  
****     B     IN100                                                            
*                                                                               
IN140    CLC   =C'STA=',WORK                                                    
         BNE   IN150                                                            
         MVC   STAGFIL,WORK+4                                                   
         CLI   STAGFIL+6,C'T'                                                   
         BNE   IN100                                                            
         MVI   STAGFIL+6,C' '                                                   
         B     IN100                                                            
*                                                                               
IN150    EQU   *                                                                
         CLC   =C'ID=',WORK                                                     
         BNE   IN160                                                            
         MVC   SAVNAME,WORK+3                                                   
         B     IN100                                                            
*                                                                               
IN160    EQU   *                                                                
         CLC   =C'LOCAL=',WORK                                                  
         BNE   IN170                                                            
         MVC   SAVLOCAL,WORK+6                                                  
         B     IN100                                                            
*                                                                               
IN170    EQU   *                                                                
*                                                                               
         CLC   =C'TRACE=Y',WORK    'TRACE RUN' CARD?                            
         BE    IN172               YES- SO CONTINUE                             
         CLC   =C'TRACE=1',WORK    'TRACE RUN' CARD?                            
         BNE   IN180               NO - SO CONTINUE                             
         MVI   TRACE,C'1'          SET THE 'TRACE 1' FLAG                       
         B     IN179                                                            
*                                                                               
IN172    EQU   *                                                                
         MVI   TRACE,C'Y'          SET THE 'TRACE Y' FLAG                       
IN179    EQU   *                                                                
         MVC   P(46),=C'INITIALIZATION PHASE - DELETE OLD ATHENA STUFF'         
         GOTO1 =V(PRINTER)                                                      
         MVC   P(46),=C'----------------------------------------------'         
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         B     IN100               AND GO GET NEXT CARD                         
*                                                                               
IN180    EQU   *                                                                
*                                                                               
         CLC   =C'YEAR=',WORK      YEAR FILTER ENTERED?                         
         BNE   IN190               NO - SO CONTINUE                             
*                                                                               
         MVC   WORK+7(4),=C'0101'  FORCE TO JAN1/YY                             
         GOTO1 =V(DATCON),DMCB,(0,WORK+5),(3,WORK+11)                           
         MVC   YEAR,WORK+11        MOVE IN THE BINARY YEAR                      
         B     IN100                                                            
*                                                                               
IN190    EQU   *                                                                
*                                                                               
         CLC   =C'OFFICE=',WORK    OFFICE FILTER ENTERED?                       
         BNE   IN195               NO - SO CONTINUE                             
*                                                                               
         MVC   OFFICE,WORK+7       ELSE - MOVE IN OFFICE FILTER                 
         B     IN100               AND LOOP BACK                                
*                                                                               
IN195    EQU   *                                                                
*                                                                               
INCANCEL MVC   P(30),=C'*** UNKNOWN PARAMETER CARD ***'                         
INCAN2   GOTO1 =V(LOGIO),DMCB,1,(30,P)                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),WORK                                                       
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
*                                                                               
IN200    OC    REP,REP                                                          
         BNZ   IN220                                                            
         MVC   P(30),=CL30'** REP CARD MISSING (REP=) **'                       
         B     INCAN2                                                           
*                                                                               
IN220    L     R1,=A(BUFFALOC)                                                  
         ST    R1,BUFFC                                                         
         GOTO1 =V(COVAIL),DMCB,C'SETB',3000,600000,BUFFC                        
         ICM   R1,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFC,DMCB+12                                                    
         GOTO1 =V(BUFFALO),DMCB,=C'SET',BUFFC                                   
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',AREC,0                                             
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         MVC   WORK+15(10),SAVNAME LOAD AGENCY NAME                             
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                 
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AREC                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         MVC   UTL+4(1),3(R1)      OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'REP',FLIST,AREC                   
*                                                                               
         CLI   SAVLOCAL,C'D'       LOCAL REQUEST = DUMP?                        
         BNE   CTRL0040            NO                                           
         DC    H'0'                YES - BLOW IT UP                             
CTRL0040 EQU   *                                                                
         CLI   DELFLG,C'Y'         DELETE THIS WEEK ELEMENTS ONLY               
         BE    IN240                                                            
         CLI   WUPDTE,C'Y'         WEEKLY RE-CREATE                             
         BE    IN240                                                            
         CLI   PRGALL,C'Y'         PURGE                                        
         BE    IN240                                                            
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
IN240    MVI   DATADISP+1,34                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSDDKEY,R4                                                       
         MVI   RSDDKTYP,X'26'                                                   
         MVC   RSDDKREP(2),REP     REP                                          
         MVC   RSDDKSTA(4),=4XL1'FF' DEFAULT STATION                            
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN245               NO - SO CONTINUE                             
*                                                                               
         MVC   P(30),=C'LOOKING FOR DEFAULT SDD RECORD'                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P(21),=C'SDD DFLT BEFORE HIGH:'                                  
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN245    EQU   *                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN250               NO - SO CONTINUE                             
*                                                                               
         MVC   P(20),=C'SDD DFLT AFTER HIGH:'                                   
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN250    EQU   *                                                                
*                                                                               
         CLC   KEY(26),KEYSAVE     FOUND DEFAULT                                
         BNE   IN300                                                            
         GOTO1 GETREC                                                           
         CLI   RSDDKSTA+4,C' '     TV ?                                         
         BNE   IN260                                                            
         L     R6,AREC                                                          
         LA    R2,DEFSDDT                                                       
         BAS   RE,SVSDD                                                         
         DROP  R4                                                               
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN252               NO - SO CONTINUE                             
*                                                                               
         MVC   P(43),=C'FOUND DEFAULT FOR TV, NOW LOOKING FOR RADIO'            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN252    EQU   *                                                                
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN255               NO - SO CONTINUE                             
*                                                                               
         MVC   P(19),=C'SDD KEY AFTER SEQ: '                                    
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN255    EQU   *                                                                
*                                                                               
         CLC   KEY(26),KEYSAVE     FOUND RADIO DEFAULT                          
         BNE   IN300                                                            
         GOTO1 GETREC                                                           
*                                                                               
IN260    EQU   *                                                                
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN280               NO - SO CONTINUE                             
*                                                                               
         MVC   P(36),=C'WE HAVE A DEFAULT SDD RECORD IN INIT'                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN280    EQU   *                                                                
*                                                                               
         LA    R2,DEFSDDR                                                       
         L     R6,AREC                                                          
         BAS   RE,SVSDD                                                         
*                                                                               
IN300    MVI   FRST,C'Y'                                                        
*                                                                               
         CLI   PRGALL,C'Y'                                                      
         BE    IN320                                                            
         CLI   DELFLG,C'Y'         DELETE THIS WEEK ELEMENTS ONLY               
         BE    IN320                                                            
         CLI   RECRTE,C'Y'         RECREATE DELETE ALL ELEMENTS                 
         BE    IN320                                                            
         CLI   WUPDTE,C'Y'         WEEKLY RE-CREATE                             
         BNE   AT100                                                            
*                                                                               
IN320    EQU   *                                                                
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN340               NO - SO CONTINUE                             
*                                                                               
         MVC   P(40),=C'DELETING ATHENA RECORD ELEMENTS, PURGE, '               
         MVC   P+41(39),=C'RECREATE, UPDATE, OR DELETE CARD FOUND.'             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN340    EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'27'                                                        
         MVC   KEY+1(2),REP        REP                                          
         OC    STAGFIL,STAGFIL     A STATION                                    
         BZ    *+10                                                             
         MVC   KEY+3(7),STAGFIL                                                 
*                                                                               
IN390    EQU   *                                                                
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN395               NO - SO CONTINUE                             
*                                                                               
         MVC   P(24),=C'ATHENA KEY BEFORE HIGH: '                               
         GOTO1 =V(HEXOUT),DMCB,KEY,P+24,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN395    GOTO1 HIGH                                                             
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN420               NO - SO CONTINUE                             
*                                                                               
         MVC   P(23),=C'ATHENA KEY AFTER HIGH: '                                
         GOTO1 =V(HEXOUT),DMCB,KEY,P+24,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
         B     IN420                                                            
*                                                                               
IN400    GOTO1 SEQ                                                              
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN420               NO - SO CONTINUE                             
*                                                                               
         MVC   P(22),=C'ATHENA KEY AFTER SEQ: '                                 
         GOTO1 =V(HEXOUT),DMCB,KEY,P+24,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN420    CLC   KEY(3),KEYSAVE      DELETE THRU REP                              
         BNE   IN600                                                            
         OC    STAGFIL,STAGFIL     A STATION                                    
         BZ    *+14                                                             
         CLC   KEY+3(7),STAGFIL                                                 
         BNE   IN600                                                            
*                                                                               
         OC    MED,MED                                                          
         BZ    IN435                                                            
         CLI   MED,C'T'                                                         
         BNE   IN430                                                            
         CLI   KEY+9,C' '                                                       
         BE    IN435                                                            
         ZIC   RE,KEY+8            NOT EQUAL TO MEDIA SKIP STATION              
         LA    RE,1(RE)                                                         
         STC   RE,KEY+8                                                         
         MVI   KEY+9,C' '                                                       
         XC    KEY+10(17),KEY+10                                                
         B     IN390                                                            
*                                                                               
IN430    CLI   KEY+9,C' '          BLANK = TV ALL OTHERS RADIO                  
         BNE   IN435                                                            
         ZIC   RE,KEY+9            NOT EQUAL TO MEDIA SKIP MEDIA                
         LA    RE,1(RE)                                                         
         STC   RE,KEY+9                                                         
         XC    KEY+10(17),KEY+10                                                
         B     IN390                                                            
*                                                                               
* PURGE DELETE KEY                                                              
IN435    EQU   *                                                                
*                                                                               
         CLI   PRGALL,C'Y'                                                      
         BNE   IN440                                                            
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN437               NO - SO CONTINUE                             
*                                                                               
         MVC   P(19),=C'DELETING ATHENA KEY'                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN437    EQU   *                                                                
*                                                                               
         OI    KEY+27,X'80'                                                     
         AP    DELCNT,=P'1'                                                     
         CLI   WRITEFLG,C'Y'                                                    
         BNE   *+12                                                             
         BAS   RE,WRITE                                                         
         B     IN400                                                            
         GOTO1 =V(HEXOUT),DMCB,KEY,P,30,=C'T0G'                                 
         GOTO1 =V(PRINTER)                                                      
         B     IN400                                                            
*                                                                               
IN440    GOTO1 GETREC                                                           
         L     R2,AREC                                                          
         CLI   RECRTE,C'Y'         RECREATE DELETE ALL ELEMENTS                 
         BNE   IN500                                                            
*                                                                               
* DELETE ELEMENTS AND WRITE BACK RECORD                                         
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN445               NO - SO CONTINUE                             
*                                                                               
         MVC   P(36),=C'TRYING TO DELETE ALL WEEKLY ELEMENTS'                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN445    EQU   *                                                                
*                                                                               
         USING RATNRECD,R2                                                      
         MVC   RATNLEN,=H'57'      KEY(27) + CNTRL(7) + ELEM1&2(22) + 1         
         MVI   56(R2),0            ZERO END OF RECORD                           
         CLI   WRITEFLG,C'Y'                                                    
         BNE   IN540                                                            
         BAS   RE,PUTREC                                                        
         B     IN400                                                            
*                                                                               
* RE UPDATE 1 WEEK ELEMENTS                                                     
IN500    LR    R6,R2                                                            
         SR    R3,R3               R3 WILL = LENGTH TO MOVE                     
         ICM   R3,3,RATNLEN                                                     
         SH    R3,=H'46'           - KEY(27) CONTROL(7) ELEM 1(12)              
*                                                                               
* DELETE ELEMENT AND WRITE BACK RECORD                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
IN520    BAS   RE,NEXTEL                                                        
         BNE   IN400                                                            
         SH    R3,=H'10'                                                        
         CLC   TODAY2,2(R6)                                                     
         BNE   IN520                                                            
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   IN530               NO - SO CONTINUE                             
*                                                                               
         MVC   P(33),=C'DELETING AN ELEMENT FOR THIS WEEK'                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
IN530    EQU   *                                                                
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,RATNLEN                                                     
         SH    RE,=H'10'                                                        
         STCM  RE,3,RATNLEN                                                     
         LTR   R3,R3                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RE,10(R6)           MOVE RECORD DOWN                             
         LR    R2,R6                                                            
         LR    RF,R3                                                            
         MVCL  R2,RE               0 ALREADY AT END OF RECORD                   
         AP    DCOUNT,=P'1'                                                     
         CLI   WRITEFLG,C'Y'                                                    
         BNE   IN540                                                            
         BAS   RE,PUTREC                                                        
         B     IN400                                                            
*                                                                               
IN540    L     R4,AREC                                                          
         LA    R5,57                                                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R5),=C'2D'                
         B     IN400                                                            
         DROP  R2                                                               
*                                                                               
IN600    CLI   PRGALL,C'Y'                                                      
         BNE   IN620                                                            
         MVC   P(30),=30C'*'                                                    
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(26),=CL24'* # OF DELETED RECORDS *'                            
         EDIT  (P5,DELCNT),(8,P+30)                                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(30),=30C'*'                                                    
         GOTO1 =V(PRINTER)                                                      
         B     EN120                                                            
*                                                                               
IN620    CLI   RECRTE,C'Y'                                                      
         BE    AT100                                                            
         MVC   P(26),=CL26'* # OF ELEMENTS DELETED *'                           
         EDIT  (P5,DCOUNT),(8,P+30)                                             
         GOTO1 =V(PRINTER)                                                      
         CLI   DELFLG,C'Y'                                                      
         BE    EN120                                                            
         EJECT                                                                  
*****************************************************************               
*                                                               *               
*            -----     MAIN LINE CODE     -----                 *               
*                                                               *               
*            -----       END = ENDIT      -----                 *               
*                                                               *               
* READ CONTRACTS BY STATION - FOR EACH CONTRACT READ BUYS -     *               
* PUT BUFFALO RECS (DPT/PRGT/SPL/YM/SER/CATG/ADV/SPOT/DOLRS) -  *               
* READ UNTIL A CONTRACT STATION BREAK OCCURS -                  *               
* READ BUFFALO RECS & ADD ATHENA RECORDS - PRINT STATION TOTALS *               
* RESET BUFFALO - START NEXT STATION                            *               
*                                                               *               
*****************************************************************               
         SPACE 2                                                                
*                                                                               
AT100    EQU   *                                                                
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   AT110               NO - SO CONTINUE                             
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(33),=C'MAINLINE - RE-CREATE ATHENA STUFF'                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(33),=C'---------------------------------'                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
AT110    EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'8E'           SET KEY TYPE                                 
         MVC   KEY+1(2),REP        INSERT REP CODE                              
         MVC   KEY+3(5),STAGFIL+2  INSERT STATION (DROP GROUP/SGRP)             
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,WORK),(3,WORK)                                
         ZIC   RF,WORK             BACK UP ONE YEAR                             
         BCTR  RF,0                                                             
         CLI   WORK+1,6            TODAY = JULY OR LATER?                       
*                                                                               
*   SET TO ALWAYS BACK UP TWO YEARS                                             
*                                                                               
***>>>   BH    AT110020            YES                                          
*                                                                               
*                                                                               
         BCTR  RF,0                NO  - BACK UP ANOTHER YEAR                   
AT110020 EQU   *                                                                
         STC   RF,WORK             PUT YEAR BACK                                
         MVI   WORK+1,1            SET MONTH TO JANUARY                         
         MVI   WORK+2,1            SET DAY TO FIRST                             
         GOTO1 =V(DATCON),DMCB,(3,WORK),(2,CUTDATE)                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P+1(33),=C'**-->>     ATHENA CREATE CUTOFF: '                    
         GOTO1 =V(DATCON),DMCB,(2,CUTDATE),(5,P+40)                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
AT120    EQU   *                                                                
*                                                                               
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   AT150               NO - SO CONTINUE                             
*                                                                               
         MVC   P(27),=C'LOOKING FOR CONTRACT RECORD'                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P(21),=C'CON KEY BEFORE HIGH: '                                  
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
AT150    EQU   *                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   AT220               NO - SO CONTINUE                             
*                                                                               
         MVC   P(20),=C'CON KEY AFTER HIGH: '                                   
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     AT220                                                            
*                                                                               
AT200    GOTO1 SEQ                                                              
*                                                                               
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   AT220               NO - SO CONTINUE                             
*                                                                               
         MVC   P(19),=C'CON KEY AFTER SEQ: '                                    
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
AT220    EQU   *                                                                
         CLC   KEY(3),KEYSAVE      SAME KEY/SAME REP?                           
         BNE   ENDIT                                                            
         CLI   KEY+16,3            8E KEY/TYPE 3?                               
         BNE   AT200               NO  - SKIP IT                                
         TM    KEY+27,X'80'        NO DELETES                                   
         BO    AT200                                                            
*                                                                               
*   TEMPORARY CODE TO SKIP CONTRACT WITH INEXPLICABLE DATES                     
         CLC   KEY+12(4),=X'04806552'                                           
         BE    AT200               YES - SKIP THIS ONE                          
*                                                                               
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   AT225               NO - SO CONTINUE                             
*                                                                               
*                                                                               
         MVC   P(10),=C'RCONKOFF? '                                             
         MVC   P+10(2),KEY+17                                                   
         MVC   P+15(14),=C'OFFICE FILTER:'                                      
         MVC   P+30(2),OFFICE                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
AT225    EQU   *                                                                
*                                                                               
         OC    OFFICE,OFFICE       OFFICE FILTER REQUESTED?                     
         BZ    AT227               NO - SO CONTINUE                             
*                                                                               
         CLC   KEY+17(2),OFFICE    FOR REQUESTED OFFICE?                        
         BNE   AT200               NO - SO CONTINUE                             
*                                                                               
AT227    EQU   *                                                                
*                                                                               
         CLC   REJSTA,KEY+3                                                     
         BNE   AT230                                                            
         XC    REJSTA,REJSTA                                                    
         ZIC   RE,KEY+07           NOT EQUAL TO MEDIA SKIP STATION              
         LA    RE,1(RE)                                                         
         STC   RE,KEY+07                                                        
         XC    KEY+08(19),KEY+08                                                
         B     AT120                                                            
*                                                                               
AT230    OC    MED,MED                                                          
         BZ    AT300                                                            
         CLI   MED,C'T'                                                         
         BNE   AT240                                                            
         CLI   KEY+07,C' '                                                      
         BE    AT300                                                            
         ZIC   RE,KEY+6            NOT EQUAL TO MEDIA SKIP STATION              
         LA    RE,1(RE)                                                         
         STC   RE,KEY+6                                                         
         MVI   KEY+07,C' '                                                      
         XC    KEY+08(19),KEY+08                                                
         B     AT120                                                            
*                                                                               
AT240    CLI   KEY+07,C' '         BLANK = TV ALL OTHERS RADIO                  
         BNE   AT300                                                            
         ZIC   RE,KEY+07           NOT EQUAL TO MEDIA SKIP MEDIA                
         LA    RE,1(RE)                                                         
         STC   RE,KEY+07                                                        
         XC    KEY+08(19),KEY+08                                                
         B     AT120                                                            
*                                                                               
AT300    EQU   *                                                                
****     OC    STAGEFIL,STAGEFIL                                                
****     BZ    AT320                                                            
****     CLC   KEY+4(7),STAGEFIL                                                
****     BH    ENDIT                                                            
****     B     AT340                                                            
*                                                                               
AT320    OC    STAGFIL,STAGFIL     SINGLE STATION CREATE?                       
         BZ    *+14                NO                                           
         CLC   KEY+3(5),STAGFIL+2  YES - FINISHED WITH STATION                  
         BNE   ENDIT               YES                                          
*                                                                               
AT340    EQU   *                                                                
         CLC   KEY+10(2),CUTDATE   CONTRACT END BEFORE CUTOFF?                  
         BL    AT200               YES - SKIP THIS CONTRACT                     
*                                                                               
         LA    RE,REC2                                                          
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         CLI   FRST,C'Y'                                                        
         BE    AT400                                                            
         CLC   KEY+3(5),SVKEY+SVSTAE                                            
         BE    AT400                                                            
         MVC   HLDKEY,KEY                                                       
         BAS   RE,ADDATNA                                                       
         BAS   RE,PRNTSTA                                                       
         BAS   RE,SETNXSTA                                                      
         MVC   KEY,HLDKEY                                                       
*                                                                               
AT400    MVC   SVKEY,KEY                                                        
         BAS   RE,SETCNTRT                                                      
         BE    AT420                                                            
         XC    KEY,KEY             IF NO SDD THAN SKIP THIS STATION             
         MVC   KEY(08),SVKEY                                                    
         ZIC   RE,KEY+07                                                        
         LA    RE,1(RE)            NEXT STATION                                 
         STC   RE,KEY+07                                                        
         XC    KEY+08(19),KEY+08                                                
         B     AT120                                                            
*                                                                               
AT420    BAS   RE,READBUYS                                                      
*                                                                               
*   TEST END OF JOB                                                             
         CLC   TESTCTR,=F'10000'   TEST END OF JOB                              
****     BH    ENDIT                                                            
*   TEST END OF JOB                                                             
*                                                                               
         MVC   KEY,SVKEY           RE-READ CONTRACT                             
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FRST,C'N'                                                        
         B     AT200                                                            
*                                                                               
ENDIT    CLI   FRST,C'Y'                                                        
         BNE   EN100                                                            
         MVC   P(25),=C'NO ATHENA RECORDS CREATED'                              
         GOTO1 =V(PRINTER)                                                      
         B     EN120                                                            
         SPACE                                                                  
EN100    BAS   RE,ADDATNA                                                       
         BAS   RE,PRNTSTA                                                       
         BAS   RE,FINALPRT                                                      
EN120    CLOSE FILEOUT                                                          
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*  ROUTINE BUILDS MONTH TABLES / EXTRACTS CATG/SERV/ADV FROM CONTRACT *         
*  READS FOR STATION SDD RECORD / READS EXISTING ATHENA RECORDS       *         
*  DELETES ELEMENTS IF UPDATE OPTION - ADDS BUFF REC WITH DKA         *         
***********************************************************************         
SETCNTRT NTR1                                                                   
         LA    R4,REC2                                                          
         USING RCONKEY,R4                                                       
*                                                                               
*   LOOK FOR REVISED START / END DATES                                          
*                                                                               
         LA    R6,REC2                                                          
         MVI   ELCODE,X'1E'        GET RANDOM FLAGS ELEMENT IN CON              
         BAS   RE,GETEL                                                         
         BNE   CT020               NOT FOUND                                    
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT   ANY REVISED DATES?                           
         BZ    CT020               NO                                           
         MVC   RCONDATE(6),RCONRFLT                                             
*                                  YES - REPLACE RCONDATE WITH REVISED          
*                                                                               
CT020    EQU   *                                                                
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   CT050               NO - SO CONTINUE                             
*                                                                               
         MVC   P(10),=C'RCONKOFF1='                                             
         MVC   P+10(2),RCONKOFF                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CT050    EQU   *                                                                
*                                                                               
         XC    MTHTBL,MTHTBL                                                    
         MVC   SERV,RCONRTGS                                                    
         MVC   CATG,RCONCTGY                                                    
         MVC   PRD,SPACES                                                       
         MVC   ADV,RCONKADV                                                     
         LA    R3,MTHTBL           2(BYTES)-YM/3-ST BCM YMD/3-BCM YMD           
         LA    R5,15                                                            
         GOTO1 =V(DATCON),DMCB,(3,RCONDATE),(0,WORK+20)                         
CT100    GOTO1 =V(GETBROAD),(R1),WORK+20,WORK,V(GETDAY),V(ADDAY)                
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(DATCON),(R1),(0,WORK),(3,2(R3))                               
         GOTO1 =V(DATCON),(R1),(0,WORK+6),(3,5(R3))                             
         LA    R0,7                GET DATE WITH CORRECT MONTH                  
         GOTO1 =V(ADDAY),(R1),WORK,WORK+30,(R0)                                 
         GOTO1 =V(DATCON),(R1),(0,WORK+30),(3,WORK+38)                          
         MVC   0(2,R3),WORK+38     MOVE IN MONTH YEAR                           
         CLC   RCONDATE+3(3),5(R3)                                              
         BNH   CT200                                                            
         LA    R0,1                                                             
         GOTO1 =V(ADDAY),(R1),WORK+6,WORK+20,(R0)                               
         LA    R3,8(R3)                                                         
         BCT   R5,CT100                                                         
         DC    H'0'                                                             
*                                                                               
CT200    CLC   SVSTAG,RCONKGRP                                                  
         BE    GDEXT                                                            
         GOTO1 GETGROUP,DMCB,RCONKSTA                                           
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSDDKEY,R6                                                       
         MVI   RSDDKTYP,X'26'                                                   
         MVC   RSDDKREP,REP        REP                                          
         MVC   RSDDKSTA,RCONKSTA   DEFAULT STATION                              
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   CT205               NO - SO CONTINUE                             
*                                                                               
         MVC   P(07),=C'SVSTAG='                                                
         MVC   P+7(7),SVSTAG                                                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P(39),=C'LOOKING FOR DEFAULT SDD RECORD (AGAIN)'                 
         GOTO1 =V(PRINTER)                                                      
         MVC   P(21),=C'SDD KEY BEFORE HIGH: '                                  
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P+21(27),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CT205    EQU   *                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   CT210               NO - SO CONTINUE                             
*                                                                               
         MVC   P(20),=C'SDD KEY AFTER HIGH: '                                   
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P+21(27),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CT210    EQU   *                                                                
*                                                                               
         CLC   KEY(27),KEYSAVE     FOUND STATION SDD                            
         BNE   CT220                                                            
         TM    KEY+27,X'80'        NO DELETES                                   
         BO    CT220                                                            
         GOTO1 GETREC                                                           
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   CT215               NO - SO CONTINUE                             
*                                                                               
         MVC   P(36),=C'WE HAVE AN SDD RECORD FROM CONTRACTS'                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CT215    EQU   *                                                                
*                                                                               
         L     R6,AREC                                                          
         LA    R2,STASDDT                                                       
         BAS   RE,SVSDD                                                         
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   CT300               NO - SO CONTINUE                             
*                                                                               
         MVC   P+1(07),=C'SDD KEY:'                                             
         MVC   P+10(27),KEY                                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(31),=C'CONTENTS OF STATION SDD TABLE: '                        
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),DMCB,STASDDT,P,60,=C'T0G'                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),DMCB,STASDDT+60,P,60,=C'T0G'                          
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),DMCB,STASDDT+120,P,60,=C'T0G'                         
         GOTO1 =V(PRINTER)                                                      
         B     CT300                                                            
*                                                                               
CT220    EQU   *                                                                
*                                                                               
         LA    R2,DEFSDDT                                                       
         CLI   SVKEY+SVSTAME,C' '                                               
         BE    *+8                                                              
         LA    R2,DEFSDDR                                                       
         CLI   0(R2),0             DEFAULT SDD                                  
         BNE   CT300                                                            
         MVC   P(5),SVKEY+SVSTAE                                                
         MVC   REJSTA(5),SVKEY+SVSTAE                                           
         MVC   P+9(17),=C'HAS NO SDD RECORD'                                    
*                                                                               
         MVC   P+50(27),SVKEY                                                   
*                                                                               
         GOTO1 =V(PRINTER)                                                      
BDEXT    LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
CT300    EQU   *                                                                
*                                                                               
***      CLI   TRACE,C'Y'          TRACE MODE?                                  
***      BNE   CT350               NO - SO CONTINUE                             
*                                                                               
         MVC   P(37),=C'ASSUMING A DEFAULT SDD HAS BEEN FOUND'                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CT350    EQU   *                                                                
*                                                                               
         ST    R2,ACURSDDT                                                      
         MVC   SVSTAG+2(5),SVKEY+SVSTAE                                         
*                                  GROUP/SUBGROUP STORED EARLIER                
GDEXT    CR    RE,RE                                                            
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                  SAVE STATION:  GROUP NOT SET YET             
*                                                                               
*  GETGROUP: GET STATION RECORD PRIOR TO READING SDD RECORD.  THIS              
*        WILL PERMIT USE OF THE SAME AREA.                                      
*                                                                               
GETGROUP NTR1                                                                   
         XC    KEY,KEY             DETERMINE STATION GROUP/SUBGROUP             
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REP                                                    
         L     RF,0(R1)            SET A(STA CALLS IN CON RECD)                 
         MVC   KEY+22(5),0(RF)     INSERT STATION CALL                          
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R6,AREC                                                          
         USING RSTAREC,R6                                                       
         MVC   SVSTAG(2),RSTAGRUP  SAVE STATION GROUP/SUBGROUP                  
         DROP  R6                                                               
         XIT1                                                                   
         EJECT                                                                  
*****************************************************************               
*                                                               *               
*  ROUTINE READS BUYS FOR 1 CONTRACT - MATCH DAY/TIMES TO SDD   *               
*  RECORDS. DOES THIS BY 1ST FINDING A MATCH ON DAY/START TIME  *               
*  (CAN ONLY MATCH TO 1) THAN CHECKS END TIME FOR MATCH         *               
*  YES - READS NEXT ELEMENT, NO ADDS 1 TO SDD END TIME, STORES  *               
*  IN START TIME USE DPT SDD TO MATCH ON DAY/TIME - NO MATCH    *               
*  CRERATE PSUEDO(ZZZ) DPT. AFTER THIS MESS IT READS NEXT ELEM  *               
*  AND STARTS OVER USING MATCHED DPT.                           *               
*  CREATES BUFFALO RECORDS (UP TO 4) - READS NEXT BUY           *               
*  END OF BUYS FOR CONTRACT DONE                                *               
*                                                               *               
*****************************************************************               
READBUYS NTR1                                                                   
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   RB000               NO - SO CONTINUE                             
*                                                                               
         MVC   P(12),=C'READING BUYS'                                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
RB000    EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
         MVC   KEY+(RBUYKREP-RBUYKEY)(2),REP                                    
         UNPK  WORK+10(9),SVKEY+SVCON#(5)  CONTRACT NUMBER                      
         PACK  WORK(5),WORK+10(8)                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),WORK(5)                                               
         MVO   WORK(5),WORK+10(5)                                               
         MVC   KEY+18(4),WORK                                                   
         PACK  KEY+18(1),WORK+3(1) REVERSE THE COMPLIMENT                       
         PACK  KEY+19(1),WORK+2(1)                                              
         PACK  KEY+20(1),WORK+1(1)                                              
         PACK  KEY+21(1),WORK(1)                                                
         DS    0H                                                               
*                                                                               
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   RB050               NO - SO CONTINUE                             
*                                                                               
         MVC   P(21),=C'BUY KEY BEFORE HIGH: '                                  
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
RB050    EQU   *                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   RB120               NO - SO CONTINUE                             
*                                                                               
         MVC   P(20),=C'BUY KEY AFTER HIGH: '                                   
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
         B     RB120                                                            
*                                                                               
RB100    EQU   *                                                                
         GOTO1 SEQ                                                              
*                                                                               
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   RB120               NO - SO CONTINUE                             
*                                                                               
         MVC   P(19),=C'BUY KEY AFTER SEQ: '                                    
         GOTO1 =V(HEXOUT),DMCB,KEY,P+21,30,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
RB120    EQU   *                                                                
*                                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   RBEXT                                                            
         TM    KEY+27,X'80'        NO DELETES                                   
         BO    RB100                                                            
         CLC   =XL2'FFFF',KEY+25   PLAN REJECT BUY                              
         BE    RB100                                                            
         BAS   RE,GETREC                                                        
         XC    BFREC,BFREC                                                      
         L     R6,AREC                                                          
         USING RBUYKEY,R6                                                       
         TM    RBUYCNTL,X'80'                                                   
         BO    RB100                                                            
*                                                                               
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   RB125               NO - SO CONTINUE                             
*                                                                               
         MVC   P(9),=C'GOT A BUY'                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
RB125    EQU   *                                                                
*                                                                               
*   INCREMENT TEST COUNT                                                        
         L     RF,TESTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,TESTCTR          PUT IT BACK                                  
*   END INCREMENT                                                               
*                                                                               
*                                                                               
****     SR    R3,R3                                                            
****     ICM   R3,3,RBUYLEN                                                     
****     GOTO1 =V(PRNTBL),DMCB,(0,(R6)),(R6),C'DUMP',(R3),=C'2D'                
         MVC   BFSPL,RBUYDUR                                                    
         MVC   DUB(4),RBUYCOS                                                   
         MVC   BYTE,RBUYNW                                                      
         CLC   RBUYKPLN,=3X'FF'                                                 
         BE    RB130                                                            
         MVC   BFDPT(2),=X'FEFE'                                                
         MVC   BFSPL,=X'FFFF'                                                   
         MVI   BFPGT,X'FC'         PLAN INDICATOR                               
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   RB700               NO - SO CONTINUE                             
*                                                                               
         MVC   P(39),=C'PLAN .NE. FFFFFF - SO NO DAY/TIME STUFF'                
         GOTO1 =V(PRINTER)                                                      
         B     RB700                                                            
*                                                                               
RB130    CLI   RBUYTYP,C' '                                                     
         BE    RB140                                                            
         SR    RE,RE                                                            
         ICM   RE,1,RBUYTYP        IF TYPE GIVEN THAN NO DAYPARTS               
         BZ    RB140                                                            
         MVC   BFDPT(2),=X'FEFE'                                                
         STC   RE,BFPGT                                                         
*                                                                               
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   RB700               NO - SO CONTINUE                             
*                                                                               
         MVC   P(33),=C'TYPE GIVEN - SO NO DAY/TIME STUFF'                      
         GOTO1 =V(PRINTER)                                                      
         B     RB700                                                            
         DROP  R6                                                               
*                                                                               
RB140    EQU   *                                                                
*                                                                               
         CLI   TRACE,C'1'          TRACE MODE?                                  
         BNE   RB150               NO - SO CONTINUE                             
*                                                                               
         MVC   P(20),=C'DOING DAY/TIME STUFF'                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
RB150    EQU   *                                                                
*                                                                               
         L     R6,AREC                                                          
         MVI   ELCODE,X'02'        GET DAY/TIME ELEM                            
         BAS   RE,GETEL                                                         
         BNE   RB100               SKIP RECORD                                  
         MVI   WORK+5,C'Y'         1ST TIME SWITCH                              
         BAS   R5,CHKTIM                                                        
         L     R2,ACURSDDT                                                      
*                                                                               
*  SPECIAL CODE TO DISPLAY WHICH SDD RECORD IS BEING USED.  CHANGE              
*        ???? TO STATION CALL LETTERS FOR THE STATION DESIRED.                  
*                                                                               
         CLC   SVKEY+SVSTAE(4),=C'????'                                         
         BNE   RB200                                                            
         LR    RF,R2               A(SDD RECORD DATA)                           
         S     RF,=F'27'           BACK UP TO GET KEY                           
         MVC   P+1(08),=C'SDD REC='                                             
         MVC   P+9(27),0(RF)       LOAD KEY FROM SDD RECORD                     
         MVC   P+40(35),0(R2)                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
RB200    EQU   *                                                                
*                                                                               
*   TEST                                                                        
**       MVC   P+1(08),=C'SDD DATA'                                             
**       MVC   P+9(64),0(R2)       LOAD KEY FROM SDD RECORD                     
**       GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*                                                                               
         LR    R3,R2                                                            
         MVC   BFDPT(2),0(R2)                                                   
*                                                                               
         CLI   BFDPT,C'A'                                                       
         BL    RB500               TREAT AS UNKNOWN DAYPART                     
*                                                                               
*                                                                               
RB220    ZIC   R5,2(R2)            # OF DAYS/TIMES IN THIS DAYPART              
         LA    R2,3(R2)                                                         
RB240    ZIC   RE,0(R2)                                                         
         EX    RE,DAYTEST          DAY OVERLAP                                  
         BZ    RB260                                                            
         CLC   WORK(2),3(R2)                                                    
         BH    RB260               ELEM ST GT DPT END                           
         CLC   WORK(2),1(R2)                                                    
         BNL   RB300               ELEM ST BEFORE DPT START                     
RB260    LA    R2,5(R2)            DAY OR TIME NOT MATCHED                      
         BCT   R5,RB240                                                         
*                                                                               
         CLI   WORK+5,C'Y'         AFTER 1ST TIME NO MATCH TO ANY DPT           
         BNE   RB500                                                            
         CLI   0(R2),0             1ST TIME MUST FIND MATCH                     
         BNE   RB200                                                            
         B     RB500               START TIME HAS TO FIT                        
*                                                                               
RB300    MVI   WORK+5,C'N'                                                      
         OC    WORK+2(2),WORK+2    END TIME - NO A MATCH                        
         BZ    RB400                                                            
         CLC   WORK+2(2),3(R2)                                                  
         BNH   RB400               END BEFORE END - MATCH                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,3(R2)          ADD 1 TO END DATE (NEW START)                
         LA    RF,1(RF)                                                         
         LR    R1,RF                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'          CHECK FOR END OF AN HOUR                     
         CH    RE,=H'60'                                                        
         BNE   *+8                                                              
         AH    R1,=H'40'                                                        
         LR    RF,R1                                                            
         STCM  RF,3,WORK                                                        
         LR    R2,R3                                                            
         B     RB220                                                            
*                                                                               
RB400    BAS   RE,NEXTEL                                                        
         BNE   RB700                                                            
         BAS   R5,CHKTIM                                                        
         LR    R2,R3                                                            
         B     RB220                                                            
*                                                                               
* CHKTIM SUB ROUTINE MAY RETURN HERE                                            
RB500    MVC   BFDPT(2),=X'FFFF'                                                
*                                                                               
RB700    L     R6,AREC                                                          
         LA    R2,MTHTBL                                                        
         MVI   ELCODE,X'03'        GET WEEKS ELEM                               
         BAS   RE,GETEL                                                         
         B     *+8                                                              
RB720    BAS   RE,NEXTEL                                                        
         BNE   RB900                                                            
         MVC   WORK(3),2(R6)       BUY EL START DATE                            
RB740    CLC   WORK(3),5(R2)                                                    
         BNH   RB800               END BEFORE START                             
         BAS   R5,ADTOBUFF                                                      
RB760    LA    R2,8(R2)                                                         
         OC    0(2,R2),0(R2)       MORE DATES IN FLIGHT?                        
         BNZ   RB740               YES                                          
***      DC    H'0'                NO  - ELT OUTSIDE FLIGHT                     
         B     RB720               NO  - ELT OUTSIDE FLIGHT                     
*                                     SKIP THIS ELEMENT                         
*                                                                               
RB800    MVC   BFYM,0(R2)                                                       
         ZIC   RF,BYTE                                                          
         CLI   1(R6),5                                                          
         BE    RB820                                                            
         ZIC   RF,RBUYDTNW-RBUYDTEL(R6)                                         
RB820    LR    R1,RF                                                            
         A     RF,BFSPOT                                                        
         ST    RF,BFSPOT                                                        
         ICM   RF,15,DUB                                                        
         BZ    RB830                                                            
         CLI   BFPGT,X'FC'         IF PLAN ITS TOTAL COST                       
         BNE   *+8                                                              
         LA    R1,1                                                             
         LR    RF,R1                                                            
         M     RE,DUB                                                           
         LTR   RF,RF                                                            
         BM    *+12                                                             
         AH    RF,=H'50'                                                        
         B     *+8                                                              
         SH    RF,=H'50'                                                        
         D     RE,=F'100'                                                       
         A     RF,BFDOLS                                                        
         ST    RF,BFDOLS                                                        
*                                                                               
RB830    CLI   1(R6),5             NO END DATE - NEXT ELEM                      
         BE    RB720                                                            
         LA    R5,7                1 WEEK                                       
         TM    8(R6),X'40'                                                      
         BNO   *+8                                                              
         LA    R5,14               2 WEEKS                                      
         ZIC   RE,WORK+2                                                        
         AR    RE,R5                                                            
         CH    RE,=H'28'                                                        
         BH    *+12                                                             
         STC   RE,WORK+2           LT 28 ADD 7/14 TO DAYS                       
         B     RB840                                                            
*                                                                               
         GOTO1 =V(DATCON),DMCB,(3,WORK),(0,WORK+6)                              
         GOTO1 =V(ADDAY),(R1),WORK+6,WORK+12,(R5)                               
         GOTO1 =V(DATCON),(R1),(0,WORK+12),(3,WORK)                             
RB840    CLC   WORK(3),5(R6)                                                    
         BH    RB720                                                            
         B     RB740                                                            
*                                                                               
RB900    BAS   R5,ADTOBUFF                                                      
*                                                                               
* LOOK FOR MAKEGOOD/CREDIT ELEMENTS                                             
RB1000   MVI   ELCODE,X'06'        GET WEEKS ELEM                               
RB1020   L     R6,AREC                                                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
RB1040   BAS   RE,NEXTEL                                                        
         BNE   RB1200                                                           
*                                                                               
         ZIC   RF,RBUYMSSP-RBUYMSEL(R6)                                         
         LCR   RF,RF                                                            
         ST    RF,BFSPOT                                                        
         CLI   ELCODE,X'07'        MISSED DOLLARS ALREADY CALCULATED IN         
         BE    RB1100                                                           
         CLI   BFPGT,X'FC'         IF PLAN ITS TOTAL COST                       
         BE    RB1100                                                           
         M     RE,DUB                                                           
         LTR   RF,RF                                                            
         BM    *+12                                                             
         AH    RF,=H'50'                                                        
         B     *+8                                                              
         SH    RF,=H'50'                                                        
         D     RE,=F'100'                                                       
         ST    RF,BFDOLS                                                        
*                                                                               
RB1100   LA    R2,MTHTBL                                                        
RB1120   CLC   2(3,R6),5(R2)                                                    
         BNH   RB1140              END BEFORE START                             
         LA    R2,8(R2)                                                         
         OC    0(2,R2),0(R2)                                                    
         BNZ   RB1120                                                           
         DC    H'0'                                                             
RB1140   MVC   BFYM,0(R2)                                                       
         BAS   R5,ADTOBUFF                                                      
         B     RB1040                                                           
*                                                                               
RB1200   CLI   ELCODE,X'07'                                                     
         BE    RB100                                                            
         MVI   ELCODE,X'07'                                                     
         B     RB1020                                                           
*                                                                               
RBEXT    B     EXIT                                                             
DAYTEST  TM    3(R6),0                                                          
         SPACE                                                                  
* CHECK TIME FOR NONE/VARYIOUS/CC                                               
*                                                                               
*        **** ROUTINE RETURNS TO RB500 IF NONE OR VARYIOUS TIMES ****           
*                                                                               
CHKTIM   MVC   WORK(4),4(R6)       START/END TIME                               
         CLC   =C'NONE',WORK                                                    
         BE    RB500                                                            
         CLC   =C'VARY',WORK                                                    
         BE    RB500                                                            
         CLC   =C'CC',WORK+2                                                    
         BNE   *+10                                                             
         MVC   WORK+2(2),=H'2600'                                               
         CLC   WORK(2),=H'600'                                                  
         BNL   KM120                                                            
         CLC   WORK(2),=H'500'                                                  
         BL    KM100                                                            
         MVC   WORK(2),=H'600'     5A - 6A                                      
         CLC   WORK+2(2),=H'559'                                                
         BH    KM200                                                            
         CLC   WORK+2(2),=H'501'                                                
         BL    KM120                                                            
         MVC   WORK+2(2),=H'600'   5A = 6A                                      
         B     KM200                                                            
KM100    CLC   WORK(2),=H'201'                                                  
         BL    KM120                                                            
         MVC   WORK(2),=H'200'     2A - 459                                     
KM120    CLC   WORK+2(2),=H'600'                                                
         BNL   KM200                                                            
         CLC   WORK+2(2),=H'201'                                                
         BL    KM200                                                            
         CLC   WORK+2(2),=H'500'                                                
         BH    KM140                                                            
         MVC   WORK+2(2),=H'200'   201A - 459 = 2A                              
         B     KM200                                                            
KM140    MVC   WORK+2(2),=H'600'   5A = 6A                                      
KM200    CLC   WORK(2),WORK+2                                                   
         BNE   *+10                                                             
         XC    WORK+2(2),WORK+2                                                 
         LA    R1,WORK                                                          
         BAS   RE,ADJTIME                                                       
         CLC   =H'2600',WORK+2     LEAVE 2A ALONE                               
         BE    KM300                                                            
         SR    RE,RE                                                            
         ICM   RE,3,WORK+2                                                      
         BZ    KMEXT                                                            
         SH    RE,=H'1'            ADJUST END TIME                              
         STCM  RE,3,WORK+2                                                      
KM300    CLC   WORK(2),WORK+2      START GT END NNO MATCH                       
         BH    RB500                                                            
KMEXT    BR    R5                                                               
         EJECT                                                                  
*                                                                               
* PUT UP TO 4 BUFFALO RECORDS                                                   
ADTOBUFF OC    BFYM,BFYM                                                        
         BZ    AB500                                                            
         MVI   BFTYP,1                                                          
         XC    BFTYPC,BFTYPC                                                    
         BAS   RE,PUTBUFF                                                       
*        GOTO1 =V(HEXOUT),DMCB,BFREC,P+38,28,=C'T0G'                            
*        GOTO1 =V(PRINTER)                                                      
*                                                                               
         OC    SERV,SERV                                                        
         BZ    AB100                                                            
         MVI   BFTYP,2                                                          
         MVC   BFTYPC(1),SERV                                                   
         BAS   RE,PUTBUFF                                                       
*                                                                               
AB100    OC    CATG,CATG                                                        
         BZ    AB200                                                            
         MVI   BFTYP,3                                                          
         MVC   BFTYPC(2),CATG                                                   
         BAS   RE,PUTBUFF                                                       
*                                                                               
AB200    OC    ADV(7),ADV                                                       
         BZ    AB500                                                            
         MVI   BFTYP,4                                                          
         MVC   BFTYPC(7),ADV                                                    
         BAS   RE,PUTBUFF                                                       
*                                                                               
AB500    XC    BFYM,BFYM                                                        
         XC    BFSPOT(8),BFSPOT                                                 
ABEXT    BR    R5                                                               
         EJECT                                                                  
*  ROUTINE ADDS ATHENA RECORDS - BUILDS STATION TOTALS                          
ADDATNA  NTR1                                                                   
         LA    R6,REC                                                           
         ST    R6,AREC                                                          
         USING RATNRECD,R6                                                      
         LR    RE,R6                                                            
         LA    RF,1000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   RATNKTYP,X'27'                                                   
         MVC   RATNKREP,REP        REP                                          
*                                                                               
*   TEST                                                                        
*        DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         MVC   RATNKGRP(7),SVSTAG                                               
         MVC   RATNCODE(2),=X'010C'                                             
         MVC   RATNCREA,TODAY2                                                  
         MVC   RATNLCHG,TODAY2                                                  
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   AA000               NO - SO CONTINUE                             
*                                                                               
         MVC   P(24),=C'ATHENA KEY BEFORE HIGH: '                               
         GOTO1 =V(HEXOUT),DMCB,RATNKEY,P+27,30,=C'TOG'                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
AA000    EQU   *                                                                
*                                                                               
         BAS   RE,HIGHBUFF                                                      
         B     *+8                                                              
*                                                                               
AA100    BAS   RE,SEQBUFF                                                       
         BO    AAEXT                                                            
*                                                                               
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   AA120               NO - SO CONTINUE                             
*                                                                               
         MVC   P(27),=C'ATHENA KEY AFTER HIGH/SEQ: '                            
         GOTO1 =V(HEXOUT),DMCB,RATNKEY,P+27,30,=C'TOG'                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
AA120    EQU   *                                                                
*                                                                               
         CLI   RECRTE,C'Y'         RECREATE DELETE ALL ELEMENTS                 
         BE    AA300                                                            
         CLI   WUPDTE,C'Y'         WEEKLY RE-CREATE                             
         BE    AA300                                                            
*                                                                               
* THIS IS A NEW RECORD - BUILD & ADD                                            
         CLI   TRACE,C'Y'          TRACE MODE?                                  
         BNE   AA150               NO - SO CONTINUE                             
*                                                                               
         MVC   P(34),=C'BUILDING NEW ATHENA RECORD IN CORE'                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
AA150    EQU   *                                                                
*                                                                               
         LH    RE,=H'61'                                                        
         SLL   RE,16                                                            
         ST    RE,RECLN                                                         
         MVC   RATNKTPE(8),BFTYP                                                
         MVC   RATNKDPT(7),BFDPT                                                
*                                                                               
*   TEST                                                                        
**       MVC   P(08),=C'RATNKEY:'                                               
**       MVC   P+12(27),RATNKEY                                                 
**       GOTO1 =V(PRINTER)                                                      
*   TEST                                                                        
*                                                                               
         MVC   RATNLEN,=H'57'                                                   
         LA    R4,RATNCODE                                                      
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               INCASE ELEM 1 LENGTH CHG                     
         USING RATNSCDT,R4                                                      
         MVC   0(2,R4),=X'020A'                                                 
         MVC   RATNSCAS,TODAY2                                                  
         MVC   RATNSCSP,BFSPOT+2                                                
         MVC   RATNSCCS,BFDOLS                                                  
         LA    R3,57                                                            
         DROP  R4                                                               
*                                                                               
         OC    YEAR,YEAR           FILTER ON YEAR?                              
         BZ    AA200               NO - SO CONTINUE                             
*                                                                               
         CLC   YEAR,RATNKYM        THIS YEAR?                                   
         BNE   AA640               NO - SO SKIP THIS RECORD                     
*                                                                               
AA200    EQU   *                                                                
*                                                                               
**       MVC   P+1(12),=C'*** DAYPART:'                                         
**       MVC   P+15(2),RATNKDPT                                                 
**       GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   RATNKDPT,C'A'       DAYPART < 'A'?                               
         BNL   AA220               NO  - OKAY TO PROCESS                        
*                                  DAYPARTS ARE ALPHA (A-Z) AND                 
*                                     NUMERIC (0-9)                             
         MVC   P+1(12),=C'BAD DAYPART:'                                         
         MVC   P+15(27),RATNKEY                                                 
         GOTO1 =V(PRINTER)                                                      
         L     RF,BADCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BADCTR                                                        
         B     AA600               SKIP RECORD                                  
*                                                                               
AA220    EQU   *                                                                
         CLI   WRITEFLG,C'Y'                                                    
         BNE   AA500                                                            
*                                                                               
***      CLC   =C'KACB',RECLN+9    SPECIAL TEST                                 
***      BNE   AA240               NO                                           
***      LA    R4,RECLN                                                         
***      LH    R3,RECLN                                                         
***      GOTO1 =V(PRNTBL),DMCB,=C'OUTPUT',(R4),C'DUMP',(R3),=C'2D'              
*                                                                               
         CLI   TRACE,C'Y'          TRACE?                                       
         BNE   AA240               NO                                           
         LA    R4,RECLN                                                         
         LH    R3,RECLN                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'O/P',(R4),C'DUMP',(R3),=C'2D'                 
*                                                                               
AA240    EQU   *                                                                
         LA    R0,RECLN                                                         
         PUT   FILEOUT,(R0)                                                     
         B     AA600                                                            
*                                                                               
* SEE IF RECORD EXISTS                                                          
AA300    MVI   DMINBTS,X'80'       READ FOR UPDATE                              
         XC    KEY,KEY                                                          
         MVI   KEY,X'27'                                                        
         MVC   KEY+RATNKREP-RATNKEY(2),REP                                      
         MVC   KEY+RATNKGRP-RATNKEY(7),SVSTAG                                   
         MVC   KEY+RATNKTPE-RATNKEY(8),BFTYP                                    
         MVC   KEY+RATNKDPT-RATNKEY(7),BFDPT                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     SEE IF ANY ATNEA RECORD PER STA              
         BNE   AA400                                                            
*                                                                               
         GOTO1 GETREC                                                           
         SR    R3,R3                                                            
         ICM   R3,3,RATNLEN                                                     
         AH    R3,=H'10'           SIZE OF NEW ELEMENT                          
         CH    R3,=H'1000'                                                      
         BNH   AA302                                                            
         MVC   P(25),=C'* ERROR - RECORD TO LARGE'                              
         MVC   P+26(7),RATNKGRP                                                 
         GOTO1 =V(HEXOUT),DMCB,BFREC,P+38,20,=C'T0G'                            
         GOTO1 =V(PRINTER)                                                      
         B     AA100                                                            
*                                                                               
AA302    STCM  R3,3,RATNLEN                                                     
         XC    DUB,DUB                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
AA320    BAS   RE,NEXTEL                                                        
         BNE   AA340                                                            
         SR    RE,RE                                                            
         ICM   RE,3,RATNSCSP-RATNSCDT(R6)                                       
         A     RE,DUB                                                           
         ST    RE,DUB                                                           
         ICM   RE,15,RATNSCCS-RATNSCDT(R6)                                      
         A     RE,DUB+4                                                         
         ST    RE,DUB+4                                                         
         B     AA320                                                            
*                                                                               
AA340    CLC   BFSPOT(8),DUB                                                    
         BE    AA640               NO CHANGE TO RECORD                          
         XC    0(11,R6),0(R6)      CLEAR 1 BYTE BEYOND ELEMENETS                
         MVC   0(2,R6),=X'020A'    BUILD ELEMENT                                
         MVC   2(2,R6),TODAY2                                                   
         L     RE,BFSPOT                                                        
         S     RE,DUB                                                           
         STCM  RE,3,4(R6)                                                       
         STCM  RE,15,BFSPOT        FOR SPOT TOTALS                              
         L     RE,BFDOLS                                                        
         S     RE,DUB+4                                                         
         STCM  RE,15,6(R6)                                                      
         STCM  RE,15,BFDOLS        FOR DOLLAR TOTALS                            
*                                                                               
         OC    YEAR,YEAR           FILTER ON YEAR?                              
         BZ    AA350               NO - SO CONTINUE                             
*                                                                               
         CLC   YEAR,RATNKYM        THIS YEAR?                                   
         BNE   AA640               NO - SO SKIP THIS RECORD                     
*                                                                               
AA350    EQU   *                                                                
*                                                                               
**       MVC   P+1(12),=C'*** DAYPART:'                                         
**       MVC   P+15(2),RATNKDPT                                                 
**       GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   RATNKDPT,C'A'       DAYPART < 'A'?                               
         BNL   AA380               NO  - OKAY TO PROCESS                        
*                                  DAYPARTS ARE ALPHA (A-Z) AND                 
*                                     NUMERIC (0-9)                             
         MVC   P+1(12),=C'BAD DAYPART:'                                         
         MVC   P+15(27),RATNKEY                                                 
         GOTO1 =V(PRINTER)                                                      
         L     RF,BADCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BADCTR                                                        
         B     AA600               SKIP RECORD                                  
*                                                                               
AA380    EQU   *                                                                
         CLI   WRITEFLG,C'Y'                                                    
         BNE   AA500                                                            
*                                                                               
*                                                                               
         BAS   RE,PUTREC                                                        
         B     AA600                                                            
*                                                                               
* ADD A NEW RECORD                                                              
AA400    MVC   RATNKTPE(8),BFTYP                                                
         MVC   RATNKDPT(7),BFDPT                                                
         MVC   RATNLEN,=H'57'                                                   
         LA    R4,RATNCODE                                                      
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               INCASE ELEM 1 LENGTH CHG                     
         USING RATNSCDT,R4                                                      
         MVC   0(2,R4),=X'020A'                                                 
         MVC   RATNSCAS,TODAY2                                                  
         MVC   RATNSCSP,BFSPOT+2                                                
         MVC   RATNSCCS,BFDOLS                                                  
         LA    R3,57                                                            
         DROP  R4                                                               
*                                                                               
         OC    YEAR,YEAR           FILTER ON YEAR?                              
         BZ    AA450               NO - SO CONTINUE                             
*                                                                               
         CLC   YEAR,RATNKYM        THIS YEAR?                                   
         BNE   AA640               NO - SO SKIP THIS RECORD                     
*                                                                               
AA450    EQU   *                                                                
*                                                                               
**       MVC   P+1(12),=C'*** DAYPART:'                                         
**       MVC   P+15(2),RATNKDPT                                                 
**       GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   RATNKDPT,C'A'       DAYPART < 'A'?                               
         BNL   AA480               NO  - OKAY TO PROCESS                        
*                                  DAYPARTS ARE ALPHA (A-Z) AND                 
*                                     NUMERIC (0-9)                             
         MVC   P+1(12),=C'BAD DAYPART:'                                         
         MVC   P+15(27),RATNKEY                                                 
         GOTO1 =V(PRINTER)                                                      
         L     RF,BADCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BADCTR                                                        
         B     AA500               SKIP RECORD                                  
*                                                                               
AA480    EQU   *                                                                
         CLI   WRITEFLG,C'Y'                                                    
         BNE   AA500                                                            
*                                                                               
         BAS   RE,ADDREC                                                        
*        B     AA600                                                            
*                                                                               
AA500    L     R4,AREC                                                          
*        CLC   SVKEY+SVGSTAE(6),=C'TAKUSA'                                      
*        BE    AA520                                                            
*        CLC   SVKEY+SVGSTAE(6),=C'TAWPIV'                                      
*        BNE   AA600                                                            
AA520    CLI   TRACE,C'Y'          TRACE?                                       
         BNE   AA540               NO                                           
         GOTO1 =V(PRNTBL),DMCB,=C'O/P',(R4),C'DUMP',(R3),=C'2D'                 
AA540    EQU   *                                                                
*                                                                               
AA600    CLI   BFTYP,1             ONLY ADD 1 BUYLINES SPOTS/DOLLARS            
         BNE   AA620                                                            
*                                                                               
***      OC    YEAR,YEAR           FILTER ON YEAR?                              
***      BZ    AA610               NO - SO CONTINUE                             
*                                                                               
***      CLC   YEAR,RATNKYM        THIS YEAR?                                   
***      BNE   AA620               NO - SO SKIP THIS RECORD                     
*                                                                               
AA610    EQU   *                                                                
*                                                                               
         L     RE,TSTASP                                                        
         A     RE,BFSPOT                                                        
         ST    RE,TSTASP                                                        
         L     RE,TSTACS                                                        
         A     RE,BFDOLS                                                        
         ST    RE,TSTACS                                                        
AA620    AP    COUNT,=P'1'                                                      
         AP    TCOUNT,=P'1'                                                     
AA640    L     R6,AREC             RESET R6 TO START OF RECORD                  
         B     AA100                                                            
*                                                                               
AAEXT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*  ROUTINE PRINTS TOTAL PER STATION                                             
PRNTSTA  NTR1                                                                   
         CP    LINE,=P'48'                                                      
         BL    PS100                                                            
         ZAP   LINE,=P'100'        FORCE PAGE BREAK                             
         MVC   TITLE+10(22),=C'ATHENA CREATION REPORT'                          
PS100    MVC   P(7),SVSTAG                                                      
         CLI   P+6,C' '                                                         
         BNE   *+8                                                              
         MVI   P+6,C'T'                                                         
         MVC   P+12(29),=CL29'** STATION RECORDS CREATED **'                    
         EDIT  (P3,COUNT),(5,P+45)                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(19),=CL19'** STATION SPOTS **'                                 
         EDIT  (4,TSTASP),(6,P+22)                                              
         MVC   P+30(21),=CL21'** STATION DOLLARS **'                            
         EDIT  (4,TSTACS),(12,P+54)                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P+30(30),=CL30'** BAD DAYPART COUNT (CUME) **'                   
         EDIT  (4,BADCTR),(12,P+64)                                             
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         L     RE,TTLSP                                                         
         A     RE,TSTASP                                                        
         ST    RE,TTLSP                                                         
         L     RE,TTLCS                                                         
         A     RE,TSTACS                                                        
         ST    RE,TTLCS                                                         
*                                                                               
PSEXT    B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE PRINTS TOTAL PER STATION                                             
FINALPRT NTR1                                                                   
         ZAP   LINE,=P'100'        FORCE PAGE BREAK                             
         MVC   TITLE+10(22),=C'ATHENA CREATION REPORT'                          
         MVC   MID1+50(11),=C'FINAL TOTAL'                                      
         MVC   MID2+50(12),=15CL1'-'                                            
         MVC   P(30),=CL30'** NUMBER OF RECORDS ADDED **'                       
         EDIT  (P5,TCOUNT),(8,P+32)                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=CL17'** TOTAL SPOTS **'                                   
         EDIT  (4,TTLSP),(6,P+22)                                               
         MVC   P+30(19),=CL19'** TOTAL DOLLARS **'                              
         EDIT  (4,TTLCS),(12,P+54)                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P+30(23),=CL23'** BAD DAYPART COUNT **'                          
         EDIT  (4,BADCTR),(12,P+54)                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
FNEXT    B     EXIT                                                             
         SPACE 2                                                                
* ROUTINE TO MOVE SDD RECORDS TO GIVEN AREA                                     
* - R2 = ADDRESS OF AREA TO PLACE RECORD                                        
SVSDD    NTR1                                                                   
         LR    RF,R2               STORAGE FOR SDD TABLE                        
         S     RF,=F'27'           BACK UP 27 TO STORE KEY                      
         MVC   0(27,RF),0(R6)      STORE KEY FROM AIO AREA                      
*                                                                               
         LR    RE,R2                                                            
         LA    RF,LSDDTBL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SD100    BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         MVC   0(3,R2),3(R6)       DAYPART - # OF TIMES                         
         LA    R2,3(R2)                                                         
         LR    R3,R6                                                            
         ZIC   R5,5(R6)            # DAY TIMES IN ELEMENT                       
SD200    MVC   0(5,R2),6(R3)                                                    
         LA    R1,1(R2)                                                         
         BAS   RE,ADJTIME                                                       
         CLC   =H'2600',3(R2)      LEAVE 2A ALONE                               
         BE    SD220                                                            
         SR    RE,RE                                                            
         ICM   RE,3,3(R2)                                                       
         BNZ   *+14                                                             
         MVC   3(2,R2),1(R2)       END 0 SAME AS START                          
         B     SD220                                                            
*                                                                               
         SH    RE,=H'1'            ADJUST END TIME                              
         STCM  RE,3,3(R2)                                                       
SD220    LA    R3,5(R3)            NEXT ELEM DAY TIME                           
         LA    R2,5(R2)            NEXT OF TALBE                                
         BCT   R5,SD200                                                         
         B     SD100                                                            
         SPACE                                                                  
* ADJUST TIME IF LESS THAN 600 (001-2A)                                         
ADJTIME  NTR1                                                                   
         LR    R0,2                                                             
AJ100    CLC   =H'600',0(R1)       ADJUST AFTER 1200                            
         BNH   AJ200                                                            
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         AH    RE,=H'2400'                                                      
         STCM  RE,3,0(R1)                                                       
AJ200    LA    R1,2(R1)                                                         
         OC    0(2,R1),0(R1)       IF END TIME 0 DON'T CONVERT                  
         BE    EXIT                                                             
         BCT   R0,AJ100                                                         
         B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE PRINTS TOTAL PER STATION                                             
SETNXSTA NTR1                                                                   
         BAS   RE,RSETBUFF                                                      
*                                                                               
         ZAP   COUNT,=P'0'                                                      
         XC    TSTASP,TSTASP                                                    
         XC    TSTACS,TSTACS                                                    
         XC    MTHTBL,MTHTBL                                                    
*                                                                               
NXEXT    B     EXIT                                                             
         SPACE                                                                  
* GET A MONDAY DATE      -- USES DUB/FULL(6)/HALF --                            
GETMONDT GOTO1 =V(DATCON),DMCB,(2,TODAY2),DUB                                   
         GOTO1 =V(GETDAY),(R1),DUB,FULL                                         
         ZIC   RE,0(R1)            DAY NUMBER OF AIR DATE                       
         LA    R0,1                USE THE START DAY'S NUMBER                   
         SR    R0,RE               DIFFERENCE BETWEEN THE TWO DAYS              
         BZ    MOEXT               THERE IS NONE                                
         GOTO1 =V(ADDAY),(R1),DUB,FULL,(R0)                                     
         GOTO1 =V(DATCON),(R1),(0,FULL),(2,TODAY2)                              
MOEXT    BR    R5                                                               
         EJECT                                                                  
*  BUFFALO ROUTINES                                                             
HIGHBUFF XC    BFKEY,BFKEY                                                      
         LA    R1,=C'HIGH'                                                      
         B     BUFFX                                                            
*                                                                               
SEQBUFF  LA    R1,=C'SEQ'                                                       
         B     BUFFX                                                            
*                                                                               
PUTBUFF  LA    R1,=C'PUT'                                                       
         B     BUFFX                                                            
*                                                                               
RSETBUFF LA    R1,=C'RESET'                                                     
         B     BUFFX                                                            
*                                                                               
BUFFX    NTR1                                                                   
         ST    R1,DMCB                                                          
         GOTO1 =V(BUFFALO),DMCB,,BUFFC,BFREC,1                                  
         TM    DMCB+8,X'80'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*              I/O HANDLING ROUTINES - DIRECTORY                                
         SPACE                                                                  
HIGH     MVC   COMMAND(6),=C'DMRDHI'                                            
         B     DR100                                                            
         SPACE                                                                  
SEQ      MVC   COMMAND(6),=C'DMRSEQ'                                            
         B     DR100                                                            
         SPACE                                                                  
READ     MVC   COMMAND(6),=C'DMREAD'                                            
DR100    MVC   KEYSAVE,KEY                                                      
         B     DIR                                                              
         SPACE                                                                  
WRITE    MVC   COMMAND(6),=C'DMWRT '                                            
         B     DIR                                                              
         SPACE                                                                  
ADD      MVC   COMMAND(6),=C'DMADD '                                            
         SPACE                                                                  
DIR      NTR1                                                                   
         ZIC   R4,DMINBTS                                                       
         GOTO1 =V(DATAMGR),DMCB,((R4),COMMAND),=C'REPDIR',KEY,KEY,0             
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE                                                                  
FILE     NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         ZIC   R4,DMINBTS                                                       
         GOTO1 =V(DATAMGR),DMCB,((R4),COMMAND),=C'REPFILE',            X        
               (R2),AREC,DMWORK                                                 
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVI   DMINBTS,X'08'                                                    
         CLI   8(R1),X'02'                                                      
         BE    EXIT                                                             
         CLI   8(R1),0                                                          
         BZ    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
         SPACE 3                                                                
GETEL    GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BUFNO=2,BLKSIZE=32760                                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
VREGSAVE DC    V(REGSAVE)                                                       
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
         DC    C'*DMCB*'                                                        
DMCB     DS    6F                                                               
BUFFC    DS    A                                                                
AREC     DC    A(REC)                                                           
         DC    C'*KEY'                                                          
KEY      DS    XL34                                                             
KEYSAVE  DS    XL34                                                             
SVKEY    DS    XL34                                                             
HLDKEY   DS    XL34                                                             
DMWORK   DS    XL96                                                             
COMMAND  DS    CL8                                                              
DMINBTS  DC    X'00'                                                            
GRPSTAT  DS    CL7                                                              
*                                                                               
         DS    0F                                                               
WORK     DS    CL80                                                             
UTL      DC    F'0',X'0A'                                                       
SSB      DS    0CL256              NEW SSB                                      
         DC    XL2'00'                                                          
         DC    X'FF'                                                            
         DC    X'02'                                                            
         DC    252X'00'                                                         
SAVNAME  DS    CL10                                                             
SAVLOCAL DS    CL1                                                              
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8'UREPFILE'                                                    
         DC    CL8'UREPDIR '                                                    
         DC    CL8'X'                                                           
BUFEND   DS    C                                                                
         DS    0F                                                               
COLVALS  DS    24XL16                                                           
         DC    C'*REC'                                                          
         DS    0F                                                               
RECLN    DS    F                                                                
REC      DS    1024C               IOAREA                                       
*                                                                               
REP      DS    CL2                 REP CODE                                     
MED      DS    CL1                 MEDIA                                        
STAGFIL  DS    CL7                 STATION GROUP FILTER                         
STAGEFIL DS    CL7                 END STATION GROUP FILTER                     
REJSTA   DS    CL7                 REJECTED STATION GROUP                       
WRITEFLG DS    CL1                 DON'T WRITE                                  
WUPDTE   DS    CL1                 JUST UPDATE RECORDS                          
RECRTE   DS    CL1                 RE-CREATE ATHENA'S                           
DELFLG   DS    CL1                 DELETE ELEMENTTS ONLY                        
PRGALL   DS    CL1                 DELETE ELEMENTTS ONLY                        
TRACE    DS    CL1                 'TRACE RUN' FLAG                             
YEAR     DS    CL1                 TEST RUN YEAR                                
OFFICE   DS    CL2                 TEST RUN OFFICE                              
FRST     DS    CL1                                                              
SERV     DS    CL1                 RATING SERVICE                               
CATG     DS    CL2                 CATEGORY                                     
ADV      DS    CL4                 ADVERTISER                                   
PRD      DS    CL3                 PRODUCT - ALWAYS SPACES                      
SVSTAG   DS    CL7                 LAST CONTRACT GROUP/STATION                  
ELCODE   DS    XL1                                                              
CUTDATE  DS    CL2                 COMPRESSED CUTOFF DATE                       
COUNT    DC    PL3'0'              COUNT OF ATHENA RECS/STATION                 
TCOUNT   DC    PL5'0'              COUNT OF ATHENA RECS/TOTAL                   
DCOUNT   DC    PL5'0'              COUNT OF ATHENA RECS DELETED ELEMS           
DELCNT   DC    PL5'0'              COUNT OF ATHENA RECS DELETED                 
DATADISP DS    H                                                                
TODAY2   DS    H                   TODAY"S COMRESSED DATE                       
ACURSDDT DS    A                   ADDRESS OF STATION SDD REC                   
TSTASP   DS    F                   TOTAL SPOTS/STATION                          
TSTACS   DS    F                   TOTAL DOLLARS/STATION                        
TTLSP    DS    F                   TOTAL SPOTS                                  
TTLCS    DS    F                   TOTAL DOLLARS                                
MTHTBL   DS    CL114               14 BROADCASTS MONTH DATES                    
* MAX NUMBER 0F DAY/TIMES (48) * MAX LENGTH SIZE OF ENTRY (8),                  
* + ENDING ZERO (1) = TOTAL SIZE (385)                                          
LSDDTBL  EQU   48*8+1                                                           
SDDTKEY  DS    XL27                                                             
DEFSDDT  DS    (LSDDTBL)CL1        DEFAULT TV SDD TABLE                         
         ORG   DEFSDDT                                                          
DEFDPT   DS    0CL2                - DAYPART                                    
DEFCNT   DS    0CL1                - COUNT OF DAY TIMES                         
DEFDAY   DS    0CL1                - DAY                                        
DEFTIME  DS    0CL4                - TIME                                       
         ORG   DEFSDDT+LSDDTBL                                                  
SDDRKEY  DS    XL27                                                             
DEFSDDR  DS    (LSDDTBL)CL1        DEFAULT RADIO SDD TABLE                      
         ORG   DEFSDDR                                                          
DEFDPTR  DS    0CL2                - DAYPART                                    
DEFCNTR  DS    0CL1                - COUNT OF DAY TIMES                         
DEFDAYR  DS    0CL1                - DAY                                        
DEFTIMER DS    0CL4                - TIME                                       
         ORG   DEFSDDR+LSDDTBL                                                  
STAXKEY  DS    XL27                                                             
STASDDT  DS    (LSDDTBL)CL1        STATION SDD TABLE                            
         ORG   STASDDT                                                          
STADPT   DS    0CL2                - DAYPART                                    
STACNT   DS    0CL1                - COUNT OF DAY TIMES                         
STADAY   DS    0CL1                - DAY                                        
STATIME  DS    0CL4                - TIME                                       
         ORG   STASDDT+LSDDTBL                                                  
         DS    0F                                                               
BFREC    DS    0CL24               BUFFALO RECORD LAYOUT                        
BFKEY    DS    0CL16                                                            
BFTYP    DS    CL1                 TYPE (1-4)                                   
BFTYPC   DS    CL7                 SERVICE/CATEGORY/ADVERTISER                  
BFDPT    DS    CL1                 DAYPART                                      
BFSDPT   DS    CL1                 SUB DAYPART                                  
BFPGT    DS    CL1                 PROGRAM TYPE                                 
BFSPL    DS    CL2                 SPOT LENGTH                                  
BFYM     DS    CL2                 YEAR MONTH                                   
         DS    CL1                                                              
*                                                                               
BFSPOT   DS    F                   TOTAL SPOTS                                  
BFDOLS   DS    F                   TOTAL DOLLARS                                
*                                                                               
TESTCTR  DS    F                                                                
BADCTR   DS    F                                                                
*                                                                               
REC2     DS    4008C               2ND IOAREA FOR CONTRACT RECORD               
*                                                                               
         BUFF  LINES=1,ROWS=1,COLUMNS=2,FLAVOR=BINARY,                 X        
               KEYLIST=(16,A)                                                   
*                                                                               
PRGEND   EQU   *                                                                
         SPACE 2                                                                
SVIDE    EQU   0                                                                
SVREPE   EQU   2                                                                
*SVGSTAE  EQU   4                   DOESN'T EXIST IN THE 8E KEY                 
SVSTAE   EQU   3                                                                
SVSTAME  EQU   7                                                                
SVCON#   EQU   12                                                               
         SPACE 2                                                                
* REGENBUY                                                                      
* REGENATNA                                                                     
* REGENSDD                                                                      
* DDBUFFALOD                                                                    
* DDDPRINT                                                                      
* REGENCON                                                                      
       ++INCLUDE REGENBUY                                                       
       ++INCLUDE REGENATNA                                                      
       ++INCLUDE REGENSDD                                                       
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE REGENCON                                                       
         ORG   RCONREC+6000                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'209REATHNACR 02/05/07'                                      
         END                                                                    
