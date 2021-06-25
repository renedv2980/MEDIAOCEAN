*          DATA SET PPRECON    AT LEVEL 062 AS OF 06/07/11                      
*PHASE PPRECONA                                                                 
*INCLUDE PPLDDEFN                                                               
*INCLUDE PBLDDEFN                                                               
*INCLUDE CARDS                                                                  
*INCLUDE PUBED                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE IJFVZZWZ                                                               
*INCLUDE IJDFYZZZ                                                               
*INCLUDE SORTER                                                                 
*INCLUDE REGSAVE                                                                
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM'                                 
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
*   RCRI 5/11   SUPPORT NEW STYLE RECOVERY RECS AND DELETE TRAILERS             
*                                                                               
*   BOBY 2/09   UPDATE AND SOFTEN FOR DIRECTORY ONLY RECS                       
*                                                                               
*   BPLA 9/96   OMIT BLKSIZE FROM RECOVERY DCB (WAS 8504)                       
*                                                                               
*   BPLA 6/96   USE DATCON INSTEAD OF DTCNV                                     
*                                                                               
         PRINT NOGEN                                                            
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - INIT'                          
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PPRECON  CSECT                                                                  
         NBASE 0,*PPRECON,=V(REGSAVE)                                           
*                                                                               
         LA    R8,4095(RB)         ESTABLISH R8 AS SECOND BASE REGISTER         
         LA    R8,1(R8)                                                         
         USING PPRECON+4096,R8                                                  
*                                                                               
         LAY   RC,RECONWKC         ESTABLISH RC AS WORKAREA BASE                
*                                                                               
         LA    RA,1(RC)            ESTABLISH RA AS SECOND WORKAREA BASE         
         LA    RA,4095(RA)                                                      
         USING RECONWK,RC,RA                                                    
*                                                                               
         LA    RE,RECONWK          CLEAR WORK                                   
         LHI   RF,RECONWKX-RECONWK                                              
         XCEF                                                                   
*                                                                               
         L     R9,VPPDEFN          ASSUME PRTFILE BEING RECONSTRUCTED           
         USING LDDEFND,R9          ESTABLISH FILE DEFINITION AREA               
*                                                                               
         L     R7,=V(CPRINT)       ESTABLISH PRINT WORKAREA                     
         USING DPRINT,R7                                                        
*                                                                               
         GOTOR LLOADER,DMCB,=CL8'T00AAB',0,0  LOAD A(GETINS)                    
*                                                                               
         ICM   RF,15,DMCB+4        GET ADDRES                                   
         BNZ   *+6                                                              
         DC    H'0'                PHASE NOT FOUND                              
*                                                                               
         ST    RF,VGETINS          SAVE PHASE ADDRESS                           
*                                                                               
INITIAL  DS    0H                                                               
*                                                                               
*        INIT SORT                                                              
*                                                                               
         XC    DMCB(24),DMCB       INIT DMCB                                    
*                                                                               
         GOTO1 LSORTER,DMCB,SORTCARD,RECCARD                                    
*                                                                               
         B     INIT1                                                            
*                                                                               
*        **SORT ON KEY/DATE/REC NO.                                             
*                                                                               
*        69 = RECLEN+L'REXP+L'RECVDHDR+1                                        
*              REXP - GETINS DATA FOR INSERTION                                 
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(69,25,A,39,6,A),FORMAT=BI,WORK=1'              
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4200'                                  
*                                                                               
INIT1    DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        OPEN RECOVERY FILE                                                     
*                                                                               
         LAY   R2,RECOV                                                         
*                                                                               
         OPEN  ((R2),(INPUT))                                                   
*                                                                               
         ZAP   LINE,=PL2'99'       FORCE NEW PAGE ON PRINT                      
         MVC   TITLE(20),=C'PRINTPAK RECONSTRUCT'                               
*                                                                               
         MVI   FILTYP,0            INIT FILE ID                                 
*                                                                               
*        READ CONTROL CARDS                                                     
*                                                                               
CARDLOOP DS    0H                                                               
*                                                                               
         GOTO1 LCARDS,DMCB,CARD,=C'RE00'                                        
*                                                                               
         CLC   =C'/*',CARD         DONE AT END OF CONTROL CARDS                 
         BE    CARDDONE                                                         
*                                                                               
         CLC   =C'FILE=',CARD                                                   
         BNE   CARDERR                                                          
*                                                                               
         CLC   =C'PRT',CARD+5                                                   
         BNE   *+12                                                             
         L     R9,VPPDEFN          USE PRTFILE DEFINTION                        
         B     CARDCONT                                                         
*                                                                               
         CLC   =C'PUB',CARD+5                                                   
         BNE   *+12                                                             
         L     R9,VPBDEFN          USE PUBFILE DEFINITION                       
         B     CARDCONT                                                         
*                                                                               
         B     CARDERR             ELSE ERROR                                   
*                                                                               
CARDCONT DS    0H                                                               
*                                                                               
         MVC   FILTYP,LDDDTFD1+3   SET FILE TYPE TO PRT OR PUB                  
*                                                                               
         MVC   P(80),CARD          PRINT CARD                                   
         GOTO1 LPRINTER                                                         
*                                                                               
         B     CARDLOOP                                                         
*                                                                               
*        CONTROL CARD IN ERROR                                                  
*                                                                               
CARDERR  DS    0H                                                               
*                                                                               
         MVC   P(80),CARD          PRINT CARD                                   
         MVC   P+82(14),=C'**CARD ERROR**'                                      
*                                                                               
         GOTO1 LPRINTER                                                         
*                                                                               
         B     PPRECONX                                                         
*                                                                               
CARDDONE DS    0H                                                               
*                                                                               
         MVC   LINE,=PL2'99'       FORCE NEW PAGE ON PRINT                      
*                                                                               
*        SET REPORT TITLE AND MID-LINES                                         
*                                                                               
         CLI   FILTYP,X'42'        IF PRTFILE                                   
         BNE   INIT4                                                            
*                                                                               
         MVC   TITLE(20),=C'PRTFILE RECONSTRUCT '                               
         MVC   MID1(132),=C'  AG M CLT PRD PUB             DATE     ESTX        
                LN SIN   PR ACTION               GROSS         NET   CAX        
               SH DISC    PAID NET PAID ACTUAL  '                               
         MVC   MID2(132),=C'  -- - --- --- --------------- -------- ---X        
                -- ----- -- ------               -----         ---   --X        
               -------    -------- -----------  '                               
         B     INIT8                                                            
*                                                                               
INIT4    DS    0H                                                               
*                                                                               
         CLI   FILTYP,X'43'        IF PUBFILE                                   
         BNE   INIT6                                                            
*                                                                               
         MVC   TITLE(20),=C'PUBFILE RECONSTRUCT '                               
         MVC   MID1(132),=C'  M PUB             AG RC                  X        
                   SIN   PR ACTION                                     X        
                                                '                               
         MVC   MID2(132),=C'  - --------------- -- --                  X        
                   ---   -- ------                                     X        
                                                '                               
         B     INIT8                                                            
*                                                                               
INIT6    DS    0H                                                               
*                                                                               
         MVC   P(33),=C'**PRTFILE/PUBFILE NOT SPECIFIED**'                      
         GOTO1 LPRINTER                                                         
*                                                                               
         B     PPRECONX                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
INIT8    DS    0H                                                               
*                                                                               
         MVI   DCSW,0              INIT DATE CHANGE SWITCH                      
         MVI   LASTTY,0            INIT LAST RECORD TYPE                        
         MVI   CCERR,0             INIT ERROR CODE                              
         XC    CRECIN,CRECIN       INIT RECOVERY RECORDS COUNTER                
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - INPUT'                         
***********************************************************************         
*                                                                     *         
*        INPUT ROUTINE                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INPUT    DS    0H                                                               
*                                                                               
INLOOP   DS    0H                                                               
*                                                                               
         LAY   R1,RECOV            READ 1ST/NEXT RECORD FROM RECOVERY           
         LA    R0,RECVHDR-4        RECOVERY FILE                                
         GET   (1),(0)                                                          
*                                                                               
         CLI   RRECTY,X'81'        IGNORE POINTER COPY/CHANGES                  
         BE    INLOOP                                                           
         CLI   RRECTY,X'82'                                                     
         BE    INLOOP                                                           
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    INLOOP                                                           
         CLI   RSIN,X'FE'          OFFLINE COPY/CHANGE PAIR                     
         BNE   *+10                                                             
         MVC   RSIN,=F'1'                                                       
         CLI   RSIN,X'FD'          OFFLINE CHANGES ONLY                         
         BNE   *+10                                                             
         MVC   RSIN,=F'0'                                                       
*                                                                               
         L     R1,CRECIN           BUMP RECOVERY FILE RECORD COUNTER            
         LA    R1,1(R1)                                                         
         ST    R1,CRECIN                                                        
*                                                                               
         CLC   RFILTY,LDDDTFDA+3   TEST RIGHT DA FILE                           
         BE    INLOOP1                                                          
*                                                                               
         TM    LDDMULTI,LDDQIPTR   SKIP IF NO DIR ONLY RECS                     
         BNO   INLOOP                                                           
*                                                                               
         CLC   RFILTY,LDDDTFIS+3   IF IS FILE RECORD                            
         BNE   INLOOP                                                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LDDIPDSP       DISPLACEMENT TO DIR ONLY INDICATOR           
*                                                                               
         LA    R1,RKEY(RE)         POINT TO DIRECTORY ONLY INDICATOR            
*                                                                               
         LLC   RF,LDDIPLEN         LENGTH OF DIR ONLY INDICATOR                 
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),LDDIPARG    SKIP IF NOT DIR ONLY RECORD                  
         BNE   INLOOP                                                           
*                                                                               
INLOOP1  LH    R1,RECVHDR-4        SET R1 TO POINT TO END OF RECORD             
         LA    R1,RECVHDR-4(R1)                                                 
         TM    RTIME,X'40'         TEST IF THERE IS A TRAILER                   
         BZ    INLOOP2             NO                                           
         NI    RTIME,255-X'40'     YES-REMOVE TRAILER                           
         BCTR  R1,0                                                             
         SR    RF,RF                                                            
         IC    RF,0(R1)            RF=L'TRAILER IN LAST BYTE                    
         SR    R1,RF                                                            
         LA    R1,1(R1)            R1=A(END OF RECORD WITHOUT TRAILER)          
         LA    RF,RECVHDR-4                                                     
         LR    RE,R1                                                            
         SR    RE,RF                                                            
         STH   RE,RECVHDR-4        SET NEW RECORD LENGTH                        
*                                                                               
INLOOP2  MVI   0(R1),0             SET ZERO AT END OF RECORD                    
*                                                                               
*                                                                               
*        ANALYZE RECORD TYPE                                                    
*                                                                               
         CLI   RRECTY,3            ADD                                          
         BE    ADD                                                              
         CLI   RRECTY,2            CHANGE                                       
         BE    CHG                                                              
         CLI   RRECTY,1            COPY                                         
         BE    CPY                                                              
*                                                                               
         B     INLOOP              UNKNOWN IGNORE RECORD                        
*                                                                               
*        ADD OF NEW RECORD                                                      
*                                                                               
ADD      DS    0H                                                               
*                                                                               
         CLI   LASTTY,1            IF ADD, LAST RECORD CAN'T BE COPY            
         BNE   *+8                                                              
         BRAS  RE,CPYERR                                                        
*                                                                               
         MVI   LASTTY,3            INDICATE LST TYPE WAS AN ADD                 
*                                                                               
         XC    ACTWRK,ACTWRK       INIT INSERTION WORKAREA                      
*                                                                               
         CLI   FILTYP,X'42'        SKIP IF NOT BUY RECORD                       
         BNE   ADD2                                                             
         CLI   RKEY+3,X'20'                                                     
         BNE   ADD2                                                             
*                                                                               
         LA    R2,RKEY             POINT TO INSERTION RECORD                    
         BRAS  RE,GTINS            GET GETINS DATA                              
*                                                                               
         MVC   ACTAMTS,WORK        SAVE BUCKETS FOR INSERTION                   
*                                                                               
ADD2     DS    0H                                                               
*                                                                               
         MVC   ATRANS,=C'A '       INIDCATE ADD TRANSACTION                     
*                                                                               
         B     INCONT                                                           
*                                                                               
*        COPY OF CURRENT RECORD                                                 
*                                                                               
CPY      DS    0H                                                               
*                                                                               
         CLI   LASTTY,1            PREVIOUS RECORD CAN'T BE A COPY              
         BNE   *+8                                                              
         BRAS  RE,CPYERR                                                        
*                                                                               
         MVI   LASTTY,1            INDICATE LAST REC IS NOW A COPY              
*                                                                               
         XC    SAVWRK,SAVWRK       INIT INSERTION DATA                          
*                                                                               
         CLI   FILTYP,X'42'        IF INSERTION RECORD                          
         BNE   CPY2                                                             
         CLI   RKEY+3,X'20'                                                     
         BNE   CPY2                                                             
*                                                                               
         LA    R2,RKEY                GET INSERTION DATA FROM GETINS            
         BRAS  RE,GTINS                                                         
*                                                                               
         MVC   SAVAMTS,WORK           SAVE BUCKETS                              
*                                                                               
CPY2     DS    0H                                                               
*                                                                               
         MVC   SAVKEY(31),RKEY     SAVE INSERTION KEY                           
         MVC   SAVSIN,RSIN         SAVE SYSTEM INPUT NUMBER                     
*                                                                               
         B     INLOOP                                                           
*                                                                               
*        CHANGE RECORD                                                          
*                                                                               
CHG      DS    0H                                                               
*                                                                               
         OC    RSIN,RSIN           SKIP IF ON-LINE OFF-LINE                     
         BZ    CHG1                YES                                          
*                                                                               
         CLI   LASTTY,1            LAST RECORD MUST HAVE BEEN A COPY            
         BE    CHG1                                                             
*                                                                               
         BRAS  RE,CHGERR                                                        
         B     INLOOP                                                           
*                                                                               
CHG1     DS    0H                                                               
*                                                                               
         MVI   LASTTY,2            INDICATE CHANGE RECORD                       
*                                                                               
         XC    ACTWRK,ACTWRK       INIT SAEREA                                  
*                                                                               
         CLI   FILTYP,X'42'        SKIP IF NOT BUY INSERTION RECORD             
         BNE   CHG6                                                             
         CLI   RKEY+3,X'20'                                                     
         BNE   CHG6                                                             
*                                                                               
         CLI   RPRG,0              SKIP IF OFF-LINE                             
         BE    CHG6                                                             
*                                                                               
         LA    R2,RKEY                                                          
         BRAS  RE,GTINS            GET FINANCIAL DATA                           
*                                                                               
         MVC   ACTAMTS,WORK        SAVE FINANCIAL DATA                          
*                                                                               
         TM    RKEY+27,X'80'       IF A DELETED INSERTION                       
         BNO   CHG2                                                             
         TM    SAVKEY+27,X'80'     AND COPY NOT DELETED                         
         BO    CHG2                                                             
*                                                                               
         LA    R0,3                                                             
         BRAS  RE,CMPLO               COMPLEMENT ORDERED ONLY                   
*                                                                               
         MVC   ACTAMTS,SAVAMTS        SAVE ALTERED FINANCIAL DATA               
*                                                                               
         MVC   ATRANS,=C'D '          INDICATE DELETE TRANSACTION               
*                                                                               
         B     INCONT                                                           
*                                                                               
CHG2     DS    0H                                                               
*                                                                               
         OC    RSIN,RSIN           SKIP IF OFF-LINE                             
         BZ    CHG4                YES - NO KEY CHG                             
*                                                                               
         CLC   SAVKEY,RKEY         IF KEY OF RECORD CHANGED                     
         BE    CHG4                                                             
*                                                                               
         MVI   DCSW,1                 SET CHANGED SWITCH                        
         MVC   ADATE,SAVKEY+16        SAVE OLD INSERTION DATE                   
*                                                                               
         B     CHG6                                                             
*                                                                               
CHG4     DS    0H                                                               
*                                                                               
         BRAS  RE,SUBT             CALCULATE NEW FINANCIAL DATA                 
*                                                                               
         XC    SAVALL,SAVALL       INIT SAVEAREA                                
*                                                                               
CHG6     DS    0H                                                               
*                                                                               
         MVC   ATRANS,=C'C '       INDICATE CHANGE TRANSACTION                  
*                                                                               
         CLI   RPRG,3              IF PAY PROGRAM SOURCE                        
         BNE   *+10                                                             
         MVC   ATRANS,=C'P '          INDICATE PAY TRANSACTION                  
*                                                                               
         OC    ADATE,ADATE         IF ACTIVITY DATE SET                         
         BZ    *+10                                                             
         MVC   ATRANS,=C'CF'          SET TRANSACTION CODE                      
*                                                                               
         B     INCONT                                                           
*                                                                               
INCONT   DS    0H                                                               
*                                                                               
*        WRITE RECORD TO SORT                                                   
*                                                                               
         LH    R4,RECVHDR-4        CALCULATE LENGTH OF SORT RECORD              
         LA    R4,L'REXP(R4)       RECOVERY REC LEN PLUS L'SAVEAREA             
         STH   R4,RREC-4                                                        
*                                                                               
         XC    RREC-2(2),RREC-2                                                 
*                                                                               
         MVC   REXP1,ACTWRK        ADD FINANCIAL DATA TO SORT RECORD            
         MVC   REXPRNO,CRECIN      RECORD NUMBER                                
*                                                                               
         GOTO1 LSORTER,DMCB,=C'PUT',RREC-4                                      
*                                                                               
         CLI   DCSW,0              SKIP IF NOT DATE CHANGE                      
         BE    INLOOP                                                           
*                                                                               
*        INSERTION DATE CHANGE TRANSACTION                                      
*                                                                               
         LA    R0,5                                                             
         BRAS  RE,CMPLO            COMPLEMENT ALL FINANCIAL DATA                
*                                                                               
         MVC   ATRANS,=C'CT'       INDICATE DATE CHANGE TRANSACTION             
         MVC   ADATE,RKEY+16       COPY INSERTION DATE                          
         MVC   ACTAMTS,SAVAMTS     PASS FINANCIAL DATA OF COPY                  
         MVC   RKEY(31),SAVKEY     COPY COPY RECORD KEY                         
*                                                                               
         MVC   RECVHDR-4(2),=H'59' SET RECORD LENGTH                            
*                                  RECORD KEY LENGTH(31)                        
*                                  RECOVERY HEADER LENGTH(24)                   
*                                  RECORD LENGTH BYTE (4)                       
*                                                                               
         MVI   DCSW,0              CLEAR DATE CHANGE SWITCH                     
*                                                                               
         XC    SAVALL,SAVALL       INIT SAVED FINANCIAL DATA                    
*                                                                               
         B     INCONT              ADD EXTRA RECORD TO SORT                     
*                                                                               
*        END OF RECOVERY FILE                                                   
*                                                                               
RECVEOT  DS    0H                                                               
*                                  CLOSE RECOVERY FILE                          
         LAY   R1,RECOV                                                         
         CLOSE ((R1),)                                                          
         MVI   RKEY,X'FF'          FORCE HIGH KEY                               
*                                                                               
*        END OF INPUT PHASE                                                     
*                                                                               
INPUTX   DS    0H                                                               
*                                                                               
         OC    CRECIN,CRECIN       TEST ANY INPUT                               
         BNZ   OUTPUT                                                           
*                                                                               
         MVC   P(40),=C'**NO RECOVERY INPUT - RUN DISCONTINUED**'               
*                                                                               
         GOTO1 LPRINTER                                                         
*                                                                               
         GOTO1 LSORTER,DMCB,=C'END'     END SORT                                
*                                                                               
         ABEND 662                                                              
*                                                                               
         B     PPRECONX            SHOULD NEVER GET HERE                        
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - OUTPUT'                        
***********************************************************************         
*                                                                     *         
*        OUTPUT ROUTINES                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OUTPUT   DS    0H                                                               
*                                                                               
*        OPEN INPUT AND OUTPUT FILES                                            
*                                                                               
         LAY   R2,IN               POINT TO DCBS                                
         LAY   R3,OUT                                                           
*                                                                               
         OPEN  ((R2),(INPUT),(R3),(OUTPUT))                                     
*                                                                               
*              INPUT  FILE IS OLD FILE DUMP TAPE                                
*              OUTPUT FILE IS NEW FILE LOAD TAPE                                
*                                                                               
         XC    CLTTOTS,CLTTOTS     INIT CLIENT TOTALS                           
         XC    AGYTOTS,AGYTOTS     INIT AGENCY TOTALS                           
         XC    RKEY,RKEY           INIT RECORD KEY                              
         XC    SAVKEY(31),SAVKEY   INIT SAVEKEY AND DISC ADDR                   
         MVI   BUYACT,0            INIT BUY ACTION                              
*                                                                               
         BRAS  RE,RDTAP            READ RECORD FROM DUMP TAPE                   
*                                  RECORD KEY AT FKEY                           
*                                                                               
OUTRCVLP DS    0H                                                               
*                                                                               
*        GET RECOVERY FILE RECORD FROM SORT                                     
*                                                                               
         GOTO1 LSORTER,DMCB,=C'GET'                                             
*                                                                               
         L     R3,4(R1)            POINT TO RETURNED SORT RECORD                
*                                                                               
         LTR   R3,R3               IF NO RECORD RETURNED                        
         BNZ   *+14                                                             
         MVC   NEXTKEY,=25X'FF'       FORCE HIGH KEY                            
         B     OUTRCV10                                                         
*                                                                               
         MVC   NEXTKEY,68(R3)      SAVE RECOVERY REC KEY FROM SORT REC          
*                                                                               
OUTRCV10 DS    0H                                                               
*                                                                               
OUTDMPLP DS    0H                                                               
*                                                                               
         CLC   FKEY,RKEY           DUMP RECORD VS RECOVERY RECORD               
         BL    OUTDMP6                LOW                                       
         BH    OUTDMP8                HIGH                                      
*                                                                               
*                                     EQUAL                                     
*                                                                               
         CLI   FKEY,X'FF'             IF BOTH RECORDS HAVE HIGH KEYS            
         BE    OPDONE                    ALL DONE                               
*                                                                               
         B     OUTDMP8                                                          
*                                                                               
*        DUMP RECORD LOW                                                        
*                                                                               
OUTDMP6  DS    0H                                                               
*                                                                               
         LA    R2,FKEY-4           WRITE DUMP RECORD TO LOAD TAPE               
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,RDTAP            READ NEXT DUMP RECORD                        
*                                                                               
         B     OUTDMPLP                                                         
*                                                                               
*        DUMP RECORD HIGH OR EQUAL                                              
*                                                                               
OUTDMP8  DS    0H                                                               
*                                                                               
         OC    RKEY,RKEY           SKIP IF NO RECOVERY RECORD                   
         BZ    OUTDMP8A                                                         
*                                                                               
         BRAS  RE,FMTREC           FORMAT RECOVERY RECORD                       
*                                                                               
         CLC   RKEY,NEXTKEY        COMPARE RECOVERY KEY TO LAST KEY             
         BNE   OUTDMP9                                                          
*                                                                               
OUTDMP8A DS    0H                                                               
*                                                                               
         BRAS  RE,MOVREC                                                        
*                                                                               
         B     OUTRCVLP            GET NEXT RECOV REC                           
*                                                                               
*        NO CHANGE IN KEY OF RECOVERY RECORD                                    
*                                                                               
OUTDMP9  DS    0H                                                               
*                                                                               
         CLC   ATRANS,=C'CT'       SKIP IF CHANGE IN DATE                       
         BE    OP10                                                             
*                                                                               
         LH    R2,RREC-4           GET RECOVERY RECORD LENGTH                   
         SHI   R2,L'REXP+24        DECREMENT BY HEADER LENGTH                   
         STH   R2,RKEY-4           SET LENGTH FOR RECOVERY REC W/O HDRS         
*                                                                               
         XC    RKEY-2(2),RKEY-2    CLEAR EXTRA BYTES IN LENGTH FIELD            
*                                                                               
         LA    R2,RKEY-4           PUT RECOVERY RECORD TO LOAD TAPE             
         BRAS  RE,PUT                                                           
*                                                                               
         B     OP10                                                             
*                                                                               
*        CHANGE IN DATE                                                         
*                                                                               
OP10     DS    0H                                                               
*                                                                               
         CLC   FKEY,RKEY           IF DUMP TAPE KEY SAME AS RECVRY KEY          
         BNE   *+8                                                              
         BRAS  RE,RDTAP               READ NEXT RECOVERY RECORD                 
*                                                                               
         BRAS  RE,MOVREC           MOVE RECOVERY RECORD TO RECVRY AREA          
*                                                                               
         CLI   RKEY,X'FF'          IF NO RECORD FOUND                           
         BE    OUTDMPLP               GET NEXT RECORD ON DUMP TAPE              
*                                                                               
         B     OUTRCVLP            ELSE GET NEXT RECOV REC                      
*                                                                               
OPDONE   DS    0H                                                               
*                                                                               
         BRAS  RE,PRT              PRINT 2 BLANK LINES                          
         BRAS  RE,PRT                                                           
*                                                                               
*        PRINT PROGRAM COUNTERS                                                 
*                                                                               
         LA    R2,COUNTS           POINT TO COUNTERS TABLE                      
*                                                                               
OPDCTRLP DS    0H                                                               
*                                                                               
         EDIT  (P5,0(R2)),(11,P+2),COMMAS=YES  PRINT COUNTER                    
*                                                                               
         MVC   P+15(33),7(R2)      PRINT COUNTER TITLE                          
*                                                                               
         BRAS  RE,PRT              PRINT LINE                                   
*                                                                               
OPDCTRCN DS    0H                                                               
*                                                                               
         LA    R2,40(R2)           BUMP TO NEXT COUNTER ENTRY                   
         C     R2,=A(COUNTSX)      CONTINUE IF NOT AT END OF TABLE              
         BL    OPDCTRLP                                                         
*                                                                               
OPDCTRDN DS    0H                                                               
*                                                                               
         ZAP   DUB,CIN             GET # OF DUMP TAPE RECORD                    
         AP    DUB,CADD            PLUS NUMBER OF ADDITIONS TO TAPE             
         SP    DUB,CDIROVS         LESS DIRECTORY OVERLAYS                      
*                                                                               
         CP    DUB,COUT            OKAY IF EQUAL # OF LOAD TAPE RECORDS         
         BE    OPD4                                                             
*                                                                               
         MVC   P+2(L'WARN),WARN    ELSE PRINT WARNING                           
         BRAS  RE,PRT                                                           
*                                                                               
OPD4     DS    0H                                                               
*                                                                               
         CLI   CCERR,0             SKIP IF NO ERRORS                            
         BE    OPD6                                                             
*                                                                               
         MVC   P+2(49),=C'**THERE ARE COPY/CHG ERRORS - SEE START OF LIX        
               ST**'                                                            
*                                                                               
         BRAS  RE,PRT                                                           
*                                                                               
OPD6     DS    0H                                                               
*                                                                               
         LAY   R2,IN               CLOSE DUMP AND LOAD TAPES                    
         LAY   R3,OUT                                                           
*                                                                               
         CLOSE ((R2),,(R3))                                                     
*                                                                               
OPD8     DS    0H                                                               
*                                                                               
         GOTO1 LSORTER,DMCB,=C'END'   STOP SORT                                 
*                                                                               
PPRECONX DS    0H                  ALL DONE                                     
         XBASE                                                                  
*                                                                               
VPPDEFN  DC    V(PPLDDEFN)         PRTFILE FILE DEFINITION                      
VPBDEFN  DC    V(PBLDDEFN)         PUBFILE FILE DEFINITION                      
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - FMTREC'                        
***********************************************************************         
*                                                                     *         
*        FORMAT RECOVERY RECORD                                       *         
*               TOTAL AND PRINT RECOVERY FILE RECORDS                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
FMTREC   NTR1  LABEL=*                                                          
*                                                                               
         MVC   ACTWRK,REXP         SAVE FINANCIAL DATA IN SORT RECORD           
*                                                                               
         CLI   FILTYP,X'42'        SKIP IF NOT PRTFILE RECORD                   
         BNE   FR15                                                             
*                                                                               
         LA    R2,RKEY             ESTABLISH BUY RECORD KEY                     
         USING PBUYRECD,R2                                                      
*                                                                               
         CLI   PBUYKRCD,X'20'      SKIP IF NOT A BUY RECORD                     
         BNE   FR10                                                             
*                                                                               
*        FORMAT BUY RECORD                                                      
*                                                                               
         MVI   BUYACT,X'C0'        INDICATE CLT/AGY ACTIVITY                    
*                                                                               
*        PRINT BUY RECORD DATA                                                  
*                                                                               
         MVC   PRAGY,PBUYKAGY      AGENCY                                       
         MVC   PRMED,PBUYKMED      MEDIA                                        
         MVC   PRCLT,PBUYKCLT      CLIENT                                       
         MVC   PRPRD,PBUYKPRD      PRODUCT                                      
*                                                                               
         GOTO1 =V(PUBED),DMCB,PBUYKPUB,PRPUB PUB                                
*                                  INSERTION DATE                               
         GOTO1 LDATCON,DMCB,(3,PBUYKDAT),(5,PRDATE)                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,PBUYKEST       ESTIMATE NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PREST,DUB                                                        
*                                                                               
         SR    R0,R0               BUY LINE NUMBER                              
         IC    R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PRLIN,DUB                                                        
*                                                                               
         CLI   PRLIN,C'0'          IF LEADING ZERO                              
         BNE   *+14                                                             
         MVC   PRLIN,PRLIN+1          SHIFT LEFT 1 POSITION                     
         MVI   PRLIN+L'PRLIN-1,C' '                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
         LA    R2,ACTAMTS          POINT TO ACCUMULATORS                        
         LA    R5,5                5 ACCUMULATORS                               
         LA    R6,PRAMTS           START OF PRINTING AREA                       
*                                                                               
FRAMTLP  DS    0H                                                               
*                                                                               
         EDIT  (B4,0(R2)),(12,0(R6)),2,MINUS=YES                                
*                                                                               
         LA    R6,L'PRAMTS(R6)     BUMP TO NEXT PRINT AREA                      
         LA    R2,L'AGROSS(R2)     BUMP TO NEXT ACCUMULATOR                     
         BCT   R5,FRAMTLP                                                       
*                                                                               
*        ROLL AMOUNTS TO CLIENT AND AGENCY TOTALS                               
*                                                                               
         SR    R2,R2                                                            
         LA    R0,5                                                             
*                                                                               
FRROLLLP DS    0H                                                               
*                                                                               
         L     R1,ACTAMTS(R2)                                                   
         A     R1,CLTTOTS(R2)                                                   
         ST    R1,CLTTOTS(R2)                                                   
*                                                                               
         L     R1,ACTAMTS(R2)                                                   
         A     R1,AGYTOTS(R2)                                                   
         ST    R1,AGYTOTS(R2)                                                   
*                                                                               
FRROLLCN DS    0H                                                               
*                                                                               
         LA    R2,4(R2)            BUMP TO NEXT ACCUMULATOR                     
*                                                                               
         BCT   R0,FRROLLLP                                                      
*                                                                               
         B     FR20                                                             
*                                                                               
*        NON-BUY RECORDS                                                        
*                                                                               
FR10     DS    0H                                                               
*                                                                               
         MVC   PRTRANS,ATRANS      PRINT TRANSACTION CODE                       
         GOTO1 LHEXOUT,DMCB,RKEY,PRKEY,29,=C'NORM'  DUMP REC KEY                
*                                                                               
         B     FR20                                                             
*                                                                               
*        PUB RECS                                                               
*                                                                               
FR15     DS    0H                                                               
*                                                                               
         MVC   PRPBMED,RKEY        MEDIA                                        
*                                                                               
         GOTO1 =V(PUBED),DMCB,RKEY+1,PRPBNUM  PUB NUMBER                        
*                                                                               
         MVC   PRPBAGY,RKEY+7      AGENCY                                       
*                                                                               
         GOTO1 LHEXOUT,DMCB,RKEY+9,PRPBTYP,1,=C'NORM' REC TYPE                  
*                                                                               
         B     FR20                                                             
*                                                                               
*        PRINT SYSTEM INPUT NUMBER, TRANSACTION AND ACTIVITY DATE               
*                                                                               
FR20     DS    0H                                                               
*                                                                               
         L     R0,RSIN             PRINT SYSTEM INPUT NUMBER                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PRSIN,DUB                                                        
*                                                                               
         GOTO1 LHEXOUT,DMCB,RPRG,PRPRG,1,=C'NORM'  PROGRAM                      
*                                                                               
         MVC   PRTRAN,ATRANS       TRANSACTION CODE                             
*                                                                               
         OC    ADATE,ADATE         SKIP IF NO TRANSACTION DATE                  
         BZ    FR20B                                                            
*                                                                               
         MVC   PRTOFRM,=C'TO'      ASSUME TRANSFER 'TO'                         
*                                                                               
         CLI   ATRANS+1,C'T'       CHECK SECONDARY TRANS CODE                   
         BE    *+10                                                             
         MVC   PRTOFRM,=C'FR'      TRANSFER 'FROM                               
*                                                                               
         GOTO1 LDATCON,DMCB,(3,ADATE),(5,PRTDATE)   TRANS DATE                  
*                                                                               
FR20B    DS    0H                                                               
*                                                                               
         CLI   ATRANS,C'A'         'ADD' OF KEY ON FILE =                       
         BNE   FR20D               'DIRECTORY OVERLAY'                          
*                                                                               
         CLC   SAVKEY,RKEY         SKIP IF KEY NE LAST KEY                      
         BNE   FR20B1                                                           
*                                                                               
         TM    SAVKEY+27,X'80'     SKIP IF RECORD DELETED                       
         BO    FR20C                                                            
*                                                                               
         MVC   PRTOFRM(6),=C'******'                                            
*                                                                               
         B     FR20D                                                            
*                                                                               
FR20B1   DS    0H                                                               
*                                                                               
         CLC   FKEY,RKEY           SKIP IF CHANGE IN KEY                        
         BNE   FR20D                                                            
*                                                                               
FR20C    DS    0H                                                               
*                                                                               
         AP    CDIROVS,=P'1'       BUMP DIRECORY OVERLAYS COUNTER               
         MVC   P+58(6),=C'DIR OV'  PRINT IDENTIFIER                             
*                                                                               
FR20D    DS    0H                                                               
*                                                                               
         MVC   SAVKEY(31),RKEY     SAVE RECOVERY FILE KEY                       
*                                                                               
         BRAS  RE,PRT              PRINT LINE                                   
*                                                                               
*        INCREMENT TRANSACTION COUNTERS                                         
*                                                                               
         LA    R2,COUNTS           POINT TO TABLE OF COUNTERS                   
*                                                                               
FR21     DS    0H                                                               
*                                                                               
         CLC   ATRANS,5(R2)        MATCH TRANSACTION CODE                       
         BE    *+12                                                             
         LA    R2,40(R2)           NEXT ENTRY IN TABLE                          
         B     FR21                                                             
*                                                                               
         AP    0(5,R2),=P'1'       BUMP COUNTER                                 
*                                                                               
FMTRECX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - RDTAP'                         
***********************************************************************         
*                                                                     *         
*        MOVE A VARIABLE LENGTH RECORD TO RECOVERY REC AREA           *         
*                                                                     *         
*NTR     R3==> INCOMING RECORD                                        *         
*                                                                     *         
***********************************************************************         
         DS    0D                  ALIGNMENT                                    
MOVREC   NTR1  LABEL=*                                                          
*                                                                               
         CLI   NEXTKEY,X'FF'       IF AT END OF TAPE INPUT                      
         BNE   MOVREC2                                                          
*                                                                               
         MVC   RKEY,NEXTKEY           MOVE HIGH VALUES TO RECORD AREA           
*                                                                               
         B     MOVRECX                DONE                                      
*                                                                               
MOVREC2  DS    0H                  MOVE VARIABLE LENGTH RECORD                  
*                                                                               
         LH    R1,0(R3)                                                         
         LA    RF,RREC-4           MOVE TO RECOVERY RECORD SAVEAREA             
         LR    RE,R3                                                            
*                                                                               
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
MOVRECX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - RDTAP'                         
***********************************************************************         
*                                                                     *         
*        READ RECORD FROM INPUT DUMP TAPE                             *         
*                                                                     *         
*              RECORD READ INTO FREC AREA                             *         
*                                                                     *         
***********************************************************************         
         DS    0D                  ALIGNMENT                                    
RDTAP    NTR1  LABEL=*                                                          
*                                                                               
         CLC   FKEY,=25X'FF'       SKIP IF END OF TAPE ALREADY REACHED          
         BE    RDTAPX                                                           
*                                  READ FROM TAPE INTO FREC                     
         LAY   R1,IN                                                            
         LA    R0,FREC-4                                                        
         GET   (1),(0)                                                          
*                                                                               
         AP    CIN,=P'1'           BUMP INPUT RECORD COUNTER                    
*                                                                               
         B     RDTAPX                                                           
*                                                                               
EOIN     DS    0H                  END OF INPUT TAPE                            
*                                                                               
         MVC   FKEY,=25X'FF'       SET HIGH VALUES IN KEY                       
*                                                                               
RDTAPX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
CHKSIN   DC    C'N'                                                             
         LTORG                                                                  
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - PUT'                           
***********************************************************************         
*                                                                     *         
*        PUT RECORD TO LOAD TAPE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
PUT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R1,OUT                                                           
*                                                                               
         PUT   (R1),(R2)           PUT RECORD TO OUTPUT TAPE                    
*                                                                               
         AP    COUT,=P'1'          UPDATE OUTPUT RECORD COUNTER                 
*                                                                               
         CLI   FILTYP,X'42'        DONE IF NOT PRTFILE                          
         BNE   PUTX                                                             
*                                                                               
         LA    R0,RKEY-4           POINT TO RECOVERY FILE RECORD                
*                                                                               
         CR    R2,R0               SKIP IF RECORD PUT WASN'T                    
         BNE   PUTX                   FROM RECOVERY TAPE                        
*                                                                               
         LA    R4,RKEY             POINT TO BUY RECORD                          
         USING PBUYREC,R4          ESTABLISH AS BUY RECORD                      
*                                                                               
         CLI   PBUYKRCD,X'20'      SKIP IF NOT A BUY RECORD                     
         BNE   PUT4                                                             
*                                  SKIP IF NOT A NEW CLIENT                     
         CLC   PBUYKEY(PBUYKPRD-PBUYKEY),NEXTKEY                                
         BE    PUTX                                                             
*                                                                               
         TM    BUYACT,X'80'        SKIP IF NOT CLIENT BUY ACTIVITY REC          
         BZ    PUT4                                                             
*                                                                               
*        ON CHANGE IN CLIENT                                                    
*                                                                               
         BRAS  RE,PRT              PRINT BLANK LINE                             
*                                                                               
         MVC   PRAGY,PBUYKAGY      PRINT AGENCY                                 
         MVC   PRMED,PBUYKMED      MEDIA                                        
         MVC   PRCLT,PBUYKCLT      CLIENT                                       
         MVC   P+11(17),=C'**CLIENT TOTALS**'                                   
*                                                                               
         LA    R2,CLTTOTS          POINT TO CLIENT TOTALS                       
*                                                                               
         BRAS  RE,EDT              PRINT CLIENT TOTALS                          
*                                                                               
         OI    BUYACT,X'01'        INDICATE PAGE SKIP                           
*                                                                               
         XC    CLTTOTS,CLTTOTS     CLEAR CLINET TOTALS                          
*                                                                               
         NI    BUYACT,X'FF'-X'80'  TURN OFF CLIENT ACTIVITY INDICATOR           
*                                                                               
         BRAS  RE,PRT              PRINT LINE                                   
*                                                                               
PUT4     DS    0H                                                               
*                                                                               
         CLC   PBUYKEY(PBUYKRCD-PBUYKEY),NEXTKEY SKIP IF SAME AGY/MED           
         BE    PUTX                                                             
*                                                                               
         TM    BUYACT,X'40'        SKIP IF NOT AGY BUY ACTIVITY                 
         BZ    PUTX                                                             
*                                                                               
         BRAS  RE,PRT              PRINT BLANK LINE                             
*                                                                               
         MVC   PRAGY,PBUYKAGY      PRINT AGENCY                                 
         MVC   PRAGY,PBUYKAGY      PRINT AGENCY                                 
         MVC   PRMED,PBUYKMED      MEDIA                                        
         MVC   P+11(18),=C'**AGY/MED TOTALS**'                                  
*                                                                               
         LA    R2,AGYTOTS          POINT TO AGENCY TOTALS                       
         BRAS  RE,EDT              PRINT AGENCY TOTALS                          
*                                                                               
         XC    AGYTOTS,AGYTOTS     CLEAR AGENCY TOTALS                          
*                                                                               
         NI    BUYACT,X'FF'-X'40'  CLEAR AGENCY ACTIVITY INDICATOR              
*                                                                               
         BRAS  RE,PRT              PRINT LINE                                   
*                                                                               
PUTX     DS    0H                                                               
*                                                                               
         TM    BUYACT,X'01'        IF PAGE SKIP NEEDED                          
         BNO   *+14                                                             
         ZAP   LINE,=PL2'99'       FORCE NEW PAGE ON PRINT                      
         NI    BUYACT,X'FF'-X'01'     CLEAR PAGE SKIP INDICATOR                 
*                                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - EDT'                           
***********************************************************************         
*                                                                     *         
*        PRINT DOLLARS FOR BUY                                        *         
*                                                                     *         
*NTR1    R2==> BUCKETS TO PRINT                                       *         
*                                                                     *         
***********************************************************************         
         DS    0D                  ALIGNMENT                                    
EDT      NTR1  LABEL=*                                                          
*                                                                               
         LA    R5,5                NUMBER OF BUCKETS                            
         LA    R6,PRAMTS           POINT TO PRINT AREA                          
*                                                                               
EDTLOOP  DS    0H                                                               
*                                  EDIT AMOUNT                                  
         EDIT  (B4,0(R2)),(12,0(R6)),2,MINUS=YES                                
*                                                                               
EDTCONT  DS    0H                                                               
*                                                                               
         LA    R6,12(R6)           BUMP PRINT AREA POINTER                      
         LA    R2,4(R2)            BUMP BUCKET POINTER                          
         BCT   R5,EDTLOOP                                                       
*                                                                               
EDTDONE  DS    0H                                                               
*                                                                               
EDTX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - COUNTERS'                      
***********************************************************************         
*                                                                     *         
*        COUNTERS TABLE                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
COUNTS   DS    0C                                                               
CADD     DC    PL5'0',C'A ',CL33'ADDS'                                          
CDIROVS  DC    PL5'0',C'DO',CL33'DIRECTORY OVERLAYS'                            
CDEL     DC    PL5'0',C'D ',CL33'DELETES-(CHANGES)'                             
CPAY     DC    PL5'0',C'P ',CL33'PAYS -(CHANGES)'                               
CCHG     DC    PL5'0',C'C ',CL33'REGULAR CHANGES'                               
CCTO     DC    PL5'0',C'CT',CL33'DATE CHANGES -DROPPED'                         
CCFR     DC    PL5'0',C'CF',CL33'DATE CHANGES -ADDED'                           
CIN      DC    PL5'0',C'IP',CL33'INPUT FILE COUNT'                              
COUT     DC    PL5'0',C'OP',CL33'OUTPUT FILE COUNT'                             
COUNTSX  EQU   *-1                                                              
*                                                                               
WARN     DC    C'**WARNING-  INPUT + ADDS - DIRECTORY OVERLAYS NOT EQUAX        
               L TO OUTPUT**'                                                   
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - PRT'                           
***********************************************************************         
*                                                                     *         
*        PRINT A LINE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
PRT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 LPRINTER            PRINT THE LINE                               
*                                                                               
PRTX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - CPYERR'                        
***********************************************************************         
*                                                                     *         
*        NO CHANGE RECORD FOLLOWING COPY ERROR                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
CPYERR   NTR1  BASE=*,LABEL=*                                                   
*                                  ERROR MESSAGE                                
         MVC   P+1(20),=C'**STAND ALONE COPY**'                                 
*                                  DUMP RECORD KEY                              
         GOTO1 LHEXOUT,DMCB,SAVKEY,P+30,31,=C'NORM'                             
*                                                                               
         GOTO1 LPRINTER            PRINT ERROR MESSAGE                          
*                                                                               
         MVI   CCERR,1             SET ERROR INDICATOR                          
         XC    P,P                 CLEAR PRINT LINE                             
*                                                                               
CPYERRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - CHGERR'                        
***********************************************************************         
*                                                                     *         
*        NO COPY RECORD FOLLOWING CHANGE RECORD                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
CHGERR   NTR1  BASE=*,LABEL=*                                                   
*                                  ERROR MESSAGE                                
         MVC   P+1(27),=C'**CHG NOT PRECEDED BY CPY**'                          
*                                  DUMP RECORD KEY                              
         GOTO1 LHEXOUT,DMCB,RKEY,P+30,31,=C'NORM'                               
*                                                                               
         GOTO1 LPRINTER            PRINT RECORD                                 
*                                                                               
         MVI   CCERR,1             SET ERROR INDICATOR                          
         XC    P,P                 CLEAR PRINT LINE                             
*                                                                               
CHGERRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - GTIND'                         
***********************************************************************         
*                                                                     *         
*        FIND FINANCIAL DATA FOR BUY VIA GETINS                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
GTINS    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VGETINS,DMCB,(R2),PVALUES,7(R2)                                  
*                                                                               
         L     R0,GROSS            SET UP BUCKETS FOR BUY                       
         ST    R0,WORK             GROSS                                        
         S     R0,AGYCOM                                                        
         ST    R0,WORK+4           NET                                          
         L     R0,CSHDSC                                                        
         ST    R0,WORK+8           CASH DISCOUNT                                
         L     R0,PGROSS                                                        
         S     R0,PCSHDSC                                                       
         ST    R0,WORK+12          PAID NET                                     
         S     R0,PAGYCOM                                                       
         ST    R0,WORK+16          PAID NET LESS CD                             
*                                                                               
GTINSX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - SUBT'                          
***********************************************************************         
*                                                                     *         
*        CALCULATE CHANGES BETWEEN COPY AND CHANGED BUY RECORDS       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
SUBT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,16               DISPLACEMENT TO LAST BUCKET                  
*                                                                               
SUBTLOOP DS    0H                                                               
*                                                                               
         L     R0,ACTAMTS(RF)      SUBTRACT SAVED FROM CUURENT                  
         S     R0,SAVAMTS(RF)                                                   
         ST    R0,ACTAMTS(RF)                                                   
*                                                                               
SUBTCONT DS    0H                                                               
*                                                                               
         SH    RF,=H'4'            BACK UP TO PREVIOUS BUCKET                   
         BNM   SUBTLOOP            CONTINUE IF MORE BUCKETS                     
*                                                                               
SUBTDONE DS    0H                  END OF BUCKETS                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - CMPLO'                         
***********************************************************************         
*                                                                     *         
*        COMPLEMENT AMOUNTS IN BUCKETS                                *         
*                                                                     *         
*        R0 =  # OF BUCKETS                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
CMPLO    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,SAVAMTS                                                       
*                                                                               
CMPLLOOP L     R1,0(RF)                                                         
         LCR   R1,R1                                                            
         ST    R1,0(RF)                                                         
*                                                                               
CMPLCONT DS    0H                                                               
*                                                                               
         LA    RF,4(RF)                                                         
         BCT   R0,CMPLLOOP                                                      
*                                                                               
CMPLDONE DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - DCBS'                          
***********************************************************************         
*                                                                     *         
*        DCBS                                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
RECOV    DCB   DDNAME=RECVIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=GM,                                               X        
               EODAD=RECVEOT                                                    
         SPACE 2                                                                
IN       DCB   DDNAME=FILEIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=GM,                                               X        
               EODAD=EOIN                                                       
         SPACE 2                                                                
OUT      DCB   DDNAME=FILEOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=8200,                                             X        
               BLKSIZE=27648,                                          X        
               MACRF=PM                                                         
*                                                                               
         DS    0D                  ALIGNMENT                                    
RECONWKC DS    9000C               WORKARE                                      
*                                                                               
         DS    0D                  ALIGNMENT                                    
SORTLOC  DS    45800C                                                           
*                                                                               
         TITLE 'PRINTPAK - RECONSTRUCT PROGRAM - RECONWK'                       
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
RECONWK  DSECT                                                                  
*                                                                               
*                                                                               
VGETINS  DS    A                   A(GETINS)                                    
*                                                                               
FILTYP   DS    X                                                                
LASTTY   DS    X                   LAST REC TYPE                                
CCERR    DS    X                   COPY/CHG ERR                                 
SAVR3    DS    F                                                                
SAVR1    DS    F                                                                
SAVR9    DS    F                                                                
CRECIN   DS    F                   RECOVERY RECORDS COUNTER                     
DMCB     DS    6F                                                               
WORK     DS    CL100                                                            
*                                                                               
CARD     DS    CL80                                                             
         DS    0F                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
         DS    0F                                                               
CLTTOTS  DS    CL20                                                             
AGYTOTS  DS    CL20                                                             
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
         DS    0F                                                               
ACTWRK   DS    0CL32                                                            
ACTAMTS  DS    0CL20                                                            
AGROSS   DS    F                                                                
ANET     DS    F                                                                
ACD      DS    F                                                                
APAIDN   DS    F                                                                
APAIDA   DS    F                                                                
ATRANS   DS    CL2                                                              
ADATE    DS    CL3                                                              
         DS    CL7                                                              
*                                                                               
         DS    0F                                                               
SAVALL   DS    0CL65                                                            
SAVWRK   DS    0CL32                                                            
SAVAMTS  DS    CL20                                                             
         DS    CL5                                                              
         DS    CL7                                                              
*                                                                               
SAVKEY   DS    CL25                                                             
         DS    CL6                                                              
SAVSIN   DC    XL4'00'                                                          
BUYACT   DS    X                                                                
UPSI     DS    X                   '80' = PUB NOT PRT                           
DCSW     DS    X                   DATE CHANGE SWITCH                           
NEXTKEY  DS    CL25                                                             
*                                                                               
RRECRLEN DS    F                                                                
RREC     DS    0C                                                               
REXP     DS    0CL40                                                            
REXP1    DS    CL32                                                             
         DS    XL2                                                              
         DS    XL2                                                              
REXPRNO  DS    XL4                                                              
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RKEY     DS    CL25                                                             
         DS    4000C                                                            
*                                                                               
FRECRLEN DS    F                                                                
FREC     DS    0C                                                               
FKEY     DS    CL25                                                             
         DS    4000C                                                            
RECONWKX EQU   *                                                                
         EJECT                                                                  
*                                                                               
RECVEXTD DSECT                     RECOVERY RECORD EXTENSION                    
       ++INCLUDE DMRCVREXT                                                      
*                                                                               
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
*                                                                               
       ++INCLUDE DDDPRINT                                                       
*                                                                               
         ORG   P                                                                
*                                                                               
*        PRINT LINE FOR INSERTION                                               
*                                                                               
         DS    CL2                                                              
PRAGY    DS    CL2                 AGENCY                                       
         DS    CL1                                                              
PRMED    DS    CL1                 MEDIAT                                       
         DS    CL1                                                              
PRCLT    DS    CL3                 CLIENT                                       
         DS    CL1                                                              
PRPRD    DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
PRPUB    DS    CL15                PUB                                          
         DS    CL1                                                              
PRDATE   DS    CL8                 INSERTION DATE                               
         DS    CL1                                                              
PREST    DS    CL3                 ESTIMATE                                     
         DS    CL1                                                              
PRLIN    DS    CL3                 LINE NUMBER                                  
         DS    CL1                                                              
*                                                                               
         ORG   P+47                                                             
PRSIN    DS    CL5                 SYSTEM INPUT NUMBER                          
         DS    CL1                                                              
PRPRG    DS    CL2                 PROGRAM NUMBER                               
         DS    CL1                                                              
PRTRANS  DS    CL1                 TRANSACTION CODE                             
         DS    CL1                                                              
PRTOFRM  DS    CL2                 TO/FROM                                      
         DS    CL1                                                              
PRTDATE  DS    CL8                 TRANSACTION DATE                             
*                                                                               
*        NON-BUY RECORDS                                                        
*                                                                               
         ORG   P+71                                                             
PRAMTS   DS    0CL12               INSERTION AMOUNTS                            
PRGRS    DS    CL12                GROSS                                        
PRNET    DS    CL12                NET                                          
PRCD     DS    CL12                CD                                           
PRPDGCD  DS    CL12                PAID GROSS LESS CD                           
PRPDNET  DS    CL12                PAID NET LESS CD                             
         ORG   P+56                                                             
PRTRAN   DS    CL1                 TRANSACTION CODE                             
         DS    CL9                 SPARE                                        
PRKEY    DS    CL58                RECORD KEY IN HEX                            
*                                                                               
*        PUB RECORDS                                                            
*                                                                               
         ORG   P                                                                
         DS    CL2                                                              
PRPBMED  DS    CL1                 PUB MEDIA                                    
         DS    CL1                                                              
PRPBNUM  DS    CL15                PUB NUMBER                                   
         DS    CL1                                                              
PRPBAGY  DS    CL2                 PUB AGENCY                                   
         DS    CL1                                                              
PRPBTYP  DS    CL2                 PUB RECORD TYPE                              
         ORG                                                                    
       ++INCLUDE DMLDDEFN                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062PPRECON   06/07/11'                                      
         END                                                                    
