*          DATA SET PPFIXMT02  AT LEVEL 052 AS OF 09/08/04                      
*PHASE PPFM02A                                                                  
*INCLUDE MININAM                                                                
         TITLE 'PPFM02 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM RESEQUENCES INVOICE MATCH RECORDS                              
*                                                                               
*        QOPT4=Y    TEST CLEARING OF DELETED RECORDS (PRINT THEM)               
*        QOPT5=Y    DO NOT MARK RECORDS                                         
*        QOPT6=N    N IS YEAR OF INVOICE                                        
*                                                                               
PPFM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPFM02,R7                *** R7=SECOND BASE                    
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PASS1                                                            
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'         RECORDS READ                                 
         ZAP   FLGCNT,=P'0'        DELETED RECORDS READ                         
         ZAP   DELCNT,=P'0'        HEADERS FLAGGED FOR DELETION                 
         ZAP   ADDCNT,=P'0'        RECORDS ADDED                                
         ZAP   CHGCNT,=P'0'        RECORDS CHANGED (SHOULD BE ONLY 1)           
         ZAP   DUPCNT,=P'0'    DUP MINELEM KEYS ("UPPED" BY 1 FOR HDR)          
         ZAP   MARKCNT,=P'0'       RECORDS ACTUALLY WRITTEN                     
         ZAP   CLRCNT,=P'0'        "CLEARED RECORDS"                            
         ZAP   DMPCNT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
PASS1    DS    0H                  FIRST REC FOR REQUEST                        
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         LHI   R1,4001            CLEAR THE I/O AREA                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE              LIKE XCEF BUT "SHORTER"                       
*                                                                               
         XC    OLDMINK,OLDMINK    4-BYTE "OLD" MINELEM KEY (40XL4)              
         XC    NEWMINK,NEWMINK    4-BYTE "NEW" MINELEM KEY                      
         XC    INVTBL,INVTBL      1 OLD, 1 NEW SEQUENCE NO. (120XL2)            
         LA    R0,INVTBL                                                        
         ST    R0,INVADR          POINT TO START OF INVOICE # TBL               
         LA    R0,OLDMINK                                                       
         ST    R0,MINKADR         POINT TO START OF MINELEM KEY TBL             
         XC    INVCNT,INVCNT      NUMBER OF X'10' ELEMENTS                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'50'          INVOICE MATCHING RECORD                     
         MVC   KEY+04(3),QCLIENT         CLIENT CODE                            
         MVC   KEY+07(3),QPRODUCT        PRODUCT CODE                           
         GOTO1 PUBVAL,DMCB,QPUB,WORK                                            
         MVC   KEY+10(6),WORK            PUB                                    
         MVC   KEY+16(1),QOPT6           YEAR                                   
*                                                                               
         MVC   STARTKEY,KEY       SAVE 1ST 17 BYTES (THRU YEAR)                 
*                                                                               
         OI    DMINBTS,X'08'      SET TO READ DELETES                           
*                                                                               
         GOTO1 HIGH                                                             
         B     PASS1E                                                           
*                                                                               
PASS1C   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PASS1E   DS    0H                                                               
         CLC   KEY(17),KEYSAVE     CHECK AGY/MED/RC/CLT/PRD/PUB/YEAR            
         BNE   PASS1X              SET UP NEW NUMBERS, ETC                      
*                                                                               
         LA    R0,PBUYREC                                                       
         LHI   R1,4001            CLEAR THE I/O AREA                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE              LIKE XCEF BUT "SHORTER"                       
*                                                                               
         GOTO1 GETPRT                                                           
         AP    INCNT,=P'1'                                                      
*                                                                               
         TM    KEY+25,X'80'       DELETED ?                                     
         BZ    *+10               NO                                            
         AP    FLGCNT,=P'1'                                                     
*                                                                               
         CLC   KEY+21(4),=X'FFFFFFFF'                                           
         BE    PASS1G              DO NOT STORE THIS MINELEM KEY                
*                                                                               
         L     RE,MINKADR                                                       
         MVC   0(4,RE),KEY+21     STORE MATCH RECORD MINELEM KEY(S)             
         LA    RE,4(RE)             IN OLDMINK TABLE                            
         ST    RE,MINKADR                                                       
*                                                                               
PASS1G   DS    0H                                                               
         MVI   ELCODE,X'10'                                                     
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'10'                                                      
         BE    PASS1P                                                           
PASS1L   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PASS1C              NEXT RECORD                                  
*                                                                               
PASS1P   DS    0H                                                               
         L     RE,INVCNT                                                        
         LA    RE,1(RE)           ADD TO X'10' (INV HDR) COUNT                  
         ST    RE,INVCNT                                                        
*                                                                               
         L     RE,INVADR                                                        
         MVC   0(1,RE),2(R2)      STORE HDR SEQ NUM                             
         LA    RE,2(RE)           NEXT NUMBER ADDRESS                           
         ST    RE,INVADR                                                        
*                                                                               
         B     PASS1L             NEXT ELEMENT                                  
*                                                                               
PASS1X   DS    0H                  COMPUTE A NEW INCREMENT                      
*                                                                               
         L     R6,INVCNT                                                        
         CHI   R6,120                                                           
         BNH   *+6                                                              
         DC    H'0'               TOO MANY INVOICES                             
*                                                                               
         SR    R4,R4                                                            
         LA    R5,240             DIVIDEND                                      
         DR    R4,R6              R6 HAS INVOICE COUNT (DIVISOR)                
         ST    R5,INCRMNT         NEW SEQUENCE NUMBER "GAP" (QUOTIENT)          
*                                                                               
         LA    RE,10              NEW STARTING SEQUENCE NUMBER                  
         LA    R4,INVTBL                                                        
         L     R0,INVCNT                                                        
CALC10   DS    0H                                                               
         AR    RE,R5              R5 HAS NEW SEQUENCE NUMBER "GAP"              
         STC   RE,1(R4)           0(R4)=OLD, 1(R4)=NEW                          
         CLI   1(R4),X'FA'        "NEW" GREATER THAN 250 ?                      
         BNH   *+6                                                              
         DC    H'0'               YES - NG                                      
         LA    R4,2(R4)                                                         
         BCT   R0,CALC10                                                        
         CLI   0(R4),0            END OF SEQ NUMBER TABLE ?                     
         BE    *+6                SHOULD NOT HAPPEN                             
         DC    H'0'                                                             
*                                 CHECK FOR DUPLICATE KEYS GENERATION           
         XC    KEY,KEY                                                          
         MVC   KEY(17),STARTKEY   START AT BEGINNING AGAIN                      
*                                                                               
         MVI   DUPSW,C' '         CLEAR DUP MINELEM KEY INDICATOR               
*                                                                               
PASS2    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     PASS2E                                                           
*                                                                               
PASS2C   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PASS2E   DS    0H                                                               
         CLC   KEY(17),KEYSAVE     CHECK AGY/MED/RC/CLT/PRD/PUB/YEAR            
         BNE   PASS2X              DONE - GO DO DELETES AND ADDS                
*                                                                               
         TM    KEY+25,X'80'       DELETED ?                                     
         BO    PASS2C             YES - NO ELEMS TO TEST - NEXT RECORD          
*                                                                               
         LA    R0,PBUYREC                                                       
         LHI   R1,4001            CLEAR THE I/O AREA                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE              LIKE XCEF BUT "SHORTER"                       
*                                                                               
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R2,PBUYREC+33                                                    
PASS2L   DS    0H                                                               
         CLI   0(R2),0            END OF RECORD ?                               
         BE    PASS2P             YES - CHECK FOR KEY DUPLICATION               
*                                                                               
         LA    RE,INVTBL                                                        
PASS2L10 DS    0H                                                               
         CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'               MATCH MUST BE FOUND                           
         CLC   2(1,R2),0(RE)      MATCHING "OLD" SEQUENCE NO ?                  
         BE    PASS2M             YES                                           
         LA    RE,2(RE)           NEXT "OLD" NUMBER                             
         B     PASS2L10                                                         
PASS2M   DS    0H            SET UP NEW MINELEM KEY IN NEWMINK                  
         MVC   NEWMINK(1),0(R2)                                                 
         MVC   NEWMINK+1(3),2(R2)                                               
         MVC   NEWMINK+1(1),1(RE)   "NEW" SEQUENCE NO                           
         ZIC   RE,1(R2)                                                         
         AR    R2,RE              BUMP TO NEXT ELEM                             
         B     PASS2L                                                           
PASS2P   DS    0H                 TEST FOR KEY DUPLICATION                      
         LA    R3,OLDMINK                                                       
PASS2P10 CLI   0(R3),0            END ?                                         
         BE    PASS2C             NOT DUPLICATE - NEXT RECORD                   
         CLC   NEWMINK,0(R3)                                                    
         BNE   PASS2P40                                                         
*                                                                               
         BAS   RE,RPRT                                                          
         MVC   P+10(24),=C'** DUP MINI KEY FOUND - '                            
         GOTO1 HEXOUT,DMCB,NEWMINK,P+34,4,=C'N'                                 
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
         MVI   DUPSW,C'X'         SET DUP MINELEM KEY INDICATOR                 
*                                                                               
PASS2P40 LA    R3,4(R3)           NEXT TABLE ENTRY                              
         B     PASS2P10                                                         
*                                                                               
*                              FLAG FOR DELETION ALL HEADERS EXCEPT             
PASS2X   DS    0H              "LAST" (MINELEM KEY=X'FFFFFFFF')                 
*                                                                               
*NOP*    CLI   DUPSW,C'X'         DUP MINELEM KEY FOUND ?                       
*NOP*    BE    PASSEND            YES - END RUN                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(17),STARTKEY   START AT BEGINNING AGAIN                      
*                                                                               
         LA    R0,FIXRECT         TABLE OF DELETED KEYS                         
         ST    R0,FIXRECA                                                       
         LHI   R1,1001            CLEAR THE "DELETED" KEYS TABLE                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE              LIKE XCEF BUT "SHORTER"                       
*                                                                               
         OI    DMINBTS,X'08'      SET TO READ DELETES                           
*                                                                               
PASS3    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     PASS3E                                                           
*                                                                               
PASS3C   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PASS3E   DS    0H                                                               
         CLC   KEY(17),KEYSAVE     CHECK AGY/MED/RC/CLT/PRD/PUB/YEAR            
         BNE   PASS3X              DONE - GO DO CHANGES AND ADDS                
*                                                                               
         TM    KEY+25,X'80'       DELETED ?                                     
         BO    PASS3C             YES - NEXT RECORD                             
*                                                                               
         L     RE,FIXRECA         POINT TO DELETED KEYS TABLE                   
         MVC   0(25,RE),KEY       STORE THIS KEY                                
         LA    RE,25(RE)                                                        
         ST    RE,FIXRECA         NEXT ENTRY SPACE                              
*                                                                               
         CLC   KEY+21(4),=X'FFFFFFFF'                                           
         BE    PASS3X              DO NOT DELETE THIS RECORD                    
*                                                                               
         XC    SAVKEY,SAVKEY                                                    
         MVC   SAVKEY(25),KEY     SAVE FOR "RESTART"                            
*                                                                               
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         OI    KEY+25,X'80'                                                     
         AP    DELCNT,=P'1'                                                     
         CLI   QOPT5,C'Y'          MARK FILE ?                                  
         BE    PASS3G              NO                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,PRTDIR,KEY,KEY                                
         AP    MARKCNT,=P'1'                                                    
*                                                                               
PASS3G   DS    0H                                                               
         BAS   RE,DMPKEY           DUMP REWRITTEN RECORDS                       
         MVC   P+70(09),=C'**AFTER**'                                           
         BAS   RE,RPRT                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),SAVKEY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE    CHECK ENTIRE KEY                              
         BE    PASS3C             BACK TO READ NEXT                             
         DC    H'0'               MUST BE FOUND                                 
*                                                                               
*                                                                               
*                                                                               
PASS3X   DS    0H                 RESEQUENCING OF RECORDS NEXT                  
*                                                                               
         LA    R0,FIXRECT         TABLE OF DELETED KEYS                         
         ST    R0,FIXRECA                                                       
*                                                                               
         OI    DMINBTS,X'08'      SET TO READ DELETES                           
*                                                                               
PASS4    DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         L     RE,FIXRECA         POINT TO DELETED KEYS TABLE                   
         CLI   0(RE),0            END ?                                         
         BE    PASS4X             YES - GO CLEAR RECORDS IF DELETED             
         MVC   KEY(25),0(RE)      SET KEY                                       
         LA    RE,25(RE)                                                        
         ST    RE,FIXRECA         NEXT KEY ENTRY                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE    CHECK ENTIRE KEY                              
         BE    *+6                                                              
         DC    H'0'               MUST BE THERE                                 
*                                                                               
         CLC   KEY+21(4),=X'FFFFFFFF'                                           
         BE    PASS4C             PASS FOR CHANGE AND PUT                       
*                                                                               
         CLI   QOPT5,C'Y'         MARK FILE ?                                   
         BE    PASS4C             NO - CANNOT BE FLAGGED FOR DELETION           
         TM    KEY+25,X'80'       DELETED ?                                     
         BO    PASS4C             YES - PASS FOR CHANGE AND ADD                 
         DC    H'0'               MUST BE DELETED                               
*                                                                               
PASS4C   DS    0H                                                               
*                                                                               
         LA    R0,PBUYREC                                                       
         LHI   R1,4001            CLEAR THE I/O AREA                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE              LIKE XCEF BUT "SHORTER"                       
*                                                                               
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R2,PBUYREC+33                                                    
PASS4F   DS    0H                                                               
         CLI   0(R2),0            END OF RECORD ?                               
         BE    PASS4K             YES - CHECK FOR KEY DUPLICATION               
*                                                                               
         LA    RE,INVTBL                                                        
PASS4F10 DS    0H                                                               
         CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'               MATCH MUST BE FOUND                           
         CLC   2(1,R2),0(RE)      MATCHING "OLD" SEQUENCE NO ?                  
         BE    PASS4G             YES                                           
         LA    RE,2(RE)           NEXT "OLD" NUMBER                             
         B     PASS4F10                                                         
PASS4G   DS    0H            SET UP NEW MINELEM KEY IN NEWMINK                  
         MVC   NEWMINK(1),0(R2)                                                 
         MVC   NEWMINK+1(3),2(R2)                                               
         MVC   NEWMINK+1(1),1(RE)   "NEW" SEQUENCE NO                           
         MVC   2(1,R2),1(RE)      "NEW" SEQUENCE NO TO ELEMENT                  
*                                                                               
         ZIC   RE,1(R2)                                                         
         AR    R2,RE              BUMP TO NEXT ELEMENT                          
         B     PASS4F                                                           
*                                                                               
PASS4K   DS    0H                 CHECK FOR KEY DUPLICATION AGAIN               
         CLC   KEY+21(4),=X'FFFFFFFF'                                           
         BE    PASS4P             OK TO REWRITE                                 
*                                                                               
         LA    RE,OLDMINK                                                       
PASS4K20 CLI   0(RE),0            END ?                                         
         BE    PASS4K60           NOT DUPLICATE - NEXT RECORD                   
         CLC   NEWMINK,0(RE)                                                    
         BNE   PASS4K40                                                         
*                                                                               
         AP    DUPCNT,=P'1'       DUP MINELEM KEY "UPPED" BY 1 FOR HDR          
*                                                                               
         ZIC   RE,NEWMINK+3                                                     
         LA    RE,1(RE)           BUMP UP SEQ NUM BY 1                          
         STC   RE,NEWMINK+3                                                     
         B     PASS4K60                                                         
*                                                                               
PASS4K40 LA    RE,4(RE)           NEXT TABLE ENTRY                              
         B     PASS4K20                                                         
*                                                                               
PASS4K60 DS    0H                                                               
         LA    R2,PBUYREC                                                       
         USING PINVKEYD,R2                                                      
         MVC   PINVMINI,NEWMINK   NEW MINELEM KEY                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PINVKEY    SET NEW KEY FOR ADD                           
         DROP  R2                                                               
*                                                                               
         LA    RE,FIXRECT         POINT TO DELETED KEYS TABLE                   
PASS4L   DS    0H                                                               
         CLI   0(RE),0            END ?                                         
         BE    PASS4N             YES - OK TO ADD                               
         CLC   KEY(25),0(RE)      NEW KEY EQUAL TO AN OLD ?                     
         BNE   *+6                NO                                            
         DC    H'0'               SHOULD NOT HAPPEN                             
         LA    RE,25(RE)                                                        
         B     PASS4L             TEST NEXT KEY ENTRY                           
*                                                                               
PASS4N   DS    0H                                                               
         AP    ADDCNT,=P'1'                                                     
         MVC   P+70(09),=C'** NEW **'                                           
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
         CLI   QOPT5,C'Y'         MARK FILE ?                                   
         BE    PASS4T             NO                                            
*                                                                               
         GOTO1 ADDPRT                                                           
         AP    MARKCNT,=P'1'                                                    
         B     PASS4T                                                           
*                                                                               
PASS4P   DS    0H                                                               
         AP    CHGCNT,=P'1'                                                     
         MVC   P+70(09),=C'** CHG **'                                           
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
         CLI   QOPT5,C'Y'         MARK FILE ?                                   
         BE    PASS4T             NO                                            
*                                                                               
         GOTO1 PUTPRT                                                           
         AP    MARKCNT,=P'1'                                                    
*****    B     PASS4T                                                           
*                                                                               
*                                                                               
PASS4T   DS    0H                                                               
         B     PASS4              GET NEXT RECORD                               
*                                                                               
*                                                                               
PASS4X   DS    0H          "CLEAR" (CREATE 33-BYTE RECORD) FOR ALL KEYS         
*                               MARKED AS DELETED                               
*NOP*    CLI   QOPT4,C'Y'         TESTING RECORD CLEARING ?                     
*NOP*    BE    PASS5              YES                                           
*                                                                               
*NOP*    CLI   QOPT5,C'Y'         MARK FILE ?                                   
*NOP*    BE    PASS5X             NO - NOTHING FLAGGED FOR DELETION             
*                                      BY THIS PROGRAM - GO TO END              
PASS5    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(17),STARTKEY   START AT BEGINNING AGAIN                      
*                                                                               
         OI    DMINBTS,X'08'      SET TO READ DELETES                           
*                                                                               
         GOTO1 HIGH                                                             
         B     PASS5E                                                           
*                                                                               
PASS5C   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PASS5E   DS    0H                                                               
         CLC   KEY(17),KEYSAVE     CHECK AGY/MED/RC/CLT/PRD/PUB/YEAR            
         BNE   PASS5X              DONE                                         
         CLC   KEY+21(4),=X'FFFFFFFF'                                           
         BNE   PASS5G                                                           
         TM    KEY+25,X'80'       DELETED ?                                     
         BZ    PASS5X             NO - OK - DONE                                
         DC    H'0'        X'FFFFFFFF' MINELEM KEY MAY NOT BE DELETED           
*                                                                               
PASS5G   DS    0H                                                               
         TM    KEY+25,X'80'       DELETED ?                                     
         BZ    PASS5C             NO - NEXT RECORD                              
*                                                                               
PASS5R   DS    0H                 KEY IS DELETED                                
         GOTO1 GETPRT                                                           
*                                                                               
         CLC   PBUYREC+26(2),=X'2180'    ALREADY CLEARED ?                      
         BE    PASS5C             YES - NEXT RECORD                             
*                                                                               
         LA    R0,PBUYREC                                                       
         LHI   R1,4001            CLEAR THE RECORD (ENTIRE I/O AREA)            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE              LIKE XCEF BUT "SHORTER"                       
*                                                                               
         MVC   PBUYREC(25),KEY           SET UP NEW RECORD WITH LENGTH          
         MVC   PBUYREC+26(2),=X'2180'    OF 33 AND TURN ON DELETE FLAG          
*                                                                               
         AP    CLRCNT,=P'1'                                                     
         MVC   P+70(09),=C'* CLEAR *'                                           
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
         CLI   QOPT5,C'Y'         MARK FILE ?                                   
         BE    PASS5C             NO - NEXT RECORD                              
*                                                                               
         GOTO1 PUTPRT                                                           
*                                                                               
         AP    MARKCNT,=P'1'                                                    
         B     PASS5C             NEXT RECORD                                   
*                                                                               
PASS5X   DS    0H                                                               
*                                                                               
*                                                                               
PASSEND  MVI   KEY,X'FF'           DONE                                         
         MVI   MODE,RUNLAST                                                     
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
PROC80   DS    0H                                                               
         CLI   KEY,X'FF'          END OF FILE                                   
         BE    EXIT                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+3,X'FF'             SKIP TO NEXT AGY/MED                     
         XC    KEY+4(28),KEY+4                                                  
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'               END OF FILE                              
         BE    EXIT                                                             
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'25'             CLEARANCE STATUS                         
         MVI   FORCEHED,C'Y'                                                    
*NOP*    B     PROC2                                                            
*                                                                               
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    RUNL10                                                           
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
RUNL10   DS    0H                                                               
         MVC   P(13),=C'INVOICE COUNT'                                          
         EDIT  INVCNT,(3,P+20),ALIGN=LEFT                                       
         GOTO1 REPORT                                                           
         MVC   P(13),=C'INCREMENT IS '                                          
         EDIT  INCRMNT,(3,P+20),ALIGN=LEFT                                      
         GOTO1 REPORT                                                           
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVI   RCSUBPRG,2                                                       
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
         LA    R5,KEY                                                           
         LA    R2,32                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+90(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'MATCH RECS READ'                                            
FLGCNT   DS    PL8                                                              
         DC    CL15'DELTD RECS READ'                                            
DELCNT   DS    PL8                                                              
         DC    CL15'MATCH RECS DLTD'                                            
ADDCNT   DS    PL8                                                              
         DC    CL15'MTCH RECS ADDED'                                            
CHGCNT   DS    PL8                                                              
         DC    CL15'MTCH RECS CHGED'                                            
DUPCNT   DS    PL8                                                              
         DC    CL15'DUPL KEYS CHGED'                                            
CLRCNT   DS    PL8                                                              
         DC    CL15'RECORDS CLEARED'                                            
MARKCNT  DS    PL8                                                              
         DC    CL15'RECORDS WRITTEN'                                            
         DC    X'FF'                                                            
DMPCNT   DS    PL8                                                              
*                                                                               
STARTKEY DS    CL17                                                             
SAVKEY   DS    CL32                                                             
*                                                                               
INVCNT   DS    F                  NUMBER OF X'10' INVOICE ELEMENTS              
INCRMNT  DS    F                  INCREMENT FOR RENUMBERING INVOICES            
INVADR   DS    F                  ADDRESS OF "NEXT" OLD INVOICE NO              
MINKADR  DS    F                  ADDRESS OF "NEXT" OLD MINIO KEY               
FIXRECA  DS    F                  ADDRESS OF FIXRECT ENTRY                      
INVTBL   DS    XL241              1 OLD INVOICE NO, 1 NEW - 120 TIMES           
*                                                                               
OLDMINK  DS    XL161              ROOM FOR 40 OLD 4-BYTE MINELEM KEYS           
NEWMINK  DS    XL4                NEW MINELEM KEY                               
*                                                                               
FIXRECT  DS    CL1001             ROOM FOR 40 25-BYTE KEYS                      
*                                                                               
         DS    0F                                                               
RECAREA  DS    CL4000                                                           
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
DUPSW    DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PINVREC                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052PPFIXMT02 09/08/04'                                      
         END                                                                    
