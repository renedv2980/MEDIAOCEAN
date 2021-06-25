*          DATA SET ACREPIB02  AT LEVEL 034 AS OF 04/10/15                      
*PHASE ACIB02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'COKE  - BUDGET INTERFACE '                                      
ACIB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**IB02**,R9                                                    
         L     RA,0(,R1)                                                        
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACIBD,RC                                                         
*                                                                               
         CLI   MODE,REQFRST    EVERYTHING DONE AT REQFRST                       
         BE    REQF                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*        REQUEST FIRST                                               *          
**********************************************************************          
         SPACE 1                                                                
*   INITIALIZE TOTALS                                                           
*                                                                               
REQF     LA    R1,PKFLDS           R1 = A(START OF PACKED 8 FIELDS)             
         LA    R0,PKFLDSQ                                                       
         ZAP   0(L'PKFLDS,R1),=P'0'                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(40,ASORTC)                         
         OPEN  (BUDTAPE,(INPUT))                                                
         LA    R2,INREC                                                         
         USING TAPED,R2                                                         
         XC    FLAG,FLAG                                                        
*                                                                               
REQF10   GET   BUDTAPE,INREC                                                    
         CLC   QSELECT,SPACES                                                   
         BE    *+14                                                             
         CLC   QSELECT(4),TPYEAR                                                
         BNE   REQF10                                                           
         AP    TYPETOT,=P'1'                                                    
         CLI   TPTYPE,C'3'                                                      
         BE    REQF30                                                           
         CLI   TPTYPE,C'2'                                                      
         BE    REQF20                                                           
         CLI   TPTYPE,C'1'                                                      
         BNE   REQF12                                                           
         AP    TYPE1CNT,=P'1'                                                   
         B     REQF10                                                           
REQF12   AP    ERRCNT,=P'1'        ERROR COUNT                                  
         B     REQF10                                                           
*                                                                               
REQF20   DS    0H                  TYPE 2 - TV/RADIO                            
         AP    TYPE2CNT,=P'1'                                                   
         LA    R7,OUTREC                                                        
         USING SRTBAD,R7                                                        
         LA    R4,SRTKEY                                                        
         USING BUDRECD,R4                                                       
         BAS   RE,BLDKEY            BUILD KEY FOR SORTER RECORD                 
         MVI   BUDKCCPY+5,C'S'      SPOT                                        
         MVC   BUDKCCPY+6(1),TP2MEDIA    T, R  OR   C                           
                                                                                
*** OLD STUFF REMOVED 05/9/97 ************                                      
*        MVC   BUDKCCPY+5(2),=C'ST'      SPOT TV                                
*        CLI   TP2MEDIA,TP2RADIO                                                
*        BNE   *+10                                                             
*        MVC   BUDKCCPY+5(2),=C'SR'      SPOT RADIO                             
******************************************                                      
                                                                                
         BAS   RE,BLDEL            INITIALIZE ELEMENTS                          
         MVC   NUMTHS,=F'3'                                                     
         PACK  DUB1,TP2Q1          FIRST QTR AMOUNT                             
         PACK  MISCAMT,TP2Q1M      FIRST QTR MISC AMOUNT                        
         AP    DUB1,MISCAMT        ADDED TOGETHER                               
         LA    R1,BQTR1                                                         
         BAS   RE,DIVMN            SPLIT IT INTO 3 MONTHS                       
         PACK  DUB1,TP2Q2          SECOND QTR AMOUNT                            
         PACK  MISCAMT,TP2Q2M                                                   
         AP    DUB1,MISCAMT                                                     
         LA    R1,BQTR2                                                         
         BAS   RE,DIVMN            SPLIT IT INTO 3 MONTHS                       
         PACK  DUB1,TP2Q3          THIRD QTR AMOUNT                             
         PACK  MISCAMT,TP2Q3M                                                   
         AP    DUB1,MISCAMT                                                     
         LA    R1,BQTR3                                                         
         BAS   RE,DIVMN            SPLIT IT INTO 3 MONTHS                       
         PACK  DUB1,TP2Q4          FOURTH QTR AMOUNT                            
         PACK  MISCAMT,TP2Q4M                                                   
         AP    DUB1,MISCAMT                                                     
         LA    R1,BQTR4                                                         
         BAS   RE,DIVMN            SPLIT IT INTO 3 MONTHS                       
         BAS   RE,ADDEM            ADDS ELEMENTS TO RECORD                      
         B     REQF10                                                           
*                                                                               
REQF30   DS    0H                  TYPE 3 - NON TV                              
         AP    TYPE3CNT,=P'1'                                                   
         LA    R7,OUTREC                                                        
         USING SRTBAD,R7                                                        
         LA    R4,SRTKEY                                                        
         USING BUDRECD,R4                                                       
         BAS   RE,BLDKEY                                                        
*                                                                               
*        XC    BUDRLEN(10),BUDRLEN                                              
*        MVC   BUDKCCPY+5(2),=C'SR'      RADIO                                  
*        BAS   RE,BLDEL            INITIALIZE ELEMENTS                          
*        MVC   NUMTHS,=F'12'                                                    
*        PACK  DUB1,TP3RADIO       YEARLY BUDGETS                               
*        LA    R1,BQTR1                                                         
*        BAS   RE,DIVMN            SPLIT IT INTO 12 MONTHS                      
*        BAS   RE,ADDEM                                                         
*                                                                               
REQF35   XC    BUDRLEN(10),BUDRLEN                                              
         MVC   BUDKCCPY+5(2),=C'PO'      OUTDOOR                                
         BAS   RE,BLDEL            INITIALIZE ELEMENTS                          
         MVC   NUMTHS,=F'12'                                                    
         PACK  DUB1,TP3OUTDR       YEARLY BUDGETS                               
         LA    R1,BQTR1                                                         
         BAS   RE,DIVMN            SPLIT IT INTO 12 MONTHS                      
         BAS   RE,ADDEM                                                         
*                                                                               
         XC    BUDRLEN(10),BUDRLEN                                              
         MVC   BUDKCCPY+5(2),=C'PN'      NEWSPAPER                              
         BAS   RE,BLDEL            INITIALIZE ELEMENTS                          
         MVC   NUMTHS,=F'12'                                                    
         PACK  DUB1,TP3NEWS        YEARLY BUDGETS                               
         LA    R1,BQTR1                                                         
         BAS   RE,DIVMN            SPLIT IT INTO 12 MONTHS                      
         BAS   RE,ADDEM                                                         
*                                                                               
         XC    BUDRLEN(10),BUDRLEN                                              
         MVC   BUDKCCPY+5(2),=C'PS'      MISCELLANEOUS                          
         BAS   RE,BLDEL            INITIALIZE ELEMENTS                          
         MVC   NUMTHS,=F'12'                                                    
         PACK  DUB1,TP3MISC        YEARLY BUDGETS                               
         LA    R1,BQTR1                                                         
         BAS   RE,DIVMN            SPLIT IT INTO 12 MONTHS                      
         BAS   RE,ADDEM                                                         
*                                                                               
         XC    BUDRLEN(10),BUDRLEN                                              
         MVC   BUDKCCPY+5(2),=C'PL'      LOCAL EXTRA                            
         BAS   RE,BLDEL            INITIALIZE ELEMENTS                          
         MVC   NUMTHS,=F'12'                                                    
         PACK  DUB1,TP3EXTRA       YEARLY BUDGETS                               
         LA    R1,BQTR1                                                         
         BAS   RE,DIVMN            SPLIT IT INTO 12 MONTHS                      
         BAS   RE,ADDEM                                                         
         B     REQF10                                                           
*                                                                               
SRTXIT   GOTO1 SORTER,DMCB,=C'END'                                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* COMPARE SORTED RECORDS TO FILE RECORDS AND ADD, UPDATE OR DELETE   *          
**********************************************************************          
         SPACE 1                                                                
EOJ      CLOSE BUDTAPE             CLOSE I/P TAPE                               
         CP    OUTCNT,=P'0'                                                     
         BNE   EOJ10                                                            
         B     SRTXIT              NO SORT  RECORDS                             
*                                                                               
EOJ10    ZAP   PDUMP,=P'0'                                                      
         BAS   RE,GTSRT                                                         
         BE    EOJ20               HAVE ANOTHER SORT RECORD, SO PROCESS         
         MVI   FSW,C'F'            SET SWTCH TO SHOW 1ST TIME                   
         BAS   RE,PRTRST           IF END OF SORT - PRINT                       
         MVI   FSW,C' '                                                         
         B     EXIT                THEN EXIT                                    
*                                                                               
         USING SRTBAD,R7                                                        
         USING BUDRECD,R4                                                       
EOJ20    MVC   OLDYEAR,YEAR        SAVE THE OLD YEAR                            
         LA    R7,OUTREC                                                        
         MVC   COMPKEY(2),SRTKEY                                                
         LA    R4,SAVEKEY                                                       
         MVC   BUDKCULA(32),SPACES  BUILD KEY FOR 1ST FILE READ                 
         XC    BUDKBUDN(9),BUDKBUDN                                             
         MVI   BUDKTYP,X'1B'                                                    
         MVC   BUDKCULA(1),SRTACC                                               
         MVC   BUDKUNT(2),=C'SE'                                                
         MVI   BUDKACT+1,X'41'                                                  
         MVC   FILEREC(42),SAVEKEY                                              
         B     EOJ34                                                            
*                                                                               
EOJ30    BAS   RE,GTSRT            GET A RECORD FROM SORTER                     
         BE    EOJ34               HAVE ANOTHER SORT RECORD                     
EOJ32    BAS   RE,PRTRST           IF END OF SORT - PRINT                       
         B     EXIT                THEN EXIT                                    
*                                                                               
EOJ34    NI    FLAG,X'FF'-FLGPRT   TURN OFF PRINTED FLAG                        
         CLC   YEAR,OLDYEAR                                                     
         BE    EOJ35               START NEW YEAR                               
         MVC   TOTMSG,=CL15'TOTALS FOR'                                         
         MVC   TOTMSG+11(4),SVYEAR                                              
         BAS   RE,PRTTOT                                                        
         MVC   PAGE,=H'1'          RESET PAGE NUMBER                            
         B     EOJ20                                                            
*                                                                               
         USING BUDRECD,R4                                                       
EOJ35    LA    R7,OUTREC                                                        
         LA    R4,FILEREC                                                       
         CLC   BUDKEY,SRTKEY       COMPARE FILE KEY TO SORTED KEY               
         BNL   EOJ50               IF NOT LOW WE DON'T NEED TO READ             
*                                                                               
EOJ40    GOTO1 RDBUD,DMCB,SAVEKEY,FILEREC                                       
*                                                                               
EOJ50    LA    R4,FILEREC                                                       
         CLC   BUDKEY(2),SRTKEY      COMPARE COMPANY                            
         BNE   EOJ60                 RAN OUT OF BUDGETS TO CHECK                
         CLC   BUDKEY,SRTKEY         COMPARE FILE KEY TO SORTED KEY             
         BH    EOJ90                 SORT RECORD NOT ON FILE - ADD              
         BE    EOJ100                EQUAL - UPDATE FILE RECORD                 
         CLI   QOPT1,C'Y'            IS THIS A REPLACEMENT TAPE                 
         BNE   EOJ80                 FILE RECORD NOT ON TAPE - PRINT IT         
         BAS   RE,DFIL      IF REPLACEMENT TAPE DELETE ITEM ON FILE             
         B     EOJ40        GET NEXT FILE RECORD                                
         EJECT                                                                  
***********************************************************************         
*        ADD REMAINING SORTER RECORDS TO ACCOUNT FILE                 *         
***********************************************************************         
         SPACE 1                                                                
EOJ60    MVI   EOFSW,X'01'         TURN ON SWITCH FOR LATER CHECK               
         CLC   SRTSTAT(2),=H'0'    TRIED TO DELETE NONEXISTANT                  
         BE    EOJ70               RECORD                                       
         GOTO1 DMGADD,DMCB,SRTKEY,SRTKEY                                        
         GOTO1 ACUMEL,DMCB,SRTKEY,CURBUD,CURTOT                                 
         ZAP   TAPEAMT,CURBUD                                                   
         AP    TAPETOT,TAPEAMT                                                  
         GOTO1 PRTRT,DMCB,SRTKEY                                                
*                                                                               
EOJ70    BAS   RE,GTSRT                                                         
         BNE   EOJ72               NO MORE SORT RECORDS                         
         CLC   YEAR,OLDYEAR        SEE IF YEAR CHANGED                          
         BE    EOJ60               ADD  ANOTHER                                 
         MVC   TOTMSG,=CL15'TOTALS FOR'                                         
         MVC   TOTMSG+11(4),SVYEAR                                              
         BAS   RE,PRTTOT                                                        
         MVC   PAGE,=H'1'          RESET PAGE NUMBER                            
         NI    EOFSW,TURNOFF-X'01' YES, SO START OVER                           
*        MVC   SAVEKEY,SRTKEY      RESET, SINCE SKIPPED OVER                    
*        GOTO1 RDBUD,DMCB,SAVEKEY,FILEREC                                       
*        MVC   OLDYEAR,YEAR                                                     
         B     EOJ20                                                            
*                                                                               
EOJ72    BAS   RE,PRTRST           IF END OF SORT - PRINT                       
         B     EXIT                THEN EXIT                                    
         EJECT                                                                  
***********************************************************************         
*        PRINT THE FILE RECORD - NO CHANGE THIS RUN                   *         
***********************************************************************         
         SPACE 1                                                                
EOJ80    GOTO1 ACUMEL,DMCB,FILEREC,PRVBUD,PRVTOT                                
         ZAP   CURBUD,PRVBUD                                                    
         AP    CURTOT,PRVBUD                                                    
         MVI   TAPESW,X'01'                                                     
         GOTO1 PRTRT,DMCB,FILEREC                                               
         OI    FLAG,FLGPRT         SHOW THAT FILEREC WAS PRINTED                
         B     EOJ40                                                            
***********************************************************************         
*        ADD A SORTER RECORD TO THE ACCOUNT FILE                      *         
***********************************************************************         
         SPACE 1                                                                
EOJ90    CLC   SRTSTAT(2),=H'0'    TRIED TO DELETE REC NOT ON FILE              
         BE    EOJ95                                                            
         CLC   YEAR,OLDYEAR                                                     
         BE    EOJ94               SAME YEAR, SO ADD RECORD                     
         MVC   TOTMSG,=CL15'TOTALS FOR'                                         
         MVC   TOTMSG+11(4),SVYEAR                                              
         BAS   RE,PRTTOT                                                        
         MVC   PAGE,=H'1'          RESET PAGE NUMBER                            
*        MVC   SAVEKEY,SRTKEY      RESET, SINCE SKIPPED OVER                    
*        GOTO1 RDBUD,DMCB,SAVEKEY,FILEREC                                       
*        MVC   OLDYEAR,YEAR                                                     
         B     EOJ20               TRY AGAIN                                    
*                                                                               
EOJ94    GOTO1 DMGADD,DMCB,SRTKEY,SRTKEY SORTER RECORD TO THE FILE              
         GOTO1 ACUMEL,DMCB,SRTKEY,CURBUD,CURTOT                                 
         ZAP   TAPEAMT,CURBUD                                                   
         AP    TAPETOT,TAPEAMT                                                  
         GOTO1 PRTRT,DMCB,SRTKEY                                                
         MVC   SAVEKEY,SRTKEY      RESET, SINCE SKIPPED OVER                    
         GOTO1 RDBUD,DMCB,SAVEKEY,FILEREC                                       
         B     EOJ30                                                            
*                                                                               
EOJ95    BAS   RE,GTSRT            GET A RECORD FROM SORTER                     
         BNE   EOJ32               NO MORE RECORDS                              
         NI    FLAG,X'FF'-FLGPRT   TURN OFF PRINTED FLAG                        
         B     EOJ50                                                            
***********************************************************************         
*        UPDATE A FILE RECORD WITH A SORTER RECORD                    *         
***********************************************************************         
         SPACE 1                                                                
EOJ100   GOTO1 ACUMEL,DMCB,FILEREC,PRVBUD,PRVTOT                                
         CLC   SRTSTAT(2),=H'0'                                                 
         BNE   EOJ110                                                           
         BAS   RE,DELRC                                                         
         B     EOJ30                                                            
*                                                                               
EOJ110   DS    0H                                                               
         GOTO1 ACUMEL,DMCB,SRTKEY,CURBUD,CURTOT                                 
         ZAP   TAPEAMT,CURBUD                                                   
         AP    TAPETOT,TAPEAMT                                                  
         GOTO1 PRTRT,DMCB,FILEREC                                               
         OI    FLAG,FLGPRT              SHOW THAT FILEREC WAS PRINTED           
EOJ120   GOTO1 DELL,DMCB,(X'1D',FILEREC),(1,YEAR)                               
         CLI   ELERR,0                                                          
         BE    EOJ120                                                           
*                                                                               
         USING BAMELD,R3                                                        
         LA    R3,SRTBAEL                                                       
EOJ130   CLI   BAMEL,0                   CHECK FOR ANOTHER ELEMENT              
         BE    EOJ140                                                           
         GOTO1 ADDL,DMCB,FILEREC,(R3)                                           
         LA    R3,BAMLNQ(R3)                                                    
         B     EOJ130                                                           
EOJ140   LA    R4,FILEREC                                                       
         USING ACCRECD,R4                                                       
         CLI   ACCORFST(R4),0                                                   
         BNE   EOJ150                                                           
         OI    ACCOSTAT(R4),X'80'                                               
         AP    RECSDEL,=P'1'                                                    
         B     *+14                                                             
EOJ150   AP    RECSCHG,=P'1'                                                    
         NI    ACCOSTAT(R4),ALL-DELETED    UNDEL RECS WITH ELEMENTS             
         GOTO1 DMGUPD,DMCB,FILEREC,FILEREC                                      
         B     EOJ30                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
*  DELRC DELETES RECS ON THE FILE THAT HAVE CORRESPONDING RECS ON    *          
*  THE I/P TAPE WITH ZERO AMOUNTS.                                   *          
**********************************************************************          
         SPACE 1                                                                
DELRC    NTR1                                                                   
*                                                                               
DELRC10  GOTO1 DELL,DMCB,(X'1D',FILEREC),(1,YEAR)                               
         CLI   ELERR,0                                                          
         BE    DELRC10                                                          
         LA    R4,FILEREC                                                       
         USING ACCRECD,R4                                                       
         CLI   ACCORFST(R4),0                                                   
         BNE   DELRC20                                                          
         OI    ACCOSTAT(R4),X'80'                                               
         AP    RECSDEL,=P'1'                                                    
         B     *+14                                                             
DELRC20  AP    RECSCHG,=P'1'                                                    
         NI    ACCOSTAT(R4),ALL-DELETED    UNDEL RECS WITH ELEMENTS             
         GOTO1 DMGUPD,DMCB,FILEREC,FILEREC                                      
         GOTO1 ACUMEL,DMCB,FILEREC,CURBUD,CURTOT                                
         GOTO1 PRTRT,DMCB,FILEREC                                               
         OI    FLAG,FLGPRT         SHOW THAT FILEREC WAS PRINTED                
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*              PRINT REMAINING ACCOUNT RECORDS                       *          
**********************************************************************          
         SPACE 1                                                                
PRTRST   NTR1                                                                   
*                                                                               
PRTRST10 TM    EOFSW,X'01'                                                      
         BZ    PRTRST20                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   TOTMSG,=CL15'TOTALS FOR'                                         
         MVC   TOTMSG+11(4),SVYEAR                                              
         BAS   RE,PRTTOT           PRINT OUT SUB TOTALS                         
         LA    R1,PKTOTQ           PACKED TOTALS                                
         LA    R0,PKGRNDQ          PACKED GRAND TOTALS                          
         CR    R0,R1                                                            
         BE    *+6                 THESE HAVE TO BE EQUAL OR SOMEONE            
         DC    H'0'                ADDED A FIELD INCORRECTLY TO ONE             
         LA    R1,PKTOT            R1 = A(TOTAL FIELDS)                         
         LA    R5,PKGRND           R5 = A(GRAND TOTALS)                         
         ZAP   0(L'PKTOT,R1),0(L'PKGRND,R5)                                     
         LA    R1,L'PKTOT(R1)                                                   
         LA    R5,L'PKGRND(R5)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   TOTMSG,=CL15'GRAND TOTALS'                                       
         OI    FLAG,FLGGND         TURN ON GRAND TOTAL FLAG                     
         BAS   RE,PRTTOT           PRINT OUT TOTALS                             
         B     SRTXIT                                                           
*                                                                               
PRTRST20 CLI   FSW,C'F'                                                         
         BE    PRTRST40                                                         
         TM    FLAG,FLGPRT         WAS FILEREC ALREADY PRINTED?                 
         BO    PRTRST40                                                         
         CLI   QOPT1,C'Y'          IS THIS FULL REPLACE                         
         BNE   PRTRST30                                                         
         BAS   RE,DFIL             IF IT IS DELETE FILE RECORD                  
         B     PRTRST40                                                         
*                                                                               
PRTRST30 GOTO1 ACUMEL,DMCB,FILEREC,PRVBUD,PRVTOT                                
         ZAP   CURBUD,PRVBUD                                                    
         AP    CURTOT,PRVBUD                                                    
         MVI   TAPESW,X'01'                                                     
         GOTO1 PRTRT,DMCB,FILEREC                                               
*                                                                               
PRTRST40 GOTO1 RDBUD,DMCB,SAVEKEY,FILEREC                                       
         NI    FLAG,X'FF'-FLGPRT   TURN OFF PRINTED FLAG                        
*                                                                               
PRTRST50 LA    R4,FILEREC                                                       
         USING BUDRECD,R4                                                       
         CLC   BUDKEY(2),COMPKEY                                                
         BE    PRTRST10            IF NOT EQUAL - END OF CLIENT                 
         MVC   TOTMSG,=CL15'TOTALS FOR'                                         
         MVC   TOTMSG+11(4),SVYEAR                                              
         BAS   RE,PRTTOT           PRINT OUT SUB TOTALS                         
         LA    R1,PKTOTQ           PACKED TOTALS                                
         LA    R0,PKGRNDQ          PACKED GRAND TOTALS                          
         CR    R0,R1                                                            
         BE    *+6                 THESE HAVE TO BE EQUAL OR SOMEONE            
         DC    H'0'                ADDED A FIELD INCORRECTLY TO ONE             
         LA    R1,PKTOT            R1 = A(TOTAL FIELDS)                         
         LA    R5,PKGRND           R5 = A(GRAND TOTALS)                         
         ZAP   0(L'PKTOT,R1),0(L'PKGRND,R5)                                     
         LA    R1,L'PKTOT(R1)                                                   
         LA    R5,L'PKGRND(R5)                                                  
         BCT   R0,*-14                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   TOTMSG,=CL15'GRAND TOTALS'                                       
         OI    FLAG,FLGGND         TURN ON GRAND TOTAL FLAG                     
         BAS   RE,PRTTOT           PRINT OUT TOTALS                             
         B     SRTXIT                                                           
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*              PUT A RECORD TO SORTER                                *          
**********************************************************************          
         SPACE 1                                                                
PTSRT    NTR1                                                                   
         XR    R3,R3                                                            
         LA    R7,OUTREC                                                        
         USING SRTBAD,R7                                                        
         MVC   SRTYEAR,TPYEAR                                                   
         LA    R4,SRTKEY                                                        
         USING ACCRECD,R4                                                       
         ICM   R3,3,ACCRLEN                                                     
         LTR   R3,R3                                                            
         BZ    PTSRT20                                                          
         LA    R3,8(R3)                                                         
         STH   R3,RECLEN                                                        
         B     PTSRT30                                                          
*                                                                               
PTSRT20  ICM   R3,3,DATADISP                                                    
         LA    R3,8(R3)                                                         
         STH   R3,RECLEN                                                        
PTSRT30  GOTO1 SORTER,DMCB,=C'PUT',RECLEN                                       
*                                                                               
PTSRTX   AP    OUTCNT,=P'1'                                                     
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO GET A RECORD FROM SORTER                   *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTBAD,R7                                                        
GTSRT    NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R7,15,DMCB+4                                                     
         BZ    GTSRTX              SET CONDITION CODE FOR RETURN                
         LA    R2,RECLEN                                                        
         LA    R3,1004                                                          
         LR    R4,R7                                                            
         SR    R5,R5                                                            
         LH    R5,0(,R7)                                                        
         MVCL  R2,R4                                                            
*                                                                               
         LA    R7,OUTREC                                                        
         AP    RECSIN,=P'1'                                                     
         MVC   CURYEAR,SRTYEAR                                                  
         MVC   WORK(4),SRTYEAR                                                  
         MVC   WORK+4(2),=C'01'    DUMMY UP MONTH                               
         MVC   WORK+6(2),=C'01'    DUMMY UP DAY                                 
         GOTO1 DATCON,DMCB,(9,WORK),(1,WORK+8)                                  
         MVC   YEAR,WORK+8         SAVE OFF PACKED YY                           
*                                                                               
         SR    RC,RC               SETTING CODITION CODE OF ZERO                
GTSRTX   LTR   RC,RC               SETTING CONDITION CODE OF NON ZERO           
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO READ ACCOUNT RECORDS                       *          
**********************************************************************          
         SPACE 1                                                                
RDACC    NTR1                                                                   
         LM    R4,R5,0(R1)                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R4),(R5)                        
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO READ BUDGET  RECORDS                       *          
**********************************************************************          
         SPACE 1                                                                
RDBUD    NTR1                                                                   
         LM    R4,R5,0(R1)                                                      
         GOTO1 DATAMGR,DMCB,(8,DMRDHI),=C'ACCOUNT',(R4),(R5)                    
         MVC   0(42,R4),0(R5)                 BUMP KEY BY 1                     
         IC    R1,41(R4)                      FOR NEXT READ                     
         LA    R1,1(R1)                                                         
         STC   R1,41(R4)                                                        
         GOTO1 CHKREC,DMCB,FILEREC                                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO GET AN ELEMENT                             *          
*              P1   BYTE 0    ELEMENT CODE                           *          
*                   BYTE 1-3  A(RECORD)                              *          
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT              *          
*                   BYTE 1-3  A(SEARCH ARGUMENT)                     *          
**********************************************************************          
         SPACE 1                                                                
GETL     NTR1                                                                   
         LM    R4,R5,0(R1)                                                      
         ZIC   R2,0(R1)                                                         
         ZIC   R3,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R2),(R4)),((R3),(R5))          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO ADD AN ELEMENT                             *          
*              P1   - ADDRESS OF AREA TO BE ADDED TO                 *          
*              P2   - ADDRESS OF ELEMENT TO ADD TO RECORD            *          
**********************************************************************          
         SPACE 1                                                                
ADDL     NTR1                                                                   
         LM    R4,R5,0(R1)                                                      
*                                                                               
         USING ACCRECD,R4                                                       
ADDL10   MVC   HALF,ACCRLEN                                                     
         ZIC   R2,1(R5)                                                         
         AH    R2,HALF                                                          
         CH    R2,=H'999'                                                       
         BNH   ADDL40                                                           
         MVC   BYTE,0(R5)        SAVE ELEMENT CODE                              
         LR    R1,R4                                                            
         AH    R1,DATADISP       POINT TO FIRST ELEMENT                         
ADDL20   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,0(R1)        FIND THE OLDEST ELEMENT WITH SAME CODE         
         BE    ADDL30                                                           
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         LTR   R0,R0                                                            
         BP    ADDL20                                                           
         DC    H'0'                                                             
*                                                                               
ADDL30   MVI   0(R1),DELELQ                                                     
         GOTO1 HELLO,ELIST,(C'D',=C'ACCOUNT '),('DELELQ',ACCRECD),0             
         CLI   ELERR,0                                                          
         BE    ADDL10                                                           
         DC    H'0'                                                             
*                                                                               
ADDL40   DS    0H                                                               
         GOTO1 HELLO,ELIST,(C'P',=C'ACCOUNT '),(R4),(R5)                        
         CLI   ELERR,0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
**********************************************************************          
*              ADD ELEMENTS TO RECORD                                *          
**********************************************************************          
         SPACE 1                                                                
ADDEM    NTR1                                                                   
         LA    R0,12                                                            
         LA    R3,BUDMNTH                                                       
         USING BAMELD,R3                                                        
ADDEM10  CP    BAMBUDG,=P'0'                 DON'T ADD ZERO AMOUNTS             
         BE    ADDEM20                                                          
         LA    R7,OUTREC                                                        
         USING SRTBAD,R7                                                        
         GOTO1 ADDL,DMCB,SRTKEY,(R3)                                            
ADDEM20  LA    R3,BAMLNQ(R3)                                                    
         BCT   R0,ADDEM10                                                       
         BAS   RE,PTSRT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              DIVIDE YEARLY OR QUARTERLY AMOUNT BETWEEN MONTHS      *          
**********************************************************************          
         SPACE 1                                                                
DIVMN    NTR1                                                                   
         MP    DUB1,=P'100'        ADD PENNIES                                  
         ZAP   DUB2,=P'0'                                                       
         ZAP   DUB3,=P'0'                                                       
         CVB   R2,DUB1             DIVIDE  BUDGET BY 3 OR 12                    
         SR    R3,R3                                                            
         SRDA  R2,31                                                            
         D     R2,NUMTHS                                                        
         LTR   R3,R3                                                            
         BM    *+8                                                              
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         CVD   R3,DUB2             MONTHLY INTO DUB2                            
         L     R0,NUMTHS                                                        
         BCTR  R0,0          R0 = 2 OR 11                                       
         LR    R3,R1         R3 NOW POINTS TO MONTHLY BUDGET ELEMENT            
         USING BAMELD,R3                                                        
*                                                                               
DIVMN10  ZAP   BAMBUDG,DUB2                                                     
         AP    DUB3,BAMBUDG                                                     
         LA    R3,BAMLNQ(R3)                                                    
         BCT   R0,DIVMN10                                                       
         SP    DUB1,DUB3                                                        
         ZAP   BAMBUDG,DUB1       PUT DIFFERENCE IN LAST MONTH                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              BUILD BUDGET ELEMENT LIST                             *          
**********************************************************************          
         SPACE 1                                                                
         USING TAPED,R2                                                         
         USING BAMELD,R3                                                        
BLDEL    NTR1                                                                   
         LA    R2,INREC                                                         
         MVC   WORK(4),TPYEAR                                                   
         MVC   WORK+4(2),=C'01'    DUMMY UP MONTH                               
         MVC   WORK+6(2),=C'01'    DUMMY UP DAY                                 
         GOTO1 DATCON,DMCB,(9,WORK),(1,WORK+8)                                  
         MVC   YEAR,WORK+8         SAVE OFF PACKED YY                           
*                                                                               
         LA    R0,12                                                            
         LA    R3,BUDMNTH                                                       
         ZAP   DUB1,=P'10'         COUNT PACKED MONTH 1 - 12                    
BLDEL2   MVI   BAMEL,BAMELQ        X'1D' ELEMENT CODE                           
         MVI   BAMLN,BAMLNQ                                                     
         MVC   BAMMNTH(1),YEAR     YEAR                                         
         MVC   BAMMNTH+1(1),DUB1+6 MONTH                                        
         AP    DUB1,=P'10'         INCREMENT MONTH IN PACKED FORM               
         ZAP   BAMBUDG,=P'0'                                                    
         ZIC   R1,BAMLN            BUMP TO NEXT ELEMENT                         
         AR    R3,R1                                                            
         BCT   R0,BLDEL2                                                        
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
*              BUILD KEY FOR SORTER RECORD                           *          
**********************************************************************          
         SPACE 1                                                                
BLDKEY   NTR1                                                                   
         LA    R4,SRTKEY                                                        
         USING BUDRECD,R4                                                       
         XC    BUDKEY(ACCORFST+2),BUDKEY                                        
         MVI   BUDKTYP,BUDKTYPQ       TYPE 1B                                   
         MVC   BUDKCULA,SPACES                                                  
         MVI   BUDKCULA,X'CC'         COKE                                      
         MVC   BUDKUNT(2),=C'SE'                                                
         MVC   BUDKACT(5),TPACN       BOTTLER(ACN)                              
         MVC   BUDKACT+5(3),TPBUYER   AGENCY                                    
         MVC   BUDKWORK,SPACES                                                  
         MVC   BUDKCCPY(L'BUDKCULA),SPACES                                      
         MVI   BUDKCCPY,X'CC'                                                   
         MVC   BUDKCUNT(2),=C'3P'                                               
         MVC   BUDKCACT(2),TPPRD       PRODUCT CODE                             
         MVC   BUDKBUDN,=X'0001'       BUDGET NUMBER                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*         DELETE THE FILE RECORD - IF NOT ON TAPE(IF COMPLETE TAPE)  *          
**********************************************************************          
         SPACE 1                                                                
DFIL     NTR1                                                                   
         GOTO1 ACUMEL,DMCB,FILEREC,PRVBUD,PRVTOT                                
         MVI   TAPESW,X'01'                                                     
DFIL10   GOTO1 DELL,DMCB,(X'1D',FILEREC),(1,YEAR)                               
         CLI   ELERR,0                                                          
         BE    DFIL10                                                           
         LA    R4,FILEREC                                                       
         USING ACCRECD,R4                                                       
         CLI   ACCORFST(R4),0                                                   
         BNE   DFIL20                                                           
         OI    ACCOSTAT(R4),X'80'                                               
         AP    RECSDEL,=P'1'                                                    
         B     *+14                                                             
DFIL20   AP    RECSCHG,=P'1'                                                    
         NI    ACCOSTAT(R4),ALL-DELETED    UNDEL RECS WITH ELEMENTS             
         GOTO1 DMGUPD,DMCB,FILEREC,FILEREC                                      
         GOTO1 ACUMEL,DMCB,FILEREC,CURBUD,CURTOT                                
         GOTO1 PRTRT,DMCB,FILEREC                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO DELETE AN ELEMENT                          *          
*              P1   BYTE 0    ELEMENT CODE                           *          
*                   BYTE 1-3  A(RECORD)                              *          
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT              *          
*                   BYTE 1-3  A(SEARCH ARGUMENT)                     *          
**********************************************************************          
         SPACE 1                                                                
DELL     NTR1                                                                   
         LM    R4,R5,0(R1)                                                      
         ZIC   R2,0(R1)                                                         
         ZIC   R3,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R2),(R4)),((R3),(R5))          
*                                                                               
         CLI   ELERR,0                                                          
         BNE   EXIT                                                             
         GOTO1 HELLO,ELIST,(C'D',=C'ACCOUNT '),((R2),(R4)),((R3),(R5))          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              DATA MANAGER ROUTINE                                  *          
**********************************************************************          
         SPACE 1                                                                
*              ADD A RECORD TO THE FILE                                         
DMGADD   NTR1                                                                   
         MVC   COMMAND,=CL8'DMADD'                                              
         AP    RECSADD,=P'1'                                                    
         B     DMGCALL                                                          
*                                                                               
*              WRITE A RECORD TO THE FILE (UPDATE AND DELETE)                   
DMGUPD   NTR1                                                                   
         MVC   COMMAND,=CL8'DMWRT'                                              
         B     DMGCALL                                                          
*                                                                               
DMGCALL  DS    0H                                                               
         LM    R4,R5,0(R1)                                                      
         GOTO1 CHKREC,DMCB,(R4)                                                 
         CLI   RCWRITE,C'N'                                                     
         BE    EXIT                                                             
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R4),(R5)                       
         CLI   DMCB+8,0                       ANY DATAMGR ERRORS                
         BE    *+6                            CAUSE PGM FAILURE                 
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*              ADD UP MONTHLY BUDGET TOTALS                          *          
**********************************************************************          
         SPACE 1                                                                
ACUMEL   NTR1                      ACCUMULATE ELEMENT                           
         LM    R4,R6,0(R1)                                                      
*                                                                               
         USING BAMELD,R4                                                        
         LA    R4,ACCORFST(R4)                                                  
ACUMEL10 CLI   0(R4),0                                                          
         BE    ACUMELX                                                          
         CLI   0(R4),X'1D'         BUDGET ELEMENT                               
         BNE   ACUMEL20                                                         
         CLC   BAMMNTH(1),YEAR     YEAR OF TAPE                                 
         BNE   ACUMEL20                                                         
         AP    0(8,R5),BAMBUDG                                                  
         AP    0(8,R6),BAMBUDG                                                  
ACUMEL20 SR    R0,R0                                                            
         ZIC   R0,BAMLN                                                         
         AR    R4,R0                                                            
         B     ACUMEL10                                                         
*                                                                               
ACUMELX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO PRINT A LINE                               *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R6                                                        
PRTRT    NTR1                                                                   
         LA    R6,P                                                             
         CP    PRVBUD,=P'0'                                                     
         BNE   PRTRT10                                                          
         CP    CURBUD,=P'0'                                                     
         BNE   PRTRT10                                                          
         CP    TAPEAMT,=P'0'                                                    
         BNE   PRTRT10             NO ACTIVITY                                  
         MVI   TAPESW,0                                                         
         B     EXIT                                                             
*                                                                               
PRTRT10  CLC   SVYEAR,CURYEAR                                                   
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING BUDRECD,R4                                                       
         L     R4,0(,R1)                                                        
         MVC   SVYEAR,CURYEAR                                                   
         CLC   PREVKEY,BUDKCPY                DON'T PRINT ACCT INFO             
         BE    PRTRT40                        IF SAME AS PREVIOUS LINE          
         MVC   HEAD3+55(4),SVYEAR                                               
         GOTO1 ACREPORT                       SKIP A LINE FOR NEW ACCT          
         MVC   PACN,BUDKACT                   ACN                               
         MVC   PAGY,BUDKACT+5                 AGENCY                            
         MVC   READKEY,SPACES                                                   
         MVC   READKEY(11),BUDKCPY                                              
         MVC   PREVKEY,READKEY                                                  
         GOTO1 RDACC,DMCB,READKEY,MYIO                                          
         CLI   DMCB+8,0                                                         
         BE    PRTRT20                                                          
         MVC   PACCNAM(21),=C'**ERROR** NOT ON FILE'                            
         B     PRTRT40                                                          
*                                                                               
*        GET ELEMENT                                                            
*                                                                               
PRTRT20  GOTO1 GETL,DMCB,(X'20',MYIO),0                                         
         CLI   ELERR,0                                                          
         BE    PRTRT30                                                          
         MVC   PACCNAM(21),=C'**ERROR** NOT ON FILE'                            
         B     PRTRT40                                                          
PRTRT30  L     R5,ELADDR                                                        
         USING NAMELD,R5                                                        
         SR    R2,R2                                                            
         IC    R2,NAMLN                                                         
         SH    R2,=Y(NAMLN1Q+1)                                                 
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PACCNAM(0),NAMEREC                                               
PRTRT40  DS    0H                                                               
         CLI   QOPT1,C'Y'            IS THIS A REPLACEMENT TAPE                 
         BNE   PRTRT42               DON'T PRINT IF INPUTTAPE IS                
*                                    ZERO --- PER SKAYE                         
         CLC   PACCNAM(21),=C'**ERROR** NOT ON FILE'                            
         BNE   PRTRT42                                                          
         CP    CURBUD,=P'0'                                                     
         BNE   PRTRT42                                                          
         CP    TAPEAMT,=P'0'                                                    
         BNE   PRTRT42                                                          
         SP    PRVTOT,PRVBUD         ADJUST PREVIOUS TOTALS                     
         ZAP   PRVBUD,=P'0'                                                     
         MVI   TAPESW,0                                                         
         MVC   P,SPACES                                                         
         B     EXIT                                                             
*                                                                               
PRTRT42  DS    0H                                                               
         EDIT  (P8,PRVBUD),(13,PPRVBUD),2,MINUS=YES                             
         EDIT  (P8,TAPEAMT),(13,PTAPE),2,MINUS=YES                              
         CLI   TAPESW,X'00'     SWITCH ON MEANS RECORD ON FILE AND NOT          
         BE    PRTRT50          ON TAPE - WE BLANK OUT INPUT FILE AMT           
         CP    TAPEAMT,=P'0'                                                    
         BE    *+6                                                              
         DC    H'0'    TAPE AMOUNT NOT ZERO / SWITCH SHOULD NOT BE ON           
*                                                                               
         MVC   PTAPE,SPACES     INPUT TAPE WHICH MEANS DELETE THE REC.          
PRTRT50  MVI   TAPESW,X'00'     TO DISTINGUISH IT FROM ZERO AMOUNTS ON          
         EDIT  (P8,CURBUD),(13,PCURBUD),2,MINUS=YES                             
         SP    CURBUD,PRVBUD                                                    
         AP    CHNGBUD,CURBUD                                                   
PRTRT60  EDIT  (P8,CURBUD),(13,PCHNG),2,MINUS=YES                               
         MVC   PPROD,BUDKCACT                 PRODUCT                           
         MVC   PMED,BUDKCACT+2                MEDIA                             
         ZAP   PRVBUD,=P'0'                                                     
         ZAP   CURBUD,=P'0'                                                     
         ZAP   TAPEAMT,=P'0'                                                    
         MVC   HEAD3+55(4),SVYEAR                                               
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
*              CHECK RECORD FOR VALID LENGTHS                        *          
**********************************************************************          
         SPACE 1                                                                
CHKREC   NTR1                                                                   
         L     R4,0(R1)                                                         
         USING ACCRECD,R4                                                       
         CLC   ACCRLEN,=H'49'      LENGTH GT 49                                 
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   ACCRLEN,=H'999'     LENGTH LT 1000                               
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    R5,ACCORFST(R4)     1ST ELEMENT                                  
         USING BAMELD,R5                                                        
         CLI   0(R5),0             MUST BE AT LEAST 1 ELEMENT                   
         BNE   *+14                                                             
         TM    ACCOSTAT(R4),X'80'  UNLESS FLAGGED FOR DELETE                    
         BO    CHKREC20                                                         
         DC    H'0'                                                             
         SR    R3,R3                                                            
         IC    R3,BAMLN                                                         
         AR    R5,R3                                                            
CHKREC10 CLI   0(R5),0             NO MORE                                      
         BE    CHKREC20                                                         
         SR    R3,R3                                                            
         IC    R3,BAMLN                                                         
         AR    R5,R3               POINT TO NEXT ELEMENT                        
         B     CHKREC10                                                         
*                                                                               
CHKREC20 LH    R3,ACCRLEN                                                       
         AR    R4,R3               BEGINNING OF REC + LENGTH                    
         LA    R5,1(R5)            BUMP 1 TO GET PAST 00                        
         CR    R4,R5               SHOULD = 1ST BYTE OF NEXT REC                
         BE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
**********************************************************************          
*              PRINT TOTALS                                          *          
**********************************************************************          
         SPACE 1                                                                
PRTTOT   NTR1                                                                   
         USING PLINED,R6                                                        
         LA    R6,P                                                             
*                                                                               
         GOTO1 ACREPORT                                                         
         MVC   PACCNAM(L'TOTMSG),TOTMSG  PRINT SUB OR GRAND TOTALS              
         EDIT  (P8,PRVTOT),(13,PPRVBUD),2,MINUS=YES                             
         EDIT  (P8,TAPETOT),(13,PTAPE),2,MINUS=YES                              
         EDIT  (P8,CURTOT),(13,PCURBUD),2,MINUS=YES                             
         EDIT  (P8,CHNGBUD),(13,PCHNG),2,MINUS=YES                              
         MVC   HEAD3+55(4),SVYEAR                                               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         TM    FLAG,FLGGND         ARE WE DOING SUB TOTALS OR GRAND???          
         BNO   PRTTOT10              IF NOT ON - DON'T PRINT TYPE TOTAL         
         MVC   PTYPES(18),=C'TAPE INPUT RECORDS'                                
         GOTO1 ACREPORT                                                         
         MVC   PMSG(6),=C'TYPE 1'                                               
         EDIT  (P8,TYPE1CNT),(9,PTOT),0                                         
         GOTO1 ACREPORT                                                         
         MVC   PMSG(6),=C'TYPE 2'                                               
         EDIT  (P8,TYPE2CNT),(9,PTOT),0                                         
         GOTO1 ACREPORT                                                         
         MVC   PMSG(6),=C'TYPE 3'                                               
         EDIT  (P8,TYPE3CNT),(9,PTOT),0                                         
         GOTO1 ACREPORT                                                         
         MVC   PMSG(6),=C'ERRORS'                                               
         EDIT  (P8,ERRCNT),(9,PTOT),0                                           
         GOTO1 ACREPORT                                                         
         MVC   PGDMSG,=C'TOTAL'                                                 
         EDIT  (P8,TYPETOT),(9,PGDTOT),0,ZERO=NOBLANK                           
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   PTYPES(11),=C'FILE COUNTS'                                       
         GOTO1 ACREPORT                                                         
         MVC   PMSG(4),=C'ADDS'                                                 
         EDIT  (P8,RECSADD),(9,PTOT),0,MINUS=YES                                
         GOTO1 ACREPORT                                                         
         MVC   PMSG,=C'CHANGES'                                                 
         EDIT  (P8,RECSCHG),(9,PTOT),0,MINUS=YES                                
         GOTO1 ACREPORT                                                         
         MVC   PMSG,=C'DELETES'                                                 
         EDIT  (P8,RECSDEL),(9,PTOT),0,MINUS=YES                                
         GOTO1 ACREPORT                                                         
*                                                                               
*    ADD TO GRAND TOTALS                                                        
*                                                                               
PRTTOT10 LA    R1,PKTOTQ           PACKED TOTALS                                
         LA    R0,PKGRNDQ          PACKED GRAND TOTALS                          
         CR    R0,R1                                                            
         BE    *+6                 THESE HAVE TO BE EQUAL OR SOMEONE            
         DC    H'0'                ADDED A FIELD INCORRECTLY TO ONE             
         LA    R1,PKGRND           R1 = A(GRAND TOTALS)                         
         LA    R2,PKTOT            R2 = A(TOTAL FIELDS)                         
         AP    0(L'PKGRND,R1),0(L'PKTOT,R2)                                     
         LA    R1,L'PKGRND(R1)                                                  
         LA    R2,L'PKTOT(R2)                                                   
         BCT   R0,*-14                                                          
*                                                                               
*    REINITIALIZE TOTALS                                                        
*                                                                               
         LA    R1,PKTOT            R1 = A(TOTAL FIELDS)                         
         LA    R0,PKTOTQ                                                        
         ZAP   0(L'PKTOT,R1),=P'0'                                              
         LA    R1,L'PKTOT(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NTR1                                                                   
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,0(R1)                                                         
         L     R4,4(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 DUMP,DMCB,(R2),(R6)                                              
*                                                                               
         EJECT                                                                  
**********************************************************************          
*              CONSTANTS                                             *          
**********************************************************************          
         SPACE 1                                                                
SORTER   DC    V(SORTER)                                                        
HELLO    DC    V(HELLO)                                                         
PRINTER  DC    V(PRINTER)                                                       
CPRINT   DC    V(CPRINT)                                                        
PRNTBL   DC    V(PRNTBL)                                                        
ASORTC   DC    A(SORTC)                                                         
*                                                                               
PDUMP    DC    PL4'0'                                                           
RECSIN   DC    PL5'0'                                                           
*                                                                               
FSW      DC    C' '                                                             
*                                                                               
EVERY    DC    PL4'60'                                                          
DUMPCNT  DC    PL4'0'                                                           
MAXDUMP  DC    PL4'100'                                                         
MAXPRT   DC    PL4'100'                                                         
OUTCNT   DC    PL4'0'                                                           
OUTTOT   DC    PL6'0'                                                           
INTOT    DC    PL6'0'                                                           
BUDDIF   DC    PL8'0'                                                           
EOFSW    DC    X'00'                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(4,46,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(1008,,,,)'                            
         EJECT                                                                  
**********************************************************************          
*              DCB                                                              
**********************************************************************          
         SPACE 1                                                                
BUDTAPE  DCB   DDNAME=BUDTAPE,RECFM=FB,DSORG=PS,MACRF=(GM),            X        
               LRECL=132,BUFNO=2,EODAD=EOJ                                      
         EJECT                                                                  
**********************************************************************          
*              LITERALS                                                         
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*              ENTRY POINT                                                      
**********************************************************************          
         SPACE 1                                                                
         ENTRY SORTC                                                            
SORTC    DS    0D                                                               
         DS    41000C                                                           
         EJECT                                                                  
**********************************************************************          
*              DSECT FOR LOCAL STORAGE                                          
**********************************************************************          
         SPACE 1                                                                
ACIBD    DSECT                                                                  
DUB1     DS    D                                                                
DUB2     DS    D                                                                
DUB3     DS    D                                                                
MISCAMT  DS    D                                                                
SAVEREGS DS    4F                                                               
SORTSTOR DS    F                                                                
SAVERE   DS    F                                                                
TAPESW   DS    X                                                                
SAVEKEY  DS    CL42                                                             
READKEY  DS    CL42                                                             
TOTMSG   DS    CL15                FIELD FOR SUB OR GRAND TOTAL MESSAGE         
YRWORK   DS    CL3                                                              
ELCODE   DS    CL1                                                              
COMMAND  DS    CL8                                                              
PREVKEY  DS    CL11                                                             
MSG      DS    CL10                MESSAGE FIELD FOR PRNTBL                     
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
ELEMENT  DS    CL255                                                            
*                                                                               
PKFLDS   DS    0PL8                PACKED FIELDS                                
PRVBUD   DS    PL8                                                              
CURBUD   DS    PL8                                                              
TAPEAMT  DS    PL8                                                              
*                                                                               
PKTOT    DS    0PL8                PACKED TOTAL FIELDS                          
CURTOT   DS    PL8                                                              
PRVTOT   DS    PL8                                                              
CHNGBUD  DS    PL8                                                              
TAPETOT  DS    PL8                                                              
RECSADD  DS    PL8                                                              
RECSDEL  DS    PL8                                                              
RECSCHG  DS    PL8                                                              
TYPE1CNT DS    PL8                                                              
TYPE2CNT DS    PL8                                                              
TYPE3CNT DS    PL8                                                              
ERRCNT   DS    PL8                                                              
TYPETOT  DS    PL8                                                              
PKTOTQ   EQU   (*-PKTOT)/L'PKTOT   NUMBER OF TOTAL FIELDS                       
*                                                                               
PKGRND   DS    0PL8                PACKED GRAND TOTAL FIELDS                    
PKCURGD  DS    PL8                 CURRENT BUDGET GRAND TOTAL                   
PKPRVGD  DS    PL8                 PREVIOUS BUDGET GRAND TOTAL                  
PKCHNGD  DS    PL8                 CHANGE GRAND TOTAL                           
PKTPEGD  DS    PL8                 TAPE TOTAL GRAND TOTAL                       
PKADDGD  DS    PL8                 RECORDS ADDED GRAND TOTAL                    
PKDELGD  DS    PL8                 RECORDS DELETED GRAND TOTAL                  
PKRECCD  DS    PL8                 RECORDS CHANGED GRAND TOTAL                  
PKTYP1GD DS    PL8                 TYPE COUNT 1 GRAND TOTAL                     
PKTYP2GD DS    PL8                 TYPE COUNT 2 GRAND TOTAL                     
PKTYP3GD DS    PL8                 TYPE COUNT 3 GRAND TOTAL                     
PKERRGD  DS    PL8                 TYPE COUNT 3 GRAND TOTAL                     
PKTYPTOT DS    PL8                 TYPE TOTAL GRAND TOTAL                       
PKGRNDQ  EQU   (*-PKGRND)/L'PKGRND NUMBER OF PACKED GRAND TOTAL FIELDS          
PKFLDSQ  EQU   (*-PKFLDS)/L'PKFLDS NUMBER OF PACKED FIELDS                      
*                                                                               
BUDMNTH  DS    12CL(BAMLNQ)                                                     
         ORG   BUDMNTH                                                          
BQTR1    DS    3CL(BAMLNQ)                                                      
BQTR2    DS    3CL(BAMLNQ)                                                      
BQTR3    DS    3CL(BAMLNQ)                                                      
BQTR4    DS    3CL(BAMLNQ)                                                      
*                                                                               
NUMTHS   DS    F                                                                
*                                                                               
INREC    DS    CL200                                                            
YEAR     DS    X                                                                
OLDYEAR  DS    X                                                                
CURYEAR  DS    CL4                                                              
SVYEAR   DS    CL4                                                              
COMPKEY  DS    CL2                                                              
*                                                                               
FLAG     DS    X                   FLAG                                         
FLGGND   EQU   X'80'               GRAND TOTAL FLAG                             
FLGPRT   EQU   X'40'               FLAG TO SHOW FILEREC WAS PRINTED             
*                                                                               
RECLEN   DS    F                   RECORD LENGTH                                
OUTREC   DS    CL1000                                                           
FILEREC  DS    CL1000                                                           
MYIO     DS    CL1000                                                           
*                                                                               
LWSEND   EQU   *                                                                
DELELQ   EQU   X'FF'                                                            
TURNOFF  EQU   X'FF'                                                            
ALL      EQU   X'FF'                                                            
DELETED  EQU   X'80'               DELETE                                       
         EJECT                                                                  
**********************************************************************          
*              DSECT FOR TV BUGET RECORD                             *          
**********************************************************************          
         SPACE 1                                                                
TAPED    DSECT                                                                  
TPTYPE   DS    CL1                 2=TV/RADIO, 3=NON-TV                         
TPACN    DS    CL5                                                              
TPPRD    DS    CL2                                                              
TPYEAR   DS    CL4                                                              
TPBUYER  DS    CL3                                                              
         DS    CL6                 N/D                                          
TP2Q1    DS    CL9                 1ST QTR.TV                                   
TP2Q1M   DS    CL9                 1ST QTR. MISC TV                             
TP2Q2    DS    CL9                 2ST QTR.TV                                   
TP2Q2M   DS    CL9                 2ST QTR. MISC TV                             
TP2Q3    DS    CL9                 3ST QTR.TV                                   
TP2Q3M   DS    CL9                 3ST QTR. MISC TV                             
TP2Q4    DS    CL9                 4ST QTR.TV                                   
TP2Q4M   DS    CL9                 4ST QTR. MISC TV                             
TP2TOT   DS    CL9                 TOTAL                                        
TP2MEDIA DS    CL1                 MEDIA TYPE                                   
TP2TV    EQU   C'T'                - TELEVISION                                 
TP2RADIO EQU   C'R'                - RADIO                                      
TP2CABLE EQU   C'C'                - CABLE                                      
         DS    CL29                N/D                                          
         ORG   TP2Q1               *** NON TV/RADIO BUDGET RECORD ***           
TP3RADIO DS    CL9                                                              
TP3OUTDR DS    CL9                                                              
TP3NEWS  DS    CL9                                                              
TP3MISC  DS    CL9                                                              
TP3EXTRA DS    CL9                                                              
TP3NONTV DS    CL9                                                              
         DS    CL57                                                             
         ORG                                                                    
         EJECT                                                                  
**********************************************************************          
*              DSECT TO COVER PRINTLINE                              *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
PACN     DS    CL5                                                              
         DS    CL1                                                              
PAGY     DS    CL3                 AGENCY CODE                                  
         DS    CL3                                                              
PACCNAM  DS    CL35                ACCOUNT NAME                                 
         DS    CL1                                                              
PPROD    DS    CL2                 PRODUCT CODE                                 
         DS    CL1                                                              
PMED     DS    CL2                 MEDIA CODE                                   
         DS    CL1                                                              
PPRVBUD  DS    CL13                PREVIOUS BUDGET                              
         DS    CL1                                                              
PTAPE    DS    CL13                INPUT TAPE                                   
         DS    CL1                                                              
PCURBUD  DS    CL13                CURRENT BUDGET                               
         DS    CL1                                                              
PCHNG    DS    CL13                BUDGET CHANGE                                
PLNQ     EQU   *-PLINED                                                         
         ORG  PLINED                                                            
PTYPES   DS    CL1                 TYPE TOTALS AND CHANGES                      
PGDMSG   DS    CL5                 MESSAGE HEADER FOR GRAND TOTAL               
         DS    CL7                                                              
PGDTOT   DS    CL9                 GRAND TOTAL OF TYPES                         
         ORG   PTYPES                                                           
         DS    CL5                                                              
PMSG     DS    CL7                 MESSAGE HEADER                               
         DS    CL1                                                              
PTOT     DS    CL9                 TOTALS                                       
P2LNQ    EQU   *-PLINED                                                         
         EJECT                                                                  
**********************************************************************          
*              DSECT FOR SORTER RECORDS                              *          
**********************************************************************          
         SPACE 1                                                                
SRTBAD   DSECT                                                                  
SRTYEAR  DS    CL4                 ALLOW FOR MULTIPLE YEARS ON TAPE             
SRTKEY   DS    0CL42                                                            
SRT1B    DS    CL1                                                              
SRTACC   DS    CL15                                                             
SRTWRK   DS    CL2                                                              
SRTCON   DS    CL15                                                             
SRTBNO   DS    CL2                                                              
SRTSPAR  DS    CL7                                                              
SRTSTAT  DS    CL7                                                              
SRTBAEL  DS    CL1                                                              
SRTBAELE EQU  X'1D'                                                             
SRTBALEN DS    CL1                                                              
SRTBAMNT DS   CL2                                                               
SRTBABUD DS   CL8                                                               
SRTBALNE EQU  *-SRTBAD                                                          
         EJECT                                                                  
**********************************************************************          
*              INCLUDES                                                         
**********************************************************************          
         SPACE 1                                                                
*ACGENFILE                                                                      
*ACGENMODES                                                                     
*ACREPWORKD                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034ACREPIB02 04/10/15'                                      
         END                                                                    
