*          DATA SET SPTRA93    AT LEVEL 047 AS OF 05/01/02                      
*PHASE T21693A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         SPACE                                                                  
*        TITLE 'T21693 BUILD MARKET LIST OF STATIONS RECORDS'                   
***********************************************************************         
*                                                                     *         
*        THIS PROGRAM READS THE STATION FILE SEQUENTIALLY, AND FOR    *         
*        ALL STATION RECORDS ALSO HAVING TRAFFIC ADDRESS RECORDS      *         
*        BUILDS MARKET LIST RECORDS FOR TRAFFIC TBUY. THIS PROGRAM    *         
*        WILL ONLY RUN OFFLINE, AS IT USES SORTER                     *         
*                                                                     *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 - READ MARKET RECORD IN WHEN PRINTING REPORT       *         
*             AIO3 - BUILD LIST OF STATIONS IN MARKET WITH ACTION     *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - POINTER TO STATION RECORD  - AIO1                       *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE       *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - CT OF SORT RECS                                         *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
*  LEV  36-37 AUG06/87 ADD PASSIVE KEYS                                         
*  LEV  38-39 OCT01/92 ADD FILTERS AND DROP UNUSED STATIONS           *         
*  LEV  40    JAN22/93 FIX DELETING WRONG MEDIA MARKET REC            *         
*  LEV  41    FEB09/93 FIX DELETING MULTI STATIONS                    *         
*  LEV  42    APR11/94 ADD STRAFFIC                                   *         
*  LEV  43    OCT28/94 CHANGE TO SAME SCREEN AS CONTROLLER            *         
*  LEV  44    OCT31/94 FIX HEXPRINT OF RECORDS                        *         
*  LEV  45    MAR13/96 FIX PASSIVE POINTER KEYS                       *         
*  LEV  46 SMUR NOV10/99 USE RECUP FROM FACPAK                       *          
*                                                                     *         
***********************************************************************         
         TITLE 'T21693 BUILD MARKET LIST OF STATIONS RECORDS'                   
         SPACE                                                                  
T21693   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21693**,RR=R4                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R4,SPTR93RR                                                      
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       NOW BUILD RECORDS                            
         BE    BLST                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
         SPACE 2                                                                
VK       LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,TRAFLTRH         VALIDATE ANY FILTERS                         
         BAS   RE,VFTR                                                          
         SPACE                                                                  
* SEE IF ALL CLIENTS STATION LIST RECS ALREADY EXIST *                          
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STLKEY,R4                                                        
         MVC   STLKID,=X'0A31'     BUILD KEY                                    
         MVC   STLKAM,BAGYMD                                                    
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   VK10                                                             
         CLI   OFFLINE,C'Y'                                                     
         BE    EXIT                                                             
         CLI   1(RA),C'*'          IS THIS A DDS TERMINAL                       
         BNE   STLSTER                                                          
         DROP  R4                                                               
VK10     B     EXIT                                                             
         SPACE 2                                                                
* INITIALIZE *                                                                  
         SPACE                                                                  
BLST     TM    WHEN,X'18'          OV OR DDS                                    
         BZ    EXIT                                                             
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         SPACE                                                                  
         SR    R7,R7               STATION RECORD COUNTER                       
         SPACE                                                                  
* BUILD KEY, AND DO READHI                                                      
         SPACE                                                                  
         L     R3,AIO1                                                          
         USING STAKEY,R3                                                        
         MVI   0(R3),C'0'                                                       
         MVC   1(14,R3),0(R3)                                                   
         XC    15(2,R3),15(R3)                                                  
         SPACE                                                                  
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         XC    STAKCALL,STAKCALL                                                
         SPACE                                                                  
         MVC   SKEY,STAKEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',SKEY,(R3)                    
         B     BLST12                                                           
         EJECT                                                                  
* NOW READ ENTIRE STATIONN FILE CREATING A SORT REC FOR EACH STATION *          
         SPACE                                                                  
BLST10   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'STATION',SKEY,(R3)                    
         SPACE                                                                  
BLST12   MVC   SKEY,STAKEY                                                      
         CLI   STAKTYPE,C'S'          STILL IN STATION RECS                     
         BNE   BLST20                                                           
         CLC   STAKMED,QMED                                                     
         BNE   BLST20                                                           
         CLC   STAKAGY,AGENCY      THIS REC FOR THIS AGENCY                     
         BNE   BLST10                                                           
         SPACE                                                                  
         CLC   STAKCLT,=C'000'     BYPASS CLIENT SPECIFIC                       
         BNE   BLST10                                                           
         SPACE                                                                  
* SEE IF TRAFFIC ADDRESS RECORD FOR THIS STATION *                              
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STADDKEY,R4                                                      
         MVC   STAKID,=X'0A28'                                                  
         MVC   STAKAM,BAGYMD                                                    
         MVC   STAKSTA,STAKCALL                                                 
         CLI   STAKSTA+4,C' '                                                   
         BNE   *+8                                                              
         MVI   STAKSTA+4,C'T'                                                   
         SPACE                                                                  
         CLI   FTRNOTRA,C'Y'       DON'T REQUIRE TRAFFIC STATION ADDR           
         BE    BLST14                                                           
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TRAFFIC STATION ADDRESS                      
         BNE   BLST16               NO                                          
         SPACE                                                                  
* BUILD SORT RECORD FOR THIS STATION *                                          
         SPACE                                                                  
BLST14   GOTO1 MSPACK,DMCB,SMKT,STAKSTA,DUB                                     
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'PUT',DUB                                      
         LA    R7,1(,R7)                                                        
         SPACE                                                                  
         DROP  R4                                                               
         SPACE                                                                  
BLST16   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',SKEY,(R3)                    
         SPACE                                                                  
         CLC   SKEY(15),STAKEY                                                  
         BE    BLST10                                                           
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
* INITIALIZE COUNTERS AND SWITCHES *                                            
         SPACE                                                                  
BLST20   LTR   R7,R7                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         ZAP   ADDRECS,=P'0'                                                    
         ZAP   CHARECS,=P'0'                                                    
         ZAP   DELRECS,=P'0'                                                    
         ZAP   ADDPKEYS,=P'0'                                                   
         ZAP   DELPKEYS,=P'0'                                                   
         ZAP   ADDSTA,=P'0'                                                     
         ZAP   DELSTA,=P'0'                                                     
         ZAP   SAMSTA,=P'0'                                                     
         SPACE                                                                  
         BAS   RE,GSRT             GO GET FIRST SORT REC                        
         SPACE                                                                  
         MVC   SVMKT,LMKT                                                       
         SPACE                                                                  
         LA    R4,KEY                                                           
         USING STLKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   STLKID,=X'0A31'                                                  
         MVC   STLKAM,BAGYMD                                                    
         SPACE                                                                  
         GOTO1 HIGH                                                             
         MVC   SVAKEY,KEY                                                       
         SPACE                                                                  
BLST22   CLC   STLKEY(3),KEYSAVE   FIND AN EXISTING REC                         
         BNE   BLST25                                                           
         CLC   STLKEY(2),=X'0A31'                                               
         BNE   BLST25                                                           
         CLC   STLKAM,BAGYMD                                                    
         BNE   BLST25                                                           
         OC    STLKCLT,STLKCLT     THIS CLIENT SPECIFIC                         
         BZ    BLST30                                                           
BLST24   GOTO1 SEQ                                                              
         MVC   SVAKEY,KEY                                                       
         B     BLST22                                                           
         SPACE                                                                  
BLST25   MVC   SVAKEY(13),=13X'FF'    SET UP FOR EOF                            
         EJECT                                                                  
* COMPARE SORTED STATION FILE TO MARKET LIST OF STATIONS RECORDS *              
         SPACE                                                                  
BLST30   LA    R4,SVAKEY                                                        
         LA    R2,BLOCK                                                         
         LR    RE,R2                                                            
         LA    RF,GLOBKEY-BLOCK                                                 
         XCEFL                                                                  
         MVI   RECSW,C' '                                                       
         MVC   SVMKT,LMKT                                                       
         SPACE                                                                  
         CLC   LSORT,=5X'FF'       AT END?                                      
         BNE   BLST34                                                           
         SPACE                                                                  
         CLC   SVAKEY(3),=5X'FF'   AT END?                                      
         BE    BLST90                                                           
         SPACE                                                                  
BLST34   CLC   LMKT,STLKMKT        MARKET LIST TO SORTED STATION FILE           
         SPACE                                                                  
         BE    BLST50               GO MAINTAIN MARKET LIST                     
         SPACE                                                                  
         BH    BLST80               DELETE THIS RECORD                          
         EJECT                                                                  
* NEED TO ADD RECORD FOR THIS MARKET                                            
         SPACE                                                                  
         AP    ADDRECS,=P'1'                                                    
         MVI   RECSW,C'A'                                                       
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   STLKID,=X'0A31'                                                  
         MVC   STLKAM,BAGYMD                                                    
         MVC   STLKMKT,LMKT                                                     
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         XC    0(256,R6),0(R6)                                                  
         MVC   0(13,R6),STLKEY                                                  
         MVC   13(2,R6),=H'24'                                                  
         MVC   STLAGYA-STLKEY(2,R6),AGENCY                                      
         SPACE                                                                  
* ADD STATION TO LIST                                                           
         SPACE                                                                  
BLST40   XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING STLDTAEL,R6                                                      
         MVI   STLDTAEL,X'10'                                                   
         MVI   STLDTALN,STLDTAX-STLDTAEL                                        
         MVC   STLSTA,LSTA                                                      
         GOTO1 ADDELEM                                                          
         MVC   0(3,R2),LSTA                                                     
         MVI   3(R2),C'A'                                                       
         LA    R2,4(,R2)                                                        
         AP    ADDSTA,=P'1'                                                     
         SPACE                                                                  
         BAS   RE,GSRT                                                          
         SPACE                                                                  
         CLC   LMKT,SVMKT          CHANGE IN MARKET                             
         BE    BLST40                                                           
         SPACE                                                                  
         CLI   FTRHEXP,C'Y'        PRINT OUT HEX PDUMP OF RECS                  
         BNE   BLST44                                                           
         L     R1,AIO1                                                          
         SR    R0,R0                                                            
         ICM   R0,3,13(R1)                                                      
         GOTO1 =V(PRNTBL),DMCB,(6,=C'ADDREC'),AIO1,C'DUMP',(R0),=C'0D'          
         SPACE                                                                  
BLST44   CLI   TWAWRITE,C'N'       WRITE = NO                                   
         BE    BLST46                                                           
         SPACE                                                                  
         GOTO1 ADDREC                                                           
         SPACE                                                                  
* ADD PASSIVE POINTER FOR NEW STATION LIST RECORD *                             
         SPACE                                                                  
         L     R6,AIO1                                                          
         SPACE                                                                  
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JUST ADDED RECORD, MUST BE THERE             
         SPACE                                                                  
         MVC   FULL,KEY+14                                                      
         SPACE                                                                  
         LA    R4,KEY                                                           
         OI    STLKID+1,X'80'       CHANGE 31 TO B1                             
         MVC   STLPMKT,STLKMKT-STLKEY(R6)                                       
         XC    KEY+5(8),KEY+5                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLST46                                                           
         MVC   STLKEY,KEYSAVE                                                   
         MVC   KEY+14,FULL         RESTORE DISK ADDR                            
         SPACE                                                                  
         AP    ADDPKEYS,=P'1'                                                   
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'TRFDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
BLST46   MVC   KEY(L'SVKEY),SVAKEY  RESTORE ACTIVE KEY                          
         SPACE                                                                  
         BAS   RE,PSTL             PRINT OUT STATION LIST                       
         SPACE                                                                  
         CLC   SVAKEY(3),=5X'FF'   AT END?                                      
         BE    BLST30                                                           
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         B     BLST30                                                           
         EJECT                                                                  
* NOW UPDATE EXISTING RECORD *                                                  
         SPACE                                                                  
BLST50   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         CLI   FTRHEXP,C'Y'        PRINT OUT HEX PDUMP OF RECS                  
         BNE   BLST54                                                           
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(6,=C'GETREC'),AIO1,C'DUMP',(R0),=C'0D'          
         SPACE                                                                  
BLST54   MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    BLST60                                                           
         LA    R6,=X'FFFFFFFF'                                                  
         USING STLDTAEL,R6                                                      
BLST60   CLC   STLSTA,LSTA                                                      
         BE    BLST72              NO CHANGE                                    
         BL    BLST70              DEL STATION FROM LIST                        
         SPACE                                                                  
* ADD STATION TO RECORD                                                         
         SPACE                                                                  
         MVI   RECSW,C'C'                                                       
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING STLDTAEL,R6                                                      
         MVI   STLDTAEL,X'10'                                                   
         MVI   STLDTALN,STLDTAX-STLDTAEL                                        
         MVC   STLSTA,LSTA                                                      
         GOTO1 ADDELEM                                                          
         MVC   0(3,R2),LSTA                                                     
         MVI   3(R2),C'A'                                                       
         LA    R2,4(,R2)                                                        
         AP    ADDSTA,=P'1'                                                     
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
BLST62   CLC   STLSTA,LSTA                                                      
         BE    BLST64                                                           
         BAS   RE,NEXTEL                                                        
         BE    BLST62                                                           
         DC    H'0'                                                             
BLST64   BAS   RE,NEXTEL                                                        
         BE    *+8                                                              
         LA    R6,=X'FFFFFFFF'                                                  
         B     BLST74                                                           
         SPACE                                                                  
* DELETE THIS STATION FROM THE RECORD                                           
         SPACE                                                                  
BLST70   MVI   RECSW,C'C'                                                       
         SPACE                                                                  
         MVC   0(3,R2),2(R6)       SAVE DELETED STA FOR REPORT                  
         MVI   3(R2),C'D'                                                       
         LA    R2,4(,R2)                                                        
         AP    DELSTA,=P'1'                                                     
         SPACE                                                                  
         GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         SPACE                                                                  
         CLI   STLDTAEL,X'10'                                                   
         BE    BLST60                                                           
         LA    R6,=X'FFFFFFFF'                                                  
         B     BLST60                                                           
         SPACE                                                                  
BLST72   MVC   0(3,R2),2(R6)       SAVE DELETED STA FOR REPORT                  
         MVI   3(R2),C' '                                                       
         LA    R2,4(,R2)                                                        
         AP    SAMSTA,=P'1'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+8                                                              
         LA    R6,=X'FFFFFFFF'                                                  
         SPACE                                                                  
BLST74   BAS   RE,GSRT                                                          
         SPACE                                                                  
         CLC   LMKT,SVMKT          CHANGE IN MARKET                             
         BE    BLST60                                                           
         SPACE                                                                  
* DELETE ALL REMAINING STATIONS FROM THIS RECORD                                
         SPACE                                                                  
         CLI   STLDTAEL,X'10'                                                   
         BNE   BLST78                                                           
BLST76   MVI   RECSW,C'C'                                                       
         SPACE                                                                  
         MVC   0(3,R2),2(R6)       SAVE DELETED STA FOR REPORT                  
         MVI   3(R2),C'D'                                                       
         LA    R2,4(,R2)                                                        
         AP    DELSTA,=P'1'                                                     
         SPACE                                                                  
         GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         SPACE                                                                  
         CLI   STLDTAEL,X'10'                                                   
         BE    BLST76                                                           
         SPACE                                                                  
BLST78   CLI   RECSW,C'C'          HAS RECORD CHANGED                           
         BNE   BLST88                                                           
         SPACE                                                                  
         AP    CHARECS,=P'1'                                                    
         SPACE                                                                  
         CLI   FTRHEXP,C'Y'        PRINT OUT HEX PDUMP OF RECS                  
         BNE   BLST79                                                           
         L     R1,AIO1                                                          
         SR    R0,R0                                                            
         ICM   R0,3,13(R1)                                                      
         GOTO1 =V(PRNTBL),DMCB,(6,=C'PUTREC'),AIO1,C'DUMP',(R0),=C'0D'          
         SPACE                                                                  
BLST79   CLI   TWAWRITE,C'N'       WRITE = NO                                   
         BE    BLST88                                                           
         SPACE                                                                  
         GOTO1 PUTREC                                                           
         SPACE                                                                  
         B     BLST88              SET UP SEQ READ                              
         EJECT                                                                  
* DELETE ALL STATIONS FROM THE RECORD                                           
         SPACE                                                                  
BLST80   MVI   RECSW,C'D'                                                       
         AP    DELRECS,=P'1'                                                    
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         CLI   FTRHEXP,C'Y'        PRINT OUT HEX PDUMP OF RECS                  
         BNE   BLST82                                                           
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(6,=C'DELREC'),AIO1,C'DUMP',(R0),=C'0D'          
         SPACE                                                                  
BLST82   MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    BLST84                                                           
         DC    H'0'                                                             
BLST84   MVC   0(3,R2),2(R6)       SAVE DELETED STA FOR REPORT                  
         MVI   3(R2),C'D'                                                       
         LA    R2,4(,R2)                                                        
         AP    DELSTA,=P'1'                                                     
         SPACE                                                                  
         BAS   RE,NEXTEL                                                        
         BE    BLST84                                                           
         SPACE                                                                  
BLST86   L     R6,AIO                                                           
         CLI   TWAWRITE,C'N'       WRITE = NO                                   
         BE    BLST88                                                           
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMDEL',=C'TRFFIL',KEY,AIO1                       
         CLI   DMCB+8,0                                                         
         BE    BLST88                                                           
         DC    H'0'                                                             
         SPACE                                                                  
         SPACE                                                                  
BLST88   BAS   RE,PSTL             PRINT OUT STATION LIST                       
         SPACE                                                                  
         MVC   KEY,SVAKEY                                                       
         OI    DMINBTS,X'08'       READ DELETED RECORD                          
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'       SET OFF READ DELETED RECORD                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLST24                                                           
         DC    H'0'                                                             
         SPACE                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
* AT END OF ADD/UPDATE STATION LIST RECORDS *                                   
         SPACE                                                                  
BLST90   MVC   P(24),=CL24'MARKET LIST RECS ADDED ='                            
         EDIT  ADDRECS,(5,P+27),0                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P(26),=CL26'MARKET LIST RECS CHANGED ='                          
         EDIT  CHARECS,(5,P+27),0                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P(26),=CL26'MARKET LIST RECS DELETED ='                          
         EDIT  DELRECS,(5,P+27),0                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P(25),=CL25'STATIONS ADDED TO LISTS ='                           
         EDIT  ADDSTA,(5,P+27),0                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P(25),=CL25'STATIONS DELETED        ='                           
         EDIT  DELSTA,(5,P+27),0                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P(25),=CL25'STATIONS NO CHANGE      ='                           
         EDIT  SAMSTA,(5,P+27),0                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P(20),=CL20'PASSIVE KEYS ADDED ='                                
         EDIT  ADDPKEYS,(5,P+27),0                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CP    CHARECS,=P'0'                                                    
         BE    BLST94                                                           
BLST94   GOTO1 =V(SORTER),DMCB,=C'END'                                          
         B     EXIT                                                             
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE                                                    
         SPACE                                                                  
         DS    0H                                                               
PSTL     NTR1                                                                   
         MVC   P,SPACES                                                         
         SPACE                                                                  
* READ MARKET RECORD FOR NAME *                                                 
         SPACE                                                                  
         MVI   SKEY,C'0'                                                        
         MVC   SKEY+1(16),SKEY                                                  
         MVI   SKEY,C'M'                                                        
         MVC   SKEY+1(1),TRAMED                                                 
         L     R1,AIO1                                                          
         SR    R0,R0                                                            
         ICM   R0,3,STLKMKT-STLKEY(R1)                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SKEY+2(4),DUB                                                    
         MVC   SKEY+6(2),AGENCY                                                 
*                                                                               
         L     R5,AIO2                                                          
         USING MKTRECD,R5                                                       
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',SKEY,(R5)                    
         SPACE                                                                  
         LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   SKEY(8),0(R5)                                                    
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         SPACE                                                                  
         MVC   PMKTNM,0(R1)                                                     
         MVC   PST,RECSW                                                        
         MVC   PMKT,SKEY+2                                                      
         SPACE                                                                  
         LA    R6,BLOCK                                                         
         SPACE                                                                  
PSTL10   LA    R2,L'PSTALST/10                                                  
         LA    R3,PSTALST                                                       
PSTL14   CLI   3(R6),C'E'          IS THIS NONE FOR DELETE?                     
         BNE   PSTL15                                                           
         MVC   0(4,R3),0(R6)                                                    
         B     PSTL20                                                           
PSTL15   MVC   BSTA,0(R6)                                                       
         BAS   RE,FMTSTA                                                        
         MVC   0(7,R3),STAPRNT                                                  
         SPACE                                                                  
         CLI   3(R6),C' '                                                       
         BNH   PSTL16                                                           
         MVI   7(R3),C'-'                                                       
         MVC   8(1,R3),3(R6)                                                    
         SPACE                                                                  
PSTL16   LA    R3,10(,R3)                                                       
         LA    R6,4(,R6)                                                        
         CLI   0(R6),0             AT END OF STATION LIST                       
         BE    PSTL20                                                           
         BCT   R2,PSTL14                                                        
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         B     PSTL10                                                           
PSTL20   GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE FILTER ROUTINES - NOTRAFF, TRACE, ROD, TYPE, LEN, REL DATE *         
         SPACE                                                                  
         DS    0H                                                               
VFTR     NTR1                                                                   
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    EXIT                NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,4                                                             
         B     VFTR04                                                           
VFTR02   ZIC   R1,5(R2)                                                         
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VFTR08                                                           
VFTR06   XC    CONHEAD,CONHEAD                                                  
         SPACE                                                                  
         MVC   CONHEAD(L'FTRHELP),FTRHELP                                       
         GOTO1 ERREX2                                                           
         SPACE                                                                  
VFTR08   GOTO1 SCANNER,DMCB,(R2),(5,BLOCK)                                      
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         SPACE                                                                  
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         SPACE                                                                  
         EX    R1,VFTRCLCA         TRACE                                        
         BNE   VFTR20                                                           
         MVI   FTRTRACE,C'Y'                                                    
         B     VFTR80                                                           
VFTR20   EX    R1,VFTRCLCB         NOTRAFFIC - TRAFFIC ADDRESS NOT REQ          
         BNE   VFTR30                                                           
         MVI   FTRNOTRA,C'Y'                                                    
         B     VFTR80                                                           
         SPACE                                                                  
VFTR30   EX    R1,VFTRCLCC         HEXPRINT                                     
         BNE   VFTR90                                                           
         MVI   FTRHEXP,C'Y'                                                     
         B     VFTR80                                                           
         SPACE                                                                  
VFTR80   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
         B     EXIT                                                             
         SPACE                                                                  
VFTR90   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(FTRMSGL),FTRMSG                                          
         B     ERREXIT                                                          
         SPACE                                                                  
VFTRCLCA CLC   12(0,R4),=C'TRACE '                                              
VFTRCLCB CLC   12(0,R4),=C'NOTRAFFIC '                                          
VFTRCLCC CLC   12(0,R4),=C'HEXPRINT '                                           
         EJECT                                                                  
* FORMAT MARKET/STATION FOR PRINTING                                            
         SPACE                                                                  
         DS    0H                                                               
FMTSTA   NTR1                                                                   
         GOTO1 MSUNPK,DMCB,BMKTSTA,QMKT,WORK                                    
         CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),WORK                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C' '                                                       
FMTSTAX  B     EXIT                                                             
         SPACE                                                                  
* GET ANOTHER SORT RECORD                                                       
         SPACE                                                                  
GSRT     NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         SPACE                                                                  
         ICM   RF,15,4(R1)         GET REC ADDR                                 
         BNZ   GSRT10               NOT END OF SORT                             
         MVC   LSORT,=5X'FF'                                                    
         B     GSRTX                                                            
         SPACE                                                                  
GSRT10   MVC   LSORT,0(RF)                                                      
         SPACE                                                                  
GSRTX    B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
STLSTER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'STLSTMS),STLSTMS                                       
ERREXIT  GOTO1 ERREX2                                                           
         SPACE                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
STLSTMS  DC    C'** ERROR ** MARKET LIST OF STATIONS ALREADY EXISTS *'          
SORTCARD DC    CL80'SORT FIELDS=(1,5,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=5'                                     
FTRMSG   DC    C'* ERROR * '                                                    
FTRHELP  DC    C'VALID FILTERS=NOTRAFFIC/TRACE/HEXPRINT *'                      
FTRMSGL  EQU   *-FTRMSG                                                         
         EJECT                                                                  
         SPACE 3                                                                
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'--------------'                                           
         SSPEC H1,30,C'M A R K E T  S T A T I O N  L I S T'                     
         SSPEC H2,30,C'-----------------------------------'                     
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,3,PAGE                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,10,C'ST'                                                      
         SSPEC H9,10,C'--'                                                      
         SSPEC H8,13,C'MARKET NAME'                                             
         SSPEC H9,13,C'--------------------'                                    
         SSPEC H8,39,C'STATION LIST'                                            
         SSPEC H9,39,C'---------------------------------------------'           
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
       ++INCLUDE SPTRSTAL                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAA7D                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         SPACE                                                                  
* OFFLINE REPORT                                                                
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PST      DS    CL1                                                              
         DS    CL2                                                              
PMKTNM   DS    CL24                                                             
         DS    CL2                                                              
PSTALST  DS    CL70                                                             
         SPACE 3                                                                
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR93RR DS    F                                                                
FILTERS  DS    0CL3                                                             
FTRTRACE DS    CL1                                                              
FTRNOTRA DS    CL1                                                              
FTRHEXP  DS    CL1                                                              
         SPACE                                                                  
SVAKEY   DS    CL18                SAVE ACTIVE MARKET STATION LIST KEY          
SKEY     DS    CL17                STATION KEY WORK AREA                        
SVMKT    DS    XL2                                                              
         SPACE                                                                  
RECSW    DS    CL1                                                              
ADDRECS  DS    PL4                 RECORDS ADDED                                
DELRECS  DS    PL4                 RECORDS DELETED                              
CHARECS  DS    PL4                 RECORDS CHANGED                              
ADDPKEYS DS    PL4                 PASSIVE KEYS ADDED                           
DELPKEYS DS    PL4                 PASSIVE KEYS DELETED                         
ADDSTA   DS    PL4                 STATIONS ADDED                               
DELSTA   DS    PL4                 STATIONS DELETED                             
SAMSTA   DS    PL4                 STATIONS NO CHANGE                           
         SPACE                                                                  
LSORT    DS    0CL5                                                             
LMKT     DS    XL2                                                              
LSTA     DS    XL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047SPTRA93   05/01/02'                                      
         END                                                                    
