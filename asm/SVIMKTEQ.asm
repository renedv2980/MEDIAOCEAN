*          DATA SET SVIMKTEQ   AT LEVEL 002 AS OF 05/01/02                      
*PHASE SVIMKTEQ,*,NOAUTO                                                        
*INCLUDE CARDS                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*INCLUDR IJDFYZZZ                                                               
*INCLUDR IJFVZZWZ                                                               
         SPACE 2                                                                
*                                                                               
* THIS PROGRAM DEALS WITH A CHANGE IN MARKET NUMBER ON THE SVIFILE.             
* CONTROL CARDS INDICATE THE RATING SERVICE, OLD MARKET NUMBER AND              
* NEW NUMBER.  ALL INPUT RECORDS ARE COPIED TO OUTPUT.  HOWEVER,                
* A SECOND COPY OF THE OLD MARKET RECORDS IS WRITTEN TO THE OUTPUT              
* TAPE UNDER THE NEW MARKET NUMBER.  CONTROL CARD OPTIONS FOLLOW.               
*                                                                               
* SERVICE=     ARB, OR NSI - THE FIRST LETTER IS SUFFICIENT                     
*                                                                               
* OLDMKT=      XXXX  SPECIFIES OLD MARKET NUMBER                                
*                                                                               
* NEWMKT=      XXXX  SPECIFIES NEW MARKET NUMBER                                
*                                                                               
* DUMP=        YES, NO, OR A NUMBER.  WHEN A NUMBER IS SPECIFIED,               
*              EVERY NTH OLD MARKET RECORD AND THE GENERATED NEW                
*              MARKET RECORD WILL BE PRINTED OUT IN DUMP FORMAT.                
*              DEFAULT IS TO SUPPRESS DUMPING OF OUTPUT RECORDS.                
         TITLE 'SVIFILE MARKET EQUIVALENCE FIX'                                 
SVIFIX   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**SVIFIX,=V(REGSAVE),R9,RR=RE,CLEAR=YES              
         USING WORKD,RC                                                         
         USING SVIFIX+4096,R9                                                   
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK                             
         MVC   TITLE+10(30),=C'SVIFILE MARKET EQUIVALENCE FIX'                  
         GOTO1 PRINTER                                                          
         SPACE                                                                  
         MVC   P+10(13),=C'CONTROL CARDS'                                       
         GOTO1 PRINTER                                                          
         MVI   P+10,C'-'                                                        
         MVC   P+11(12),P+10                                                    
         GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         EJECT                                                                  
CARDREAD GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   CARD(2),=C'/*'                                                   
         BE    ENDCARD                                                          
         SPACE                                                                  
         MVC   P+10(L'CARD),CARD                                                
         GOTO1 PRINTER                                                          
         SPACE                                                                  
         LA    R0,80               CREATE A PSEUDO HEADER FOR SCANNER           
         LR    R1,R0                                                            
         LA    R1,8(R1)                                                         
         STC   R1,HEADER                                                        
         LA    R2,CARD+79                                                       
         CLI   0(R2),C' '          FIND INPUT DATA LENGTH                       
         BNE   *+10                                                             
         BCTR  R2,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,HEADER+5                                                      
         LTR   R0,R0               TEST FOR NO DATA ON CARD                     
         BZ    CONERR                                                           
         SPACE 2                                                                
* SCAN THE CONTROL CARD                                                         
*                                                                               
SCAN     GOTO1 SCANNER,DMCB,HEADER,(4,WORK),0                                   
         CLI   DMCB+4,0            TEST FOR ERROR                               
         BE    CONERR                                                           
         ZIC   R3,DMCB+4           NUMBER OF BLOCKS                             
         LA    R2,WORK             SCAN BLOCK POINTER                           
         SPACE 1                                                                
SCAN2    DS    0H                                                               
         CLI   0(R2),3             PRESUMED KEYWORD MUST                        
         BL    OPTERR              BE AT LEAST 3 AND NO MORE                    
         CLI   0(R2),8             THAN 8 BYTES                                 
         BH    OPTERR                                                           
         CLI   1(R2),0             TEST FOR MISSING PARAMETER                   
         BE    CONERR                                                           
*                                                                               
         LA    R4,OPTTAB           TABLE OF KEYWORD OPTIONS                     
         LA    R0,OPTIONS          COUNTER                                      
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
*                                                                               
SCAN3    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),0(R4)      CHECK AGAINST OPTION KEYWORD                 
         BE    SCAN4               FOUND                                        
         LA    R4,L'OPTTAB(R4)     POINT TO NEXT ENTRY                          
         BCT   R0,SCAN3                                                         
         B     OPTERR              INVALID KEYWORD                              
         SPACE                                                                  
SCAN4    LA    R4,8(R4)            CALL OPTION PROCESSING ROUTINE               
         ICM   RF,15,0(R4)                                                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         LA    R2,32(R2)           NEXT BLOCK                                   
         BCT   R3,SCAN2                                                         
         B     CARDREAD            GET NEXT CARD                                
         EJECT                                                                  
* VALIDATE RATING SERVICE                                                       
*                                                                               
VALSER   DS    0H                                                               
         ST    RE,SAVEREG          SAVE RETURN POINT                            
         TM    3(R2),X'40'         TEST FOR ALPHA INPUT                         
         BZ    PARMERR                                                          
         LA    RE,SERVTAB          POINTER TO TABLE                             
         LA    R0,SERVICES         COUNTER OF ENTRIES                           
         CLI   1(R2),3             L'PARAMETER MUST BE 3-8                      
         BH    PARMERR                                                          
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
*                                                                               
VALSER2  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R2),0(RE)      TEST VS. TABLE                               
         BE    VALSER4             FOUND                                        
         LA    RE,L'SERVTAB(RE)    TRY NEXT ENTRY                               
         BCT   R0,VALSER2                                                       
         B     PARMERR             INVALID                                      
         SPACE                                                                  
VALSER4  CLI   SERVICE,0           TEST FOR DUPLICATE SERVICE OPTIONS           
         BNE   DUPERR                                                           
         MVC   SERVICE,22(R2)      SAVE LETTER OF SERVICE                       
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE MARKET NUMBER                                                        
*                                                                               
VALMKT   DS    0H                                                               
         ST    RE,SAVEREG                                                       
         TM    3(R2),X'80'         TEST FOR VALID NUMERIC DATA                  
         BZ    PARMERR                                                          
         ICM   RF,15,8(R2)                                                      
         BZ    PARMERR                                                          
         LA    R1,OLDMKT                                                        
         CLI   12(R2),C'O'         TEST FOR OLD MARKET                          
         BE    *+8                                                              
         LA    R1,NEWMKT                                                        
         OC    0(2,R1),0(R1)       TEST FOR DUPLICATE PARAMETER                 
         BNZ   DUPERR                                                           
         STCM  RF,3,0(R1)                                                       
         L     RE,SAVEREG          RETURN POINT                                 
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE DUMP OPTION                                                          
*                                                                               
VALDUMP  DS    0H                                                               
         ST    RE,SAVEREG                                                       
         TM    3(R2),X'80'         TEST FOR NUMERIC INPUT                       
         BO    VALDUMP2                                                         
         TM    3(R2),X'40'         TEST FOR ALPHA INPUT                         
         BZ    PARMERR                                                          
         CLI   1(R2),3                                                          
         BH    PARMERR                                                          
         CLI   DUMPOPT,0           TEST FOR DUPLICATE DUMP OPTIONS              
         BNE   DUPERR                                                           
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         MVC   DUMPOPT,22(R2)                                                   
         EX    R1,YESCOMP          TEST FOR YES                                 
         BE    VALDUMPX                                                         
         EX    R1,NOCOMP                                                        
         BE    VALDUMPX                                                         
         B     PARMERR                                                          
         SPACE 1                                                                
VALDUMP2 DS    0H                                                               
         CLI   DUMPOPT,0                                                        
         BNE   DUPERR                                                           
         MVI   DUMPOPT,C'Y'                                                     
         ICM   RF,15,8(R2)                                                      
         BZ    PARMERR                                                          
         STCM  RF,3,DUMPNUM                                                     
         B     VALDUMPX                                                         
         SPACE 1                                                                
VALDUMPX L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 1                                                                
YESCOMP  CLC   22(0,R2),=C'YES'                                                 
NOCOMP   CLC   22(0,R2),=C'NO '                                                 
         EJECT                                                                  
* EOF ON READER - CHECK CONTROL OPTIONS BEFORE PROCESSING TAPES                 
*                                                                               
ENDCARD  CLI   SERVICE,0           TEST FOR MISSING PARAMETERS                  
         BE    MISPARM                                                          
         OC    OLDMKT,OLDMKT                                                    
         BZ    MISPARM                                                          
         OC    NEWMKT,NEWMKT                                                    
         BZ    MISPARM                                                          
         CLI   DUMPOPT,0           TEST FOR USER DUMP OPTION                    
         BNE   *+8                                                              
         MVI   DUMPOPT,C'N'        SET DEFAULT OPTION                           
         OC    DUMPNUM,DUMPNUM     TEST FOR DUMPNUM OPTION                      
         BNZ   *+10                                                             
         MVC   DUMPNUM,=H'1'       DEFAULT IS TO DUMP EVERY ONE                 
         B     INITIAL             GO INITIALIZE TAPE READ                      
         SPACE 2                                                                
* ERROR ROUTINE                                                                 
*                                                                               
MISPARM  DS    0H                                                               
         MVC   P+10(30),=CL30'**MISSING CONTROL OPTION**'                       
         B     ERRPRT                                                           
         SPACE                                                                  
OPTERR   DS    0H                                                               
         MVC   P+10(30),=CL30'**INVALID OPTION KEYWORD**'                       
         B     ERRPRT                                                           
         SPACE 1                                                                
PARMERR  DS    0H                                                               
         MVC   P+10(30),=CL30'**INVALID PARAMETER VALUE**'                      
         B     ERRPRT                                                           
         SPACE 1                                                                
CONERR   DS    0H                                                               
         MVC   P+10(30),=CL30'**INVALID CONTROL CARD**'                         
         B     ERRPRT                                                           
         SPACE                                                                  
DUPERR   DS    0H                                                               
         MVC   P+10(30),=CL30'**DUPLICATE CONTROL CARD**'                       
         B     ERRPRT                                                           
         SPACE 1                                                                
ERRPRT   GOTO1 PRINTER                                                          
         GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   CARD(2),=C'/*'      FLUSH THE READER                             
         BE    *+14                EOF ENCOUNTERED                              
         MVC   P+10(80),CARD                                                    
         B     ERRPRT                                                           
*                                                                               
         GOTO1 PRINTER                                                          
         MVC   P+10(30),=CL30'**JOB ENDED DUE TO ERROR**'                       
         GOTO1 PRINTER                                                          
         B     ENDJOB                                                           
         SPACE 2                                                                
ENDJOB   XBASE                                                                  
         EJECT                                                                  
INITIAL  DS    0H                                                               
         OPEN  IN,OUT                                                           
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK                             
         GOTO1 PRINTER                                                          
         B     INPUT                                                            
         SPACE 2                                                                
INPUT    LA    R2,INREC-4                                                       
         GET   IN,(R2)                                                          
         SPACE                                                                  
         LH    R1,INREC-4          RECORD LENGTH                                
         LA    R1,INREC-4(R1)      POINT TO EOR AND MARK IT                     
         XC    0(3,R1),0(R1)                                                    
         L     R1,RECIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT COUNT                        
         ST    R1,RECIN                                                         
*                                                                               
         LA    RF,OUTREC-4         COPY INPUT TO OUTPUT                         
         LA    R1,1004             VARIABLE HEADER PLUS REC BUFFER              
         LA    RE,INREC-4                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    R2,OUTREC-4                                                      
         PUT   OUT,(R2)                                                         
         L     R1,RECOUT           INCREMENT OUTPUT COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,RECOUT                                                        
*                                                                               
         LA    R4,INREC                                                         
         USING SRKEYD,R4                                                        
         CLC   SRSRC,SERVICE       TEST FOR DESIRED SERVICE                     
         BNE   INPUT                                                            
         CLC   SRHIMAR,OLDMKT      TEST FOR OLD MARKET                          
         BNE   INPUT               GET NEXT RECORD                              
*                                                                               
         L     R1,NEWMREC                                                       
         LA    R1,1(R1)                                                         
         ST    R1,NEWMREC                                                       
         SR    R0,R0               FIRST TIME FOR DUMP                          
         BAS   RE,DUMPREC                                                       
*                                                                               
         LA    R4,OUTREC                                                        
         MVC   SRHIMAR,NEWMKT      INSERT NEW MARKET                            
         LA    R2,OUTREC-4                                                      
         PUT   OUT,(R2)                                                         
         L     R1,RECOUT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECOUT                                                        
         LA    R0,1                SECOND CALL TO DUMP                          
         BAS   RE,DUMPREC                                                       
         B     INPUT               NEXT RECORD                                  
         EJECT                                                                  
* ROUTINE TO PRINT RECORDS IN DUMP FORMAT.  AT ENTRY R0=0 FOR FIRST             
* CALL AND NON-ZERO FOR COPIED RECORD                                           
*                                                                               
DUMPREC  NTR1                                                                   
         CLI   DUMPOPT,C'Y'        TEST FOR DUMP OPTION                         
         BNE   DUMPRECX                                                         
         L     RF,NEWMREC                                                       
         SR    RE,RE                                                            
         SR    R1,R1                                                            
         ICM   R1,3,DUMPNUM                                                     
         DR    RE,R1                                                            
         LTR   RE,RE               TEST FOR REMAINDER                           
         BNZ   DUMPRECX                                                         
         SPACE                                                                  
DUMPREC2 DS    0H                                                               
         MVC   MSG,SPACES                                                       
         MVC   MSG,=CL15'OLD MARKET REC'                                        
         LTR   R0,R0                                                            
         BZ    *+10                                                             
         MVC   MSG,=CL15'NEW MARKET REC'                                        
         LA    R3,15               HEADER LENGTH                                
         LH    R2,OUTREC-4         RECORD LENGTH                                
         GOTO1 PRNTBL,DMCB,((R3),MSG),OUTREC-4,C'DUMP',(R2),=C'2D'              
         SPACE 1                                                                
DUMPRECX XIT1                                                                   
         EJECT                                                                  
* ROUTINE TO DEAL WITH EOF CONDITION                                            
*                                                                               
ENDIN    CLOSE IN,OUT                                                           
         SPACE                                                                  
ENDIN2   LA    R0,L'INMSG                                                       
         GOTO1 LOGIO,DMCB,1,((R0),INMSG)                                        
         MVI   ANSWER,C' '                                                      
         GOTO1 (RF),(R1),0,(3,ANSWER)                                           
         CLC   =C'EOF',ANSWER                                                   
         BE    ENDIN4                                                           
         CLC   =C'EOV',ANSWER                                                   
         BNE   ENDIN2                                                           
         OPEN  IN,OUT                                                           
         B     INPUT                                                            
         SPACE                                                                  
ENDIN4   DS    0H                                                               
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK FOR TOTALS                  
         MVC   MID1+10(26),=C'END OF FILE SUMMARY TOTALS'                       
         MVI   MID2+10,C'-'                                                     
         MVC   MID2+11(25),MID2+10                                              
         GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         SPACE                                                                  
         LA    R3,BUCKTAB                                                       
         LA    R4,BUCKETS                                                       
         SPACE 1                                                                
ENDIN5   DS    0H                                                               
         MVC   P+10(20),4(R3)      DESCRIPTION                                  
         MVI   P+30,C'='                                                        
         L     R2,0(R3)            BUCKET VALUE                                 
         EDIT  (R2),(10,P+32)                                                   
         GOTO1 PRINTER                                                          
         LA    R3,L'BUCKTAB(R3)                                                 
         BCT   R4,ENDIN5                                                        
         GOTO1 PRINTER                                                          
         B     ENDJOB                                                           
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
INMSG    DC    C'SYS007 (SVIFILE) IS IT EOF OR EOV ?'                           
         SPACE 2                                                                
* EXTERNAL MODULE DIRECTORY                                                     
*                                                                               
CARDS    DC    V(CARDS)                                                         
LOGIO    DC    V(LOGIO)                                                         
PRINTER  DC    V(PRINTER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
SCANNER  DC    V(SCANNER)                                                       
         SPACE 2                                                                
* BUCKET TABLE                                                                  
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
RECIN    DC    F'0',CL20'RECORDS IN'                                            
NEWMREC  DC    F'0',CL20'NEW MARKET RECORDS'                                    
RECOUT   DC    F'0',CL20'RECORDS OUT'                                           
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 2                                                                
* TABLE OF CONTROL CARD OPTION KEYWORDS AND VALIDATION ROUTINES                 
*                                                                               
         DS    0D                                                               
OPTTAB   DS    0CL12                                                            
         DC    CL8'SERVICE',AL4(VALSER)                                         
         DC    CL8'OLDMKT',AL4(VALMKT)                                          
         DC    CL8'NEWMKT',AL4(VALMKT)                                          
         DC    CL8'DUMP',AL4(VALDUMP)                                           
OPTIONS  EQU   (*-OPTTAB)/L'OPTTAB                                              
         SPACE 2                                                                
SERVTAB  DS    0CL3                                                             
         DC    C'ARB'                                                           
         DC    C'NSI'                                                           
SERVICES EQU   (*-SERVTAB)/L'SERVTAB                                            
         EJECT                                                                  
* DTF AND BUFFER AREAS                                                          
*                                                                               
IN       DTFMT DEVADDR=SYS007,BLKSIZE=8000,RECFORM=VARBLK,             X        
               TYPEFLE=INPUT,IOAREA1=INAREA,FILABL=STD,                X        
               EOFADDR=ENDIN,WORKA=YES,REWIND=UNLOAD                            
         SPACE 2                                                                
OUT      DTFMT DEVADDR=SYS008,BLKSIZE=8000,RECFORM=VARBLK,             X        
               TYPEFLE=OUTPUT,IOAREA1=OUTAREA,FILABL=STD,              X        
               WORKA=YES,REWIND=UNLOAD                                          
         SPACE 2                                                                
INAREA   DS    CL8000                                                           
OUTAREA  DS    CL8000                                                           
         SPACE 2                                                                
* WORKING STORAGE                                                               
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
SAVEREG  DS    F                                                                
*                                                                               
ANSWER   DS    CL3                                                              
BYTE     DS    C                                                                
MSG      DS    CL60                                                             
         DS    0D                                                               
HEADER   DS    CL8                                                              
CARD     DS    CL80                                                             
*                                                                               
* CONTROL VALUES FROM INPUT CARDS                                               
*                                                                               
SERVICE  DS    C                                                                
OLDMKT   DS    XL2                                                              
NEWMKT   DS    XL2                                                              
DUMPOPT  DS    C                                                                
DUMPNUM  DS    XL2                                                              
*                                                                               
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
*                                                                               
WORK     DS    CL480                                                            
*                                                                               
         DS    0D                                                               
         DS    F                                                                
INREC    DS    CL1000                                                           
         DS    F                                                                
OUTREC   DS    CL1000                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* RZSVIFILE                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE RZSVIFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SVIMKTEQ  05/01/02'                                      
         END                                                                    
