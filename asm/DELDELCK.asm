*          DATA SET DELDELCK   AT LEVEL 002 AS OF 05/01/02                      
*PHASE DELDELCK,+0                                                              
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'DELDELCK - LOAD/DUMP EXTERN TO CHECK PAV RECORDS'               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         ST    R5,RELO             SAVE RELOCATION FACTOR                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*****************************************************************               
* INITIALIZE LOGIC                                              *               
*****************************************************************               
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
**************************************************************                  
* PROCESS RECORD LOGIC                                       *                  
* LOOK AT 'P' RECORDS ON PAVFIL FOR MISSING UNIVERSE         *                  
* ELEMENTS.  KEEP A TABLE OF STATION/BOOKS THAT HAVE MISSING *                  
* UNIVERSES.                                                 *                  
**************************************************************                  
         SPACE 1                                                                
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PRKEY,R3                                                         
         L     R1,RECIN                                                         
         LA    R1,1(R1)                                                         
         ST    R1,RECIN                                                         
         CLI   PRCODE,C'P'         TEST FOR 'P' RECORD                          
         BNE   DMXKEEP                                                          
         CLI   PRMEDIA,C'T'                                                     
         BNE   DMXKEEP                                                          
*                                                                               
         MVC   HALF,PRBOOK         EXTRACT BOOK                                 
         CLI   HALF,70             CHECK YEAR                                   
         BL    DMXRECB                                                          
         CLI   HALF,99             COMPARE AGAINST MAXIMUM YEAR                 
         BH    DMXRECB                                                          
         CLI   HALF+1,1            TEST FOR VALID MONTH                         
         BL    DMXRECB                                                          
         CLI   HALF+1,12                                                        
         BH    DMXRECB                                                          
*                                                                               
         CLC   HALF,=X'520A'       ONLY LOOK AT OCT/82 AND AFTER                
         BL    DMXKEEP                                                          
         SR    R1,R1                                                            
         ICM   R1,3,PRRLEN                                                      
         AR    R1,R3               POINT TO EOR                                 
         XC    0(10,R1),0(R1)      AND CLEAR OUT A MARKER                       
*                                                                               
         MVI   ELCODE,X'37'        LOOK FOR X'37' EL ON NSI                     
         CLI   PRSRC,C'N'                                                       
         BE    *+16                                                             
         MVI   ELCODE,X'45'        LOOK FOR X'45' EL ON ARB                     
         CLI   PRSRC,C'A'                                                       
         BNE   DMXKEEP                                                          
         LR    R5,R3               SET RECORD START                             
         BAS   RE,GETEL                                                         
         BE    DMXKEEP             EXIT IF ELEMENT IS THERE                     
         SPACE 1                                                                
DMXREC2  DS    0H                                                               
         XC    WORK,WORK           BUILD A KEY TO SEARCH TABLE                  
         LA    R6,WORK                                                          
         USING TABENTD,R6                                                       
         MVC   TABSRC,PRSRC                                                     
         MVC   TABSTA,PRSTAT                                                    
         MVC   TABBOOK,PRBOOK                                                   
         MVI   ELCODE,X'01'                                                     
         LR    R5,R3               RECORD START                                 
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         USING MARELEM,R5                                                       
         MVC   TABMKT,MARNO        RATING SERVICE MARKET NUMBER                 
         DROP  R5                                                               
         SPACE 1                                                                
DMXREC3  DS    0H                                                               
         LA    RE,TABLE                                                         
         LA    R0,1000             COUNTER ON MAXIMUM ENTRIES                   
DMXREC4  OC    0(L'TABENT,RE),0(RE) TEST FOR BLANK ENTRY                        
         BZ    DMXREC6             INSERT NEW ENTRY                             
         CLC   TABKEY,0(RE)        KEY AGAINST TABLE                            
         BE    DMXREC5                                                          
         LA    RE,L'TABENT(RE)     POINT TO NEXT ENTRY                          
         BCT   R0,DMXREC4                                                       
         DC    H'0'                BLOW UP IF TABLE IS TOO SMALL                
*                                                                               
DMXREC5  ICM   R1,15,TABCOUNT-TABENTD(RE)                                       
         LA    R1,1(R1)            INCREMENT COUNT OF RECORDS                   
         STCM  R1,15,TABCOUNT-TABENTD(RE)                                       
         B     DMXKEEP                                                          
         SPACE 1                                                                
DMXREC6  DS    0H                                                               
         LA    R1,1                                                             
         STCM  R1,15,TABCOUNT      INITIALIZE COUNT                             
         MVC   0(L'TABENT,RE),TABENT ADD ENTRY TO TABLE                         
         DROP  R6                                                               
         MVC   WORK,SPACES         DUMP THE FIRST RECORD FOR STATION            
         MVC   WORK+1(5),=C'FIRST'                                              
         MVI   WORK,5                                                           
         BAS   RE,DUMPREC                                                       
         B     DMXKEEP                                                          
         SPACE 1                                                                
DMXRECB  DS    0H                                                               
         L     R1,BADREC           UPDATE BAD RECORD COUNT                      
         LA    R1,1(R1)                                                         
         ST    R1,BADREC                                                        
         B     DMXKEEP                                                          
         EJECT                                                                  
***************************************************************                 
* END-OF-FILE LOGIC                                           *                 
* SHOW TOTALS FOR RECORDS IN AND BAD RECORDS.  THEN PRINT     *                 
* THE TABLE ENTRIES OUT.                                      *                 
***************************************************************                 
         SPACE 1                                                                
DMXEOF   DS    0H                                                               
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK                             
         GOTO1 VPRINTER                                                         
         MVC   P+10(26),=C'END OF FILE SUMMARY TOTALS'                          
         GOTO1 VPRINTER                                                         
         MVI   P+10,C'-'                                                        
         MVC   P+11(25),P+10                                                    
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         LA    R4,BUCKETS          PRINT THE BUCKETS AND DESCRIPTIONS           
         LA    R3,BUCKTAB          IN A LOOP                                    
DMXEOFA  MVC   P+10(20),4(R3)      DESCRIPTION                                  
         MVI   P+30,C'='                                                        
         L     R2,0(R3)            BUCKET VALUE                                 
         EDIT  (R2),(10,P+32)                                                   
         GOTO1 VPRINTER                                                         
         LA    R3,L'BUCKTAB(R3)    POINT TO NEXT BUCKET                         
         BCT   R4,DMXEOFA                                                       
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
DMXEOF1  DS    0H                                                               
         LA    R6,TABLE                                                         
         LA    R7,1000             COUNTER                                      
         SR    R4,R4               CLEAR RECORDS ACCUMULATOR                    
         SR    R3,R3               CLEAR ENTRIES ACCUMULATOR                    
         MVC   P+10(20),=C'STATION/BOOK DETAILS'                                
         MVC   P+40(12),=C'RECORD COUNT'                                        
         GOTO1 VPRINTER                                                         
         MVI   P+10,C'-'                                                        
         MVC   P+11(19),P+10                                                    
         MVC   P+40(12),P+10                                                    
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         USING TABENTD,R6                                                       
DMXEOF2  DS    0H                                                               
         OC    TABENT,TABENT       TEST FOR END OF TABLE                        
         BZ    DMXEOFN                                                          
         MVC   P+10(1),TABSRC                                                   
         MVC   P+13(5),TABSTA                                                   
         MVC   THREE(2),TABBOOK                                                 
         MVI   THREE+2,X'01'                                                    
         GOTO1 DATCON,DMCB,(3,THREE),(6,P+20),RR=RELO                           
         MVC   HALF,TABMKT                                                      
         LH    R2,HALF                                                          
         EDIT  (R2),(4,P+30)                                                    
         ICM   R2,15,TABCOUNT                                                   
         EDIT  (R2),(6,P+40)                                                    
         AR    R4,R2               UPDATE COUNT OF PROBLEM RECORDS              
         LA    R3,1(R3)            INCREMENT ENTRIES COUNT                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         LA    R6,L'TABENT(R6)     BUMP TO NEXT TABLE ENTRY                     
         BCT   R7,DMXEOF2                                                       
         SPACE 1                                                                
DMXEOFN  DS    0H                                                               
         MVC   P+10(19),=C'TOTAL RECORDS FOUND'                                 
         MVI   P+30,C'='                                                        
         EDIT  (R4),(6,P+32)                                                    
         GOTO1 VPRINTER                                                         
         MVC   P+10(19),=C'STATION/BOOK/MKTS  '                                 
         MVI   P+30,C'='                                                        
         EDIT  (R3),(6,P+32)                                                    
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
* DUMPREC EXPECTS HEADER MESSAGE LENGTH IN WORK AND TEXT AT WORK+1              
*                                                                               
DUMPREC  NTR1                                                                   
         MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
         LH    R5,HALF                                                          
         ZIC   R2,WORK             HEADER MESSAGE LENGTH                        
         GOTO1 PRNTBL,DMCB,((R2),WORK+1),(R3),C'DUMP',(R5),=C'2D',     X        
               RR=RELO                                                          
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,23,ELCODE                                                     
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* BUCKET TABLE                                                                  
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
RECIN    DC    F'0',CL20'RECORDS IN'                                            
BADREC   DC    F'0',CL20'BAD RECORDS'                                           
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 2                                                                
* ROUTINE ADDRESSES                                                             
*                                                                               
DATCON   DC    V(DATCON)                                                        
PRNTBL   DC    V(PRNTBL)                                                        
         SPACE 2                                                                
* TABLE OF FLAGGED STATION/BOOK/MARKETS                                         
*                                                                               
TABLE    DC    1000XL14'00'                                                     
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
THREE    DS    CL3                                                              
FULL     DS    F                                                                
TODAY    DS    CL6                                                              
WORK     DS    CL64                                                             
ELCODE   DS    C                                                                
WORKX    EQU   *                                                                
         SPACE 2                                                                
* TABLE ENTRY DSECT                                                             
*                                                                               
TABENTD  DSECT                                                                  
TABENT   DS    0CL14                                                            
TABKEY   DS    0CL10                                                            
TABSRC   DS    C                                                                
TABSTA   DS    CL5                                                              
TABBOOK  DS    XL2                 (BINARY YYMM)                                
TABMKT   DS    XL2                                                              
TABCOUNT DS    XL4                                                              
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DELDELCK  05/01/02'                                      
         END                                                                    
