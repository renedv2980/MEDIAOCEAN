*          DATA SET ACLDXSC2   AT LEVEL 015 AS OF 05/01/02                      
*PHASE ACLDXSC2,*                                                               
*INCLUDE DATCON                                                                 
*INCLUDE DICTATE                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'COPY SELECTED RECORDS TO TAPE'                                  
***********************************************************************         
*                                                                     *         
*  PARAMETER LIST:                                                    *         
*                                                                     *         
*    P1=A(RECORD)     PASS FIRST BYTE X'00'= INITIALIZE               *         
*                                     X'01'= RECORD IN CORE           *         
*                                     X'FF'= END OF FILE              *         
*                     RETURN VALUE    X'00'= KEEP RECORD              *         
*                                     X'FF'= PURGE RECORD             *         
*                                     X'FF'/C'EOJ'=PURGE & CAUSE EOJ  *         
*    P2=A(TAPEOUT)    PASS FIRST BYTE X'80'= TAPE INPUT               *         
*                                     X'40'= TAPE OUTPUT              *         
*                                     X'20'= RECORD IS I/S FILE RECORD*         
*    P3=A(PARAM CARD) PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN   *         
*                     RETURN          C'R' = RETURN BACK TO EXTERNAL  *         
*    P4=A(FILE DEFN)                                                  *         
*    P5=A(PRINTER)                                                    *         
*    P6=A(CPRINT)                                                     *         
*                                                                     *         
***********************************************************************         
         EJECT ,                                                                
         SPACE 1                                                                
         PRINT NOGEN                                                            
ACLDXSC2 CSECT                                                                  
         NMOD1 ACLDSCRX-ACLDSCRD,DMLDSCR                                        
         USING ACLDSCRD,RC                                                      
         SPACE 1                                                                
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILIZE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
EXIT     XIT1                                                                   
         EJECT ,                                                                
         SPACE 1                                                                
***********************************************************************         
*  INITIALIZE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED             *         
***********************************************************************         
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
         ZAP   RCDNUM,=P'0'        RECORD NUMBER                                
*                                                                               
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   DUMPECNT,=P'0'                                                   
         ZAP   PDUMP,=P'0'                                                      
*                                                                               
         ZAP   LINE,=P'99'         NEW    PAGE                                  
         MVI   P,C' '              SKIP   A    LINE                             
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT ,                                                                
***********************************************************************         
*  PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R3                                                       
         SPACE 1                                                                
DMXREC   L     R3,AREC             ->    RECORD                                 
         LR    R4,R3               SAVE  ADDRESS    OF   RECORD                 
         CLI   RESKTYP,RESKTYPQ    X'2D'                                        
         BNE   DMXPURGE                                                         
*                                                                               
         CLI   RESKSUB,RESKSUBQ    X'02'                                        
         BNE   DMXPURGE                                                         
*                                                                               
         CLI   RESKCPY,X'6F'       COMPANY     DWNY HEX  CODE ?                 
         BNE   DMXPURGE                                                         
*                                                                               
         CLC   RESKFORM(3),=CL3'DDS' DDS RECORDS ?                              
         BNE   DMXPURGE                                                         
*                                                                               
         MVI   WRTSW,YES           WRITE THIS  RECORD                           
         MVI   RESKCPY,X'DB'       COMPANY     DDSB HEX  CODE                   
         MVC   DBGRCD,RESKEY       SAVE  RCD   HDR  FOR  DEBUGGING              
*                                                                               
         MVC   CURRFORM,RESKFORM   FORMAT                                       
*                                                                               
         AP    RCDNUM,=P'1'        RECORD    NUMBER                             
*                                                                               
         USING PDLD,R4             DETAIL    OUTPUT    LINE                     
*                                                                               
         LA    R4,P                                                             
*                                                                               
         ZAP   WORK(8),RCDNUM                                                   
         OI    WORK+7,X'0F'                                                     
         UNPK  PDLRCD#,WORK(8)                                                  
         MVC   PDLFNDFC,=CL13'FOUND FORMAT '                                    
         MVC   PDLFNDFM,CURRFORM   REPORT    TYPE                               
         GOTO1 VPRINTER                                                         
*                                                                               
*                                                                               
         DROP  R4                  KEEP IT   CLEAN                              
*                                                                               
         CLI   WRTSW,YES                                                        
         BNE   RS80                                                             
*                                                                               
         SR    R5,R5               MOVE THE  UPDATES   TO   THE  RECORD         
         LR    R4,R3               ->   WORK AREA                               
         ICM   R5,3,RESRLEN        GET  RECORD    LENGTH                        
         L     R0,AREC             ->   RECORD                                  
         LR    R1,R5               GET  RCD  LENGTH                             
         MVCL  R0,R4               COPY THE  UPDATES                            
         BAS   RE,DMPPUT           DUMP RECORD    AFTER     FIXES               
*                                                                               
RS80     DS    0H                                                               
         B     DMXKEEP             NO,       WRITE     THE  RECORD              
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                 *         
***********************************************************************         
         SPACE 1                                                                
DMXRET   DS    0H                                                               
         EJECT ,                                                                
***********************************************************************         
*  END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                *         
***********************************************************************         
         SPACE 1                                                                
DMXEOF   DS    0H                                                               
         B     DMXIT               EXIT                                         
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO DELETE AN ELEMENT                                       *         
*                                                                     *         
*        P1    BYTE 0    ELEMENT CODE                                 *         
*              BYTE 1-3  A(RECORD)                                    *         
*        P2    BYTE 0    LENGTH OF SEARCH ARGUMET                     *         
*              BYTE 1-3  A(SEARCH ARGUMENT)                           *         
*                                                                     *         
*  E.G.  GOTO1 DELEL,DMCB,('PTRELQ',AREC),0                           *         
***********************************************************************         
         SPACE 1                                                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCMST  '),((R4),(R2)),((R5),(R3))           
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO GET AN ELEMENT                                          *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO ADD AN ELEMENT                                          *         
*                                                                     *         
*        P1    A(RECORD)                                              *         
*        P2    A(ELEMENT)                                             *         
***********************************************************************         
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCMST  '),(R2),(R3)                         
         CLI   DMCB+12,0                                                        
         BE    EXIT                                                             
         TM    DMCB+12,X'05'                                                    
         BNZ   *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         MVC   P(30),=CL30'RECORD TOO BIG TO ADD TO'                            
         GOTO1 VPRINTER                                                         
         MVI   WRTSW,NO                                                         
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO FIND AN ELEMENT                                         *         
*                                                                     *         
*        P1    BYTE 0    ELEMENT CODE                                 *         
*              BYTE 1-3  A(RECORD)                                    *         
*        P2    BYTE 0    LENGTH OF SEARCH ARGUMENT                    *         
*              BYTE 1-3  A(SEARCH ARGUMENT)                           *         
***********************************************************************         
         SPACE 1                                                                
FNDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',=C'ACCBIG  '),((R4),(R2)),((R5),(R3))           
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  DUMP ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R3          GENERAL   RECORD    DSECT                    
         SPACE 1                                                                
DMPGET   TM    UPSI,UPSIGET        MUST REQUEST   DUMP                          
         BZR   RE                                                               
         CLI   DMPSW,YES           ALREADY   DUMPED    THIS RECORD ?            
         BNER  RE                                                               
         MVI   DMPSW,NO                                                         
         NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   EXIT                                                             
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    EXIT                                                             
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMP                                                             
         LA    R6,=C'GET'                                                       
         ICM   R6,8,=AL1(3)                                                     
         B     DUMP                                                             
*                                                                               
DMPPUT   TM    UPSI,UPSIPUT        MUST REQUEST   DUMP                          
         BZR   RE                                                               
         CLI   WRTSW,YES           WILL WE   WRITE/PUT THIS RECORD ?            
         BNER  RE                  NO,  SO   DO   NOT  SHOW PUT                 
         MVI   DMPSW,NO                                                         
         NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   EXIT                                                             
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    EXIT                                                             
         L     R3,AREC             ->   RECORD                                  
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMP                                                             
         LA    R6,=C'PUT'                                                       
         ICM   R6,8,=AL1(3)                                                     
         SPACE 1                                                                
DUMP     SR    R8,R8                                                            
         ICM   R8,3,ACCRLEN        RECORD    LENGTH                             
         B     DUMPNOW                                                          
*                                                                               
DMPELMNT TM    UPSI,UPSIELMT       MUST REQUEST   DUMP                          
         BZR   RE                                                               
         CLI   CHGELMNT,YES        DID  WE   CHANGE    THIS ELEMENT ?           
         BNER  RE                  NO,  SO   DO   NOT  SHOW ELEMENT             
*        MVI   DMPSW,NO                                                         
         NTR1  ,                                                                
         AP    DUMPECNT,=P'1'                                                   
         ZAP   DUB,DUMPECNT                                                     
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   EXIT                                                             
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    EXIT                                                             
         LR    R3,R4               DUMP THE   ELEMENT                           
         SR    R8,R8                                                            
         IC    R8,1(,R3)           ELEMENT    LENGTH                            
         LA    R6,TEXTMSG                                                       
         ICM   R6,8,=AL1(L'TEXTMSG)                                             
         CLC   TEXTMSG,SPACES                                                   
         BH    DUMPNOW                                                          
         LA    R6,=C'ELEMENT'                                                   
         ICM   R6,8,=AL1(7)                                                     
*        B     DUMPNOW                                                          
*                                                                               
DUMPNOW  GOTO1 PRNTBL,DMCB,(R6),(R3),C'DUMP',(R8),=C'2D'                        
         XC    TEXTMSG,TEXTMSG                                                  
         B     EXIT                                                             
*                                                                               
         DROP  R3                  KEEP IT   CLEAN                              
         EJECT ,                                                                
***********************************************************************         
*  EQUATES                                                            *         
***********************************************************************         
         SPACE 1                                                                
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'               END OF TABLE                                 
ESCHIGHQ EQU   48                  X'30' ABOVE ESCAPE SEQUENCE                  
MXRLNQ   EQU   2100                MAX   LENGTH    OF   RECORD                  
MAX#PARM EQU   4                   MAX   NUMBER    OF   KEYWORD PARMS           
MAX#PRS  EQU   6                   MAX   NUMBER    OF   PARSE   ELEM.S          
         SPACE 3                                                                
*        SUBDIVISION OF BATCH TYPES                                             
         SPACE 1                                                                
TY30DI   EQU   229                 TYPE 30 DIFFERENCE (FOREIGN CURR)            
TY06MN   EQU   230                 TYPE 06 MANUAL BILLING                       
TY30CH   EQU   231                 TYPE 30 CHECK                                
TY30OF   EQU   232                 TYPE 30 OFFSET                               
TY30WO   EQU   233                 TYPE 30 WRITE OFF                            
TY30TT   EQU   234                 TYPE 30 TRANSFER TO                          
TY30TF   EQU   235                 TYPE 30 TRANSFER FROM                        
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  CONSTANTS                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRNTBL   DC    V(PRNTBL)           ->    PRNTBL                                 
HELLO    DC    V(HELLO)            ->    HELLO                                  
HEXIN    DC    V(HEXIN)            ->    HEXIN                                  
HEXOUT   DC    V(HEXOUT)           ->    HEXOUT                                 
DATCON   DC    V(DATCON)           ->    DATCON                                 
DICTATE  DC    V(DICTATE)          ->    DICTATE                                
*                                                                               
EVERY    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'500'                                                         
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  CONSTANTS TO BE UPDATED                                            *         
***********************************************************************         
         SPACE 1                                                                
UPSI     DC    XL1'00'             RUN   UPSI CODE                              
UPSIGET  EQU   X'80'               .     DUMP GET  REQUESTS                     
UPSIPUT  EQU   X'40'               .     DUMP PUT  REQUESTS                     
UPSIELMT EQU   X'20'               .     DUMP UPDATED   ELEMENTS                
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  VARIABLES THAT MUST BE PART OF THIS MODULE                         *         
***********************************************************************         
         SPACE 1                                                                
RCDNUM   DS    PL8                 RECORD    NUMBER                             
*                                                                               
DATADISP DC    H'56'                                                            
*                                                                               
DUMPCNT  DS    PL4                 RECORDS   DUMPED                             
DUMPECNT DS    PL4                 ELEMENTS  DUMPED                             
PDUMP    DS    PL4                 TOTAL     DUMPED                             
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  VARIABLES THAT ARE NOT PART OF THIS MODULE                         *         
***********************************************************************         
         SPACE 1                                                                
ACLDSCRD DSECT                                                                  
DUB      DS    D                   DOUBLE     WORD                              
*                                                                               
DMCB     DS    6F                  PARM  LIST TO   CALL SUBROUTINES             
*                                                                               
ADELIM   DS    A                   ->    DELIMITER FOR  KEYWORD                 
APARM    DS    A                   ->    PARM LIST FROM CALLER                  
*                                                                               
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
HALF     DS    H                   HALF  WORD WORK AREA                         
*                                                                               
WORK     DS    XL64                WORK  AREA                                   
*                                                                               
BYTE     DS    CL1                 ONE   BYTE WORK AREA                         
*                                                                               
CURRFORM DS    CL(L'RESKFORM)      CURRENT    FORMAT                            
*                                                                               
CHGELMNT DS    CL1                 THIS  ELEMENT   WAS  CHANGED   (Y/N)         
*                                                                               
TEXTMSG  DS    CL12                                                             
*                                                                               
WRTSW    DS    CL1                 UPDATE     REQUESTED                         
PURGESW  DS    CL1                 PURGE      REQUESTED                         
DMPSW    DS    CL1                 OKAY  TO   DUMP THIS RECORD                  
*                                                                               
ELCODE   DS    CL1                                                              
ELEMENT  DS    CL255                                                            
DBGRCD   DS    CL(RESRFST-RESRECD) DEBUG   RCD  ID                              
DBGELMNT DS    CL256               DEBUG   ELEMENT                              
ACLDSCRX EQU   *                                                                
         EJECT ,                                                                
***********************************************************************         
*  DETAIL OUTPUT LINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
PDLD     DSECT                                                                  
*                                  DETAIL  LINE                                 
         DS    CL2                                                              
PDLRCD#  DS    CL3                 RECORD  NUMBER                               
         DS    CL1                                                              
PDLFNDFC DS    CL13                FOUND   FORMAT                               
         DS    CL1                                                              
PDLFNDFM DS    CL8                 CURRENT FORMAT                               
         DS    CL1                                                              
PDLLNQ   EQU   *-PDLD              LENGTH OF A DETAIL OUTPUT LINE               
         EJECT ,                                                                
         SPACE 1                                                                
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACLDXSC2  05/01/02'                                      
         END                                                                    
