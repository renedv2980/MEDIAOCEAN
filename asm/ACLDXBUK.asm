*          DATA SET ACLDXBUK   AT LEVEL 021 AS OF 03/14/03                      
*PHASE ACLDXBUK                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'DELETE CONTRA HEADERS AND FIX SOME BUCKETS'                     
***********************************************************************         
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
***********************************************************************         
ACLDXBUK CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,ACLDXBUK,R9                                          
         USING WORKD,RC                                                         
         ST    R1,APARM                                                         
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'                                                      
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE                                                          *         
***********************************************************************         
DMXINIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD                                                      *         
***********************************************************************         
DMXREC   DS    0H                                                               
         L     R2,AREC             R2=A(INPUT RECORD)                           
         USING ACTRECD,R2                                                       
         GOTO1 RECTYP,DMCB,(C'D',ACTRECD)                                       
         MVC   RECTYPE,0(R1)       RECORD TYPE                                  
         MVC   COMPANY,1(R1)       COMPANY CODE                                 
         MVC   COMPDSP,2(R1)       DISPLACEMENT TO COMPANY IN KEY               
*                                                                               
         CLI   RECTYPE,ACRTCHDH    PURGE CONTRA HEADERS                         
         BNE   DMXREC3                                                          
         USING CHDRECD,R2                                                       
         CLC   CHDKSPCS,SPACES     WITH NON BLANK TYPE                          
         BNE   DMXREC2                                                          
         OC    CHDKNULL,CHDKNULL                                                
         BNZ   DMXREC2                                                          
         MVC   SAVCHDK,CHDKEY      SAVE A REAL CONTRA HEADER                    
         B     DMXKEEP                                                          
*                                                                               
DMXREC2  CLC   SAVCHDK,CHDKEY      TEST ALREADY HAVE A HEADER                   
         BE    DMXREC2P                                                         
         MVC   EIGHT,=CL8'BEFORE'                                               
*&&DO*&& BAS   RE,DMPREC                                                        
         MVI   CHDKBTYP,C' '                                                    
         CLC   CHDKSPCS,SPACES     FIX UP THE ONE WITH A TYPE                   
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    CHDKNULL,CHDKNULL                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVCHDK,CHDKEY      SAVE THIS NEW CONTRA HEADER                  
         MVC   EIGHT,=CL8'AFTER'                                                
*&&DO*&& BAS   RE,DMPREC                                                        
         B     DMXKEEP                                                          
*                                                                               
DMXREC2P CLI   CHDRFST,CACELQ      SHOULD ONLY HAVE 1 ELEMENT                   
         BNE   DMXKEEP                                                          
         SR    R0,R0                                                            
         LA    R3,CHDRFST                                                       
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   DMXKEEP             KEEP OTHERS                                  
         AP    PURGED,=P'1'                                                     
         MVC   EIGHT,=CL8'PURGED'                                               
*&&DO*&& BAS   RE,DMPREC                                                        
         B     DMXPURGE                                                         
*                                                                               
DMXREC3  CLI   RECTYPE,ACRTCAC     TEST BUCKET RECORD                           
         BNE   DMXKEEP                                                          
         USING CACRECD,R2                                                       
         MVI   FLAG,0              SET FIRST TIME                               
*                                                                               
DMXREC4  LA    R3,CACRFST          MARK PRIOR BUCKETS                           
         SR    R0,R0                                                            
*                                                                               
DMXREC6  CLI   0(R3),0                                                          
         BE    DMXREC10                                                         
         CLI   0(R3),PBKELQ                                                     
         BNE   DMXREC8                                                          
         USING PBKELD,R3                                                        
         CLC   PBKLOW,PBKHI        ONE MONTH ?                                  
         BNE   DMXREC8                                                          
         CLI   FLAG,0              FIRST TIME ?                                 
         BNE   DMXREC7             NO,                                          
         ZAP   DRB,=P'0'           DR/CR BEFORE AND AFTER                       
         ZAP   CRB,=P'0'                                                        
         ZAP   DRA,=P'0'                                                        
         ZAP   CRA,=P'0'                                                        
         MVC   EIGHT,=CL8'BEFORE'  YES, DUMP IT                                 
*&&DO*&& BAS   RE,DMPREC                                                        
         BAS   RE,GETTOT                                                        
         MVI   FLAG,1                                                           
DMXREC7  MVI   0(R3),X'56'                                                      
DMXREC8  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DMXREC6                                                          
*                                                                               
DMXREC10 LA    R3,CACRFST          GET PRIOR BUCKETS                            
         SR    R0,R0                                                            
*                                                                               
DMXREC12 CLI   0(R3),0                                                          
         BE    DMXREC19                                                         
         CLI   0(R3),X'56'                                                      
         BE    DMXREC15                                                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DMXREC12                                                         
*                                                                               
         USING PBKELD,R3                                                        
DMXREC15 CLI   PBKDR,0             TEST ELEMENT BIG ENOUGH                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PBKCR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,BUKELEM1                                                      
         USING BUKELD,R1                                                        
         MVI   BUKEL,BUKELQ                                                     
         MVI   BUKLN,BUKLN7Q                                                    
         MVC   BUKMOS,PBKLOW                                                    
         ZAP   BUKDR7,PBKDR                                                     
         ZAP   BUKCR7,PBKCR                                                     
*                                                                               
         MVI   PBKEL,DELELQ                                                     
         GOTO1 VHELLO,ELIST,(C'D',FILE),('DELELQ',CACRECD),0                    
*                                                                               
         LA    R1,BUKELEM1                                                      
         GOTOR ADDBUK,CACRECD      ADD BUCKET ELEMENT TO RECORD                 
         B     DMXREC10            START AGAIN                                  
*                                                                               
DMXREC19 CLI   FLAG,0              ANY CHANGES ?                                
         BE    DMXKEEP                                                          
         MVC   EIGHT,=CL8'AFTER'   YES, DUMP IT                                 
*&&DO*&& BAS   RE,DMPREC                                                        
         AP    FIXED,=P'1'                                                      
         BAS   RE,GETTOT                                                        
         CP    DRA,DRB             TEST STILL HAVE SAME TOTAL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    CRA,CRB             TEST STILL HAVE SAME TOTAL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DMXKEEP                                                          
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                  *         
***********************************************************************         
DMXRET   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS                                                        *         
***********************************************************************         
DMXEOF   DS    0H                                                               
*&&DO                                                                           
         MVC   P,SPACES                                                         
         EDIT  PURGED,(7,P+1)                                                   
         MVC   P+10(20),=CL20'RECORDS PURGED'                                   
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
*                                                                               
         EDIT  FIXED,(7,P+1)                                                    
         MVC   P+10(20),=CL20'RECORDS FIXED'                                    
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
*&&                                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* XIT CONDITIONS                                                      *         
***********************************************************************         
DMXPURGE L     R1,APARM            PURGE RECORD XIT                             
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     XIT                                                              
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD XIT                              
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     XIT                                                              
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     XIT                                                              
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     XIT                                                              
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF XIT                
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     XIT                                                              
*                                                                               
EXIT     DS    0H                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                                  
***********************************************************************         
         USING ACCRECD,R2                                                       
DMPREC   DS    0H                                                               
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BHR   RE                                                               
         LR    R0,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,3,ACCRLEN                                                     
         GOTO1 PRNTBL,DMCB,(8,EIGHT),(R2),C'DUMP',(RF),=C'2D'                   
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET RECORD TOTAL                                                              
***********************************************************************         
         USING CACRECD,R2                                                       
GETTOT   LA    R1,CACRFST                                                       
         SR    R0,R0                                                            
         LA    RF,DRB              BEFORE                                       
         CLI   EIGHT,C'B'                                                       
         BE    *+8                                                              
         LA    RF,DRA              OR AFTER                                     
*                                                                               
GETTOT3  CLI   0(R1),0                                                          
         BER   RE                                                               
         CLI   0(R1),BUKELQ                                                     
         BNE   GETTOT5                                                          
         USING BUKELD,R1                                                        
         AP    0(L'DRB,RF),BUKDR                                                
         AP    L'DRB(L'DRB,RF),BUKCR                                            
         B     GETTOT7                                                          
*                                                                               
GETTOT5  CLI   0(R1),PBKELQ                                                     
         BNE   GETTOT7                                                          
         USING PBKELD,R1                                                        
         AP    0(L'DRB,RF),PBKDR                                                
         AP    L'DRB(L'DRB,RF),PBKCR                                            
*                                                                               
GETTOT7  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETTOT3                                                          
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A BUCKET ELEMENT TO A BUCKET RECORD                  *         
*                                                                     *         
* NTRY - R1=A(RECORD TO ADD BUCKET INTO)                              *         
*        BUKELEM1 CONTAINS BUCKET ELEMENT                             *         
***********************************************************************         
                                                                                
ADDBUK   NTR1  ,                                                                
         LR    R4,R1                                                            
                                                                                
         USING ACCRECD,R4          R4=A(RECORD CONTAINING BUCKETS)              
BUK1     USING BUKELD,BUKELEM1     BUKELEM1=BUCKET ELEMENT                      
                                                                                
ADDBUK02 GOTOR VHELLO,ELIST,(C'G',FILE),('BUKELQ',ACCRECD),            *        
               (L'BUKMOS,BUK1.BUKMOS)                                           
         CLI   ELERR,0             TEST ANY MORE MATCHING ELEMENTS              
         BNE   ADDBUK08            NO                                           
         L     R1,ELADDR                                                        
REC      USING BUKELD,R1                                                        
*&&DO                                                                           
         TM    TRNINDS2,TRNIRBKA   TEST REPLACING BUCKET VALUES                 
         BNZ   ADDBUK06                                                         
*&&                                                                             
ADDBUK04 AP    BUK1.BUKDR7,REC.BUKDR                                            
         AP    BUK1.BUKCR7,REC.BUKCR                                            
                                                                                
ADDBUK06 MVI   REC.BUKEL,DELELQ                                                 
         GOTOR VHELLO,ELIST,(C'D',FILE),('DELELQ',ACCRECD),0                    
         B     ADDBUK02                                                         
         DROP  REC                                                              
                                                                                
BUK2     USING BUKELD,BUKELEM2                                                  
ADDBUK08 XC    BUK2.BUKELD(BUKLN7Q),BUK2.BUKELD                                 
         CLI   BUK1.BUKDR7,0       TEST VALUES FIT IN PL6 ACCUMS                
         BNE   *+12                                                             
         CLI   BUK1.BUKCR7,0                                                    
         BE    ADDBUK12            YES - DON'T SPILT                            
                                                                                
         LA    RF,ACCRECD                                                       
         AH    RF,DATADISP         POINT TO FIRST BUCKET ELEMENT                
REC      USING BUKELD,RF                                                        
         CLI   REC.BUKEL,BUKELQ    TEST BUCKET ELEMENT                          
         BNE   ADDBUK09            OK TO SPLIT                                  
         CLC   BUK1.BUKMOS,REC.BUKMOS  TEST TRYING TO SPLIT LOW BUCKET          
         BNL   ADDBUK09                NO - OK TO SPLIT                         
SAV      USING BUKELD,BUKSAVE                                                   
         MVC   SAV.BUKELD(BUKDR-BUKELD),BUK1.BUKELD                             
         ZAP   SAV.BUKDR7,BUK1.BUKDR7                                           
         ZAP   SAV.BUKCR7,BUK1.BUKCR7                                           
         B     ADDBUK24                                                         
         DROP  REC,SAV                                                          
                                                                                
ADDBUK09 MVC   BUK2.BUKELD(BUKDR-BUKELD),BUK1.BUKELD                            
         ZAP   BUK2.BUKDR7,PZERO                                                
         ZAP   BUK2.BUKCR7,PZERO                                                
         CLI   BUK1.BUKDR7,0       TEST DEBIT BUCKET OVERFLOW                   
         BE    ADDBUK10            NO                                           
         ZAP   BUK2.BUKDR7,BUK1.BUKDR7                                          
         ZAP   DUB,PL6MAX                                                       
         CP    BUK2.BUKDR7,PZERO                                                
         BH    *+10                                                             
         MP    DUB,PMINUS1                                                      
         ZAP   BUK1.BUKDR7,DUB                                                  
         SP    BUK2.BUKDR7,DUB                                                  
                                                                                
ADDBUK10 CLI   BUK1.BUKCR7,0       TEST CREDIT BUCKET OVERFLOW                  
         BE    ADDBUK12            NO                                           
         ZAP   BUK2.BUKCR7,BUK1.BUKCR7                                          
         ZAP   DUB,PL6MAX                                                       
         CP    BUK2.BUKCR7,PZERO                                                
         BH    *+10                                                             
         MP    DUB,PMINUS1                                                      
         ZAP   BUK1.BUKCR7,DUB                                                  
         SP    BUK2.BUKCR7,DUB                                                  
                                                                                
ADDBUK12 LA    RF,ACCRECD                                                       
         AH    RF,DATADISP         POINT TO FIRST CONTRA-HEADER ELEMENT         
         SR    RE,RE               RE=A(OLDEST BUCKET ELEMENT)                  
         SR    R1,R1               R1=NUMBER OF BUCKET ELEMENTS                 
         SR    R0,R0                                                            
REC      USING BUKELD,RF                                                        
ADDBUK14 CLI   REC.BUKEL,0         TEST EOR                                     
         BE    ADDBUK18                                                         
         CLI   REC.BUKEL,BUKELQ    TEST BUCKET ELEMENT                          
         BNE   ADDBUK16                                                         
         AHI   R1,1                BUMP NUMBER OF BUCKETS                       
         LTR   RE,RE               TEST FIRST BUCKET ELEMENT                    
         BNZ   ADDBUK16                                                         
         LA    RE,REC.BUKELD       YES - SAVE IT'S ADDRESS                      
ADDBUK16 IC    R0,REC.BUKLN        BUMP TO NEXT ELEMENT                         
         AR    RF,R0                                                            
         B     ADDBUK14                                                         
         DROP  REC                                                              
                                                                                
ADDBUK18 CHI   R1,BUKMAXN          TEST ENOUGH ROOM FOR NEW BUCKET(S)           
         BNH   ADDBUK26            YES                                          
                                                                                
BLD      USING BUKELD,WORK         WORK=BUCKET BUILD AREA                       
                                                                                
         LTR   RE,RE               POINT TO OLDEST (FIRST) BUCKET               
         BNZ   *+6                                                              
         DC    H'0'                                                             
OLD      USING BUKELD,RE                                                        
SAV      USING BUKELD,BUKSAVE                                                   
         MVC   SAV.BUKELD(BUKDR-BUKELD),OLD.BUKELD                              
         ZAP   SAV.BUKDR7,OLD.BUKDR                                             
         ZAP   SAV.BUKCR7,OLD.BUKCR                                             
                                                                                
ADDBUK20 MVI   OLD.BUKEL,DELELQ    MARK ELEMENT FOR DELETION                    
         IC    R0,OLD.BUKLN        BUMP TO NEXT ELEMENT                         
         AR    RE,R0                                                            
         CLC   OLD.BUKELD(BUKDR-BUKELD),SAV.BUKELD                              
         BNE   ADDBUK22                                                         
         AP    SAV.BUKDR7,OLD.BUKDR                                             
         AP    SAV.BUKCR7,OLD.BUKCR                                             
         B     ADDBUK20                                                         
                                                                                
ADDBUK22 GOTOR VHELLO,ELIST,(C'D',FILE),('DELELQ',ACCRECD),0                    
                                                                                
         GOTOR BLDBUK              BUILD BUCKET ELEMENT IN WORK                 
         GOTOR VHELLO,ELIST,(C'P',FILE),ACCRECD,BLD.BUKEL,0                     
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
                                                                                
         GOTOR VHELLO,ELIST,(C'G',FILE),('PBKELQ',ACCRECD),0                    
         CLI   ELERR,0                                                          
         BNE   ADDBUK24                                                         
         L     R3,ELADDR           UPDATE PRIOR BUCKET ELEMENT                  
         USING PBKELD,R3                                                        
         CLC   PBKLOW,SAV.BUKYEAR                                               
         BL    *+10                                                             
         MVC   PBKLOW,SAV.BUKYEAR                                               
         CLC   PBKHI,SAV.BUKYEAR                                                
         BH    *+10                                                             
         MVC   PBKHI,SAV.BUKYEAR                                                
         AP    PBKDR,SAV.BUKDR7                                                 
         AP    PBKCR,SAV.BUKCR7                                                 
         B     ADDBUK28                                                         
                                                                                
ADDBUK24 XC    ELEMENT,ELEMENT     CREATE PRIOR BUCKET ELEMENT                  
         LA    R3,ELEMENT                                                       
         MVI   PBKEL,PBKELQ                                                     
         MVI   PBKLN,PBKLNQ                                                     
         MVC   PBKLOW,SAV.BUKYEAR                                               
         MVC   PBKHI,SAV.BUKYEAR                                                
         ZAP   PBKDR,SAV.BUKDR7                                                 
         ZAP   PBKCR,SAV.BUKCR7                                                 
         GOTOR VHELLO,ELIST,(C'P',FILE),ACCRECD,PBKELD,0                        
         CLI   ELERR,0                                                          
         BE    ADDBUK28                                                         
         DC    H'0'                CAN'T ADD THE ELEMENT                        
                                                                                
ADDBUK26 GOTOR BLDBUK              BUILD BUCKET ELEMENT IN WORK                 
         GOTOR VHELLO,ELIST,(C'P',FILE),ACCRECD,BLD.BUKEL,0                     
         CLI   ELERR,0                                                          
         BE    ADDBUK28                                                         
         DC    H'0'                CAN'T ADD THE ELEMENT                        
                                                                                
ADDBUK28 CLI   BUK2.BUKEL,0        TEST ANY REMAINDER                           
         BE    ADDBUKX                                                          
         MVC   BUK1.BUKELD(BUKLN7Q),BUK2.BUKELD                                 
         B     ADDBUK08            CAN'T ADD THE ELEMENT                        
         DROP  BUK2                                                             
                                                                                
ADDBUKX  J     EXIT                                                             
                                                                                
BLDBUK   MVC   BLD.BUKELD(BUKDR-BUKELD),BUK1.BUKELD                             
         ZAP   BLD.BUKDR,BUK1.BUKDR7                                            
         ZAP   BLD.BUKCR,BUK1.BUKCR7                                            
         MVI   BLD.BUKLN,BUKLNQ                                                 
         BR    RE                                                               
         DROP  R3,R4,RB,BLD,SAV,OLD                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DC    V(PRINT)                                                         
VHELLO   DC    V(HELLO)                                                         
RECTYP   DC    V(ACRECTYP)                                                      
*                                                                               
ACCMST   DC    CL8'ACCMST'                                                      
FILE     DC    CL8'ACCMST'                                                      
*                                                                               
SAVCHDK  DS    CL(CHDKSPCS-CHDKEY)                                              
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'5000'                                                        
DMPS     DC    AL1(DMPSN)                                                       
DMPSY    EQU   1                                                                
DMPSN    EQU   0                                                                
*                                                                               
PURGED   DC    PL4'0'                                                           
FIXED    DC    PL4'0'                                                           
DELELQ   EQU   X'FF'               FOR ELEMENT DELETION                         
PZERO    DC    P'0'                                                             
PMINUS1  DC    P'-1'                                                            
PL6MAX   DC    PL6'99999999999'                                                 
DATADISP DC    Y(ACCRFST-ACCRECD)                                               
FLAG     DC    XL1'00'                                                          
BUKMAXN  EQU   108                 MAXIMUM N'BUCKET ELEMENTS ON RECORD          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORK AREA                                                           *         
***********************************************************************         
WORKD    DSECT                                                                  
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL64                                                             
HALF     DS    H                                                                
*                                                                               
EIGHT    DS    CL8                                                              
*                                                                               
RECTYPE  DS    XL1                                                              
COMPANY  DS    XL1                                                              
COMPDSP  DS    XL1                                                              
*                                                                               
BUKELEM1 DS    XL(BUKLN7Q)         BUCKET ELEMENT 1 BUILT HERE                  
BUKELEM2 DS    XL(BUKLN7Q)         BUCKET ELEMENT 2 BUILT HERE                  
BUKSAVE  DS    XL(BUKLNQ)          BUCKET ELEMENT SAVE AREA                     
*                                                                               
ELIST    DS    3A                  HELLO PARAMETER LIST                         
ELERR    DS    0X                  HELLO ERROR RETURN BYTE                      
ELADDR   DS    A                   HELLO ELEMENT ADDRESS (GET)                  
ELADDR2  DS    A                   HELLO ELEMENT ADDRESS (ADD)                  
ELADDR3  DS    A                                                                
ELADDR4  DS    A                                                                
*                                                                               
DRB      DS    PL8                                                              
CRB      DS    PL8                                                              
DRA      DS    PL8                                                              
CRA      DS    PL8                                                              
*                                                                               
ELEMENT  DS    XL256                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
BUKELD   DSECT                                                                  
         ORG   BUKDR                                                            
BUKDR7   DS    PL7                                                              
BUKCR7   DS    PL7                                                              
BUKLN7Q  EQU   *-BUKELD                                                         
                                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACLDXBUK  03/14/03'                                      
         END                                                                    
