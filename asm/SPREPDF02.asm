*          DATA SET SPREPDF02  AT LEVEL 001 AS OF 05/28/09                      
*PHASE SPDF02A                                                                  
         TITLE 'SPDF02 - CONVERTS DFORMULA RECS INTO A WORKER FILE'             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                       INPUT FILE FORMAT                             *         
* DEMO / BOOK / DEMO VALUE / ANY FIELDS NEEDED FOR EXCEL CALCULATION  *         
* EG = HOMES/0837/11450/114500                                        *         
*                       WORKER FILE OUTPUT                            *         
*               TWO LINES OF OUTPUT FOR EVERY LINE OF INPUT           *         
* P/C(1) - (U/L)DEMO(7) - BOOK(7) - DEMO VALUE(60)(*10000)            *         
* EG = P UHOMES   0837    11450                                       *         
*      C LHOMES   0837    11450*10000                                 *         
*                                                                     *         
*  QOPT1=T =TST SYSTEM                                                *         
*  QOPT1=C =CSC SYSTEM                                                *         
*  QOPT2=Y =WRKR FILE ON HOLD                                         *         
*  QOPT3=Y =TRACE                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
SPDF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPDF02,R8,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    DF10                                                             
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DF10     DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,TDAY)                                       
*                                                                               
         BAS   RE,WRKROPEN          OPEN WORKER FILE                            
*                                                                               
         OPEN  (FILEIN,(INPUT))     OPEN THE INPUT FILE                         
         LTR   RF,RF                OPENED SUCCESSFULLY?                        
         BZ    *+6                  YES                                         
         DC    H'0'                 NO - DEATH                                  
*                                                                               
DF20     LA    R0,IOLEN             RECORD LENGTH                               
         XC    IO,IO                CLEAR THE RECORD AREA                       
         GET   FILEIN,(0)           GET 1 LINE FROM THE DATASET                 
*                                                                               
         LA    R7,IO                POINT TO RECORD                             
*                                                                               
         USING WRECD,R4                                                         
         LA    R4,IO2               COPY TO WORKER RECORD                       
         XC    0(256,R4),0(R4)      CLEAR WORKER FILE ENTRY                     
*                                                                               
         MVI   WRFILE,C'P'             FILE=P                                   
         MVI   WRFILE+L'WRFILE,X'05'   TAB                                      
*                                                                               
         BAS   RE,FINDLEN              FIND DEMO FIELD LENGTH                   
         MVI   WRDEMO,C'U'             DEMO STARTS WITH A "U"                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WRDEMO+1(0),0(R2)       MOVE DEMO INTO WORKER FILE               
         MVI   WRDEMO+L'WRDEMO,X'05'   TAB                                      
*                                                                               
         BAS   RE,FINDLEN              FIND BOOK FIELD LENGTH                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WRBOOK(0),0(R2)         MOVE BOOK INTO WORKER FILE               
         MVI   WRBOOK+L'WRBOOK,X'05'   TAB                                      
*                                                                               
         BAS   RE,FINDLEN              FIND BOOK FIELD LENGTH                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WRDEMVAL(0),0(R2)       MOVE DEMO VALUE INTO WORKER FILE         
*                                                                               
         MVC   IO2L(2),=Y(WRLENQ)      L'WORKER FILE ENTRY                      
         BAS   RE,WRKR                 ADD LINE TO WORKER FILE                  
*                                                                               
         MVI   WRFILE,C'C'             FILE=C                                   
         MVI   WRDEMO,C'L'             DEMO STARTS WITH A "L"                   
         LA    R2,WRDEMVAL             DEMO VALUE                               
         AR    R2,R1                   ADD DEMO VALUE LENGTH-1                  
         MVC   1(6,R2),=C'*10000'      MOVE '*10000' AFTER DEMO VALUE           
         BAS   RE,WRKR                 ADD LINE TO WORKER FILE                  
*                                                                               
         LA    R1,350                                                           
         C     R1,SEQNUM            IF AT LEAST 350 WRKR RECS                   
         BH    *+12                 OPEN NEW WORKER FILE                        
         BAS   RE,WRKRCLSE          CLOSE CURRENT WORKER FILE                   
         BAS   RE,WRKROPEN          OPEN A NEW WORKER FILE                      
         B     DF20                 READ NXT DFORM LINE FROM INPUT FILE         
         DROP  R4                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        EXIT ROUNTINE                                *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DFX      DS    0H                                                               
         BAS   RE,WRKRCLSE                                                      
         GOTO1 AENDREQ                                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                  FIND LENGTH OF INPUT FILE FIELD                    *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
FINDLEN  DS    0H                                                               
*                                                                               
         XR    R1,R1                R1=LENGTH OF FIELD                          
         LR    R2,R7                SAVE A(INPUT FILE FIELD)                    
*                                                                               
FL10     CLI   0(R7),X'05'          TAB?                                        
         BE    FL20                 YES - DONE                                  
         AHI   R1,1                 BUMP COUNTER                                
         LA    R7,1(R7)             BUMP INPUT FILE POINTER                     
         B     FL10                                                             
*                                                                               
FL20     LA    R7,1(R7)             POINT TO NEXT DATA FIELD                    
         BCTR  R1,0                 -1 FOR EX                                   
         BR    RE                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   OPEN WORKER FILE                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKROPEN NTR1                                                                   
         L     R0,AWKBUFF          CLEAR WKFILE BUFFER                          
         L     R1,=A(WKBUFFX-WKBUFF)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,=X'0011'     AGENCY USER ID NUMBER                       
         MVC   WLSYSPRG(3),=C'SDF'                                              
***      MVI   WLSUBPRG,C'T'       SET FACTST                                   
         MVI   WLSUBPRG,C'F'       SET FACCSC                                   
         MVC   WLDAY,TDAY+2                                                     
         MVI   WLCLASS,C'T'        CLASS T FOR WRKF SCRIPTS                     
         MVI   WLTYPE,C'A'         TYPE A FOR IMMEDIATE EXEC                    
         MVC   WLPSWD,SPACES                                                    
         OI    WLATTB,WLATOBJ                                                   
*                                                                               
         LA    R3,IO2                                                           
         MVI   FIXED,C'Y'                                                       
         BAS   RE,WRKR                                                          
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         XC    WRKRINDX,WRKRINDX                                                
         LA    R5,WRKRINDX                                                      
         USING UKRECD,R5                                                        
         MVC   UKUSRID,=X'0011'    AGENCY ID NUM FOR SJR                        
         MVC   UKSYSPRG(3),=C'SDF' SPOT DFORM                                   
         MVC   UKFILENO,WLREPRNO   WORKER FILE NUMBER                           
         DROP  R5                                                               
*                                                                               
         MVC   WRKFNO,WLREPRNO     WORKER FILE NUMBER                           
         MVI   FIXED,C'N'                                                       
*                                                                               
         MVC   P(16),=C'WORKER FILE ID ='                                       
         EDIT  (B2,WLUSRID),(4,P+20)                                            
         MVI   P+24,C','                                                        
         MVC   P+25(4),WLFILEID                                                 
         GOTO1 HEXOUT,DMCB,WLDAY,P+29,1,=C'TOG'                                 
         MVC   P+31(1),WLCLASS                                                  
         MVI   P+32,C','                                                        
         EDIT  WLREPRNO,(5,P+33),0,ALIGN=LEFT                                   
         GOTO1 REPORT                                                           
         DROP  R4                                                               
*                                                                               
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPTDFORM'                                            
         MVI   18(R1),C'I'           SET TYPE TO INSERT                         
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'           INSERT ERRORS AT FILE END                  
         MVC   IO2L(2),=H'76'        72 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
*                                                                               
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
         MVC   30(8,R1),=CL8'SJR     '                                          
         MVC   38(8,R1),=CL8'DDS     '                                          
         MVC   IO2L(2),=H'50'        46 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
*                                                                               
         B     EXIT                                                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   CLOSE WORKER FILE                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKRCLSE NTR1                                                                   
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)           BUILD HEADER                           
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         MVI   FIXED,C'Y'                                                       
         XC    SEQNUM,SEQNUM                                                    
         BAS   RE,WRKR                                                          
*                                                                               
         CLI   QOPT2,C'Y'                OPTION TO SET TO STATUS HOLD           
         BNE   EXIT                                                             
WRKRC10  GOTO1 DATAMGR,DMCB,=C'HOLD    ',WRKFILEN,WRKRINDX,IO2,AWKBUFF          
WRKRCLX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                    ADD LINE TO WORKER FILE                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKR     NTR1                                                                   
         OC    SEQNUM,SEQNUM                                                    
         BZ    WRKR10                                                           
         MVC   IO2(4),=F'2102'                                                  
         EDIT  SEQNUM,(6,IO2+4),0,FILL=0                                        
                                                                                
WRKR10   DS    0H                                                               
         LA    R3,IO2                                                           
         CLI   FIXED,C'Y'                                                       
         BE    *+8                                                              
         LA    R3,IO2L                                                          
                                                                                
         GOTO1 DATAMGR,DMCB,DMPRINT,WRKFILE,0,(R3),AWKBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         ST    R1,SEQNUM                                                        
         CLI   QOPT3,C'Y'                                                       
         BNE   EXIT                                                             
         MVC   P,IO2                                                            
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         LTORG                                                                  
         EJECT                                                                  
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
AWKBUFF  DC    A(WKBUFF)                                                        
SEQNUM   DS    F                                                                
WRKFNO   DS    XL2                 WORKER FILE NUMBER                           
TDAY     DS    XL3                 YYMMDD PWOS                                  
FIXED    DS    CL1                                                              
WRKRINDX DS    CL42                                                             
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,LRECL=84,               X        
               MACRF=GM,EODAD=DFX                                               
*                                                                               
         DS    D                                                                
IOLEN    DS    XL4                                                              
IO       DS    CL80                                                             
*                                                                               
         DS    0D                                                               
IO2L     DS    F                                                                
IO2      DS    XL600               IO AREA                                      
WKBUFF   DS    14336C                                                           
WKBUFFX  EQU   *                                                                
***********************************************************************         
*        WORKER FILE DSECT                                                      
***********************************************************************         
WRECD    DSECT                                                                  
WRHDR    DS    CL30                HEADER                                       
WRFILE   DS    CL1                                                              
         DS    CL1                                                              
WRDEMO   DS    CL8                 DFORM DEMO                                   
         DS    CL1                                                              
WRBOOK   DS    CL7                 DFORM BOOK                                   
         DS    CL1                                                              
WRDEMVAL DS    CL60                DEMO VALUE                                   
WRLENQ   EQU   *-WRECD                                                          
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPDF02 05/28/09'                                      
         END                                                                    
