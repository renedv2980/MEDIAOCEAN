*          DATA SET PPPUB07    AT LEVEL 043 AS OF 05/01/02                      
*PHASE T40607A,+0                                                               
         TITLE 'T40607  PUBFILE MAINT. NEWSPAPER RATE SCREEN'                   
*                                                                               
********* CHANGE LOG                                                            
*                                                                               
*   SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                  
*                 ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                
*                                                                               
*  SMYE 12/07/95  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                    
*                                                                               
*   BPLA 3/95    ONLY DISALLOW DISCOUNT LEVEL "L" FOR INCH RATE PUBS            
*                ALSO INCHERR WAS EQU 116 - CHANGED TO 2                        
T40607   CSECT                                                                  
         NMOD1 0,T40607                                                         
         PRINT NOGEN                                                            
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T406FFD,RA                                                       
         LA    R9,PUBIO                                                         
         USING PUBREC,R9                                                        
         LA    R4,PUBIO                                                         
         LA    R5,16                                                            
CLEARIO  XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         BCT   R5,CLEARIO                                                       
         XC    ELEAREA(250),ELEAREA                                             
         XC    ELEAREA+250(250),ELEAREA+250                                     
         LA    R3,53                                                            
         LA    R2,PBLPUBH                                                       
         OC    PUBADDR,PUBADDR                                                  
         BZ    ERROR                                                            
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
         MVI   PUBIND,0                                                         
         XC    BDIV,BDIV                                                        
         LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'50'                                                      
NEXT50   BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    CKDATES                                                          
         CLI   0(R6),X'51'                                                      
         BE    NEXT50                                                           
         B     CKCOMB                                                           
*                                                                               
CKDATES  EQU   *                                                                
         USING PUBRATEL,R6                                                      
         CLC   BDATE(3),PUBRSTRT                                                
         BL    NEXT50                                                           
         CLI   PUBRSPCE,C' '       BYPASS SAU RATES                             
         BH    NEXT50                                                           
         OI    PUBIND,X'10'        RATE ELEMENT FOUND                           
         MVC   BDIV,PUBRSTRT       SAVE EFF DATE DISPLAYED                      
         B     CKCOMB                                                           
*                                                                               
CKCOMB   LA    R3,COMBERR                                                       
         CLI   BACT,1                                                           
         BNE   RATESCRN                                                         
         TM    PUBIND,X'10'                                                     
         BNO   RATESCRN                                                         
         CLC   BDATE(3),PUBRSTRT   RATE ELEMENT FOR BDATE CAN'T EXIST           
         BE    ERROR                                                            
         MVI   PUBIND,0                                                         
         XC    BDIV,BDIV           NO RATES DISPLAYED                           
         B     RATESCRN                                                         
         EJECT                                                                  
*                                                                               
RATESCRN CLI   BYTE2,1             SEE IF ACTION=FORMAT                         
         BE    FORMATR                                                          
         CLI   BACT,2                                                           
         BH    FORMATR                                                          
*                                                                               
EDIT     DS    0H                                                               
         LA    R7,ELEAREA                                                       
         USING RATEEL,R7                                                        
*              FORMAT  RATE ELEMENT                                             
         MVC   PUBRATEL(2),=X'501E'                                             
         LA    R2,RATEFFDH                                                      
         GOTO1 VDATVAL,DMCB,(0,RATEFFD),WORK                                    
         LA    R3,DATERR                                                        
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,PUBRSTRT)                                
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PUBRSTRT)                               
         LA    R3,ADDERR                                                        
         LA    R4,RATDTS                                                        
         LA    R5,6                                                             
         CLC   0(4,R4),=C'NONE'                                                 
         BE    CKADDB                                                           
CKADD    GOTO1 VDATVAL,DMCB,(0,0(R4)),WORK                                      
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,WORK+10)                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+10)                                
         CLC   PUBRSTRT(3),WORK+10                                              
         BE    ERROR                                                            
         LA    R4,9(R4)                                                         
         BCT   R5,CKADD                                                         
CKADDB   CLI   BACT,1              SEE IF ADD                                   
         BE    EDITA                                                            
*                                                                               
EDITO    CLC   RATBRAT(3),=C'DEL'        SPECIAL DELETE CODE                    
         BE    UPDATE                                                           
*               GO DELETE ELEMENTS                                              
*                                                                               
EDITA    LA    R2,RATBRATH                                                      
         LA    R3,2                INVALID FIELD                                
         MVC   PUBRATE(5),=PL5'0'                                               
         BAS   RE,ANY                                                           
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(5,RATBRAT),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         CP    DUB(8),=P'999999999'                                             
         BH    ERROR                                                            
         MVC   PUBRATE(5),DUB+3                                                 
         LA    R2,RATAGATH                                                      
         MVI   PUBRTYP,0                                                        
         CLI   5(R2),0                                                          
         BE    EDIT1                                                            
         CLI   8(R2),C'R'                                                       
         BE    EDIT1                                                            
         MVI   PUBRTYP,X'40'                                                    
         CLI   8(R2),C'A'                                                       
         BE    EDIT1                                                            
         MVI   PUBRTYP,X'20'       INCHES                                       
         CLI   8(R2),C'I'                                                       
         BNE   ERROR                                                            
*                                                                               
EDIT1    LA    R2,RATDLTYH                                                      
         CLI   RATDLTY,C'L'                                                     
         BE    EDIT1A                                                           
         CLI   RATDLTY,C'X'                                                     
         BE    EDIT1A                                                           
         CLI   RATDLTY,C'P'                                                     
         BE    EDIT1A                                                           
         CLI   RATDLTY,C'$'                                                     
         BE    EDIT1A                                                           
         CLI   RATDLTY,C'I'                                                     
         BE    EDIT1A                                                           
         MVI   PUBDLTYP,0                                                       
         CLI   5(R2),0                                                          
         BE    EDIT2                                                            
         B     ERROR                                                            
EDIT1A   EQU   *                                                                
         MVC   PUBDLTYP,RATDLTY                                                 
         CLI   PUBDLTYP,C'I'       CHECK RTYP/DLTYP COMBO                       
         BNE   EDIT1C                                                           
         TM    PUBRTYP,X'20'                                                    
         BNZ   EDIT2                                                            
EDIT1B   LA    R3,INCHERR                                                       
         B     ERROR                                                            
*                                                                               
EDIT1C   DS    0H                                                               
         CLI   PUBDLTYP,C'L'                                                    
         BNE   EDIT2                                                            
*                                                                               
         TM    PUBRTYP,X'20'    ONLY DISALLOW DISCOUNT LEVEL IND L              
         BNZ   EDIT1B           FOR INCH RATES                                  
*                                                                               
EDIT2    LA    R2,RATDRTYH                                                      
         MVI   PUBDRTYP,X'80'                                                   
         CLC   RATDRTY(2),=C'LR'                                                
         BE    EDITL                                                            
         CLI   RATDRTY,C'R'                                                     
         BE    EDITL                                                            
         MVI   PUBDRTYP,X'40'                                                   
         CLI   RATDRTY,C'%'                                                     
         BE    EDITL                                                            
         MVI   PUBDRTYP,0                                                       
         CLI   5(R2),0                                                          
         BE    EDITL                                                            
         B     ERROR                                                            
         EJECT                                                                  
EDITL    LA    R8,ELEAREA+30                                                    
         USING DSCTEL,R8                                                        
         LA    R4,30                                                            
         LA    R5,RATL11H                                                       
EDIT3A   CLI   5(R5),0                                                          
         BNE   EDIT3B                                                           
         CLI   22(R5),0                                                         
         BE    NEXTF                                                            
         LA    R2,0(R5)                                                         
         LA    R3,2                                                             
         B     ERROR               RATE WITH NO DIS LEVEL                       
NEXTF    LA    R5,34(R5)                                                        
         BCT   R4,EDIT3A                                                        
         B     UPDATE                                                           
*                                                                               
EDIT3B   LA    R2,RATDLTYH                                                      
         LA    R3,LEVERR           LEVEL TYPE MUST BE INPUT                     
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         LA    R2,RATDRTYH                                                      
         LA    R3,RATERR           SO MUST RATE TYPE                            
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         LA    R3,1                MISSING INPUT                                
         LA    R2,17(R5)                                                        
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         LA    R2,0(R5)                                                         
         LA    R3,2                INVALID FIELD                                
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R0)                                     
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R0,DMCB+4                                                        
         AH    R0,=H'50'                                                        
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB(6),=P'0'                                                     
         BNH   ERROR                                                            
         CP    DUB(6),=P'999999999'                                             
         BH    ERROR                                                            
         MVC   PUBDSCLV(5),DUB+1                                                
         LA    R2,17(R5)                                                        
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(5,8(R2)),(R0)                                     
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'99999999'   LOGICAL MAX 999.99999                         
         BH    ERROR                                                            
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
         MVC   PUBDSCRT(5),DUB+3                                                
         MVC   PUBDSCEL(2),=X'510E'                                             
         LA    R8,14(R8)                                                        
         B     NEXTF                                                            
*                                                                               
         DROP  R8                                                               
         EJECT                                                                  
UPDATE   DS    0H                                                               
         TM    PUBIND,X'10'        SEE IF RATES DISPLAYED                       
         BNO   UPDATE2             NO GO ADD NEW ONES                           
*                                  YES - DELETE OLD RATES                       
         LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'50'                                                      
UPDATE1  BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    CKDTS                                                            
         CLI   0(R6),X'51'                                                      
         BE    UPDATE1                                                          
         B     UPDATE2                                                          
*                                                                               
CKDTS    DS    0H                                                               
         CLI   13(R6),C' '       BYPASS SAU RATES                               
         BH    UPDATE1                                                          
         CLC   2(3,R6),BDIV                                                     
         BH    CKDTS1                                                           
         BE    DELETE50                                                         
         B     UPDATE2                                                          
*                                                                               
CKDTS1   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF REC                                   
         BE    UPDATE2                                                          
         CLI   0(R6),X'50'                                                      
         BE    CKDTS                                                            
         BL    UPDATE1             KEEP LOOKING                                 
         CLI   0(R6),X'51'                                                      
         BE    CKDTS1                                                           
         B     UPDATE2     END OF PRESENT RATES                                 
*                                                                               
DELETE50 GOTO1 VRECUP,DMCB,(1,PUBREC),0(R6)                                     
         CLI   0(R6),X'51'                                                      
         BNE   CLEARREC                                                         
         B     DELETE50                                                         
*                                                                               
*                                                                               
CLEARREC SR    R5,R5                                                            
         IC    R5,PUBIO+25                                                      
         SLL   R5,8                                                             
         IC    R5,PUBIO+26                                                      
         SR    RE,RE                                                            
         LA    RE,PUBIO                                                         
         AR    RE,R5                                                            
         SR    RF,RF                                                            
         LA    RF,PUBIO+1999                                                    
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
         B     UPDATE2                                                          
*                                                                               
*                                                                               
UPDATE2  CLC   RATBRAT(3),=C'DEL'        SPECIAL DELETE CODE                    
         BNE   UPDATE2A                                                         
         NI    PBLMEDH+4,X'DF'        UNVALIDATE MEDIA                          
         B     WRITEIT                                                          
*                                                                               
*                                                                               
UPDATE2A DS    0H                                                               
         LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'50'                                                      
UPDATE2C BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    UPDATE2D                                                         
         CLI   0(R6),X'51'         DISC ELEM                                    
         BE    UPDATE2C                                                         
         B     UPDATE2G                                                         
*                                                                               
UPDATE2D CLI   13(R6),C' '         NEW SAU RATES                                
         BH    UPDATE2C                                                         
         CLC   2(3,R6),PUBRSTRT                                                 
         BH    UPDATE2C                                                         
         BNE   UPDATE2G                                                         
*              SHOULD HAVE BEEN DELETED                                         
         LA    R3,ADDERR              RATES ALREADY EXIST MESSAGE               
         LA    R2,RATEFFDH                                                      
         B     ERROR                                                            
*                                                                               
*                                                                               
UPDATE2G GOTO1 VRECUP,DMCB,(1,PUBREC),0(R7),0(R6)                               
         LA    R7,30(R7)                                                        
         LA    R6,30(R6)                                                        
         CLI   0(R7),0                                                          
         BE    WRITEIT                                                          
         LA    R5,0(R6)                                                         
*        SAVE  ADDRESS OF FIRST 51 ADDED                                        
UPDATE3  GOTO1 VRECUP,DMCB,(1,PUBREC),0(R7),0(R6)                               
         LA    R7,14(R7)                                                        
         CLI   0(R7),0                                                          
         BE    WRITEIT                                                          
         CP    2(5,R7),2(5,R6)                                                  
         BH    UPDATE4                                                          
         BE    ERRORL                                                           
         LA    R6,14(R6)                                                        
         B     UPDATE5                                                          
*                                                                               
UPDATE4  LA    R6,0(R5)            RESET R6 TO FIRST 51 ADDED                   
UPDATE4A CP    2(5,R7),2(5,R6)                                                  
         BE    ERRORL                                                           
         BH    UPDATE3                                                          
         LA    R6,14(R6)                                                        
UPDATE5  CLI   0(R6),X'51'                                                      
         BE    UPDATE4A                                                         
         B     UPDATE3                                                          
*                                                                               
ERRORL   LA    R2,RATL11H                                                       
         LA    R3,DUPLEV           DUPLICATE DIS LEVELS                         
         B     ERROR                                                            
         DROP  R7                                                               
         EJECT                                                                  
WRITEIT  MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,PUTPUB                                                        
         B     DONE                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
         USING PUBRATEL,R6       R6 POINTS TO RATEL                             
*                                                                               
FORMATR  DS    0H                                                               
         CLI   SAVSCRN,X'07'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1              SEE IF ADD                                   
         BNE   FMT5                                                             
         BAS   R8,PUTDATES                                                      
         MVI   BYTE2,0             TO GENERATE TURNAROUND                       
         B     EDIT                                                             
*                                                                               
*                                                                               
FMT2     LA    R4,PBLLAST                                                       
         GOTO1 VCALLOV,WORK,(R4),X'D90406F7'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'07'                                                    
FMT5     CLI   BACT,1                                                           
         BNE   PUTFLDS                                                          
         BAS   R8,PUTDATES                                                      
         B     CKSRDS                                                           
*                R6  POINTS TO PUBRATEL                                         
PUTFLDS  LA    R2,PBLSCRH                                                       
         LA    R3,NORATE                                                        
         TM    PUBIND,X'10'                                                     
         BNZ   *+16                                                             
         BAS   R8,PUTDATES                                                      
         NI    PBLMEDH+4,X'DF'       UNVALIDATE MED FIELD                       
         B     ERROR                                                            
*        GOTO1 VDTCNV,DMCB,(1,PUBRSTRT),(3,RATEFFD)                             
         GOTO1 VDATCON,DMCB,(3,PUBRSTRT),(5,RATEFFD)                            
         FOUT  RATEFFDH                                                         
         MVC   PACKED5(5),PUBRATE                                               
         EDIT  PACKED5,(10,RATBRAT),5,ALIGN=LEFT                                
         FOUT  RATBRATH                                                         
         MVI   RATAGAT,C'A'                                                     
         TM    PUBRTYP,X'40'                                                    
         BNZ   PUT0                                                             
         MVI   RATAGAT,C'I'                                                     
         TM    PUBRTYP,X'20'                                                    
         BNZ   PUT0                                                             
         MVI   RATAGAT,C'R'                                                     
PUT0     DS    0H                                                               
         FOUT  RATAGATH                                                         
         MVC   RATDLTY,PUBDLTYP                                                 
PUT1     FOUT  RATDLTYH                                                         
         MVC   RATDRTY(2),=C'R '                                                
         CLI   PUBDRTYP,X'80'                                                   
         BE    PUT2                                                             
         MVC   RATDRTY(2),=C'% '                                                
         CLI   PUBDRTYP,X'40'                                                   
         BE    PUT2                                                             
         MVC   RATDRTY(2),=C'  '                                                
PUT2     FOUT  RATDRTYH                                                         
*                                                                               
*     SEE IF ANY DISCOUNT TABLE ELEMENTS EXIST IF NOT SEND BLANK                
*     DISCOUNT LEVEL FLDS         R6 POINTS TO PUBRATEL                         
         LA    R4,30               BCT                                          
         LA    R5,RATL11H                                                       
NEXTLVL  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'51'                                                      
         BNE   BLKFLDS                                                          
         USING PUBDSCEL,R6                                                      
         MVC   PACKED5(5),PUBDSCLV                                              
         EDIT  PACKED5,(9,8(R5)),COMMAS=YES                                     
         MVC   PACKED5(5),PUBDSCRT                                              
         EDIT  PACKED5,(9,25(R5)),5                                             
         FOUT  (R5)                                                             
         OI    23(R5),OI1T                                                      
         LA    R5,34(R5)                                                        
         BCT   R4,NEXTLVL                                                       
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'51'                                                      
         BE    DEAD                                                             
         BAS   R8,PUTDATES                                                      
         CLI   BACT,3              SRDS                                         
         BNE   CKSRDS                                                           
         B     PROTECT                                                          
DEAD     DC    H'0'           TOO MANY DISCOUNT TABLE ELEMENTS                  
*                                                                               
BLKFLDS  FOUT  (R5),SPACES,9                                                    
         OI    23(R5),OI1T                                                      
         MVC   25(9,R5),SPACES                                                  
         LA    R5,34(R5)                                                        
         BCT   R4,BLKFLDS                                                       
         BAS   R8,PUTDATES                                                      
         CLI   BACT,3              SRDS                                         
         BNE   CKSRDS                                                           
         B     PROTECT                                                          
*                                                                               
*                                                                               
PROTECT  OI    RATEFFDH+1,X'20'                                                 
         OI    RATBRATH+1,X'20'                                                 
         OI    RATDLTYH+1,X'20'                                                 
         OI    RATDRTYH+1,X'20'                                                 
         OI    RATAGATH+1,X'20'                                                 
PROTECTD LA    R4,30                                                            
         LA    R5,RATL11H                                                       
PROT1    OI    1(R5),X'20'                                                      
         OI    18(R5),X'20'                                                     
         LA    R5,34(R5)                                                        
         BCT   R4,PROT1                                                         
         MVI   SAVSCRN,0           SO SCREEN WILL BE RELOADED                   
         CLI   BACT,2                                                           
         BH    DONE                                                             
         BE    PROT2                                                            
         LA    R2,RATEFFDH                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
PROT2    LA    R2,RATEFFDH                                                      
         B     EXIT                                                             
CKSRDS   CLC   AGYALPHA(2),PUBKAGY                                              
         BNE   PROTECT                                                          
         CLI   BACT,2                                                           
         BE    PROT2                                                            
         LA    R2,RATEFFDH                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
*    ROUTINES                                                                   
*                                                                               
NXTEL    EQU   *              BUMP TO NEXT ELEMENT AND TEST ELCOD               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    *+12                                                             
         CLC   0(1,R6),ELCOD       SETS CONDITION CODE                          
         BR    RE                                                               
         LTR   R6,R6                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
PUTDATES XC    RATDTS,RATDTS                                                    
         LA    R6,PUBREC+33                                                     
         USING PUBRATEL,R6                                                      
         MVI   ELCOD,X'50'                                                      
         LA    R4,RATDTSH+8                                                     
         LA    R5,6                                                             
DATES1   BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    DATES2                                                           
         CLI   0(R6),X'51'                                                      
         BE    DATES1                                                           
         B     DATES3                                                           
DATES2   EQU   *                                                                
         CLI   PUBRSPCE,C' '       BYPASS SAU RATES                             
         BH    DATES1                                                           
*        GOTO1 VDTCNV,DMCB,(1,PUBRSTRT),(3,0(R4))                               
         GOTO1 VDATCON,DMCB,(3,PUBRSTRT),(5,0(R4))                              
         CLI   BACT,1                                                           
         BE    DATES2C                                                          
         CLC   RATEFFD(8),0(R4)    CHECKSAME DATE                               
         BNE   *+14                                                             
         XC    0(8,R4),0(R4)                                                    
         B     DATES1                                                           
*                                                                               
DATES2C  MVI   8(R4),C','                                                       
         LA    R4,9(R4)                                                         
         BCT   R5,DATES1                                                        
*                                                                               
DATES3   FOUT  RATDTSH                                                          
         CLI   RATDTS,0                                                         
         BNE   *+12                                                             
         MVC   RATDTS(4),=C'NONE'                                               
         BR    R8                                                               
         BCTR  R4,R0                                                            
         MVI   0(R4),0                                                          
         BR    R8                                                               
*                                                                               
         EJECT                                                                  
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
SPACES   DC    40C' '                                                           
PUBIND   DS    CL1                                                              
ACTSW    DS    CL1                                                              
PACKED5  DS    PL5                                                              
ELCOD    DS    CL1                                                              
NORATE   EQU   129                                                              
*                                                                               
VIRGERR  DC    H'0'                                                             
COMBERR  EQU   112                                                              
DATERR   EQU   20                                                               
DUPLEV   EQU   170                                                              
LEVERR   EQU   171                                                              
RATERR   EQU   172                                                              
ADDERR   EQU   173                                                              
INCHERR  EQU   2            INVALID INPUT                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PUGENEROL                                                      
*                                                                               
         LTORG                                                                  
*                                                                               
ELEAREA  DS    500C                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PUGENOLD                                                       
PUBIO    DS    4000C                                                            
         ORG   PUBIO                                                            
       ++INCLUDE PUBREC                                                         
         EJECT                                                                  
*                                                                               
RATEEL   DSECT                                                                  
       ++INCLUDE PUBRATEL                                                       
         EJECT                                                                  
*                                                                               
DSCTEL   DSECT                                                                  
       ++INCLUDE PUBDSCEL                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPUBFFD                                                       
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBF7D                                                       
         ORG   T406FFD                                                          
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BSCR     DS    CL1                                                              
OLNUM    DS    CL1                                                              
PUBADDR  DS    F                                                                
LTLADDR  DS    F                                                                
BPUB     DS    CL6                                                              
BCLT     DS    CL3                                                              
BDIV     DS    CL3                                                              
BDATE    DS    CL3                                                              
APROF    DS    CL1                                                              
SAVSCRN  DS    CL1                                                              
APROF13  DS    CL1                                                              
BCODE    DS    CL3                                                              
         ORG   BCODE                                                            
BSPACE   DS    0CL17                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043PPPUB07   05/01/02'                                      
         END                                                                    
