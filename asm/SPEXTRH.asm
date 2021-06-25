*          DATA SET SPEXTRH    AT LEVEL 020 AS OF 05/03/88                      
*PHASE SPEXTRH,*                                                                
*INCLUDE CLUNPK                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
*        THIS FIX FOR IS FOR SPOT 3 TO SPLIT AGENCY RH MCCAFFREY                
*        ALL OFFICE R REMAINS, ALL OFFICE N GETS TRANSFERRED TO MY              
*        SPOT 3 NUMBER 1                                                        
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT,R7                                                    
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
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
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         AP    TOTRD,=P'1'        TOTAL READ                                    
         SPACE                                                                  
         L     R3,AREC             POINT TO RECORD                              
         CLI   0(R3),0             TEST POSSIBLE CLIENT REC                     
         BNE   DMXREC20             NO - CK                                     
         SPACE                                                                  
         OC    4(9,R3),4(R3)       TEST CLIENT                                  
         BNZ   DMXREC20             NO - CK                                     
         SPACE                                                                  
         AP    TCLTCT,=P'1'                                                     
         MVI   WORK,0                                                           
         MVZ   WORK(1),1(R3)                                                    
         CLI   WORK,X'20'                                                       
         BNE   DMXPURGE                                                         
         SPACE                                                                  
         USING CLTHDRD,R3                                                       
         CLI   COFFICE,C'R'        OFFICE R STAYS                               
         BE    DMXPURGE                                                         
         CLI   COFFICE,C'N'        OFFICE N CHANGES                             
         BNE   PRTOFF               WHAT??????????                              
         LA    R4,100                                                           
         LA    R5,CLTABLE                                                       
DMXREC10 OC    0(6,R5),0(R5)       EMPTY SLOT                                   
         BZ    DMXREC14                                                         
         CLC   3(3,R5),2(R3)                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R5,6(,R5)                                                        
         BCT   R4,DMXREC10                                                      
         DC    H'0'                                                             
DMXREC14 GOTO1 =V(CLUNPK),DMCB,2(R3),0(R5)                                      
         MVC   3(3,R5),1(R3)                                                    
         NI    1(R3),X'0F'                                                      
         OI    1(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         AP    CLTMVDCT,=P'1'                                                   
         AP    TMVDCT,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CLT MOVE'                                               
         B     PRT                                                              
PRTOFF   DS    0H                                                               
         MVC   P+2(5),=C'MEDIA'                                                 
         MVI   WORK,0                                                           
         MVN   WORK(1),1(R3)                                                    
         MVI   P+8,C'T'                                                         
         CLI   WORK,1                                                           
         BE    PRTOFF10                                                         
         MVI   P+8,C'R'                                                         
         CLI   WORK,2                                                           
         BE    PRTOFF10                                                         
         MVI   P+8,C'N'                                                         
         CLI   WORK,3                                                           
         BE    PRTOFF10                                                         
         MVI   P+8,C'?'                                                         
PRTOFF10 MVC   P+12(6),=C'CLIENT'                                               
         GOTO1 =V(CLUNPK),DMCB,2(R3),P+19                                       
         MVC   P+22(12),=C'IS IN OFFICE'                                        
         MVC   P+35(1),COFFICE                                                  
         GOTO1 VPRINTER                                                         
         B     DMXPURGE                                                         
         DROP  R3                                                               
         SPACE                                                                  
DMXREC20 CLI   0(R3),2             CLT,PRD,EST,BILL OR GOAL REC?                
         BL    STCLT                                                            
         BE    STGOL                                                            
         SPACE                                                                  
         CLI   0(R3),05            ADVHDR                                       
         BE    ADVHDR               YES.                                        
         SPACE                                                                  
         CLI   0(R3),06            AGYHDR                                       
         BE    AGYHDR               YES.                                        
         SPACE                                                                  
         CLI   0(R3),08            DAYPT HDR                                    
         BE    DPTHDR               YES.                                        
         SPACE                                                                  
         CLI   0(R3),09            EQU HDR                                      
         BE    EQUHDR               YES.                                        
         SPACE                                                                  
         CLI   0(R3),11            INVOICE                                      
         BE    INVOICE              YES.                                        
         SPACE                                                                  
         CLC   =X'0D01',0(R3)      PRDGRP                                       
         BE    PRDGRP               YES.                                        
         SPACE                                                                  
         CLC   =X'0D02',0(R3)      MKTGRP                                       
         BE    MKTGRP               YES.                                        
         SPACE                                                                  
         CLC   =X'0D03',0(R3)      MKTASS                                       
         BE    MKTASS               YES.                                        
         SPACE                                                                  
         CLC   =X'0D0D',0(R3)      FLT                                          
         BE    FLT                  YES.                                        
         SPACE                                                                  
         CLC   =X'0D13',0(R3)      SPILL                                        
         BE    SPILL                YES.                                        
         SPACE                                                                  
         CLC   =X'0D26',0(R3)      DEMO MENU                                    
         BE    DEMENU               YES.                                        
         SPACE                                                                  
         CLC   =X'0D28',0(R3)      DEMO DEF                                     
         BE    DEMDEF               YES.                                        
         SPACE                                                                  
         CLC   =X'0D47',0(R3)      COMP STA LIST                                
         BE    STALST               YES.                                        
         SPACE                                                                  
         CLC   =X'0E01',0(R3)      IS THIS A STATION BILL?                      
         BE    STSTAB               YES.                                        
         SPACE                                                                  
* BUY RECORD *                                                                  
         SPACE                                                                  
         AP    TBUYCT,=P'1'                                                     
         MVI   WORK,0                                                           
         MVZ   WORK(1),0(R3)                                                    
         CLI   WORK,X'20'          IS THIS AGENCY MEDIA RH                      
         BNE   DMXPURGE                                                         
         LA    R4,CLTABLE                                                       
         USING CLTABLED,R4                                                      
STBUY10  CLC   CLTMD(3),0(R3)      IN TABLE?                                    
         BE    STBUY14                                                          
         LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   STBUY10                                                          
         B     DMXPURGE                                                         
         SPACE                                                                  
STBUY14  DS    0H                                                               
         AP    BUYCTMVD,=P'1'                                                   
         AP    TMVDCT,=P'1'                                                     
         NI    0(R3),X'0F'                                                      
         OI    0(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'BUY MOVE'                                               
         B     PRT                                                              
         EJECT                                                                  
* ADVHDR *                                                                      
         SPACE                                                                  
ADVHDR   DS    0H                                                               
         AP    TADVCT,=P'1'                                                     
         LA    R4,CLTABLE                                                       
         USING CLTABLED,R4                                                      
ADVHDR10 CLC   CLTCLTP,1(R3)      IN TABLE?                                     
         BE    ADVHDR20                                                         
         LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   STBUY10                                                          
         B     DMXPURGE                                                         
ADVHDR20 MVC   20(2,R3),=C'MY'                                                  
         AP    TADVMVD,=P'1'                                                    
         AP    TMVDCT,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'ADVHDR MOVE'                                            
         B     PRT                                                              
         SPACE                                                                  
* AGYHDR *                                                                      
         SPACE                                                                  
AGYHDR   DS    0H                                                               
         AP    TAGYCT,=P'1'                                                     
         CLC   =C'RH',1(R3)                                                     
         BNE   DMXPURGE                                                         
         AP    TAGYMVD,=P'1'                                                    
         AP    TMVDCT,=P'1'                                                     
         MVC   1(2,R3),=C'MY'                                                   
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'AGYHDR MOVE'                                            
         B     PRT                                                              
         SPACE                                                                  
* DPTHDR *                                                                      
         SPACE                                                                  
DPTHDR   DS    0H                                                               
         AP    TDPTCT,=P'1'                                                     
         CLC   =C'RH',1(R3)                                                     
         BNE   DMXPURGE                                                         
         AP    TDPTMVD,=P'1'                                                    
         AP    TMVDCT,=P'1'                                                     
         MVC   1(2,R3),=C'MY'                                                   
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'DPTHDR MOVE'                                            
         B     PRT                                                              
         SPACE                                                                  
* EQUHDR *                                                                      
         SPACE                                                                  
EQUHDR   DS    0H                                                               
         AP    TEQUCT,=P'1'                                                     
         CLC   =C'RH',1(R3)                                                     
         BNE   DMXPURGE                                                         
         OC    4(3,R3),4(R3)       CLT SPEC                                     
         BZ    EQUHDR20                                                         
         LA    R4,CLTABLE                                                       
EQUHDR10 CLC   CLTCLT,4(R3)        CLT TO MOVE                                  
         BE    EQUHDR20             YES                                         
         LA    R4,L'CLTENT(R4)                                                  
         CLI   0(R4),0                                                          
         BNE   EQUHDR10                                                         
         B     DMXPURGE                                                         
EQUHDR20 AP    TEQUMVD,=P'1'                                                    
         AP    TMVDCT,=P'1'                                                     
         MVC   1(2,R3),=C'MY'                                                   
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'EQUHDR MOVE'                                            
         B     PRT                                                              
         SPACE                                                                  
* INVOICE *                                                                     
         SPACE                                                                  
INVOICE  DS    0H                                                               
         AP    TINVCT,=P'1'                                                     
         MVI   WORK,0                                                           
         MVZ   WORK(1),1(R3)                                                    
         CLI   WORK,X'20'          IS THIS AGENCY MEDIA RH                      
         BNE   DMXPURGE                                                         
         LA    R4,CLTABLE                                                       
         USING CLTABLED,R4                                                      
INV10    CLC   CLTMD(1),1(R3)      IN TABLE?                                    
         BNE   INV14                                                            
         CLC   CLTCLT,5(R3)        IN TABLE?                                    
         BE    INV20                                                            
INV14    LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   INV10                                                            
         B     DMXPURGE                                                         
         SPACE                                                                  
INV20    DS    0H                                                               
         AP    TINVMVDCT,=P'1'                                                  
         AP    TMVDCT,=P'1'                                                     
         NI    1(R3),X'0F'                                                      
         OI    1(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'INVOICE MOVE'                                           
         B     PRT                                                              
         SPACE                                                                  
* PRDGRP *                                                                      
         SPACE                                                                  
PRDGRP   DS    0H                                                               
         AP    TPRGCT,=P'1'                                                     
         MVI   WORK,0                                                           
         MVZ   WORK(1),2(R3)                                                    
         CLI   WORK,X'20'          IS THIS AGENCY MEDIA RH                      
         BNE   DMXPURGE                                                         
         OC    3(2,R3),3(R3)       IF NO CLT, COPY                              
         BZ    PRDG20                                                           
         LA    R4,CLTABLE                                                       
         USING CLTABLED,R4                                                      
PRDG10   CLC   CLTMD(3),2(R3)      IN TABLE?                                    
         BE    PRDG20                                                           
         LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   PRDG10                                                           
         B     DMXPURGE                                                         
         SPACE                                                                  
PRDG20   DS    0H                                                               
         AP    TPRGMVDCT,=P'1'                                                  
         AP    TMVDCT,=P'1'                                                     
         NI    2(R3),X'0F'                                                      
         OI    2(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'PRDGRP MOVE'                                            
         B     PRT                                                              
         SPACE                                                                  
* MKTGRP *                                                                      
         SPACE                                                                  
MKTGRP   DS    0H                                                               
         AP    TMKGCT,=P'1'                                                     
         MVI   WORK,0                                                           
         MVZ   WORK(1),2(R3)                                                    
         CLI   WORK,X'20'          IS THIS AGENCY MEDIA RH                      
         BNE   DMXPURGE                                                         
         OC    3(2,R3),3(R3)       IF NO CLT, COPY                              
         BZ    MKTG20                                                           
         LA    R4,CLTABLE                                                       
         USING CLTABLED,R4                                                      
MKTG10   CLC   CLTMD(3),2(R3)      IN TABLE?                                    
         BE    MKTG20                                                           
         LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   MKTG10                                                           
         B     DMXPURGE                                                         
         SPACE                                                                  
MKTG20   DS    0H                                                               
         AP    TMKGMVDCT,=P'1'                                                  
         AP    TMVDCT,=P'1'                                                     
         NI    2(R3),X'0F'                                                      
         OI    2(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'MKTGRP MOVE'                                            
         B     PRT                                                              
         SPACE                                                                  
* MKTASS *                                                                      
         SPACE                                                                  
MKTASS   DS    0H                                                               
         AP    TMKACT,=P'1'                                                     
         MVI   WORK,0                                                           
         MVZ   WORK(1),8(R3)                                                    
         CLI   WORK,X'20'          IS THIS AGENCY MEDIA RH                      
         BNE   DMXPURGE                                                         
         OC    9(2,R3),9(R3)       IF NO CLT, COPY                              
         BZ    MKTA20                                                           
         LA    R4,CLTABLE                                                       
         USING CLTABLED,R4                                                      
MKTA10   CLC   CLTMD(3),8(R3)      IN TABLE?                                    
         BE    MKTA20                                                           
         LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   MKTA10                                                           
         B     DMXPURGE                                                         
         SPACE                                                                  
MKTA20   DS    0H                                                               
         AP    TMKAMVDCT,=P'1'                                                  
         AP    TMVDCT,=P'1'                                                     
         NI    8(R3),X'0F'                                                      
         OI    8(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'MKTASS MOVE'                                            
         B     PRT                                                              
         SPACE                                                                  
* FLIGHT *                                                                      
         SPACE                                                                  
FLT      DS    0H                                                               
         AP    TFLTCT,=P'1'                                                     
         MVI   WORK,0                                                           
         MVZ   WORK(1),2(R3)                                                    
         CLI   WORK,X'20'          IS THIS AGENCY MEDIA RH                      
         BNE   DMXPURGE                                                         
         OC    2(2,R3),2(R3)       IF NO CLT, COPY                              
         BZ    FLT20                                                            
         LA    R4,CLTABLE                                                       
         USING CLTABLED,R4                                                      
FLT10    CLC   CLTMD(3),2(R3)      IN TABLE?                                    
         BE    FLT20                                                            
         LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   FLT10                                                            
         B     DMXPURGE                                                         
         SPACE                                                                  
FLT20    DS    0H                                                               
         AP    TFLTMVDCT,=P'1'                                                  
         AP    TMVDCT,=P'1'                                                     
         NI    2(R3),X'0F'                                                      
         OI    2(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'FLIGHT MOVE'                                            
         B     PRT                                                              
         SPACE                                                                  
* SPILL *                                                                       
         SPACE                                                                  
SPILL    DS    0H                                                               
         AP    TSPLCT,=P'1'                                                     
         CLC   =C'RH',2(R3)        THIS AGY?                                    
         BNE   DMXPURGE                                                         
         OC    10(2,R3),10(R3)     IF NO CLT, COPY                              
         BZ    SPL20                                                            
         LA    R4,CLTABLE                                                       
         USING CLTABLED,R4                                                      
SPL10    CLC   CLTCLTP,10(R3)      CLT IN TABLE?                                
         BE    SPL20                                                            
         LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   SPL10                                                            
         B     DMXPURGE                                                         
         SPACE                                                                  
SPL20    DS    0H                                                               
         AP    TSPLMVDCT,=P'1'                                                  
         AP    TMVDCT,=P'1'                                                     
         MVC   2(2,R3),=C'MY'                                                   
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'SPILL MOVE'                                             
         B     PRT                                                              
         SPACE                                                                  
* DEMO MENU *                                                                   
         SPACE                                                                  
DEMENU   DS    0H                                                               
         AP    TDEMCT,=P'1'                                                     
         MVI   WORK,0                                                           
         MVZ   WORK(1),2(R3)                                                    
         CLI   WORK,X'20'          IS THIS AGENCY MEDIA RH                      
         BNE   DMXPURGE                                                         
         AP    TDEMMVDCT,=P'1'                                                  
         AP    TMVDCT,=P'1'                                                     
         NI    2(R3),X'0F'                                                      
         OI    2(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'DEMO MENU MOVE'                                         
         B     PRT                                                              
         SPACE                                                                  
* DEMO DEF *                                                                    
         SPACE                                                                  
DEMDEF   DS    0H                                                               
         AP    TDEFCT,=P'1'                                                     
         OC    10(2,R3),10(R3)     IF NO CLT, COPY                              
         BZ    SPL20                                                            
         LA    R4,CLTABLE                                                       
         USING CLTABLED,R4                                                      
DEMD10   CLC   CLTMD(3),2(R3)      CLT IN TABLE?                                
         BE    DEMD20                                                           
         LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   DEMD10                                                           
         B     DMXPURGE                                                         
         SPACE                                                                  
DEMD20   DS    0H                                                               
         AP    TDEFMVDCT,=P'1'                                                  
         AP    TMVDCT,=P'1'                                                     
         NI    2(R3),X'0F'                                                      
         OI    2(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'DEMO DEF MOVE'                                          
         B     PRT                                                              
         SPACE                                                                  
* COMP STA LIST *                                                               
         SPACE                                                                  
STALST   DS    0H                                                               
         AP    TSTLCT,=P'1'                                                     
         MVI   WORK,0                                                           
         MVZ   WORK(1),2(R3)                                                    
         CLI   WORK,X'20'          IS THIS AGENCY MEDIA RH                      
         BNE   DMXPURGE                                                         
         AP    TSTLMVDCT,=P'1'                                                  
         AP    TMVDCT,=P'1'                                                     
         NI    2(R3),X'0F'                                                      
         OI    2(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'COMP STA LIST MOVE'                                     
         B     PRT                                                              
         SPACE                                                                  
* STATION BILLING RECORD *                                                      
         SPACE                                                                  
STSTAB   DS    0H                                                               
         AP    TBILLCT,=P'1'                                                    
         LA    R4,CLTABLE                                                       
STSTA10  CLC   CLTMD(3),2(R3)                                                   
         BE    STSTA20                                                          
         LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   STSTA10                                                          
         B     DMXPURGE                                                         
         SPACE                                                                  
STSTA20  DS    0H                                                               
         AP    BILLCTMVD,=P'1'                                                  
         AP    TMVDCT,=P'1'                                                     
         NI    2(R3),X'0F'                                                      
         OI    2(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'BILL LOAD'                                              
         B     PRT                                                              
         SPACE                                                                  
         EJECT                                                                  
* GOAL RECORDS *                                                                
         SPACE                                                                  
STGOL    AP    TGOLCT,=P'1'                                                     
         B     STCLT02                                                          
         SPACE                                                                  
* CLIENT, PRODUCT, ESTIMATE, OR BILL RECORDS *                                  
         SPACE                                                                  
STCLT    DS    0H                                                               
         OC    4(9,R3),4(R3)       CLIENT REC                                   
         BNZ   *+14                                                             
         AP    TCLTCT,=P'1'                                                     
         B     STCLT02                                                          
         OC    7(6,R3),7(R3)       PROD HDR                                     
         BNZ   *+14                                                             
         AP    TPHDCT,=P'1'                                                     
         B     STCLT02                                                          
         OC    8(5,R3),8(R3)       EST HDR                                      
         BNZ   *+14                                                             
         AP    TESTCT,=P'1'                                                     
         B     STCLT02                                                          
         AP    TBILCT,=P'1'                                                     
STCLT02  LA    R4,CLTABLE                                                       
         SPACE                                                                  
STCLT20  CLC   CLTMD(3),1(R3)     IS THIS A SPLIT A/M CLT                       
         BE    STCLT30                                                          
         LA    R4,L'CLTENT(,R4)                                                 
         CLI   0(R4),255                                                        
         BNE   STCLT20                                                          
         B     DMXPURGE                                                         
         SPACE                                                                  
STCLT30  CLI   0(R3),2             GOAL                                         
         BE    STCLTGOL                                                         
         OC    4(9,R3),4(R3)       CLIENT REC                                   
         BZ    STCLTCLT                                                         
         OC    7(6,R3),7(R3)       PROD HDR                                     
         BZ    STCLTPHD                                                         
         OC    8(5,R3),8(R3)       EST HDR                                      
         BZ    STCLTEHD                                                         
         SPACE                                                                  
         LA    R4,=CL20'STA BILL MOVE'                                          
         AP    BILCTMVD,=P'1'                                                   
         AP    TMVDCT,=P'1'                                                     
         NI    1(R3),X'0F'                                                      
         OI    1(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         B     PRT                                                              
STCLTCLT AP    CLTMVDCT,=P'1'                                                   
         AP    TMVDCT,=P'1'                                                     
         NI    1(R3),X'0F'                                                      
         OI    1(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CLIENT MOVE'                                            
         B     PRT                                                              
STCLTPHD AP    PHDMVDCT,=P'1'                                                   
         AP    TMVDCT,=P'1'                                                     
         NI    1(R3),X'0F'                                                      
         OI    1(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'PROD HDR MOVE'                                          
         B     PRT                                                              
         SPACE                                                                  
STCLTEHD DS    0H                                                               
         AP    ESTMVDCT,=P'1'                                                   
         AP    TMVDCT,=P'1'                                                     
         NI    1(R3),X'0F'                                                      
         OI    1(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'EST HDR MOVE'                                           
         B     PRT                                                              
         SPACE                                                                  
STCLTGOL DS    0H                                                               
         AP    GOLCTMVD,=P'1'                                                   
         AP    TMVDCT,=P'1'                                                     
         NI    1(R3),X'0F'                                                      
         OI    1(R3),X'10'                                                      
         MVC   20(2,R3),=C'MY'                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'GOAL MOVE'                                              
         B     PRT                                                              
         SPACE                                                                  
PRT      DS    0H                                                               
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(100),RTITLE                                                    
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         MVC   P(32),=CL32'MEDIA CLT TRANS WITH OFFICE N'                       
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         LA    R2,CLTABLE                                                       
         LA    R3,100                                                           
DMXEOF10 MVI   WORK,0                                                           
         MVN   WORK(1),3(R2)                                                    
         MVI   P+2,C'T'                                                         
         CLI   WORK,1                                                           
         BE    DMXEOF14                                                         
         MVI   P+2,C'R'                                                         
         CLI   WORK,2                                                           
         BE    DMXEOF14                                                         
         MVI   P+2,C'N'                                                         
         CLI   WORK,3                                                           
         BE    DMXEOF14                                                         
         MVI   P+2,C'?'                                                         
DMXEOF14 MVC   P+6(3),0(R2)                                                     
         GOTO1 VPRINTER                                                         
         LA    R2,6(,R2)                                                        
         OC    0(6,R2),0(R2)                                                    
         BZ    *+8                                                              
         BCT   R3,DMXEOF10                                                      
         SPACE                                                                  
         LA    R2,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF20 MVC   P(28),4(R3)                                                      
         EDIT  (P4,(R3)),(8,P+30)                                               
         GOTO1 VPRINTER                                                         
         LA    R3,32(,R3)                                                       
         BCT   R2,DMXEOF20                                                      
         B     DMXIT                                                            
         EJECT                                                                  
         DS    0D                                                               
TOTRD    DC    PL4'0',CL28'TOTAL RECS READ'                                     
TCLTCT   DC    PL4'0',CL28'TOT CLTS READ'                                       
CLTMVDCT DC    PL4'0',CL28'CLT RECS MOVED'                                      
TPHDCT   DC    PL4'0',CL28'TOT PROD HDRS READ'                                  
PHDMVDCT DC    PL4'0',CL28'PRD HDR RECS MOVED'                                  
TESTCT   DC    PL4'0',CL28'TOT ESTS READ'                                       
ESTMVDCT DC    PL4'0',CL28'EST RECS MOVED'                                      
TADVCT   DC    PL4'0',CL28'TOT ADVHDRS READ'                                    
TADVMVD  DC    PL4'0',CL28'ADV HDR RECS MOVED'                                  
TAGYCT   DC    PL4'0',CL28'TOT AGYHDRS READ'                                    
TAGYMVD  DC    PL4'0',CL28'TOT AGYHDRS MOVED'                                   
TDPTCT   DC    PL4'0',CL28'TOT DPTHDRS READ'                                    
TDPTMVD  DC    PL4'0',CL28'AGY DPTHDRS MOVED'                                   
TEQUCT   DC    PL4'0',CL28'TOT EQUHDRS READ'                                    
TEQUMVD  DC    PL4'0',CL28'TOT EQUHDRS MOVED'                                   
TINVCT   DC    PL4'0',CL28'TOT INVOICES READ'                                   
TINVMVDCT DC    PL4'0',CL28'TOT INVOICES MOVED'                                 
TPRGCT   DC    PL4'0',CL28'TOT PRDGRPS READ'                                    
TPRGMVDCT DC    PL4'0',CL28'TOT PRDGRPS MOVED'                                  
TMKGCT   DC    PL4'0',CL28'TOT MKTGRPS READ'                                    
TMKGMVDCT DC    PL4'0',CL28'TOT MKTGRPS MOVED'                                  
TMKACT   DC    PL4'0',CL28'TOT MKT ASGN READ'                                   
TMKAMVDCT DC    PL4'0',CL28'TOT MKT ASGN MOVED'                                 
TFLTCT   DC    PL4'0',CL28'TOT FLIGHTS READ'                                    
TFLTMVDCT DC    PL4'0',CL28'BUY FLIGHTS MOVED'                                  
TSPLCT   DC    PL4'0',CL28'TOT SPILLS READ'                                     
TSPLMVDCT DC    PL4'0',CL28'TOT SPILLS MOVED'                                   
TDEMCT   DC    PL4'0',CL28'TOT DEM MENUS READ'                                  
TDEMMVDCT DC    PL4'0',CL28'TOT DEM MENUS MOVED'                                
TDEFCT   DC    PL4'0',CL28'TOT USER DEMO DEFS READ'                             
TDEFMVDCT DC    PL4'0',CL28'TOT USER DEM DEFS MOVED'                            
TSTLCT   DC    PL4'0',CL28'TOT COMP STA LISTS READ'                             
TSTLMVDCT DC    PL4'0',CL28'TOT COMP STA LISTS MOVED'                           
TBUYCT   DC    PL4'0',CL28'TOT BUYS READ'                                       
BUYCTMVD DC    PL4'0',CL28'BUY RECS MOVED'                                      
TBILLCT  DC    PL4'0',CL28'TOT STA BILLS READ'                                  
BILLCTMVD DC   PL4'0',CL28'STA BILL RECS MOVED'                                 
TGOLCT   DC    PL4'0',CL28'TOT GOAL READ'                                       
GOLCTMVD DC    PL4'0',CL28'GOAL RECS MOVED'                                     
TBILCT   DC    PL4'0',CL28'TOT BILLS READ'                                      
BILCTMVD DC    PL4'0',CL28'BILL RECS MOVED'                                     
TMVDCT   DC    PL4'0',CL28'TOTAL RECS MOVED'                                    
TOTCTRS  EQU   (*-TOTRD)/32                                                     
WORK     DS    CL64                                                             
         LTORG                                                                  
RTITLE   DC    CL100'SPLIT AGENCY RH TO AGENCY MY'                              
CLTABLE  DC    100XL6'00' 1ST 3 CLT, NEXT MEDIA, NEXT 2 PACKED CLT              
         DC    X'FF'                                                            
         SPACE 5                                                                
CLTABLED DSECT                                                                  
CLTENT   DS    0CL6                                                             
CLTCLT   DS    CL3                                                              
CLTMD    DS    XL1                                                              
CLTCLTP  DS    XL2                                                              
         SPACE 5                                                                
WORKD    DSECT                                                                  
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
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPEXTRH   05/03/88'                                      
         END                                                                    
