*          DATA SET BUFIL46    AT LEVEL 064 AS OF 05/01/02                      
*PHASE T50246A                                                                  
*INCLUDE ACJOBCOL                                                               
         TITLE 'T50246 - BUDGET EXTRACT - ACCPAK'                               
T50246   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         NMOD1 (ACWKX-ACWKD),T50246,RA,RR=R8,CLEAR=YES                          
         USING T50246+4096,RA                                                   
         ST    R8,RELO                                                          
         SPACE 1                                                                
         LR    R7,RC                                                            
         USING ACWKD,R7                                                         
         SPACE 1                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         SPACE 1                                                                
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 1                                                                
         L     R0,VADUMMY                                                       
         ST    R0,NEXTADDR         SET NEXT AVAILABLE CORE ADDRESS              
         SPACE 1                                                                
         L     R3,ATWA                                                          
         USING T502FFD,R3                                                       
         SPACE 1                                                                
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 TWAVBUFF,DMCB,=C'SET',BUFFBUFF                                   
         SPACE 1                                                                
         L     R2,ARULDATA                                                      
         USING QRD,R2                                                           
         CLC   QRAGYC,=C'DF'       TEST FOR AGENCY=DF                           
         BE    BADAGY              YES-STOP ACCESS NOW                          
*                                                                               
         BAS   RE,TSTRUL                                                        
         ICM   R2,15,QRNEXT                                                     
         BNZ   *-8                                                              
*                                                                               
         MVC   VJOBCOL,=V(ACJOBCOL)                                             
         MVC   ACOLIST,=A(COLIST)                                               
         MVC   ACOLTAB,=A(COLTAB)                                               
         MVC   LCOLTAB,=A(COLTABL)                                              
         MVC   AOPVTAB,=A(OPVTAB)                                               
         MVC   LOPVTAB,=A(OPVTABL)                                              
         MVC   AGOBUFF,=A(GOBUFF)                                               
         MVC   LGOBUFF,=A(GOBUFFL)                                              
         MVC   AJOBIO,=A(JOBIO)                                                 
         MVC   AACIOS,=A(ACIOS)                                                 
         LA    R1,GOBLOCK                                                       
         ST    R1,AGOBLOCK                                                      
*                                                                               
         L     RE,AACIOS           RE=A(RECORD SAVE AREA)                       
         LA    RF,ADLIST           RF=A(ADCON LIST)                             
         LA    R0,ADCONS           R0=N'ADCONS                                  
         ST    RE,0(RF)                                                         
         LA    RE,1000(RE)         NEXT SAVE AREA                               
         LA    RF,4(RF)            NEXT ADCON                                   
         BCT   R0,*-12                                                          
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT PHASES                  
         LA    R0,CORES                                                         
         LA    R4,CORETAB                                                       
         LA    R5,COREMODS                                                      
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
GETCORE  MVC   DMCB+7(1),0(R5)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
         LA    R5,1(R5)            GET NEXT MODULE NUMBER                       
         LA    R4,4(R4)            GET NEXT ADDRESS                             
         BCT   R0,GETCORE          GET THE NEXT ONE                             
*                                                                               
         GOTO1 VJOBCOL,DMCB,COLFLDH,ACOLIST,ACOMFACS                            
*                                                                               
         BAS   RE,BLDMOS           BUILD MOS DATES                              
         LA    R1,SVDTYPES                                                      
         USING SVDTD,R1                                                         
         LA    R0,MAXDTYP                                                       
         SR    R4,R4               R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS           R5=A(EXTRACT TYPES TABLES)                   
*                                                                               
ACEX03   CLI   SVDTEX,0            TEST FOR EOT                                 
         BE    ACEX06              YES                                          
         LA    RE,EXTTBL           RE=A(VALID DATA TYPE TABLE)                  
         LA    RF,EXTYPES          RF=N'VALID DATA TYPES                        
*                                                                               
ACEX04   CLC   SVDTEX,0(RE)        TEST IF VALID FOR PROD EXTRACT               
         BE    *+16                YES                                          
         LA    RE,L'EXTTBL(RE)                                                  
         BCT   RF,ACEX04                                                        
         B     ACEX05                                                           
*                                                                               
         OC    DATAINDS,1(RE)      UPDATE CUMULATIVE MASK                       
         MVC   0(1,R5),0(RE)                                                    
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
*                                                                               
ACEX05   LA    R1,SVDTL(R1)        NEXT DATA TYPE                               
         BCT   R0,ACEX03                                                        
         DROP  R1                                                               
         EJECT                                                                  
ACEX06   LTR   R4,R4                                                            
         BZ    EXIT                NO DATA TYPES                                
         STC   R4,NEXTYPS                                                       
         SPACE 1                                                                
         L     R2,ARULDATA         GET ADDRESS OF FIRST RULE                    
         MVC   RCCOMPFL,QRAGY       SAVE AGY                                    
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
         LA    R4,KEY                                                           
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         MVC   KEY,SPACES            GET AGENCY RECORD                          
         MVC   ACKEYACC(1),RCCOMPFL                                             
         BAS   RE,ACHIGH                                                        
         CLC   KEY(42),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,ADCOMP                                                        
         BAS   RE,SAVREC           SAVE THE COMPANY RECORD                      
         SPACE 1                                                                
         MVI   ACKEYACC+1,C'S'                                                  
         BAS   RE,ACHIGH                                                        
         CLC   KEY(42),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BAD UNIT RECORD                              
         LA    R5,ADUNIT                                                        
         BAS   RE,SAVREC           SAVE THE UNIT RECORD                         
         SPACE 1                                                                
         MVI   ACKEYACC+2,C'J'                                                  
         BAS   RE,ACHIGH                                                        
         CLC   KEY(42),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BAD LEDGER RECORD                            
         LA    R5,ADLEDGER                                                      
         BAS   RE,SAVREC           SAVE THE LEDGER RECORD                       
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'16',ADLEDGER),0                                     
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO HIERARCHY ELEMENT                         
         L     R6,ELADDR                                                        
         ST    R6,ADLDGHIR                                                      
         SPACE 1                                                                
         USING ACHEIRD,R6                                                       
         LA    R3,ACKEYACC+3         BUILD KEY FOR CLIENT RECORD                
         ZIC   R1,ACHRLEVA                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),QRCLT                                                    
         SPACE 1                                                                
         BAS   RE,ACHIGH                                                        
         CLC   KEY(42),KEYSAVE                                                  
         BNE   BADCLI              BAD CLIENT RECORD                            
         LA    R5,ADHEIRA                                                       
         BAS   RE,SAVREC           SAVE THE CLIENT RECORD                       
*                                                                               
         MVC   CLTFIL,SPACES                                                    
         GOTO1 GETL,DMCB,(X'30',ADHEIRA),0                                      
         CLI   ELERR,0                                                          
         BNE   ACEX07                                                           
         L     RF,ELADDR                                                        
         USING ACSTATD,RF                                                       
         MVC   CLTFIL(2),ACSTFILT  EXTRACT CLIENT'S ACCOUNT FILTERS             
         MVC   CLTFIL+2(1),ACSTANAL  F3                                         
         MVC   CLTFIL+3(1),ACSTSUB   F4                                         
         CLI   ACSTLEN,ACSTLNQ2                                                 
         BL    *+10                                                             
         MVC   CLTFIL+4(1),ACSTFLT5                                             
         SPACE 1                                                                
ACEX07   CLC   QRPRD,=C'ALL'       TEST FOR ALL PRODUCT EXTRACT                 
         BE    ACEX25              YES                                          
         ZIC   R3,ACHRLEVA                                                      
         ZIC   R1,ACHRLEVB                                                      
         SR    R1,R3                                                            
         LA    R3,ACKEYACC+3(R3)     BUILD KEY FOR PRODUCT RECORD               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),QRPRD                                                    
         SPACE 1                                                                
         BAS   RE,ACHIGH                                                        
         CLC   KEY(42),KEYSAVE                                                  
         BNE   BADPRD              BAD PRODUCT RECORD                           
         LA    R5,ADHEIRB                                                       
         BAS   RE,SAVREC           SAVE THE PRODUCT RECORD                      
*                                                                               
         MVC   PRDFIL,SPACES                                                    
         GOTO1 GETL,DMCB,(X'30',ADHEIRB),0                                      
         CLI   ELERR,0                                                          
         BNE   ACEX09                                                           
         L     RF,ELADDR                                                        
         USING ACSTATD,RF                                                       
         MVC   PRDFIL(2),ACSTFILT  EXTRACT PRODUCT ACCOUNT FILTERS              
         MVC   PRDFIL+2(1),ACSTANAL  F3                                         
         MVC   PRDFIL+3(1),ACSTSUB   F4                                         
         CLI   ACSTLEN,ACSTLNQ2                                                 
         BL    *+10                                                             
         MVC   PRDFIL+4(1),ACSTFLT5                                             
         SPACE 1                                                                
ACEX09   ZIC   R3,ACHRLEVB                                                      
         LA    R3,ACKEYACC+3(R3)     GET MEDIA INTO KEY                         
         MVC   0(1,R3),QRMED                                                    
         OI    0(R3),X'40'                                                      
         MVC   STKEY,ACKEYACC     KEEP MY START KEY                             
         ZIC   R1,ACHRLEVB                                                      
         LA    RF,3(R1)                                                         
         STC   RF,STJOB            START OF JOB FIELD                           
         LA    R1,2(R1)                                                         
         CLI   0(R3),X'40'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,STKEYLN          LENGTH OF START KEY FOR EX                   
         ZIC   R1,ACHRLEVC                                                      
         ZIC   RF,ACHRLEVB                                                      
         SR    R1,RF                                                            
         STC   R1,STJOBLN          SAVE LENGTH OF JOB FIELD                     
         EJECT                                                                  
         USING ACKEYD,R4                                                        
         BAS   RE,ACHIGH                                                        
         B     ACEX12                                                           
ACEX10   BAS   RE,ACSEQ                                                         
ACEX12   L     R4,AIO                                                           
         LA    R3,STKEY           TEST SAME CLI/PRD/(MED)                       
         ZIC   R1,STKEYLN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R3)                                                    
         BNE   ACEX900           IF NOT, ALL DONE, WRITE WORKER FILE            
         CLI   ACRECORD,X'44'                                                   
         BE    ACEX15                                                           
         SPACE 1                                                                
         GOTO1 GETL,DMCB,(X'32',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   ACEX10              NOT A NEW ACCOUNT                            
         LA    R5,ADACC                                                         
         BAS   RE,SAVREC                                                        
*                                                                               
         BAS   RE,SETJOB           CALL GETOPT BEFORE JOBBER CALL               
*                                                                               
         MVC   COMPFIL,CLTFIL      INITIALIZE COMPOSITE FILTER W CLIENT         
         GOTO1 COMPOSE,PRDFIL                                                   
         GOTO1 GETL,DMCB,(X'30',ADACC),0                                        
         CLI   ELERR,0                                                          
         BNE   ACEX13                                                           
         MVC   JOBFIL,SPACES                                                    
         L     RF,ELADDR                                                        
         USING ACSTATD,RF                                                       
         MVC   JOBFIL(2),ACSTFILT    F1 AND F2                                  
         MVC   JOBFIL+2(1),ACSTANAL  F3                                         
         MVC   JOBFIL+3(1),ACSTSUB   F4                                         
         CLI   ACSTLEN,ACSTLNQ2                                                 
         BL    *+10                                                             
         MVC   JOBFIL+4(1),ACSTFLT5  F5                                         
         GOTO1 COMPOSE,JOBFIL                                                   
*                                                                               
ACEX13   TM    DATAIND,ORDEREDB    DO THEY WANT ESTIMATES                       
         BZ    ACEX10                                                           
         TM    PLANIND,BUPLNACT    TEST FOR ACTUAL OPTION                       
         BO    *+8                 YES-IGNORE WORKCODE ESTIMATES                
         BAS   RE,GETEST                                                        
         B     ACEX10              READ NEXT RECORD                             
         SPACE 1                                                                
ACEX15   TM    DATAIND,ORDEREDB    TEST FOR ORDERED                             
         BZ    ACEX17                                                           
         TM    PLANIND,BUPLNACT    TEST FOR ORDERED=ACTUAL OPTION               
         BZ    ACEX17              NO                                           
         BAS   RE,ORD                                                           
         SPACE 1                                                                
ACEX17   TM    DATAIND,BILLEDB                                                  
         BNZ   *+12                                                             
         TM    DATAIND1,EXBAADVB+EXBADATB+EXBAINVB                              
         BZ    ACEX20              NO TRANSACTION DATA REQUIRED                 
         BAS   RE,PROCTRNS                                                      
         B     ACEX10                                                           
         SPACE 1                                                                
ACEX20   LA    R4,KEY                                                           
         MVI   ACKEYWRK,X'FF'                                                   
         BAS   RE,ACHIGH           SKIP TO NEXT ACCOUNT                         
         B     ACEX12                                                           
         EJECT                                                                  
*        PRODUCT=ALL PROCESSING                                                 
*                                                                               
ACEX25   MVC   STKEY,ACKEYACC      SAVE START KEY                               
         ZIC   R1,ACHRLEVA         R1=L'CLIENT FIELD                            
         LA    R1,2(R1)            ADD IN COMP/UNIT/LEDGER LESS 1               
         STC   R1,STKEYLN          R1=COMPARE LENGTH FOR EXECUTE                
*                                                                               
         ZIC   R1,ACHRLEVB         R1=L'CLIENT+PRODUCT                          
         LA    R1,3(R1)                                                         
         STC   R1,STJOB            R1=DISP TO START OF JOB FIELD                
*                                                                               
         XC    LASTPRD,LASTPRD     CLEAR PRODUCT BREAK CONTROL                  
         BCTR  R1,0                                                             
         STC   R1,STPRDLN          COMPARE LENGTH FOR CLIENT/PRODUCT            
         LA    R1,ACLENGTH-ACKEYD  R1=L'KEY                                     
         ZIC   RF,STJOB                                                         
         LA    RF,1(RF)            POINT PAST MEDIA                             
         SR    R1,RF               COMPUTE L'KEY AFTER MEDIA                    
         BCTR  R1,0                                                             
         STC   R1,LENREST          EXECUTE LENGTH OF KEY AFTER MEDIA            
*                                                                               
         ZIC   R1,ACHRLEVC                                                      
         ZIC   RF,ACHRLEVB                                                      
         SR    R1,RF                                                            
         STC   R1,STJOBLN          L'JOB FIELD                                  
*                                                                               
         USING ACKEYD,R4                                                        
ACEX27   BAS   RE,ACSEQ                                                         
*                                                                               
ACEX28   L     R4,AIO              R4=A(RECORD)                                 
         LA    R3,STKEY                                                         
         ZIC   R1,STKEYLN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R3)       TEST FOR SAME CLIENT                         
         BNE   ACEX900             NO-WRAP IT UP                                
*                                                                               
ACEX30   LA    R3,LASTPRD                                                       
         ZIC   R1,STPRDLN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R3)       TEST FOR CHANGE IN PRODUCT                   
         BE    ACEX35              NO                                           
*                                                                               
         MVC   LASTPRD,ACKEYACC    YES-SAVE NEW PRODUCT KEY                     
         LA    R5,ADHEIRB                                                       
         BAS   RE,SAVREC           SAVE NEW PRODUCT RECORD                      
*                                                                               
         MVC   PRDFIL,SPACES                                                    
         GOTO1 GETL,DMCB,(X'30',ADHEIRB),0                                      
         CLI   ELERR,0                                                          
         BNE   ACEX32                                                           
         L     RF,ELADDR                                                        
         USING ACSTATD,RF                                                       
         MVC   PRDFIL(2),ACSTFILT  EXTRACT PRODUCT ACCOUNT FILTERS              
         MVC   PRDFIL+2(1),ACSTANAL                                             
         MVC   PRDFIL+3(1),ACSTSUB                                              
         CLI   ACSTLEN,ACSTLNQ2                                                 
         BL    *+10                                                             
         MVC   PRDFIL+4(1),ACSTFLT5                                             
*                                                                               
ACEX32   CLI   QRMED,0             TEST FOR MEDIA=ALL                           
         BE    ACEX50              YES                                          
         LA    R4,KEY                                                           
         ZIC   R3,STJOB                                                         
         LA    R3,ACKEYACC(R3)     R3=A(MEDIA IN KEY)                           
         MVC   0(1,R3),QRMED       SKIP TO DESIRED MEDIA                        
         BAS   RE,ACHIGH                                                        
         B     ACEX28                                                           
*                                                                               
ACEX35   CLI   QRMED,0             TEST MEDIA=ALL                               
         BE    ACEX40              YES                                          
         LA    R4,KEY                                                           
         ZIC   R3,STJOB                                                         
         LA    R3,ACKEYACC(R3)     R3=A(START OF JOB IN KEY)                    
         CLC   0(1,R3),QRMED       APPLY MEDIA FILTER                           
         BE    ACEX40              YES                                          
         MVC   0(1,R3),QRMED       SET DESIRED MEDIA IN KEY IF LOW              
         BL    *+8                                                              
         MVI   0(R3),X'FF'         IF HIGH, FORCE TO NEW PRODUCT                
         LA    R3,1(R3)            POINT PAST MEDIA                             
         ZIC   R1,LENREST                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      SPACE PAD REST OF KEY                        
         BAS   RE,ACHIGH           AND READ HIGH TO SKIP AHEAD                  
         B     ACEX28                                                           
*                                                                               
ACEX40   L     R4,AIO                                                           
         CLI   ACRECORD,X'44'      TEST TRANSACTION RECORD                      
         BE    ACEX42                                                           
*                                                                               
         GOTO1 GETL,DMCB,(X'32',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   ACEX50              NOT AN ACCOUNT RECORD                        
         LA    R5,ADACC            SAVE NEW ACCOUNT RECORD                      
         BAS   RE,SAVREC                                                        
*                                                                               
         BAS   RE,SETJOB           CALL GETOPT BEFORE JOBBER                    
*                                                                               
         MVC   COMPFIL,CLTFIL      INITIALIZE COMPOSITE FILTER W CLIENT         
         GOTO1 COMPOSE,PRDFIL                                                   
         GOTO1 GETL,DMCB,(X'30',ADACC),0                                        
         CLI   ELERR,0                                                          
         BNE   ACEX41                                                           
*                                                                               
         MVC   JOBFIL,SPACES                                                    
         L     RF,ELADDR                                                        
         USING ACSTATD,RF                                                       
         MVC   JOBFIL(2),ACSTFILT                                               
         MVC   JOBFIL+2(1),ACSTANAL                                             
         MVC   JOBFIL+3(1),ACSTSUB                                              
         CLI   ACSTLEN,ACSTLNQ2                                                 
         BL    *+10                                                             
         MVC   JOBFIL+4(1),ACSTFLT5                                             
         GOTO1 COMPOSE,JOBFIL                                                   
*                                                                               
ACEX41   TM    DATAIND,ORDEREDB    TEST ORDERED REQUIRED                        
         BZ    ACEX50                                                           
         TM    PLANIND,BUPLNACT    TEST ORDERED=ACTUAL OPTION                   
         BO    *+8                 YES-IGNORE WORKCODE ESTIMATES                
         BAS   RE,GETEST           YES-GO AFTER ESTIMATES                       
         B     ACEX50                                                           
*                                                                               
ACEX42   TM    DATAIND,ORDEREDB    TEST ORDERED REQUESTED                       
         BZ    ACEX43                                                           
         TM    PLANIND,BUPLNACT    YES-TEST FOR ACTUAL OPTION                   
         BZ    ACEX43                                                           
         BAS   RE,ORD              YES                                          
*                                                                               
ACEX43   TM    DATAIND,BILLEDB     TEST BILLED NEEDED                           
         BNZ   *+12                                                             
         TM    DATAIND1,EXBAADVB+EXBADATB+EXBAINVB                              
         BZ    ACEX44                                                           
         BAS   RE,PROCTRNS                                                      
         B     ACEX50              GO BACK FOR NEXT RECORD                      
*                                                                               
ACEX44   LA    R4,KEY                                                           
         MVI   ACKEYWRK,X'FF'                                                   
         BAS   RE,ACHIGH           SKIP TO NEXT ACCOUNT                         
         B     ACEX28                                                           
*                                                                               
ACEX50   B     ACEX27              READ NEXT RECORD                             
         EJECT                                                                  
* WRITE WORKER RECORDS *                                                        
         SPACE 1                                                                
ACEX900  L     R2,ARULDATA         R2=A(RULE ENTRY)                             
*                                                                               
ACEX902  ZIC   R4,NEXTYPS          R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS           R5=A(EXTRACT TYPE TABLE)                     
         XC    BUFFREC,BUFFREC                                                  
         ST    R2,BUFFRULE                                                      
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         GOTO1 VDATAHD                                                          
*                                                                               
ACEX904  MVC   BUFFTYPE,0(R5)      SET EXTRACT TYPE                             
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
*                                                                               
ACEX906  MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         BAS   RE,BUFFGET                                                       
         CLI   DMCB+8,0            TEST IF ITEM FOUND                           
         BE    ACEX908             YES                                          
         ZAP   BUFFGRS,=P'0'       NO-MANUFACTURE ZERO RECORD                   
         ZAP   BUFFNET,=P'0'                                                    
         ZAP   BUFFSPTS,=P'0'                                                   
*                                                                               
ACEX908  MVI   DMCB,BUPPUT                                                      
         TM    WHEN,X'18'          TEST OVERNIGHT                               
         BNZ   ACEX910             YES                                          
         MVI   DMCB,BUPADD                                                      
         TM    WHEN,X'20'          TEST SOON                                    
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACEX910  GOTO1 VBUPPER                                                          
         LA    R6,SVEXL(R6)        NEXT PERIOD                                  
         OC    SVEXPER,SVEXPER     TEST FOR LAST PERIOD                         
         BNZ   ACEX906             NO                                           
*                                                                               
         LA    R5,1(R5)            NEXT EXTRACT TYPE                            
         BCT   R4,ACEX904                                                       
*                                                                               
         MVI   DMCB,BUPFINAL       FINAL CALL FOR OUTLINE                       
         GOTO1 VBUPPER                                                          
         ICM   R2,15,QRNEXT                                                     
         BNZ   ACEX902                                                          
         B     EXIT                NO MORE RULES                                
         DROP  R6                                                               
         EJECT                                                                  
*              PROCESS TRANSACTION                                              
         USING TRANSD,R6                                                        
PROCTRNS NTR1                                                                   
         XC    BATCHPER,BATCHPER                                                
         SPACE 1                                                                
         LA    R6,ACRECORD                                                      
         LA    R1,MOSTAB                                                        
         LA    R0,12                                                            
         CLC   TRNSBTCH(2),2(R1)   MATCH MONTH OF SERVICE                       
         BE    *+16                                                             
         LA    R1,4(R1)                                                         
         BCT   R0,*-14                                                          
         B     *+10                NOT THE MONTH I WANT                         
         MVC   BATCHPER,0(R1)      SET PERIOD                                   
         SPACE 1                                                                
         MVC   HALF,ACDTUSED       GET RUN-ON DATE                              
         CLC   ACKEYWRK,=C'99'     TEST FOR BILLING RECORD                      
         BNE   *+10                                                             
         MVC   HALF,TRNSNARR+33                                                 
         GOTO1 DATCON,DMCB,(2,HALF),(3,FULL)                                    
         GOTO1 SETPER,DATPER                                                    
         SPACE 1                                                                
PRCTRN1  GOTO1 DATCON,DMCB,(1,TRNSDATE),(3,FULL)                                
         GOTO1 SETPER,INVPER                                                    
*                                                                               
PRCTRN2  L     R2,ARULDATA                                                      
PRCTRN3  BAS   RE,ACCFIL           FILTER ON ACCOUNT FILTERS                    
         CLI   FILTSW,C'N'                                                      
         BE    PRCTRN4                                                          
         BAS   RE,MEDFILT          FILTER ON MEDIA                              
         CLI   FILTSW,C'N'                                                      
         BE    PRCTRN4                                                          
         BAS   RE,JOBFILT          FILTER THE MEDIA JOB                         
         CLI   FILTSW,C'N'                                                      
         BE    PRCTRN4                                                          
         BAS   RE,TRNFILT          FILTER THE TRANSACTION                       
         CLI   FILTSW,C'Y'                                                      
         BE    PRCTRN5                                                          
PRCTRN4  ICM   R2,15,QRNEXT                                                     
         BNZ   PRCTRN3             PROCESS THIS RULE                            
         B     EXIT                NO MORE RULES                                
*                                                                               
PRCTRN5  XC    BUFFRULE,BUFFRULE                                                
         TM    DATAIND,EXBADVB+EXBNADVB                                         
         BNZ   *+12                                                             
         TM    DATAIND1,EXBAADVB                                                
         BZ    PRCTRN7                                                          
         OC    BATCHPER,BATCHPER                                                
         BZ    PRCTRN7                                                          
         ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,BATCHPER                                                 
         ZAP   BUFFSPTS,=P'0'                                                   
         ZAP   BUFFGRS,ACGROSS                                                  
         ZAP   BUFFNET,ACNET                                                    
*                                                                               
         TM    DATAIND,EXBADVB                                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBADV                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND1,EXBAADVB   ACTUAL BILL                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBAADV                                                 
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBNADVB    TEST NET BILLING NEEDED                      
         BZ    PRCTRN7                                                          
         MVI   BUFFTYPE,EXBNADV                                                 
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
         SPACE 1                                                                
PRCTRN7  TM    DATAIND,EXBDATB+EXBNDATB                                         
         BNZ   *+12                                                             
         TM    DATAIND1,EXBADATB                                                
         BZ    PRCTRN9                                                          
         OC    DATPER,DATPER       TEST IF BILL RUN ON W/IN PLAN                
         BZ    PRCTRN9             NO                                           
         ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,DATPER      SET PERIOD                                   
         ZAP   BUFFSPTS,=P'0'                                                   
         ZAP   BUFFGRS,ACGROSS                                                  
         ZAP   BUFFNET,ACNET                                                    
*                                                                               
         TM    DATAIND,EXBDATB     TEST GROSS BILLING NEEDED                    
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBDATE                                                 
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND1,EXBADATB   TEST ACTUAL BILLING NEEDED                   
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBADAT                                                 
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBNDATB    TEST NET BILLING NEEDED                      
         BZ    PRCTRN9                                                          
         MVI   BUFFTYPE,EXBNDATE                                                
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
PRCTRN9  TM    DATAIND,EXBINVB+EXBNINVB                                         
         BNZ   *+12                                                             
         TM    DATAIND1,EXBAINVB                                                
         BZ    PRCTRN12                                                         
         OC    INVPER,INVPER                                                    
         BZ    PRCTRN12                                                         
         CLC   ACKEYWRK,=C'99'     TEST FOR BILLING RECORD                      
         BNE   PRCTRN12                                                         
         ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,INVPER                                                   
*                                                                               
PRCTRN10 ZAP   BUFFSPTS,=P'0'                                                   
         ZAP   BUFFGRS,ACGROSS                                                  
         ZAP   BUFFNET,ACNET                                                    
*                                                                               
         TM    DATAIND,EXBINVB                                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBINV                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND1,EXBAINVB   ACTUAL BILL                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBAINV                                                 
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBNINVB    TEST NET BILLING NEEDED                      
         BZ    PRCTRN12                                                         
         MVI   BUFFTYPE,EXBNINV                                                 
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
PRCTRN12 OC    BUFFRULE,BUFFRULE   TEST IF POSTED AGAINST RULE                  
         BZ    EXIT                NO-FAILED ALL PERIOD TESTS                   
         ICM   R2,15,QRJUMP                                                     
         BNZ   PRCTRN3             SEE IF TRANS. FITS ANOTHER RULE              
         B     EXIT                                                             
         EJECT                                                                  
*              PROCESS TRANSACTIONS FOR ORDERED                                 
         USING TRANSD,R6                                                        
ORD      NTR1                                                                   
         XC    BATCHPER,BATCHPER                                                
         LA    R6,ACRECORD                                                      
         LA    R1,MOSTAB                                                        
         LA    R0,12                                                            
         CLC   TRNSBTCH(2),2(R1)   MATCH MONTH OF SERVICE                       
         BE    ORD1                                                             
         LA    R1,4(R1)                                                         
         BCT   R0,*-14                                                          
         B     EXIT                MOS NOT IN THIS EXTRACT                      
*                                                                               
ORD1     MVC   BATCHPER,0(R1)      SET PERIOD                                   
         L     R2,ARULDATA                                                      
*                                                                               
ORD3     BAS   RE,ACCFIL           FILTER ON ACCOUNT FILTERS                    
         CLI   FILTSW,C'N'                                                      
         BE    ORD4                                                             
         BAS   RE,MEDFILT          FILTER ON MEDIA                              
         CLI   FILTSW,C'N'                                                      
         BE    ORD4                                                             
         BAS   RE,JOBFILT          FILTER THE MEDIA JOB                         
         CLI   FILTSW,C'N'                                                      
         BE    ORD4                                                             
         BAS   RE,ORDFILT          FILTER THE TRANSACTION                       
         CLI   FILTSW,C'Y'                                                      
         BE    ORD5                                                             
*                                                                               
ORD4     ICM   R2,15,QRNEXT                                                     
         BNZ   ORD3                PROCESS THIS RULE                            
         B     EXIT                NO MORE RULES                                
*                                                                               
ORD5     ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,BATCHPER                                                 
         ZAP   BUFFSPTS,=P'0'                                                   
         ZAP   BUFFGRS,ACGROSS                                                  
         ZAP   BUFFNET,ACNET                                                    
*                                                                               
         TM    DATAIND,EXORDB                                                   
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXORD                                                   
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXORDNB     TEST NET ORDERED NEEDED                      
         BZ    ORD7                                                             
         MVI   BUFFTYPE,EXORDN                                                  
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
         SPACE 1                                                                
*                                                                               
ORD7     ICM   R2,15,QRJUMP                                                     
         BNZ   ORD3                SEE IF TRANS. FITS ANOTHER RULE              
         B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO SET PERIOD FOR TRANSACTION                                     
* AT ENTRY, R1=A(PERIOD) AND FULL(3) CONTAINS BINARY YMD                        
*                                                                               
SETPER   ST    RE,SAVERE                                                        
         LR    RE,R1                                                            
         XC    0(2,RE),0(RE)                                                    
         CLC   FULL(3),SVEXTSTB    TEST RUN ON DATE W/IN PLAN                   
         BL    SETPERX                                                          
         CLC   FULL(3),SVEXTNDB                                                 
         BH    SETPERX                                                          
*                                                                               
         LA    R1,SVEXTDTS                                                      
         USING SVEXD,R1                                                         
*                                                                               
SETPER2  CLC   FULL(3),SVEXSTB     MATCH ON PERIOD START DATE                   
         BL    SETPER3                                                          
         CLC   FULL(3),SVEXENDB    AND ON END DATE                              
         BH    SETPER3                                                          
         MVC   0(2,RE),SVEXPER                                                  
         B     SETPERX                                                          
*                                                                               
SETPER3  LA    R1,SVEXL(R1)                                                     
         B     SETPER2                                                          
         DROP  R1                                                               
*                                                                               
SETPERX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              SUBROUTINE TO CALL GETOPT                              *         
***********************************************************************         
*                                                                               
         USING ACHEIRD,R2                                                       
SETJOB   NTR1                                                                   
         L     R2,ADLDGHIR                                                      
         L     R5,ADACC                                                         
*                                                                               
         MVC   GOABUFF,AGOBUFF                                                  
         MVC   GOLBUFF,LGOBUFF                                                  
         MVC   GOADM,DATAMGR                                                    
         MVC   GOACOMP,ADCOMP                                                   
         MVC   GOALEDG,ADLEDGER                                                 
         MVC   GOACLI,ADHEIRA                                                   
         MVC   GOAPRO,ADHEIRB                                                   
         MVC   GOAJOB,ADACC                                                     
         MVC   GOAKEY,ADACC                                                     
         MVC   GOSELCUL,0(R5)                                                   
*                                                                               
         MVC   GOSELCLI,SPACES                                                  
         LA    R5,3(R5)            BUMP PAST C/U/L                              
         SR    R1,R1                                                            
         IC    R1,ACHRLEVA                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELCLI(0),0(R5)                                                
         LA    R5,1(R1,R5)                                                      
*                                                                               
         MVC   GOSELPRO,SPACES                                                  
         SR    R4,R4                                                            
         IC    R4,ACHRLEVA                                                      
         IC    R1,ACHRLEVB                                                      
         SR    R1,R4                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELPRO(0),0(R5)                                                
         LA    R5,1(R1,R5)                                                      
*                                                                               
         MVC   GOSELJOB,SPACES                                                  
         IC    R4,ACHRLEVB                                                      
         IC    R1,ACHRLEVC                                                      
         SR    R1,R4                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELJOB(0),0(R5)                                                
         LA    R5,1(R1,R5)                                                      
*                                                                               
         GOTO1 GETOPT,DMCB,AGOBLOCK                                             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS ESTIMATE ELEMENTS FOR A JOB (ACCOUNT)                  
*                                                                               
* UNIT OF DATA PASSED AGAINST RULE TABLE IS WORKCODE ESTIMATE                   
* AT ENTRY, HAVE JUST READ JOB.                                                 
*                                                                               
         USING QRD,R2                                                           
GETEST   NTR1                                                                   
         BAS   RE,JOBPER           FIND PERIOD FOR ESTIMATE                     
         BNE   EXIT                OUTSIDE EXTRACT PERIOD                       
*                                                                               
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         MVC   JBAGOBLK,AGOBLOCK                                                
         MVC   JBAIO,AJOBIO                                                     
         MVC   JBAKEY,AIO                                                       
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACOLTAB                                                 
         MVC   JBLCOLTB,LCOLTAB                                                 
         MVC   JBAOPVTB,AOPVTAB                                                 
         MVC   JBLOPVTB,LOPVTAB                                                 
*                                                                               
         GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING JBCOLD,R6                                                        
         L     R6,ACOLTAB                                                       
         LH    R4,JBNROWS                                                       
         LTR   R4,R4                                                            
         BZ    EXIT                NO ESTIMATE                                  
*                                                                               
         L     R2,ARULDATA         FIND FIRST RULE FOR THE JOB                  
GETEST2  BAS   RE,ACCFIL           FILTER ON ACCOUNT FILTERS                    
         CLI   FILTSW,C'Y'                                                      
         BNE   GETEST3                                                          
         BAS   RE,MEDFILT          FILTER ON MEDIA                              
         CLI   FILTSW,C'Y'                                                      
         BNE   GETEST3                                                          
         BAS   RE,JOBFILT          FILTER MEDIA / JOB                           
         CLI   FILTSW,C'Y'                                                      
         BE    GETEST4             FOUND A RULE APPLYING TO JOB                 
*                                                                               
GETEST3  ICM   R2,15,QRNEXT                                                     
         BZ    EXIT                EOL-NO RULES APPLY TO JOB                    
         B     GETEST2                                                          
*                                                                               
GETEST4  LH    R4,JBNROWS                                                       
         XR    R5,R5               TEST IF FIRST RULE HAS WC LIST               
         ICM   R5,3,QRWCDDSP                                                    
         BZ    GETEST20            NO WORK CODE FILTERS                         
         LA    R5,0(R2,R5)         R5 TO WORK CODE LIST                         
         ZIC   R0,0(R5)            NUMBER OF ENTRIES                            
         LTR   R0,R0                                                            
         BZ    GETEST20            NO WORK CODE FILTERS                         
*                                                                               
* FIRST JOB HAS A WORKCODE LIST - PROCESS EACH WCD ESTIMATE AGAINST             
* RULE TABLE STARTING AT FIRST RULE FOR JOB UNTIL EOL OR FIND MATCH             
*                                                                               
         ST    R2,AFSTRULE         SAVE A(FIRST RULE FOR JOB)                   
         B     GETEST9             APPLY RULES TO FIRST WORKCODE ELEM           
*                                                                               
* RETURN POINT TO PASS WCD ESTIMATE AGAINST RULES                               
*                                                                               
GETEST6  L     R2,AFSTRULE         BEGIN AT FIRST RULE FOR JOB                  
*                                                                               
GETEST7  BAS   RE,ACCFIL           APPLY ACCOUNT FILTERS                        
         CLI   FILTSW,C'Y'                                                      
         BNE   GETEST7A                                                         
         BAS   RE,MEDFILT          APPLY MEDIA FILTERS                          
         CLI   FILTSW,C'Y'                                                      
         BNE   GETEST7A                                                         
         BAS   RE,JOBFILT          APPLY JOB FILTERS                            
         CLI   FILTSW,C'Y'         TEST IF RULE APPLIES TO JOB                  
         BE    GETEST8             YES-GO HANDLE WORKCODE LIST                  
*                                                                               
GETEST7A ICM   R2,15,QRNEXT        NEXT RULE                                    
         BZ    GETEST15            EOL-NO RULES COVER THIS WORKCODE             
         B     GETEST7             NEXT RULE                                    
*                                                                               
GETEST8  XR    R5,R5                                                            
         ICM   R5,3,QRWCDDSP                                                    
         BZ    GETEST12            NO WCD LIST SO WCD MUST FIT HERE             
         LA    R5,0(R2,R5)         R5=A(WORK CODE LIST)                         
         ZIC   R0,0(R5)                                                         
         LTR   R0,R0                                                            
         BZ    GETEST12            NO WCD LIST SO WCD MUST FIT HERE             
*                                                                               
GETEST9  LA    R5,1(R5)            POINT TO FIRST WORKCODE                      
         BAS   RE,SETSIGN                                                       
         CLI   SIGN,C'-'           TEST FOR NEGATIVE RULE                       
         BE    GETES10A                                                         
*                                                                               
GETEST10 CLI   JBCOLTYP,JBCOLTWC   IS THIS A WORKCODE ?                         
         BNE   GETEST15            NO, GET NEXT                                 
         CLC   0(2,R5),JBCOLWC     TEST RULE COVERS WCD ESTIMATE                
         BE    GETEST12                                                         
         LA    R5,2(R5)                                                         
         BCT   R0,GETEST10                                                      
         B     GETEST11            NO MATCHES AGAINST RULE                      
*                                                                               
GETES10A MVC   DUB(2),0(R5)        EXTRACT RULE                                 
         OI    DUB,X'40'                                                        
         CLC   DUB(2),JBCOLWC                                                   
         BE    GETEST11            MATCH ON WCD-REJECT ESTIMATE                 
         LA    R5,2(R5)                                                         
         BCT   R0,GETES10A                                                      
         B     GETEST12            POST AGAINST THIS RULE                       
*                                  RULE DOES NOT APPLY TO THIS WCD              
GETEST11 ICM   R2,15,QRNEXT                                                     
         BNZ   GETEST7             PROCESS NEXT RULE                            
         B     GETEST15            EOL-NEXT WCD ELEMENT                         
*                                                                               
GETEST12 BAS   RE,INITEST          INITIALIZE BUFFALO RECORD                    
         MVC   WKCODE,JBCOLWC                                                   
         ZAP   ACNET,JBCOLVAL(6)                                                
         ZAP   ACGROSS,JBCOLVAL+6(6)                                            
         ZAP   BUFFGRS,ACGROSS                                                  
         ZAP   BUFFNET,ACNET                                                    
         CLC   BUFFGRS(24),=3PL8'0'                                             
         BE    GETEST14                                                         
*                                                                               
         TM    DATAIND,EXORDB      TEST FOR GROSS ORDERED                       
         BZ    *+8                                                              
         BAS   RE,BUFFPUT                                                       
         TM    DATAIND,EXORDNB     TEST FOR NET ORDERED                         
         BZ    GETEST14                                                         
         MVI   BUFFTYPE,EXORDN                                                  
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
GETEST14 ICM   R2,15,QRJUMP                                                     
         BNZ   GETEST7             SEE IF WC EST FITS ANOTHER RULE              
*                                                                               
GETEST15 AH    R6,JBLCOL           GET NEXT WORKCODE                            
         BCT   R4,GETEST6                                                       
         B     EXIT                NO MORE WORKCODES-ALL DONE                   
***********************************************************************         
*             FOUND A RULE COVERING JOB WITH NO WORKCODE LIST. ADD UP *         
*             WORKCODE ESTIMATES ON JOB RECORD, PUT TO BUFFALO, AND   *         
*             EXIT.                                                   *         
***********************************************************************         
*                                                                               
GETEST20 BAS   RE,INITEST                                                       
*                                                                               
GETEST22 CLI   JBCOLTYP,JBCOLTWC   IS THIS A WORKCODE ?                         
         BNE   GETEST23            NO, GET NEXT                                 
         MVC   WKCODE,JBCOLWC                                                   
         ZAP   ACNET,JBCOLVAL(6)                                                
         ZAP   ACGROSS,JBCOLVAL+6(6)                                            
         AP    BUFFGRS,ACGROSS                                                  
         AP    BUFFNET,ACNET                                                    
*                                                                               
GETEST23 AH    R6,JBLCOL                                                        
         BCT   R4,GETEST22                                                      
*                                                                               
GETEST24 CLC   BUFFGRS(24),=3PL8'0'                                             
         BE    GETEST25            NO DATA TO PUT                               
         TM    DATAIND,EXORDB      TEST FOR GROSS ORDERED                       
         BZ    *+8                                                              
         BAS   RE,BUFFPUT                                                       
         TM    DATAIND,EXORDNB     TEST FOR NET ORDERED                         
         BZ    GETEST25                                                         
         MVI   BUFFTYPE,EXORDN                                                  
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
GETEST25 ICM   R2,15,QRJUMP        PICK UP RULE TO JUMP TO                      
         BZ    EXIT                NO MORE RULES-EXIT                           
         B     GETEST2             START ALL OVER AGAIN                         
         EJECT                                                                  
* SUB-ROUTINE TO WORK OUT PERIOD FOR JOB OPEN DATE AND TEST                     
* IF IT FALLS WITHIN EXTRACT PERIOD (CALLED FROM GETEST)                        
*                                                                               
* ON EXIT, CC=EQ IF WITHIN PLAN, CC=NEQ IF OUTSIDE PLAN                         
*          ESTPER CONTAINS PERIOD                                               
*                                                                               
JOBPER   NTR1                                                                   
         XC    ESTPER,ESTPER       CLEAR ESTIMATE PERIOD                        
         GOTO1 GETL,DMCB,(X'30',ADACC),0                                        
         CLI   ELERR,0                                                          
         BNE   JOBPER2                                                          
         L     RF,ELADDR           USE OPENED DATE FOR PERIOD                   
         USING ACSTATD,RF                                                       
         GOTO1 DATCON,DMCB,(1,ACSTBFDT),(3,ESTPER)                              
         SPACE 1                                                                
JOBPER2  GOTO1 GETL,DMCB,(X'26',ADACC),0                                        
         CLI   ELERR,0                                                          
         BNE   JOBPER4                                                          
         L     RF,ELADDR                                                        
         USING ACJOBD,RF                                                        
         CLI   ACJBLEN,16                                                       
         BL    JOBPER4                                                          
         OC    ACJBOPND,ACJBOPND                                                
         BZ    JOBPER4                                                          
         GOTO1 DATCON,DMCB,(1,ACJBOPND),(3,ESTPER)                              
*                                                                               
JOBPER4  CLC   ESTPER(2),STARTPER  TEST IF BEFORE START PERIOD                  
         BL    JOBPERN                                                          
         CLC   ESTPER(2),ENDPER    TEST IF AFTER END PERIOD                     
         BH    JOBPERN                                                          
         CR    RB,RB               SET CC=EQ                                    
         B     JOBPERX                                                          
*                                                                               
JOBPERN  LTR   RB,RB               SET CC=NEQ                                   
*                                                                               
JOBPERX  B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO INITIALIZE A BUFFALO RECORD FOR A RULE                         
* (CALLED FROM GETEST)                                                          
* AT ENTRY, R2=A(RULE)                                                          
*                                                                               
INITEST  ST    R2,BUFFRULE                                                      
         MVI   BUFFTYPE,EXORD                                                   
         ZAP   BUFFSPTS,=P'0'                                                   
         ZAP   BUFFGRS,=P'0'                                                    
         ZAP   BUFFNET,=P'0'                                                    
         MVC   BUFFPER,ESTPER      SET PERIOD                                   
         BR    RE                                                               
         EJECT                                                                  
*              FILTER ON MEDIA                                                  
         SPACE 1                                                                
MEDFILT  NTR1                                                                   
         MVI   FILTSW,C'Y'                                                      
         SR    R5,R5                                                            
         ICM   R5,3,QRMEDDSP                                                    
         BZ    EXIT                NO MEDIA FILTERS                             
         LA    R5,0(R5,R2)         R2 TO MEDIA LIST                             
         SR    R0,R0                                                            
         ICM   R0,1,0(R5)          NUMBER OF ENTRIES                            
         BZ    EXIT                NO MEDIA FILTERS                             
*                                                                               
         LA    R5,1(R5)                                                         
         ZIC   R1,STJOB                                                         
         L     R4,AIO                                                           
         LA    R4,0(R1,R4)         R4=A(MEDIA IN JOB KEY)                       
         BAS   RE,SETSIGN                                                       
         CLI   SIGN,C'+'           TEST FOR POSITIVE RULE                       
         BE    *+8                 YES                                          
         MVI   FILTSW,C'N'         SET DEFAULT FILTER TO NO                     
         SPACE 1                                                                
MEDFILT2 MVC   BYTE,0(R5)          EXTRACT RULE                                 
         OI    BYTE,X'40'          RESTORE LOWER CASE BIT                       
         CLC   BYTE,0(R4)          FILTER LIST VS RECORD                        
         BE    EXIT                MATCHES SO RETURN OK                         
         LA    R5,1(R5)                                                         
         BCT   R0,MEDFILT2                                                      
         MVI   FILTSW,C'N'                                                      
         CLI   SIGN,C'+'                                                        
         BE    *+8                                                              
         MVI   FILTSW,C'Y'         NO MATCHES MEANS YES FOR NEG RULE            
         B     EXIT                DID NOT MATCH ANY JOB CODE                   
         EJECT                                                                  
*              FILTER MEDIA/JOB                                                 
         SPACE 1                                                                
JOBFILT  NTR1                                                                   
         MVI   FILTSW,C'Y'                                                      
         XR    R5,R5                                                            
         ICM   R5,3,QRJOBDSP                                                    
         BZ    EXIT                NO JOB FILTERS                               
         LA    R5,0(R5,R2)         R2 TO JOB LIST                               
         ZIC   R0,0(R5)            NUMBER OF ENTRIES                            
         LTR   R0,R0                                                            
         BZ    EXIT                NO JOB FILTERS                               
*                                                                               
         LA    R5,1(R5)                                                         
         ZIC   R1,STJOB                                                         
         L     R4,AIO                                                           
         AR    R4,R1     R4 TO START OF JOB NUMBER FOR RECORD IN IO             
         ZIC   R1,STJOBLN                                                       
         BCTR  R1,0                LENGTH OF JOB CODE                           
         BAS   RE,SETSIGN                                                       
         CLI   SIGN,C'+'           TEST FOR POSITIVE RULE                       
         BE    *+8                 YES                                          
         MVI   FILTSW,C'N'         SET DEFAULT FILTER TO NO                     
         SPACE 1                                                                
JOBFILT2 MVC   DUB(6),0(R5)        EXTRACT RULE                                 
         OI    DUB,X'40'           RESTORE LOWER CASE BIT                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),0(R4)        FILTER LIST VS IO                            
         BE    EXIT                MATCHES SO RETURN OK                         
         LA    R5,6(R5)                                                         
         BCT   R0,JOBFILT2                                                      
         MVI   FILTSW,C'N'                                                      
         CLI   SIGN,C'+'                                                        
         BE    *+8                                                              
         MVI   FILTSW,C'Y'         NO MATCHES MEANS YES FOR NEG RULE            
         B     EXIT                DID NOT MATCH ANY JOB CODE                   
         EJECT                                                                  
*              FILTER WORKCODE                                                  
         SPACE 1                                                                
WCDFILT  NTR1                                                                   
         MVI   FILTSW,C'Y'                                                      
         SR    R5,R5                                                            
         ICM   R5,3,QRWCDDSP                                                    
         BZ    EXIT                                                             
         LA    R5,0(R5,R2)         R5 TO WCD LIST                               
         ZIC   R0,0(R5)            NUMBER OF ENTRIES                            
         LTR   R0,R0                                                            
         BZ    EXIT                NO WCD FILTERS                               
         LA    R5,1(R5)                                                         
         L     R4,AIO              R4=A(KEY)                                    
         USING ACKEYD,R4                                                        
         BAS   RE,SETSIGN                                                       
         CLI   SIGN,C'+'           TEST FOR POSITIVE RULE                       
         BE    *+8                 YES                                          
         MVI   FILTSW,C'N'         SET DEFAULT FILTER TO NO                     
         SPACE 1                                                                
WCDFILT2 MVC   DUB(LRUWCD),0(R5)   EXTRACT RULE                                 
         OI    DUB,X'40'           RESTORE LOWER CASE BIT                       
         CLC   DUB(LRUWCD),ACKEYWRK                                             
         BE    EXIT                MATCHES SO EXIT                              
         LA    R5,LRUWCD(R5)                                                    
         BCT   R0,WCDFILT2                                                      
*                                                                               
         MVI   FILTSW,C'N'         NOT IN WCD TABLE                             
         CLI   SIGN,C'+'                                                        
         BE    *+8                                                              
         MVI   FILTSW,C'Y'         NO MATCHES MEANS YES FOR NEG RULE            
         B     EXIT                DID NOT MATCH ANY WCD CODE                   
         EJECT                                                                  
*              FILTER TRANSACTION ETC.                                          
         SPACE 1                                                                
TRNFILT  NTR1                                                                   
         ZAP   ACNET,=P'0'                                                      
         ZAP   ACGROSS,=P'0'                                                    
         MVI   FILTSW,C'N'                                                      
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
TRNFIL1  L     R4,AIO                                                           
         XR    R5,R5                                                            
         ICM   R5,3,QRWCDDSP                                                    
         BZ    TRNFIL99            NO WORK CODE FILTERS                         
         LA    R5,0(R5,R2)         R2 TO WORK CODE LIST                         
         ZIC   R0,0(R5)            NUMBER OF ENTRIES                            
         LTR   R0,R0                                                            
         BZ    TRNFIL99            NO WORK CODE FILTERS                         
         LA    R5,1(R5)                                                         
         BAS   RE,SETSIGN                                                       
         CLI   SIGN,C'-'           TEST FOR NEGATIVE RULE                       
         BE    TRNFIL4                                                          
         SPACE 1                                                                
TRNFIL2  CLC   0(2,R5),ACKEYWRK                                                 
         BE    TRNFIL5                                                          
         LA    R5,2(R5)                                                         
         BCT   R0,TRNFIL2                                                       
         B     EXIT                NOT IN WORK CODE TABLE                       
         SPACE 1                                                                
TRNFIL4  MVC   DUB(2),0(R5)                                                     
         OI    DUB,X'40'                                                        
         CLC   DUB(2),ACKEYWRK                                                  
         BE    EXIT                                                             
         LA    R5,2(R5)                                                         
         BCT   R0,TRNFIL4                                                       
         B     TRNFIL5             POST THE CHARGE                              
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
TRNFIL5  L     R4,AIO                                                           
         CLC   ACKEYWRK,=C'99'                                                  
         BE    EXIT                EXCLUDE BILLING                              
         OC    ACDTUSED,ACDTUSED                                                
         BZ    EXIT                NOT BILLED                                   
         LA    R6,ACRECORD                                                      
         USING TRANSD,R6                                                        
         MVC   WKCODE,ACKEYWRK                                                  
         ZAP   ACNET,TRNSAMNT                                                   
         ZAP   ACGROSS,TRNSAMNT                                                 
         ZAP   CASHD,=P'0'                                                      
*              ADD IN CASH DISCOUNT IF PRESENT                                  
         GOTO1 GETL,DMCB,(X'50',AIO),1,=C'D'                                    
         CLI   ELERR,0                                                          
         BNE   TRNFIL8                                                          
         L     RE,ELADDR                                                        
         USING TRCASHD,RE                                                       
         ZAP   CASHD,TRCSAMNT                                                   
         AP    ACNET,CASHD                                                      
         AP    ACGROSS,CASHD                                                    
         SPACE 1                                                                
TRNFIL8  TM    TRNSSTAT,X'01'                                                   
         BO    *+8                 NON-COMMISSIONABLE                           
         BAS   RE,GETRULES         GET COMMISSION RATE                          
         SP    ACNET,CASHD                                                      
         SP    ACGROSS,CASHD                                                    
         B     TRNYES                                                           
         SPACE 1                                                                
TRNFIL99 CLC   ACKEYWRK,=C'99'     IF NO WORKCODE FILTERS                       
         BNE   EXIT                ONLY WANT BILLING                            
         LA    R6,ACRECORD                                                      
         USING TRANSD,R6                                                        
         ZAP   ACNET,TRNSAMNT                                                   
         ZAP   ACGROSS,TRNSAMNT                                                 
*                                                                               
         CLI   GOPAYNET,C'Y'                                                    
         BE    *+10                                                             
         AP    ACGROSS,TRNSNARR+15(6)  ADDM COMMISSION                          
         SPACE 1                                                                
TRNYES   MVI   FILTSW,C'Y'                                                      
         B     EXIT                                                             
         EJECT                                                                  
*              FILTER TRANSACTION FOR ORDERED                                   
         SPACE 1                                                                
ORDFILT  NTR1                                                                   
         ZAP   ACNET,=P'0'                                                      
         ZAP   ACGROSS,=P'0'                                                    
         MVI   FILTSW,C'N'                                                      
         USING ACKEYD,R4                                                        
         L     R4,AIO                                                           
         CLC   ACKEYWRK,=C'99'     TEST FOR BILLING RECORD                      
         BE    EXIT                                                             
         BAS   RE,WCDFILT          APPLY ANY WORKCODE FILTER                    
         CLI   FILTSW,C'N'                                                      
         BE    EXIT                NO                                           
         SPACE 1                                                                
ORDFILT2 LA    R6,ACRECORD                                                      
         USING TRANSD,R6                                                        
         MVC   WKCODE,ACKEYWRK                                                  
         ZAP   ACNET,TRNSAMNT                                                   
         ZAP   ACGROSS,TRNSAMNT                                                 
         ZAP   CASHD,=P'0'                                                      
*              ADD IN CASH DISCOUNT IF PRESENT                                  
         GOTO1 GETL,DMCB,(X'50',AIO),1,=C'D'                                    
         CLI   ELERR,0                                                          
         BNE   ORDFILT4                                                         
         L     RE,ELADDR                                                        
         USING TRCASHD,RE                                                       
         ZAP   CASHD,TRCSAMNT                                                   
         AP    ACNET,CASHD                                                      
         AP    ACGROSS,CASHD                                                    
         SPACE 1                                                                
ORDFILT4 TM    TRNSSTAT,X'01'                                                   
         BO    *+8                 NON-COMMISSIONABLE                           
         BAS   RE,GETRULES         GET COMMISSION RATE                          
         SP    ACNET,CASHD                                                      
         SP    ACGROSS,CASHD                                                    
         SPACE 1                                                                
ORDFILTX B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO FORM THE COMPOSITE FILTER                                      
*                                                                               
* AT ENTRY, R1=NEW LEVEL RECORD                                                 
*                                                                               
COMPOSE  NTR1  ,                                                                
         LA    R0,L'COMPFIL                                                     
         LA    RE,COMPFIL                                                       
COMPOSE2 CLI   0(R1),C' '                                                       
         BNH   *+10                                                             
         MVC   0(1,RE),0(R1)                                                    
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,COMPOSE2                                                      
         B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO APPLY ACCOUNT FILTER RULES AGAINST JOB                         
*                                                                               
ACCFIL   NTR1                                                                   
         LA    R6,5                R6=N'FILTERS                                 
         SR    R1,R1               R1=INDEX INTO FILTERS                        
         MVI   FILTSW,C'Y'                                                      
         SPACE 1                                                                
ACCFIL2  LR    RF,R1                                                            
         SLL   RF,1                RF=INDEX TO RULE DISPLACEMENT                
         LA    RE,QRAF1DSP(RF)     RE=A(DISP TO RULE FIELD)                     
         SR    R5,R5                                                            
         ICM   R5,3,0(RE)                                                       
         BZ    ACCFIL8                                                          
         LA    R5,0(R2,R5)                                                      
         ZIC   R0,0(R5)            R0=N'RULE ENTRIES                            
         LTR   R0,R0               TEST FOR ANY RULE ENTRIES                    
         BZ    ACCFIL8                                                          
         LA    R5,1(R5)                                                         
         BAS   RE,SETSIGN                                                       
         SPACE 1                                                                
ACCFIL4  MVC   DUB(1),0(R5)        EXTRACT RULE VALUE                           
         OI    DUB,X'40'           RESTORE LOWER CASE BIT                       
         LA    RF,COMPFIL(R1)      INDEX INTO JOB'S FILTER                      
         CLI   SIGN,C'-'           TEST FOR NEGATIVE RULE                       
         BE    ACCFIL6             YES                                          
         CLC   DUB(1),0(RF)        TEST FOR MATCH ON RULE                       
         BE    ACCFIL8             ITS A HIT                                    
         B     ACCFIL7                                                          
         SPACE 1                                                                
ACCFIL6  CLC   DUB(1),0(RF)                                                     
         BNE   ACCFIL7                                                          
         MVI   FILTSW,C'N'         ON A MATCH REJECT JOB                        
         B     ACCFILX                                                          
         SPACE 1                                                                
ACCFIL7  LA    R5,1(R5)            NEXT FILTER VALUE IN RULE                    
         BCT   R0,ACCFIL4                                                       
         CLI   SIGN,C'+'           TEST FOR POSITIVE RULE                       
         BNE   ACCFIL8                                                          
         MVI   FILTSW,C'N'         DOES NOT MATCH ANY RULE VALUE                
         B     ACCFILX                                                          
         SPACE 1                                                                
ACCFIL8  LA    R1,1(R1)            INCREMENT FILTER NUMBER INDEX                
         BCT   R6,ACCFIL2                                                       
         SPACE 1                                                                
ACCFILX  B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR CONSISTENT RULES                                      
*                                                                               
TSTRUL   ST    RE,SAVERE                                                        
         LA    R6,SIGNTAB                                                       
         SPACE 1                                                                
TSTRUL2  CLI   0(R6),X'FF'         TEST FOR EOT                                 
         BE    TSTRULX                                                          
         SR    RF,RF                                                            
         ICM   RF,3,0(R6)          GET DISPLACEMENT INTO RULE                   
         LA    RF,0(R2,RF)                                                      
         SR    R5,R5                                                            
         ICM   R5,3,0(RF)          PICK UP DSP TO RULE ENTRIES                  
         BZ    TSTRUL4                                                          
         LA    R5,0(R2,R5)                                                      
         ZIC   R0,0(R5)                                                         
         LTR   R0,R0               TEST FOR ANY RULES                           
         BZ    TSTRUL4                                                          
         LA    R5,1(R5)            POINT AT FIRST JOB                           
         BAS   RE,SETSIGN          SET SIGN OF RULE FROM 1ST JOB                
         ZIC   R1,2(R6)            R1=L'RULE ENTRY                              
         GOTO1 CHKRUL,(R1)                                                      
         BE    TSTRUL4                                                          
         ICM   RF,15,3(R6)         GET ERROR BRANCH POINT                       
         A     RF,RELO                                                          
         BR    RF                                                               
         SPACE 1                                                                
TSTRUL4  LA    R6,L'SIGNTAB(R6)                                                 
         B     TSTRUL2                                                          
         SPACE 1                                                                
TSTRULX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
SETSIGN  MVI   SIGN,C'+'                                                        
         TM    0(R5),X'40'                                                      
         BO    *+8                                                              
         MVI   SIGN,C'-'                                                        
         BR    RE                                                               
         SPACE 1                                                                
* SUB-ROUTINE TO CHECK FOR CONSISTENT RULES (POSITIVE OR NEGATIVE)              
* AT ENTRY, R1=L'RULE ENTRY, R0=N'RULES, R5=A(FIRST RULE)                       
* ON EXIT, CC=EQ IF OK, NEQ IF ERROR                                            
*                                                                               
CHKRUL   CLI   SIGN,C'+'                                                        
         BNE   CHKRUL2                                                          
         TM    0(R5),X'40'                                                      
         BZ    CHKRULR                                                          
         LA    R5,0(R1,R5)                                                      
         BCT   R0,*-12                                                          
         B     CHKRULOK                                                         
         SPACE 1                                                                
CHKRUL2  TM    0(R5),X'40'                                                      
         BO    CHKRULR                                                          
         LA    R5,0(R1,R5)                                                      
         BCT   R0,CHKRUL2                                                       
         SPACE 1                                                                
CHKRULOK CR    RB,RB                                                            
         B     CHKRULX                                                          
         SPACE 1                                                                
CHKRULR  LTR   RB,RB                                                            
         SPACE 1                                                                
CHKRULX  BR    RE                                                               
         SPACE 2                                                                
SIGNTAB  DS    0XL7                                                             
         DC    AL2(QRJOBDSP-QRD),AL1(LRUJOB),AL4(BADJOB)                        
         DC    AL2(QRWCDDSP-QRD),AL1(LRUWCD),AL4(BADWCD)                        
         DC    AL2(QRAF1DSP-QRD),AL1(LRUAF1),AL4(BADAF1)                        
         DC    AL2(QRAF2DSP-QRD),AL1(LRUAF2),AL4(BADAF2)                        
         DC    AL2(QRAF3DSP-QRD),AL1(LRUAF3),AL4(BADAF3)                        
         DC    AL2(QRAF4DSP-QRD),AL1(LRUAF4),AL4(BADAF4)                        
         DC    AL2(QRAF5DSP-QRD),AL1(LRUAF5),AL4(BADAF5)                        
         DC    X'FF'                                                            
         DS    0H                                                               
         EJECT                                                                  
*              BUILD MONTH OF SERVICE LIST                                      
BLDMOS   NTR1                                                                   
         LA    R0,12                                                            
         LA    R4,SVEXTDTS                                                      
         USING SVEXD,R4                                                         
         LA    R2,MOSTAB                                                        
BLDMOS2  CLI   SVEXPER,0           TEST FOR EOT                                 
         BE    BLDMOS6                                                          
         MVC   WORK(2),SVEXPER                                                  
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(1,WORK+3)                                  
         MVC   0(2,R2),WORK        YYMM BINARY                                  
         ZIC   R3,WORK+3           YEAR                                         
         SLL   R3,28                                                            
         SRL   R3,28               STRIP OFF DECADE                             
         STC   R3,2(R2)                                                         
         OI    2(R2),X'F0'         YEAR IN CHARACTER                            
         MVC   3(1,R2),WORK+4        MONTH                                      
         OI    3(R2),X'F0'                                                      
         CLI   WORK+4,X'10'          10=A,11=B,12=C                             
         BL    BLDMOS4                                                          
         MVI   3(R2),C'A'                                                       
         CLI   WORK+4,X'11'                                                     
         BL    BLDMOS4                                                          
         MVI   3(R2),C'B'                                                       
         CLI   WORK+4,X'12'                                                     
         BL    BLDMOS4                                                          
         MVI   3(R2),C'C'                                                       
BLDMOS4  LA    R4,SVEXL(R4)                                                     
         LA    R2,L'MOSTAB(R2)                                                  
         BCT   R0,BLDMOS2                                                       
BLDMOS6  SH    R2,=Y(L'MOSTAB)     BACK UP TO LAST ENTRY                        
         MVC   ENDPER,0(R2)        EXTRACT END PERIOD                           
         MVC   STARTPER,MOSTAB     EXTRACT START PERIOD                         
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO SAVE RECORDS                                          
*                                                                               
*              AT ENTRY, R5=A(DESTINATION ADCON) AND AIO=A(SOURCE)              
*                                                                               
SAVREC   NTR1                                                                   
         L     R2,0(R5)            R2=A(DESTINATION)                            
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         SR    R5,R5                                                            
         ICM   R5,3,ACLENGTH                                                    
         LA    R3,1000             FULL IO AREA LENGTH                          
         MVCL  R2,R4                RECORD FROM IO TO ACIOS                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              GET COMMISSION RATE AND ADD TO ACGROSS                 *         
***********************************************************************         
*                                                                               
         USING ACKEYD,R4                                                        
GETRULES NTR1                                                                   
         L     R4,AIO                                                           
         CLI   GOPAYNET,C'Y'                                                    
         BE    EXIT                NO COMMISSION                                
*                                                                               
         MVC   GOAKEY,AIO                                                       
         MVC   GOSELWC,ACKEYWRK                                                 
         GOTO1 GETOPT,DMCB,AGOBLOCK                                             
         XC    GOSELWC,GOSELWC                                                  
         XC    GOAKEY,GOAKEY                                                    
*                                                                               
         ZAP   DUB,GOAGYCOM                                                     
         ZAP   PL13,ACNET                                                       
         MP    PL13,DUB            NET X RATE                                   
         SRP   PL13,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         AP    ACGROSS,PL13        ADD COMMISSION                               
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
         DROP  R4                                                               
ADDL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         SPACE 1                                                                
         USING ACKEYD,R2                                                        
         MVC   HALF,ACLENGTH                                                    
         ZIC   R4,1(R3)                                                         
         AH    R4,HALF                                                          
         CH    R4,=H'999'                                                       
         BNH   *+6                                                              
         DC    H'0'                RECORD TOO LONG (SYST 5)                     
         SPACE 1                                                                
         GOTO1 HELLO,ELIST,(C'P',=C'ACCOUNT '),(R2),(R3)                        
         CLI   ELERR,0                                                          
         BE    EXIT                                                             
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         SPACE 2                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
DELL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         SPACE 1                                                                
         CLI   ELERR,0                                                          
         BNE   EXIT                                                             
         GOTO1 HELLO,ELIST,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     EXIT                                                             
         SPACE 2                                                                
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     EXIT                                                             
         EJECT                                                                  
ACHIGH   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   DMFILE,=C'ACCOUNT'                                               
         TM    SVTRACE,X'80'                                                    
         BZ    ACGET                                                            
         LA    R0,KEYSAVE          SET TRACE PARAMETERS                         
         ST    R0,TRIO1                                                         
         MVI   TRIO1,42                                                         
         MVC   TRIO2,AIO                                                        
         MVI   TRIO2,42                                                         
         B     ACGET                                                            
*                                                                               
ACSEQ    NTR1                                                                   
         MVC   COMMAND,=C'DMRSEQ'                                               
         MVC   DMFILE,=C'ACCOUNT'                                               
         TM    SVTRACE,X'80'                                                    
         BZ    ACGET                                                            
         MVC   TRIO1,AIO                                                        
         MVI   TRIO1,42                                                         
         XC    TRIO2,TRIO2                                                      
         SPACE 1                                                                
ACGET    DC    0H'0'                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,AIO                    
         L     R2,AIO                                                           
         MVC   KEY(42),0(R2)                                                    
         TM    SVTRACE,X'80'                                                    
         BZ    *+8                                                              
         BAS   RE,ACTRACE                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              ERROR ROUTINES                                                   
         SPACE 1                                                                
         USING SPOOLD,R8                                                        
BADAGY   L     R8,ASPOOLD                                                       
         GOTO1 VPRTRULE,PARAS,0    PRINT ALL RULES                              
         MVC   P+1(30),=CL30'** ERROR - AGY DF INVALID **'                      
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADCLI   L     R8,ASPOOLD                                                       
         GOTO1 VPRTRULE,PARAS,0    PRINT ALL RULES                              
         MVC   P+1(30),=CL30'** ERROR - BAD CLIENT CODE **'                     
         B     ACTRAC2                                                          
BADPRD   L     R8,ASPOOLD                                                       
         GOTO1 VPRTRULE,PARAS,0                                                 
         MVC   P+1(30),=CL30'** ERROR - BAD PRODUCT CODE **'                    
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADJOB   L     R8,ASPOOLD                                                       
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         MVC   P+1(36),=CL36'** ERROR - INCONSISTENT JOB RULES **'              
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADWCD   L     R8,ASPOOLD                                                       
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         MVC   P+1(41),=CL36'** ERROR - INCONSISTENT WORKCODE RULES **'         
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADAF1   L     R8,ASPOOLD                                                       
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         MVC   P+1(36),=CL36'** ERROR - INCONSISTENT AF1 RULES **'              
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADAF2   L     R8,ASPOOLD                                                       
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         MVC   P+1(36),=CL36'** ERROR - INCONSISTENT AF2 RULES **'              
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADAF3   L     R8,ASPOOLD                                                       
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         MVC   P+1(36),=CL36'** ERROR - INCONSISTENT AF3 RULES **'              
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADAF4   L     R8,ASPOOLD                                                       
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         MVC   P+1(36),=CL36'** ERROR - INCONSISTENT AF4 RULES **'              
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADAF5   L     R8,ASPOOLD                                                       
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         MVC   P+1(36),=CL36'** ERROR - INCONSISTENT AF5 RULES **'              
         B     ACTRAC2                                                          
         EJECT                                                                  
ACTRACE  NTR1                                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     RE,DMCB                                                          
         MVC   P(6),0(RE)                                                       
         L     RE,DMCB+4                                                        
         MVC   P+8(7),0(RE)                                                     
*                                                                               
         LA    R4,P+16                                                          
         ZIC   R0,TRIO1                                                         
         GOTO1 HEXOUT,DMCB,TRIO1,(R4),(R0),=C'TOG'                              
*        A     R4,DMCB+16                                                       
*        LA    R4,2(R4)                                                         
*                                                                               
         OC    TRIO2,TRIO2                                                      
         BZ    ACTRAC2             FOR SEQUENTIAL NO SET PARAM                  
         LA    R4,P2+16                                                         
         ZIC   R0,TRIO2            GET OUTPUT REC LEN                           
         GOTO1 HEXOUT,DMCB,TRIO2,(R4),(R0),=C'TOG'                              
*                                                                               
ACTRAC2  GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                RETURN TO CALLER                             
         DROP  R8                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
BUFFPUT  MVC   COMMAND(8),=CL8'BPUT'                                            
         B     GOBUFFER                                                         
*                                                                               
BUFFGET  MVC   COMMAND(8),=CL8'BGET'                                            
*                                                                               
GOBUFFER LR    R0,RE               SAVE CALLING REG                             
         L     R3,ATWA                                                          
         L     RF,TWAVBUFF                                                      
         GOTO1 (RF),DMCB,COMMAND+1,BUFFBUFF,BUFFREC,1                           
         LR    RE,R0               RESTORE CALLING REG                          
*                                                                               
         TM    SVTRACE,X'40'       TEST TRACE BUFFALO                           
         BZR   RE                                                               
*                                                                               
BUFFTRC  NTR1                                                                   
         MVC   WORK(16),DMCB       SAVE DMCB                                    
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVC   P(5),COMMAND                                                     
*                                                                               
         L     R4,BUFFRULE                                                      
         LA    R5,QRNODE-QRD(R4)                                                
         GOTO1 HEXOUT,DMCB,(R5),P+7,4,=C'TOG'                                   
*                                                                               
         LA    R5,QRCODE-QRD(R4)                                                
         MVC   P+17(8),0(R5)                                                    
*                                                                               
         GOTO1 HEXOUT,DMCB,BUFFREC,P+27,32,=C'TOG'                              
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK+8,P+92,1,=C'TOG'                                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   DMCB(16),WORK       RESTORE DMCB                                 
         B     EXIT                                                             
*                                                                               
BUFFBUFF DS    A                                                                
*                                                                               
         SPACE 1                                                                
* TABLE OF VALID EXTRACT TYPES FOR PRODUCTION EXTRACT                           
*                                                                               
EXTTBL   DS    0CL3                                                             
         DC    AL1(EXORD),AL1(EXORDB),AL1(0)                                    
         DC    AL1(EXBADV),AL1(EXBADVB),AL1(0)                                  
         DC    AL1(EXBDATE),AL1(EXBDATB),AL1(0)                                 
         DC    AL1(EXORDN),AL1(EXORDNB),AL1(0)                                  
         DC    AL1(EXBNADV),AL1(EXBNADVB),AL1(0)                                
         DC    AL1(EXBNDATE),AL1(EXBNDATB),AL1(0)                               
         DC    AL1(EXBINV),AL1(EXBINVB),AL1(0)                                  
         DC    AL1(EXBNINV),AL1(EXBNINVB),AL1(0)                                
         DC    AL1(EXBAADV),AL1(0),AL1(EXBAADVB)                                
         DC    AL1(EXBADAT),AL1(0),AL1(EXBADATB)                                
         DC    AL1(EXBAINV),AL1(0),AL1(EXBAINVB)                                
EXTYPES  EQU   (*-EXTTBL)/L'EXTTBL                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
XFF      DC    16X'FF'                                                          
RELO     DC    A(0)                                                             
*                                                                               
COREMODS DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    X'8485'                                                          
CORES    EQU   (*-COREMODS)                                                     
*                                                                               
COLFLDH  DC    AL1(L'COLFLD+8),4X'00',AL1(L'COLFLD),2X'00'                      
COLFLD   DC    C'CE,CEG'                                                        
*                                                                               
ORDEREDB EQU   EXORDB+EXORDNB                                                   
BILLEDB  EQU   EXBADVB+EXBNADVB+EXBDATB+EXBNDATB+EXBINVB+EXBNINVB               
*                                                                               
         BUFF  LINES=250,ROWS=1,COLUMNS=3,FLAVOR=PACKED,               X        
               KEYLIST=(8,A)                                                    
*                                                                               
ACIOS    DS    (ADCONS*1000)C                                                   
*                                                                               
COLISTL  EQU   400                                                              
COLIST   DC    (COLISTL)X'00'                                                   
*                                                                               
COLTABL  EQU   20000                                                            
COLTAB   DC    (COLTABL)X'00'                                                   
*                                                                               
OPVTABL  EQU   24000                                                            
OPVTAB   DC    (OPVTABL)X'00'                                                   
*                                                                               
GOBUFFL  EQU   400                                                              
GOBUFF   DC    (GOBUFFL)X'00'                                                   
*                                                                               
JOBIO    DC    2000X'00'                                                        
*                                                                               
         EJECT                                                                  
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
ACWKD    DSECT                                                                  
ADLIST   DS    0A                  ADCON LIST                                   
ADCOMP   DS    A                   A(COMPANY RECORD)                            
ADUNIT   DS    A                   A(UNIT RECORD)                               
ADLEDGER DS    A                   A(LEDGER RECORD)                             
ADHEIRA  DS    A                   A(CLIENT RECORD)                             
ADHEIRB  DS    A                   A(PRODUCT RECORD)                            
ADHEIRC  DS    A                   A(JOB RECORD)                                
         ORG   ADHEIRC                                                          
ADACC    DS    A                                                                
ADCONS   EQU   (*-ADLIST)/4                                                     
*                                                                               
CORETAB  DS    0A                  CORE RESIDENT PHASES                         
GETOPT   DS    A                   T00A84                                       
JOBBER   DS    A                   T00A85                                       
*                                                                               
AACIOS   DS    A                   A(ACIOS)                                     
ADLDGHIR DS    A                   A(LEDGER ELEMENT)                            
AFSTRULE DS    A                                                                
AFSTEST  DS    A                   A(FIRST WORKCODE ESTIMATE ELEMENT)           
ESTPER   DS    XL3                 JOB OPEN DATE PERIOD (FOR ESTIMATES)         
BATCHPER DS    XL2                 BATCH HEADER MONTH PERIOD                    
DATPER   DS    XL2                 PERIOD FOR BILL RUN ON DATE                  
INVPER   DS    XL2                 INVOICE DATE PERIOD                          
RCCOMPFL DS    CL1                                                              
*                                                                               
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
*                                                                               
MOSTAB   DS    12CL4            2 BYTE BINARY YYMM/2 BYTE MOS                   
STARTPER DS    XL2                 START PERIOD (YYMM)                          
ENDPER   DS    XL2                 END PERIOD (YYMM)                            
DATAINDS DS    0XL2                                                             
DATAIND  DS    X                                                                
DATAIND1 DS    X                                                                
NEXTYPS  DS    X                   N'EXTRACT TYPES                              
EXTYPS   DS    CL(MAXDTYP)                                                      
*                                                                               
WKCODE   DS    CL2                                                              
FILTSW   DS    CL1                                                              
SIGN     DS    C                                                                
CLTFIL   DS    CL5                 CLIENT FILTERS (AF1 - AF5)                   
PRDFIL   DS    CL5                 PRODUCT FILTERS                              
JOBFIL   DS    CL5                                                              
COMPFIL  DS    CL5                                                              
PL13     DS    PL13                                                             
ACNET    DS    PL8                                                              
ACGROSS  DS    PL8                                                              
CASHD    DS    PL6                                                              
STKEY    DS    CL15                START KEY C/U/L/CLI/PRD/(M)                  
STKEYLN  DS    CL1                 LENGTH OF STKEYLN FOR EX                     
STJOB    DS    CL1                 DISP. TO JOB FIELD                           
STJOBLN  DS    CL1                 LENGTH OF JOB FILED                          
STPRDLN  DS    CL1                 COMPARE LENGTH FOR CLIENT/PRODUCT            
LENREST  DS    CL1                 L'KEY AFTER MEDIA                            
LASTPRD  DS    CL(L'ACKEYACC)      LAST CLIENT/PRODUCT                          
*                                                                               
ACOLIST  DS    A                                                                
ACOLTAB  DS    A                                                                
LCOLTAB  DS    F                                                                
AOPVTAB  DS    A                                                                
LOPVTAB  DS    F                                                                
AGOBUFF  DS    A                                                                
LGOBUFF  DS    F                                                                
AJOBIO   DS    A                                                                
AGOBLOCK DS    A                                                                
VJOBCOL  DS    V                                                                
         EJECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         EJECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
ACWKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE ACJOBBERD                                                      
         EJECT                                                                  
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
       ++INCLUDE BUEXTWORKD                                                     
         EJECT                                                                  
*  BUPPERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE BUPPERD                                                        
         PRINT ON                                                               
*  DDCOMFACS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*  ACGENBOTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064BUFIL46   05/01/02'                                      
         END                                                                    
