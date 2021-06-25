*          DATA SET REPRO28    AT LEVEL 002 AS OF 08/16/00                      
*&&      SET   NOP=N                                                            
*PHASE T80A28A                                                                  
T80A28   TITLE 'REPRO28 - ADD PROPOSAL FOR WORK/UPDATE'                         
**********************************************************************          
* CALLER MUST HAVE COMPELETED VALCON IN REPRO01                                 
**********************************************************************          
PRO28    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 OVERWRKQ,REPRO28*,R7,RR=RE                                       
         USING OVERWRKD,RC                                                      
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
*                                                                               
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         ST    RE,RELO                                                          
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*----------------------------------------------                                 
         OC    CSARBKS,CSARBKS     ANY BOOKS FROM THE CONTRACT SAR?             
         BZ    EXITL               NONE - WE'ARE DOOMED                         
*                                                                               
         XC    DISBOOKS,DISBOOKS                                                
         LA    RE,DISBOOKS                                                      
         USING BOOKLIN,RE                                                       
         LA    R3,CSARBKS                                                       
*                                                                               
SETBKS10 CLI   0(R3),0                USER DEFINED BOOK?                        
         BNE   SETBKS20               YES                                       
*                                                                               
         OC    0(5,R3),0(R3)          ANY BOOK?                                 
         BZ    SETBKSX                NO MORE BOOKS                             
*                                                                               
         MVC   BKLNBK,2(R3)           SAVE BOOK BYTES                           
         MVC   BKLNSPBK,1(R3)         SPECIAL BOOK TYPE                         
*                                                                               
         LA    RE,BKLNLENQ(RE)            BUMP TO THE NEXT BOOK                 
SETBKS20 DS    0H                                                               
         LA    R3,5(R3)                                                         
         LA    R0,CSARBKS+L'CSARBKS                                             
         CR    R3,R0                                                            
         BL    SETBKS10                                                         
         DROP  RE                                                               
*                                                                               
SETBKSX  DS    0H                                                               
         OC    DISBOOKS,DISBOOKS   ANY BOOKS FROM THE CONTRACT SAR?             
         BZ    EXITL               NONE - WE'ARE DOOMED                         
*                                                                               
*----------------------------------------------                                 
         OC    CSARDEM,CSARDEM     ANY DEMOS FROM THE CONTRACT SAR?             
         BZ    EXITL               NONE - WE'ARE DOOMED                         
*                                                                               
         XC    DISDEMOS,DISDEMOS   COPY THE CONTRACT'S DEMOS                    
         LA    RE,DISDEMOS                                                      
         USING DEMOLIN,RE                                                       
         LA    RF,CSARDEM                                                       
         LA    R0,CSARDEM+L'CSARDEM                                             
SETDEM10 MVC   DMLNDEMO,0(RF)                                                   
         LA    RE,DMLNLENQ(RE)                                                  
         LA    RF,3(RF)                                                         
         CR    RF,R0                                                            
         BL    SETDEM10                                                         
         DROP  RE                                                               
*----------------------------------------------                                 
         XC    SAVSLNS,SAVSLNS     COPY THE CONTRACT'S LENGTHS                  
         LA    RE,6                   HAS A MAX OF 6 LENGTHS                    
         LA    RF,CSARLEN                                                       
         LA    R1,SAVSLNS                                                       
SETLNS10 OC    0(2,RF),0(RF)          ANY MORE LENGTHS?                         
         BZ    SETLNS20               NO                                        
*                                                                               
         MVC   0(L'SAVSLN,R1),1(RF)   COPY THE LENGTH (1 BYTE)                  
         LA    RF,2(RF)                                                         
         LA    R1,L'SAVSLN(R1)                                                  
         BCT   RE,SETLNS10                                                      
*                                                                               
SETLNS20 DS    0H                                                               
*----------------------------------------------                                 
         XC    SAVDPTS,SAVDPTS     COPY THE CONTRACT'S DAYPARTS                 
         LA    RF,SAVDPTS                                                       
         USING DPTLIN,RF                                                        
         LA    RE,CSARDPT                                                       
         LA    R0,8                                                             
*                                                                               
SETDPT10 DS    0H                                                               
         OC    0(5,RE),0(RE)       END OF DAYPARTS?                             
         BE    SETDPT20            YES                                          
         MVC   DPLNDPT,0(RE)                                                    
         MVC   DPLNCPP,1(RE)                                                    
         TM    CCONFLG1,CCONDPMQ   USE HARD CODED TABLE?                        
         BO    *+8                 YES                                          
         OI    DPLNFLG,RPRDPFMN                                                 
*                                                                               
         LA    RE,5(RE)                                                         
         LA    RF,L'SAVDPT(RF)                                                  
         BCT   R0,SETDPT10                                                      
         DROP  RF                                                               
*                                                                               
SETDPT20 DS    0H                                                               
*----------------------------------------------                                 
         CLI   CCONKSTA,C' '       DISAPPEARING STATION CHECK                   
         BNH   EXITL                                                            
*                                                                               
***********************************************************************         
*                                                                               
***JRD*** GET PROPOSAL NUMBER AND INIT MINIO                                    
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHID+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
*                                                                               
         CLC   IOKEY(RPROKPRO-RPROKEY),IOKEYSAV   SAME UPTO PROPOSAL #?         
         BE    VALPRO10                           YES                           
         LA    R1,X'FF'                                                         
         B     VALPRO15                                                         
*                                                                               
VALPRO10 ZIC   R1,RPROKPRO                                                      
         CLI   RPROKPRO,0                                                       
         BE    EXITL               NO MORE PROPOSAL FOR THIS CONTRACT           
         DROP  R2                                                               
*                                                                               
VALPRO15 BCTR  R1,0                                                             
         STC   R1,BPRONUM                                                       
*                                               INITIALIZE MINIO                
         LA    R2,GSRECKEY                                                      
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
         DROP  R2                                                               
*                                                                               
         GOTOX (MNIOINQ,AREPRO01),BOPARM                                        
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
***************                                                                 
* DESCRIPTION ELEMENT                                                           
***************                                                                 
         L     R6,MINELEM                                                       
         USING RPRDSELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRDSEL,RPRDSELQ                                                 
         MVI   RPRDSLEN,RPRDSLNQ                                                
         MVI   RPRDSFTM,0          FETCH METHOD                                 
         MVC   RPRDSSAL,CCONSAL    SALESPERSON                                  
         MVI   RPRDSBTP,C'I'       BOOK TYPE                                    
         MVC   RPRDSSEC,SAVSLNS    SECONDS LIST                                 
         OI    RPRDSOPT,RPRDSODC   DECIMAL PRECISION ON DEMOS                   
*                                                                               
         BAS   RE,MINIOADD         ADD THE DESCRIPTION ELEMENT                  
         BE    *+6                                                              
         DC    H'0'                                                             
***************                                                                 
* SWITCH ELEMENT                                                                
***************                                                                 
         L     R6,MINELEM                                                       
         USING RPRSWELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRSWEL,RPRSWELQ                                                 
         MVI   RPRSWLEN,RPRSWLNQ                                                
         MVC   RPRSWSAL,CCONSAL                                                 
         MVC   RPRSWOFF,CCONKOFF                                                
         MVC   RPRSWTEM,CCONTEM                                                 
         MVC   RPRSWSTA,CCONKSTA                                                
         MVC   RPRSWADV,CCONKADV                                                
         MVC   RPRSWAGY,CCONKAGY                                                
         MVC   RPRSWAOF,CCONKAOF                                                
         MVC   RPRSWGRP,CCONKGRP                                                
         MVC   RPRSWDSP,CCONDVS                                                 
         MVC   RPRSWDCT,CCONDVT                                                 
         MVC   RPRSWFLT,CCONDAT                                                 
*                                                                               
         BAS   RE,MINIOADD         ADD THE DESCRIPTION ELEMENT                  
         BE    *+6                                                              
         DC    H'0'                                                             
***************                                                                 
* ACTIVITY DATE ELEMENT                                                         
***************                                                                 
         L     R6,MINELEM                                                       
         USING RPRACELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRACEL,RPRACELQ                                                 
         MVI   RPRACLEN,RPRACLNQ                                                
         MVI   RPRACTYP,RPRACADD                                                
         GOTO1 VDATCON,BODMCB,(5,0),(19,RPRACDAT)                               
*                                                                               
         BAS   RE,MINIOADD         ADD THE ACTIVITY ELEMENT                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRACELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRACEL,RPRACELQ                                                 
         MVI   RPRACLEN,RPRACLNQ                                                
         MVI   RPRACTYP,RPRACPHC                                                
         GOTO1 VDATCON,BODMCB,(5,0),(19,RPRACDAT)                               
*                                                                               
         BAS   RE,MINIOADD         ADD THE ACTIVITY ELEMENT                     
         BE    *+6                                                              
         DC    H'0'                                                             
***************                                                                 
* BOOK ELEMENT(S)                                                               
***************                                                                 
RFRABK00 LA    R2,DISBOOKS                                                      
         USING BOOKLIN,R2                                                       
         LA    R0,1                INTERNAL ORDER NUMBER                        
         L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
*                                                                               
RFRABK10 DS    0H                                                               
         OC    0(L'DISBOOK,R2),0(R2)   ANY BOOK DEFINED HERE?                   
         BZ    RFRABKX                                                          
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRBKEL,RPRBKELQ                                                 
         MVI   RPRBKLEN,RPRBKOVQ   L(ELEM IF NOT A USER DEFINED BOOK)           
         STC   R0,RPRBKIOR         INTERNAL ORDER NUMBER                        
*                                                                               
         STC   R0,RPRBKDOR         DISPLAY = INTERNAL IF ADDING                 
         MVC   RPRBKSTT,BKLNBK                                                  
         MVC   RPRBKBYM,BKLNBK+L'RPRBKSTT                                       
         MVC   RPRBKBKT,BKLNSPBK                                                
         MVI   RPRBKFIL,RPRBKINQ   DEFAULT OF INV                               
*                                                                               
         BAS   RE,MINIOADD         ADD THE BOOK ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,L'DISBOOK(R2)    LOOP UNTIL ALL BOOKS COPIED                  
         AH    R0,=H'1'            INCREMENT INTERNAL ORDER NUMBER              
*                                                                               
         LA    RE,DISBOOKS+L'DISBOOKS DID WE PASS THE END OF DISBOOKS?          
         CR    R2,RE                                                            
         BL    RFRABK10            NO, MORE BOOKS                               
*                                                                               
RFRABKX  DS    0H                                                               
         DROP  R2                                                               
***************                                                                 
* DEMO ELEMENT(S)                                                               
***************                                                                 
RFRADM00 LA    R2,DISDEMOS                                                      
         USING DEMOLIN,R2                                                       
         LA    R3,1                INTERNAL ORDER NUMBER                        
         L     R6,MINELEM                                                       
         USING RPRDMELD,R6                                                      
*                                                                               
RFRADM10 OC    DMLNDEMO,DMLNDEMO   END OF DEMO LIST?                            
         BZ    RFRADMX             YES                                          
         CLI   DMLNDEMO,X'FF'      END OF DEMO LIST?                            
         BE    RFRADMX             YES                                          
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRDMEL,RPRDMELQ                                                 
         MVI   RPRDMLEN,RPRDMLNQ                                                
         STC   R3,RPRDMIOR         INTERNAL ORDER NUMBER                        
*                                                                               
         STC   R3,RPRDMDOR         DISPLAY = INTERNAL IF ADDING                 
         MVI   RPRDMSRC,C'N'       NEILSEN SO FAR                               
         MVC   RPRDMBY1(L'DMLNDEMO),DMLNDEMO                                    
*                                                                               
         BAS   RE,MINIOADD         ADD THE DEMO ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,L'DISDEMO(R2)    LOOP UNTIL ALL BOOKS COPIED                  
         LA    R3,1(R3)            INCREMENT INTERNAL ORDER NUMBER              
*                                                                               
         LA    RE,DISDEMOS+L'DISDEMOS                                           
         CR    R2,RE                                                            
         BL    RFRADM10            NO                                           
         DROP  R2                                                               
*                                                                               
RFRADMX  DS    0H                                                               
***************                                                                 
* STATION ELEMENT                                                               
***************                                                                 
         L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRSTEL,RPRSTELQ                                                 
         MVI   RPRSTLEN,RPRSTLNQ                                                
         MVI   RPRSTICD,X'01'      CONTRACT STATION IS 1ST STATION              
*                                                                               
         MVC   RPRSTSTA,CCONKSTA                                                
*                                                                               
         BAS   RE,MINIOADD         ADD THE STATION ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***************                                                                 
* DAYPART ELEMENT(S)                                                            
***************                                                                 
RFRADP00 LA    R1,1                                                             
         LA    R2,SAVDPTS                                                       
         USING DPTLIN,R2                                                        
         L     R6,MINELEM                                                       
         USING RPRDPELD,R6                                                      
*                                                                               
RFRADP10 OC    0(L'SAVDPT,R2),0(R2)   ANY DAYPART?                              
         BZ    RFRADP20               NO SKIP                                   
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRDPEL,RPRDPELQ                                                 
         MVI   RPRDPLEN,RPRDPLNQ                                                
         STC   R1,RPRDPSEQ         INTERNAL ORDER NUMBER                        
*                                                                               
         MVC   RPRDPFLG,DPLNFLG       COPY FLAGS                                
         MVC   RPRDPDPT,DPLNDPT       COPY 1-BYTE DAYPART CODE                  
         MVC   RPRDPTAB,DPLNCPP       COPY 4 BYTE BUYER CPP                     
         DROP  R2                                                               
*                                                                               
         BAS   RE,MINIOADD         ADD DAYPART ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,1(R1)            LOOP UNTIL ALL DAYPARTS COPIED               
RFRADP20 LA    R2,L'SAVDPT(R2)                                                  
         LA    RE,SAVDPTS+L'SAVDPTS  DID WE PASS THE END OF SAVDPTS?            
         CR    R2,RE                                                            
         BL    RFRADP10              NO, WE CAN LOOP                            
*                                                                               
RFRADPX  DS    0H                                                               
         DROP  R5,R6                                                            
*******************************************************************             
         LR    RE,RA                                                            
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         MVC   0(L'SVPRONUM,RE),BPRONUM                                         
         BAS   RE,MINIOCLS                                                      
*                                                                               
         GOTOX APRG,BODMCB,OIO,IDIRGET    TO SET 'GSRECDA'                      
*                                                                               
         XC    IOKEY,IOKEY         BUILD PASSIVE 'C301' KEY                     
         LA    R6,IOKEY                                                         
         USING RPROKEY,R6                                                       
         MVI   RPROPTYP,RPROPTYQ                                                
         MVI   RPROPSTY,RPROPSBQ                                                
         MVC   RPROPRCD,CUAALF                                                  
         MVC   RPROPSAL,CCONSAL                                                 
         MVC   RPROPSTA,CCONKSTA                                                
         MVC   RPROKDA,GSRECDA                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         MVC   RPROPCON,0(RE)                                                   
         MVC   RPROPPRO,SVPRONUM-SVCONNUM(RE)                                   
         DROP  R6                                                               
*                                                                               
         L     R1,=AL4(XOREPDIR+XOADD+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    IOKEY,IOKEY         BUILD PASSIVE 'C302' KEY                     
         LA    R6,IOKEY                                                         
         USING RPROKEY,R6                                                       
         MVI   RPROOTYP,RPROOTYQ                                                
         MVI   RPROOSTY,RPROOSBQ                                                
         MVC   RPROORCD,CUAALF                                                  
         MVC   RPROOOFF,CCONKOFF                                                
         MVC   RPROOSAL,CCONSAL                                                 
         MVC   RPROKDA,GSRECDA                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         MVC   RPROOCON,0(RE)                                                   
         MVC   RPROOPRO,SVPRONUM-SVCONNUM(RE)                                   
         DROP  R6                                                               
*                                                                               
         L     R1,=AL4(XOREPDIR+XOADD+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
EXITL    CLI   *,X'FF'             SET CC LOW                                   
         MVC   FVMSGNO,=AL2(701)                                                
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     DS    0H                                                               
         XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIORD  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINRD',(R5))                                     
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS HIGH FOR A MINIO ELEMENT.                                  
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIOHI  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINHI',(R5))                                     
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         B     EXITL               OTHERWISE RETURN 'NO'                        
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS SEQUENTIAL FOR A MINIO ELEMENT.                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOSEQ NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINSEQ',(R5))                                    
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    EXITL                                                            
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOWRT NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         GOTO1 VMINIO,BODMCB,('MINWRT',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.                                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOADD NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         GOTO1 VMINIO,BODMCB,('MINADD',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    EXITL               YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIODEL NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINDEL',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         ZIC   RF,MINNBUF          NUMBER OF BUFFERS BEING USED                 
         L     RE,MINBUFF          A(1ST BUFFER BEING USED BY MINIO)            
         USING RPROHDRD,RE                                                      
MNIODEL2 CLC   RPRORLEN,=Y(RPROR1ST-RPROHDRD)   ANY ELEMENTS IN RECORD?         
         BH    MNIODEL4                         YES, CHECK NEXT BUFFER          
*                                                                               
         MVC   RPRORLEN,=Y(RPROR1ST-RPROHDRD+2) DMDALINK: MIN IS 36             
         OI    RPRORSTA,X'80'                   MARK FOR DELETE                 
         LA    R1,RPROR1ST                                                      
         XC    0(3,R1),0(R1)                    FAKE ELEMENT                    
*                                                                               
MNIODEL4 AH    RE,MINFRCLM         BUMP TO NEXT MINIO BUFFER                    
         BCT   RF,MNIODEL2         LOOP UNTIL ALL BUFFERS CHECKED               
*                                                                               
MNIODELX DS    0H                                                               
         B     EXITOK                                                           
         DROP  R5,RE                                                            
***********************************************************************         
* THIS ROUTINE CLOSES MINIO AND FLUSHES OUT THE BUFFERS TO THE MINIO            
* RECORDS.                                                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOCLS NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINCLS',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
AFRREL   DS    A                                                                
AFVADDR  DS    A                                                                
*                                                                               
RELO     DS    A                                                                
*                                                                               
DISBOOKS DS    0XL(NUMBKS*BKLNLENQ)  DISPLAY BOOKS AND UPGRADES                 
DISBOOK  DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
DISDEMOS DS    0XL(NUMDEMS*DMLNLENQ) DISPLAY DEMOS                              
DISDEMO  DS    (NUMDEMS)XL(DMLNLENQ)                                            
         DS    XL(DMLNLENQ)          EOT                                        
*                                                                               
SAVDPTS  DS    0CL(NUMDPTS*DPLNLENQ) SAVED DAYPARTS                             
SAVDPT   DS    (NUMDPTS)XL(DPLNLENQ)                                            
         DS    XL(DPLNLENQ)          EOT                                        
*                                                                               
SAVSLNS  DS    0CL(6*1)              SAVED 1-BYTE SPOT LENGTHS                  
SAVSLN   DS    6XL1                                                             
*                                                                               
OVERWRKQ EQU   *-OVERWRKD          LENGTH OF WORKING STORAGE                    
         EJECT                                                                  
       ++INCLUDE REPROLN                                                        
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REPRO28   08/16/00'                                      
         END                                                                    
