*          DATA SET NENAV11    AT LEVEL 041 AS OF 10/09/20                      
*PHASE T31811A                                                                  
*&&ONLIN SET   Y                    ONLINE ONLY PROGRAM                         
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31811   TITLE 'F NAV11 - STEWARD - HISTORY RECORD OVERLAY'                     
T31811   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV11**,RA                                                    
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     R7,ATWA                                                          
         USING TWAD,R7                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSAREC,ANETBLK                                                   
*                                                                               
         MVI   SEQNUM,1                                                         
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
*  GET SECURITY AGENCY AND PASSWORD                                             
*                                                                               
         L     R4,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,R4)                                        
         GOTO1 (RF),DMCB,0                                                      
*                                                                               
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         MVC   SECAGY,FATAGYSC     SAVE SECURITY AGENCY                         
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVPASSWD,FAPASSWD   SAVE PERSONAL ID                             
         DROP  RE                                                               
*                                                                               
         MVI   TSACTN,TSAGET        GET FIRST RECORD FROM TSAR                  
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                 MUST BE ONE RECORD                          
         B     CHAPR050                                                         
*                                                                               
*  PROCESS RETURNED BUY INFORMATION                                             
*  SEND NEXT TSAR RECORD TO THE BUY                                             
*                                                                               
CHAPR020 MVI   TSACTN,TSANXT        READ NEXT TSAR RECORD                       
         BAS   RE,CALLTSAR                                                      
         BNE   EXIT                                                             
*                                                                               
*  READ HISTORY RECORDS PASS INFO BACK TO THE PC                                
*                                                                               
CHAPR050 BAS   RE,GETPAK                                                        
         BAS   RE,GETHIST                                                       
         BAS   RE,SNDHIST                                                       
*                                                                               
****     ZIC   RE,SEQNUM            BUMP TO TTHE NEXT SEQUENCE NUMBER           
****     LA    RE,1(RE)                                                         
****     STCM  RE,1,SEQNUM                                                      
****     B     CHAPR020                                                         
         SPACE 3                                                                
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  READ THE PKG RECORD GET THE DAYPART CODE                                     
*                                                                               
GETPAK   NTR1                                                                   
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
         CLC   RUPCLI(13),PAKCLI                                                
         BE    GETPAKEX                                                         
         L     R4,AIO1                                                          
         USING NPRECD,R4                                                        
         XC    ERROR,ERROR                                                      
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,BAGYMD                                                     
         GOTO1 VCLPACK,DMCB,RUPCLI,NPKCLT                                       
         MVC   NPKNET,RUPNET                                                    
         LA    R6,RUPEST                                                        
         MVI   BYTE,3                                                           
         BRAS  RE,GETBINRY                                                      
         MVC   NPKEST,BYTE                                                      
         LA    R6,RUPPACK                                                       
         MVI   BYTE,3                                                           
         BRAS  RE,GETBINRY                                                      
         MVC   NPKPACK,BYTE                                                     
*                                                                               
* CHECK TO SEE IF WE ALREADY READ THIS PACKAGE                                  
         CLC   PAKCLI,NPKCLT                                                    
         BNE   GETPAK40                                                         
         CLC   PAKEST,NPKEST                                                    
         BNE   GETPAK40                                                         
         CLC   PAKNET,NPKNET                                                    
         BNE   GETPAK40                                                         
         CLC   PAKPAK,NPKPACK                                                   
         BE    GETPAKEX                                                         
*                                                                               
GETPAK40 GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(19),KEYSAVE                                                  
         BE    GETPAK50                                                         
         DC    H'0'                                                             
*                                                                               
GETPAK50 GOTO1 AIOCALL,DMCB,UNT+FIL+GET,AIO                                     
         L     R4,AIO                                                           
         MVC   PAKCLI,NPKCLT                                                    
         MVC   PAKEST,NPKEST                                                    
         MVC   PAKNET,NPKNET                                                    
         MVC   PAKPAK,NPKPACK                                                   
         MVC   PAKDPT,NPAKDP                                                    
         MVC   PAKCNTL,NPAKCNTL                                                 
         MVC   PAKSTAT,NPAKSTAT                                                 
GETPAKEX B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
*  READ THE HISTORY RECORD INTO AIO1                                            
*                                                                               
GETHIST  NTR1                                                                   
         L     R4,AIO1                                                          
         USING NHRECD,R4                                                        
         XC    ERROR,ERROR                                                      
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         MVI   NHKTYPE,X'40'                                                    
         MVC   NHKPAM,BAGYMD                                                    
         GOTO1 VCLPACK,DMCB,RUPCLI,BINCLT                                       
         MVC   NHKPCLT,BINCLT                                                   
         MVC   NHKNET,RUPNET                                                    
         MVC   NHKPROG,RUPPCODE                                                 
         GOTO1 VDATCON,DMCB,(4,RUPBDATE),(2,NHKDATE)                            
         LA    R6,RUPEST                                                        
         MVI   BYTE,3                                                           
         BRAS  RE,GETBINRY                                                      
         MVC   NHKEST,BYTE                                                      
         MVI   BYTE,1               DEFAULT LINE NUMBER                         
         OC    RUPSLINE,RUPSLINE                                                
         BZ    GETHST30                                                         
         LA    R6,RUPSLINE                                                      
         MVI   BYTE,3                                                           
         BRAS  RE,GETBINRY                                                      
GETHST30 MVC   NHKSUB,BYTE                                                      
         MVC   NHKDP,PAKDPT                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(19),KEYSAVE                                                  
         BE    GETHST50                                                         
         MVI   ERROR,NOTFOUND                                                   
         B     GETHSTEX                                                         
*                                                                               
GETHST50 GOTO1 AIOCALL,DMCB,UNT+FIL+GET,AIO                                     
         L     R4,AIO                                                           
*                                                                               
* READ THE CLIENT RECORD                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BINCLT                                                  
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 CLIENT RECORD MUST EXIST                    
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO2                                    
GETHSTEX B     EXIT                                                             
         DROP  R4,R5                                                            
*                                                                               
         EJECT                                                                  
*=================================================================*             
* SEND SPLIT INFO TO THE PC                                       *             
*=================================================================*             
         SPACE 1                                                                
SNDHIST  NTR1                                                                   
*                                                                               
         LHI   R1,X'58'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  CHECK IF ERROR RETURNED                                                      
*                                                                               
         OC    ERROR,ERROR                                                      
         BZ    SNP010                                                           
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
*                                                                               
         LA    R4,SEQNUM                                                        
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERROR                                                         
         LHI   R1,X'39'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VGETMSG,DMCB+12,(ERROR,WORK),(X'FF',DMCB),(7,0)                  
         LA    R4,WORK+8                                                        
         LHI   R1,X'3A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERMXFLD                                                       
         LHI   R1,X'3B'                                                         
         BAS   RE,SENDD                                                         
         MVI   BUYERRSW,C'N'                                                    
         B     SNPEX                                                            
         DROP  R6                                                               
*                                                                               
SNP010   L     R6,AIO1              GET HISTORY RECORD                          
         USING NHRECD,R6                                                        
*                                                                               
         LA    R4,SEQNUM            SEQUENCE NUMBER                             
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NHADDAT,NHADDAT                                                  
         BZ    SNP020                                                           
         LA    R4,NHADDAT           DATE UNIT BOUGHT                            
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNP020   GOTO1 VHEXOUT,DMCB,NHADDTM,WORK,4     TIME UNIT BOUGHT                 
         MVC   WORK+20(2),WORK+1                                                
         MVI   WORK+22,C':'                                                     
         MVC   WORK+23(2),WORK+3                                                
         MVI   WORK+25,C':'                                                     
         MVC   WORK+26(2),WORK+5                                                
         LHI   R1,X'03'                                                         
         LA    R4,WORK+20                                                       
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NHODATE,NHODATE                                                  
         BZ    SNP030                                                           
         LA    R4,NHODATE           BUY DATE                                    
         LHI   R1,X'04'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNP030   MVI   BYTE,C'O'            ORIGINAL UNIT                               
         TM    NHSTAT,X'01'                                                     
         BZ    *+8                                                              
         MVI   BYTE,C'N'            NEW UNIT                                    
         LA    R4,BYTE                                                          
         LHI   R1,X'05'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NHREASN,NHREASN                                                  
         BZ    SNP050                                                           
         LA    R4,NHREASN           REASON CODE                                 
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNP050   OC    NHOPRNME,NHOPRNME                                                
         BZ    SNP055                                                           
         LA    R4,NHOPRNME          PROGRAM NAME                                
         LHI   R1,X'07'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNP055   OC    NHOROT,NHOROT                                                    
         BZ    SNP060                                                           
         LA    R4,NHOROT            ROTATION                                    
         LHI   R1,X'08'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNP060   LA    R4,NHOLEN            LENGTH                                      
         LHI   R1,X'09'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         CLI   NHMAINLN,158                                                     
         BL    SNP070                                                           
         MVC   GETPRDOT,NHPROD3                                                 
         B     SNP080                                                           
SNP070   MVC   GETPRDIN,NNHOPRD     1 BYTE PRODUCTS                             
         BRAS  RE,GETPRDS                                                       
SNP080   OC    GETPRDOT,GETPRDOT                                                
         BZ    SNP085                                                           
         LA    R4,GETPRDOT          PRODUCT                                     
         LHI   R1,X'0A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNP085   OC    NHOACT,NHOACT                                                    
         BZ    SNP090                                                           
         LA    R4,NHOACT            ACTUAL COST                                 
         LHI   R1,X'0B'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNP090   OC    NHUSER,NHUSER                                                    
         BZ    SNP095                                                           
         LA    R4,NHUSER            USER CODE (PID)                             
         LHI   R1,X'0C'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNP095   OC    NHADTGRP,NHADTGRP                                                
         BZ    SNP100                                                           
         LA    R4,NHADTGRP          AUDIT GROUP                                 
         LHI   R1,X'0D'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNP100   XC    WORK,WORK            BUY TIME                                    
         GOTO1 VUNTIME,DMCB,NHOTIME,WORK                                        
         LHI   R1,X'0E'                                                         
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
*  MOVE OUT ORIGINAL MISSED OR MAKEGOOD INFO                                    
*                                                                               
         USING NHMGD,R3                                                         
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R6)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SNP120                                                           
         L     R3,12(R1)                                                        
         B     SNP130                                                           
*                                                                               
SNP120   GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',(R6)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SNP150                                                           
         L     R3,12(R1)                                                        
*                                                                               
SNP130   MVI   BYTE,C'M'                                                        
         CLI   NHMGEL,X'02'         IS THIS MISSED INF                          
         BE    *+8                                                              
         MVI   BYTE,C'G'            NO SET INDICATOR FOR MAKEGOOD               
         LA    R4,BYTE              MISSED/MAKEGOOD INDICATOR                   
         LHI   R1,X'10'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,2(R3)             PROGRAM CODE                                
         LHI   R1,X'11'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,8(R3)             PROGRAM NAME                                
         LHI   R1,X'12'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,24(R3)             DATE                                       
         LHI   R1,X'13'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,26(R3)             LINE #                                     
         LHI   R1,X'14'                                                         
         BAS   RE,SENDD                                                         
         DROP  R3                                                               
*                                                                               
* MOVE OUT THE CHANGE INFORMATION                                               
*                                                                               
SNP150   DS    0H                                                               
                                                                                
         USING NCHAEL,R3                                                        
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'05',(R6)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SNPEX                                                            
         L     R3,12(R1)                                                        
* MOVE USER AND REASON CODE OUT                                                 
*                                                                               
SNP170   LA    R4,NCHGUSER           USER CODE                                  
         LHI   R1,X'20'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NCHGDATE           CHANGE DATE                                
         LHI   R1,X'37'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,NCHGTIME,WORK,4     CHANGE TIME                     
         MVC   WORK+20(2),WORK+1                                                
         MVI   WORK+22,C':'                                                     
         MVC   WORK+23(2),WORK+3                                                
         MVI   WORK+25,C':'                                                     
         MVC   WORK+26(2),WORK+5                                                
         LHI   R1,X'38'                                                         
         LA    R4,WORK+20                                                       
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NCHGREAS           REASON CODE                                
         LHI   R1,X'21'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NCHGFCOD           CHANGE CODE                                
         LHI   R1,X'22'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
* GET CHANGE CODE BRANCH TO OUTPUT ROUTINE                                      
*                                                                               
         CLI   NCHGFCOD,C'G'        MAKEGOOD                                    
         BE    SNP200                                                           
         CLI   NCHGFCOD,C'M'        MISSED                                      
         BE    SNP200                                                           
         CLI   NCHGFCOD,C'L'        LENGTH                                      
         BE    SNP220                                                           
         CLI   NCHGFCOD,C'A'        ACTUAL COST                                 
         BE    SNP240                                                           
         CLI   NCHGFCOD,C'D'        DATE                                        
         BE    SNP260                                                           
         CLI   NCHGFCOD,C'T'        TIME                                        
         BE    SNP280                                                           
         CLI   NCHGFCOD,C'N'        PROGRAM NAME                                
         BE    SNP300                                                           
         CLI   NCHGFCOD,C'B'        PRODUCT                                     
         BE    SNP320                                                           
         CLI   NCHGFCOD,C'C'        COMMENT                                     
         BE    SNP400                                                           
         CLI   NCHGFCOD,C'P'        PRE-EMPT                                    
         BE    SNP420                                                           
         CLI   NCHGFCOD,C'R'        ROTATION                                    
         BE    SNP440                                                           
*                                                                               
* MISSED AND MAKEGOOD INFO                                                      
SNP200   LA    R2,NCHGFLD                                                       
         USING NUMGD,R2                                                         
*                                                                               
         LA    R4,NUMGPCOD           PROGRAM CODE                               
         LHI   R1,X'23'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NUMGPNM            PROGRAM NAME                               
         LHI   R1,X'24'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NUMGDATE           DATE                                       
         LHI   R1,X'25'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NUMGSUB            SUB LINE NUMBER                            
         LHI   R1,X'26'                                                         
         BAS   RE,SENDD                                                         
         B     SNP1000                                                          
         DROP  R2                                                               
*                                                                               
* LENGTH                                                                        
SNP220   LA    R4,NCHGFLD            LENGTH                                     
         LHI   R1,X'27'                                                         
         BAS   RE,SENDD                                                         
         B     SNP1000                                                          
*                                                                               
* ACTUAL COST                                                                   
SNP240   LA    R4,NCHGFLD            ACTUAL COST                                
         LHI   R1,X'28'                                                         
         BAS   RE,SENDD                                                         
         B     SNP1000                                                          
*                                                                               
* DATE                                                                          
SNP260   LA    R4,NCHGFLD            DATE                                       
         LHI   R1,X'29'                                                         
         BAS   RE,SENDD                                                         
         B     SNP1000                                                          
*                                                                               
* START-END TIME                                                                
SNP280   XC    WORK,WORK                                                        
         GOTO1 VUNTIME,DMCB,NCHGFLD,WORK                                        
         LHI   R1,X'2A'                                                         
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
         B     SNP1000                                                          
*                                                                               
* PROGRAM NAME                                                                  
SNP300   LA    R4,NCHGFLD            PROGRAM NAME                               
         LHI   R1,X'2B'                                                         
         BAS   RE,SENDD                                                         
         B     SNP1000                                                          
*                                                                               
* PRODUCTS                                                                      
SNP320   TM    NCHGSTAT,X'04'                                                   
         BZ    SNP340                                                           
         MVC   GETPRDOT,NCHGFLD                                                 
         B     SNP360                                                           
SNP340   MVC   GETPRDIN,NCHGFLD      1 BYTE PRODUCTS                            
         BRAS  RE,GETPRDS                                                       
SNP360   LA    R4,GETPRDOT           PRODUCT                                    
         LHI   R1,X'2C'                                                         
         BAS   RE,SENDD                                                         
         B     SNP1000                                                          
*                                                                               
* COMMENT                                                                       
SNP400   LA    R4,NCHGFLD            COMMENT                                    
         LHI   R1,X'2D'                                                         
         BAS   RE,SENDD                                                         
         B     SNP1000                                                          
*                                                                               
* PRE-EMPT                                                                      
SNP420   LA    R4,NCHGFLD            PRE-EMPT                                   
         LHI   R1,X'2E'                                                         
         BAS   RE,SENDD                                                         
         B     SNP1000                                                          
*                                                                               
* ROTATION                                                                      
SNP440   LA    R4,NCHGFLD            ROTATION                                   
         LHI   R1,X'2F'                                                         
         BAS   RE,SENDD                                                         
         B     SNP1000                                                          
*                                                                               
*  GET NEXT ELEMENT                                                             
*                                                                               
SNP1000  ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'05'                                                      
         BE    SNP170                                                           
         B     SNPEX                                                            
*                                                                               
SNPEX    B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR TEST                                        *             
*=================================================================*             
         SPACE 1                                                                
CALLTSR2 LR    R0,RE                                                            
         PRINT GEN                                                              
         GOTO1 VTSAR,TSARBLK                                                    
         PRINT NOGEN                                                            
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
CALBUYEX XIT1                                                                   
         EJECT                                                                  
UNTFILE  DC    CL8'UNTFILE'                                                     
NEBLOCKL EQU   NBBLKEND-NETBLOCK+1                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
*  ROUTINE CONVERTS 1 BYTE PRODUCTS TO 3 BYTE ALPHA                             
*  GETPRDIN CONTAINS UP TO 6 1 BYTE PRODUCT CODES                               
*  GETPRDOT WIL CONTAIN THE 3 BYTE ALPHAS                                       
*  CLIENT RECORD IN AIO2                                                        
*                                                                               
GETPRDS  NTR1  BASE=*,LABEL=*                                                   
         L     R5,AIO2                                                          
         USING CLTHDR,R5                                                        
*                                                                               
         XC    GETPRDOT,GETPRDOT    INIT OUTPUT                                 
         LA    R1,GETPRDOT          OUTPUT LIST                                 
         LA    R2,GETPRDIN          INPUT LIST                                  
         LA    R6,6                 MAX NUMBER OF PRODUCTS                      
GETPR010 CLI   0(R2),0              CHECK END OF LIST                           
         BE    GETPREX                                                          
         LA    RE,CLIST             FIRST CLIENT TABLE                          
         LA    RF,220                                                           
*                                                                               
GETPR030 MVC   0(3,R1),0(RE)                                                    
         CLC   3(1,RE),0(R2)                                                    
         BE    GETPR100                                                         
         LA    RE,4(RE)                                                         
         BCT   RF,GETPR030                                                      
*                                                                               
         LA    RE,CLIST2            SECOND TABLE                                
         LA    RF,35                                                            
*                                                                               
GETPR060 MVC   0(3,R1),0(RE)                                                    
         CLC   3(1,RE),0(R2)                                                    
         BE    GETPR100                                                         
         LA    RE,4(RE)                                                         
         BCT   RF,GETPR060                                                      
         DC    H'0'                                                             
*                                                                               
GETPR100 LA    R1,3(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R6,GETPR010                                                      
*                                                                               
GETPREX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*  ROUTINE CONVERTS A VARIABLE LENGTH NUMERIC                                   
*  FIELD INTO BINARY                                                            
*  R6 - ADDRESS OF INPUT FIELD                                                  
*  BYTE - LENGTH OF THE FIELD                                                   
*                                                                               
*  OUTPUT                                                                       
*  BYTE - BINARY NUMBER                                                         
*                                                                               
GETBINRY NTR1  BASE=*,LABEL=*                                                   
         SR    RF,RF                                                            
         ZIC   RE,BYTE                                                          
         LR    R1,R6                                                            
*                                                                               
GTBI040  CLI   0(R1),X'40'                                                      
         BNH   GTBI080                                                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,GTBI040                                                       
*                                                                               
GTBI080  BCTR  RF,0                                                             
         EX    RF,FLDPACK                                                       
         CVB   R0,DUB                                                           
         STCM  R0,1,BYTE                                                        
         J     EXIT                                                             
         SPACE 1                                                                
FLDPACK  PACK  DUB,0(0,R6)                                                      
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
       ++INCLUDE NENAVWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
*                                                                               
SEQNUM   DS    CL1                                                              
*                                                                               
BINCLT   DS    XL2                                                              
*                                                                               
PAKKEY   DS    0CL9                                                             
PAKCLI   DS    CL3                                                              
PAKEST   DS    CL1                                                              
PAKNET   DS    CL4                                                              
PAKPAK   DS    CL1                                                              
PAKDPT   DS    CL1                                                              
PAKCNTL  DS    CL1                                                              
PAKSTAT  DS    CL1                                                              
*                                                                               
SECAGY   DS    CL2                                                              
SVPASSWD DS    CL2                                                              
*                                                                               
ELEM     DS    CL80                                                             
*                                                                               
GETPRDIN DS    CL6                                                              
GETPRDOT DS    CL18                                                             
         DS    0D                                                               
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE NEGENHIST                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAFACTS                                                        
       PRINT ON                                                                 
       ++INCLUDE NAVDSECTS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041NENAV11   10/09/20'                                      
         END                                                                    
