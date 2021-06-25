*          DATA SET PPBUY21    AT LEVEL 029 AS OF 07/07/14                      
*PHASE T41121A                                                                  
*                                                                               
         TITLE 'T41121 - BUY AD-ID UPLOAD'                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CHANGE LOG                                            *         
*                                                                     *         
* KWAN 05/25/06 HANDLE AD CODE TO AD-ID CHANGE FOR TRAFFICKED BUYS    *         
*                                                                     *         
* KWAN 03/21/05 SUPPORT AD-ID UPLOAD FOR ADBUYER                      *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41121 - BUY CUSTOM COLUMN UPLOAD'                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*                                                                     *         
*  CALLED FROM  T41117 (ADBUYER UPLOAD CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     VALIDATES AD-ID DATA MAPS                             *         
*                                                                     *         
*  INPUTS       NO SCREENS                                            *         
*                                                                     *         
*  OUTPUTS      ALL DATA WILL BE RETURNED TO WORKER FILE              *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- LOCAL WORKING STORAGE AREA                      *         
*               R9 -- LOCAL WORKING STORAGE AREA                      *         
*               RA -- TWA                                             *         
*               RB -- BASE REGISTER                                   *         
*               RC -- GEND                                            *         
*               RD -- REGISTER CHAIN                                  *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41121 - BUY CUSTOM COLUMN UPLOAD'                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41121   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORK21X-WORK21D,T41121,RR=RE                                     
*                                                                               
         LR    R8,RC                                                            
         USING WORK21D,R8          R8, R9 = A(LOCAL STORAGE)                    
         LA    R9,WORK21D+4095                                                  
         LA    R9,1(R9)                                                         
         USING WORK21D+4096,R9                                                  
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         ST    RE,RELO21                                                        
         MVI   ERRAREA,0           SET TO NO ERROR                              
*                                                                               
         XC    WORK,WORK           CLEAR RETURN VALUE                           
*                                                                               
         CLI   DDLINKSW,C'N'       ADD INSERTION (NEW)?                         
         BE    *+08                                                             
         CLI   DDLINKSW,C'F'       DRAFT ADD?                                   
         BE    *+08                                                             
         CLI   DDLINKSW,C'C'       CHG INSERTION?                               
         BNE   EXXMOD                                                           
*                                                                               
         BRAS  RE,INITLZ           INITIALIZE WORKING STORAGE AREAS             
*                                                                               
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
         USING LQ_EL,R3                                                         
*                                                                               
         BRAS  RE,CKADMAPC         CHK FOR AD-ID MAP CODES                      
         BNE   EXXMOD                                                           
*                                                                               
         BRAS  RE,VALAD_ID         VALIDATE AD-ID                               
         BNE   EXXMOD                                                           
*                                                                               
         BRAS  RE,SETAD_ID         SET VALIDATED AD-ID                          
         MVC   WORK(L'SAVADCOD),SAVADCOD                                        
         MVC   WORK+L'SAVADCOD(L'SAVAD_ID),SAVAD_ID                             
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTWFELM SR    R0,R0               POINT TO NEXT WORKER FILE ELEM               
         ICM   R0,3,1(R3)                                                       
         AR    R3,R0                                                            
         BR    RE                                                               
*                                                                               
NXTELEM  SR    R0,R0               R5 POINTS TO FIRST BUY RECORD ELEM           
         IC    R0,1(R5)                                                         
         AR    R5,R0               FIRST ELEM IS ALWAYS X'20'                   
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
GET_ETXT L     R2,FULL             MSG NUMBER FROM CALLER                       
         ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    BUYMSG,BUYMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R2),0,(C'E',DMCB),0,0,0                            
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* CHECKING FOR AD-ID MAP CODES (R3 POINTS WRK ELEM)                   *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKADMAPC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RF,R3                                                            
         CLI   LQ_EL,0             ELEM CODES EXIST?                            
         BNE   *+6                                                              
         DC    H'0'                CORRUPTED WRK ELEM                           
*                                                                               
CKADMC20 CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    CKADMC40                                                         
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   CKADMC30                                                         
*                                                                               
         CLC   =AL2(D#ADCODE),3(R3)                                             
         BNE   CKADMC30            AD PRESENT, NO NEED TO PROCESS AD-ID         
         CLI   DDLINKSW,C'C'       CHG INSERTION?                               
         BNE   CKADMCER            AD PRESENT, NO NEED TO PROCESS AD-ID         
         ST    R3,A_ADCDEL         SAVE ADDRESS OF AD CODE ELEM                 
         B     CKADMC40                                                         
*                                                                               
CKADMC30 BRAS  RE,NXTWFELM         POINT TO NEXT WRK FILE ELEM                  
         B     CKADMC20                                                         
*                                                                               
CKADMC40 LR    R3,RF               POINT TO BEGINNING AGAIN                     
*                                                                               
CKADMC50 CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    CKADMCER            DONE, NO AD-ID MAP CODES FOUND               
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   CKADMC60                                                         
*                                                                               
         CLC   =AL2(D#AD_ID),3(R3)                                              
         BNE   CKADMC60                                                         
         ST    R3,A_ADIDEL         SAVE ADDRESS OF AD-ID ELEM                   
         OC    A_ADCDEL,A_ADCDEL                                                
         BZ    CKADMCX                                                          
         L     R3,A_ADCDEL                                                      
         BRAS  RE,NXTWFELM         POINT TO NEXT WRK FILE ELEM                  
         CLC   =AL2(D#ADCODE+1),3(R3)                                           
         BNE   CKADMCX                                                          
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)                                                       
         SHI   RE,6                MINUS OVERHEAD TO GET DATA LENGTH            
         LTR   RE,RE                                                            
         BZ    CKADMCX             REMOVING AD CODE, ADDING AD-ID               
         B     CKADMCER            CHANGING AD CODE                             
*                                                                               
CKADMC60 BRAS  RE,NXTWFELM         POINT TO NEXT WRK FILE ELEM                  
         B     CKADMC50                                                         
*                                                                               
CKADMCX  J     SETCCEQ                                                          
*                                                                               
CKADMCER J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITLZ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,LWRKREC          LOCAL WORKING STORAGE REC AREA               
         LHI   R1,4096                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         XC    A_ADCDEL,A_ADCDEL   ADDRESS OF AD CODE ELEM                      
         XC    A_ADIDEL,A_ADIDEL   ADDRESS OF AD-ID ELEM                        
         XC    SAVADCOD,SAVADCOD                                                
         XC    SAVAD_ID,SAVAD_ID                                                
         XC    WRKAD_ID,WRKAD_ID                                                
*                                                                               
         MVI   WRKSWTCH,0          WORKING ERROR SWITCH                         
         MVI   WRKSW,0             REPLY DATA SWITCH                            
*                                                                               
INIZX    J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALAD_ID NTR1  BASE=*,LABEL=*      VALIDATE AD-ID                               
*                                                                               
         LR    RF,R3                                                            
         SR    R1,R1                                                            
         MVI   WRKAD_ID,C'0'       ZERO PADDED                                  
         MVC   WRKAD_ID+1(L'WRKAD_ID-1),WRKAD_ID                                
         XC    SAVADCOD,SAVADCOD                                                
*                                                                               
VALAD20  CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    VALAD40                                                          
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   VALAD30                                                          
*                                                                               
         CLC   =AL2(D#AD_ID),3(R3)                                              
         BNE   *+8                                                              
         AHI   R1,1                                                             
*                                                                               
VALAD30  BRAS  RE,NXTWFELM         POINT TO NEXT WRK FILE ELEM                  
         B     VALAD20                                                          
*                                                                               
VALAD40  LR    R3,RF               POINT TO BEGINNING AGAIN                     
         CHI   R1,1                                                             
         BNH   VALAD50                                                          
         LHI   R2,DUPENERR                                                      
VALAD46  ST    R2,FULL             R2 HAS ERROR NUMBER                          
         BRAS  RE,BLDDDEL          SHOULD ONLY BUILD ONE DOWNLOAD ELEM          
         BRAS  RE,BLDERREL                                                      
         B     VALADERR                                                         
*                                                                               
VALAD50  LR    R3,RF               POINT TO BEGINNING AGAIN                     
VALAD54  CLI   0(R3),0             END OF REQUEST RECORD?                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =AL2(D#AD_ID),3(R3)                                              
         BE    *+12                                                             
         BRAS  RE,NXTWFELM         POINT TO NEXT WRK FILE ELEM                  
         B     VALAD54                                                          
*                                                                               
         CLI   DDLINKSW,C'C'                                                    
         BNE   VALAD60                                                          
         BRAS  RE,NXTWFELM         POINT TO NEXT WRK FILE ELEM                  
         CLC   =AL2(D#AD_ID+1),3(R3)                                            
         BE    VALAD60                                                          
         LHI   R2,INVMAPCD                                                      
         B     VALAD46             REPLY INVALID MAP CODE ERROR                 
*                                                                               
VALAD60  SR    RE,RE                                                            
         ICM   RE,3,1(R3)                                                       
         SHI   RE,6                MINUS OVERHEAD TO GET DATA LENGTH            
         CHI   RE,L'PJOBADID                                                    
         BNH   *+12                                                             
         LHI   R2,DEMAXERR                                                      
         B     VALAD46             REPLY INVALID LENGTH ERROR                   
*                                                                               
         CHI   RE,0                NO INPUT?                                    
         BH    VALAD64                                                          
         CLI   DDLINKSW,C'C'       CHANGE UPLOAD?                               
         BNE   VALADERR                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'70'                                                     
         USING PIOELEM,R5                                                       
         BRAS  RE,NXTELEM                                                       
         BNE   *+18                                                             
         OC    PIODATE,PIODATE                                                  
         BZ    *-14                                                             
         B     VALAD62                                                          
*                                                                               
         LA    R5,REC+33                                                        
         USING PWIOELEM,R5                                                      
         MVI   ELCODE,X'71'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   VALADX              NO AD ID, BLANK OUT AD CODE FLD              
         OC    PWIODATE,PWIODATE                                                
         BZ    *-14                                                             
*                                                                               
VALAD62  LHI   R2,JBDERR                                                        
         B     VALAD46             AD NUMBER CANNOT BE REMOVED                  
*                                                                               
VALAD64  BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WRKAD_ID(0),6(R3)                                                
*                                                                               
         AHI   RE,1                LENGTH OF AD-ID INPUT                        
         CHI   RE,L'PJOBADID-1     11 CHARACTERS AD-ID?                         
         JNE   *+8                                                              
         MVI   WRKAD_ID+(L'PJOBADID-1),0                                        
*                                                                               
         MVC   WRKSVKEY,KEY        SAVE ORIGINAL KEY                            
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PADIKEY,RE                                                       
         MVC   PADIKAGY,AGYALPHA                                                
         MVC   PADIKMED,BUYMD                                                   
         MVI   PADIKRCD,PADIKRCQ                                                
         MVC   PADIKCLT,BUYCL                                                   
         OC    PADIKCLT,SPACES                                                  
         MVC   PADIKPRD,BUYPR                                                   
         OC    PADIKPRD,SPACES                                                  
         MVC   PADIKADI,WRKAD_ID                                                
*                                                                               
         MVC   KEYSAVE(L'KEY),KEY                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=C'PRTDIR',KEY,KEY,        +        
               (TERMNAL,0)                                                      
*                                                                               
         CLC   KEY(L'PADIKEY),KEYSAVE                                           
         BE    *+18                                                             
         LHI   R2,NFNDERR          REPLY RECORD NOT FOUND ERROR                 
VALAD66  MVC   KEY,WRKSVKEY        RESTORE ORIGINAL KEY                         
         B     VALAD46                                                          
*                                                                               
         L     R5,AJOBIO                                                        
         GOTO1 VDATAMGR,DMCB,(0,=C'GETREC'),=C'PRTFILE',               +        
               KEY+27,(R5),(TERMNAL,DMWORK)                                     
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         LHI   R2,RECNTFIL         REPLY RECORD NOT ON FILE ERROR               
         B     VALAD66                                                          
*                                                                               
         USING PJOBREC,R5                                                       
         CLI   PJOBKRCD,X'15'      AD RECORD?                                   
         BE    *+12                                                             
         LHI   R2,INVRECER         REPLY INVALID RECORD ERROR                   
         B     VALAD66                                                          
*                                                                               
         MVC   SAVADCOD,PJOBKJOB                                                
         MVC   SAVAD_ID,PJOBADID                                                
         MVC   KEY,WRKSVKEY        RESTORE ORIGINAL KEY                         
*                                                                               
VALADX   J     SETCCEQ                                                          
*                                                                               
VALADERR J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,RE,R5                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETAD_ID NTR1  BASE=*,LABEL=*      SET VALIDATED AD-ID                          
*                                                                               
         OC    SAVADCOD,SAVADCOD   HAVE AD CODE LOOKED UP FROM AD-ID?           
         BNZ   SETAD20                                                          
*                                                                               
SETAD10  XC    BUYAD1,BUYAD1                                                    
         MVI   BUYAD1H+5,0         AD CODE LENGTH                               
         B     SETAD90                                                          
*                                                                               
SETAD20  CLI   SAVADCOD,X'FF'      AD-ID ALONE?                                 
         BNE   SETAD40                                                          
         OI    ABUPLDSW,ABADIDAQ   SET AD-ID ALONE UPLOAD INDICATOR             
         MVC   PBDJOB,SAVADCOD                                                  
         L     RE,AJOBIO                                                        
         CLI   33(RE),X'15'        FIRST ELEM OF AD RECORD PRESENT?             
         BE    *+6                                                              
         DC    H'0'                MUST HAVE VALID AD RECORD                    
         B     SETAD10                                                          
*                                                                               
SETAD40  LA    RE,SAVADCOD+L'SAVADCOD-1                                         
         LHI   RF,L'SAVADCOD                                                    
         CLI   0(RE),C' '          TRAILING SPACE?                              
         BNE   *+12                                                             
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         B     *-12                                                             
*                                                                               
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                INVALID AD CODE LENGTH                       
*                                                                               
         XC    BUYAD1,BUYAD1                                                    
         STC   RF,BUYAD1H+5        AD CODE LENGTH                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUYAD1(0),SAVADCOD  AD CODE INPUT                                
SETAD90  OI    BUYAD1H+6,X'80'     TRANSMIT                                     
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ONLY NEED TO REPLY ONE DOWNLOAD DATA ELEM                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDDDEL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    WRKSWTCH,DLELBLDQ   ALREADY BUILT?                               
         BO    BDDDX                                                            
*                                                                               
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
         XC    WRKWORK1,WRKWORK1                                                
         XC    WRKABKEY,WRKABKEY                                                
         MVI   WRKSW,0                                                          
*                                                                               
BDDD20   CLI   0(R3),0             ELEM CODES EXIST?                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BE    BDDD25                                                           
         CLI   DDLINKSW,C'F'       DRAFT ADD UPLOAD?                            
         BE    BDDD25                                                           
*                                                                               
         CLI   0(R3),LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   BDDD25                                                           
         CLC   3(2,R3),=AL2(D#INSKEY)                                           
         BNE   BDDD25                                                           
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)                                                       
         CHI   RE,255                                                           
         BNH   *+6                                                              
         DC    H'0'                INVALID INSERTION KEY (>255)                 
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID INSERTION KEY (<0)                   
         STC   RE,WRKWORK1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WRKWORK1+1(0),6(R3)                                              
*                                                                               
BDDD25   CLI   LKDRFTSW,C'F'       DRAFT CHANGE MODE?                           
         BNE   BDDD26                                                           
         CLI   0(R3),LQ_RQSTQ      REQUEST DATA ELEM?                           
         BNE   BDDD26                                                           
         CLC   3(2,R3),=AL2(D#ADBKEY)                                           
         BNE   BDDD26                                                           
         SR    RE,RE                                                            
         ICM   RE,3,1(R3)                                                       
         CHI   RE,6+20             "ADBUYER ONLY" KEY IS <20?                   
         BNH   *+6                                                              
         DC    H'0'                INVALID "ADBUYER ONLY" KEY (>20)             
         CHI   RE,6                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID "ADBUYER ONLY" KEY (<0)              
         STC   RE,WRKABKEY                                                      
         SHI   RE,6+1              6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WRKABKEY+1(0),6(R3)                                              
*                                                                               
BDDD26   CLI   0(R3),LQ_DLDDQ      DOWLOAD DATA ELEM?                           
         BNE   *+8                                                              
         OI    WRKSW,DLDATABQ      DOWNLOAD DATA ALREADY CONSTRUCTED            
         CLI   0(R3),LQ_RAWDQ      REPLY DATA ELEM?                             
         BNE   BDDD27                                                           
         CLC   3(2,R3),=AL2(D#INSKEY)                                           
         BNE   *+8                                                              
         OI    WRKSW,INSKEYBQ      REPLY INS. KEY ALREADY CONSTRUCTED           
         CLC   3(2,R3),=AL2(D#DACTN)                                            
         BNE   *+8                                                              
         OI    WRKSW,DRFTELBQ      DRAFT CHG ELEM ALREADY CONSTRUCTED           
BDDD27   CLI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    BDDD30                                                           
         BRAS  RE,NXTWFELM                                                      
         B     BDDD20                                                           
*                                                                               
BDDD30   MVC   WRKHALF1,1(R3)      SAVE RETURNED DATA HDR ELEM LENGTH           
         LH    RE,WRKHALF1                                                      
         CHI   RE,100              ENOUGH ROOM?                                 
         BNL   *+6                                                              
         DC    H'0'                RETURN RECORD IS TOO SMALL                   
*                                                                               
         TM    WRKSW,DLDATABQ      DOWNLOAD DATA ELEM BUILD?                    
         BO    BDDD35              YES, NO NEED TO BUILD ANOTHER                
*                                                                               
         MVI   0(R3),LQ_DLDDQ      DOWNLOAD DATA ELEM CODE                      
         LHI   RE,5                ELEM LENGTH                                  
         STCM  RE,3,1(R3)                                                       
         CLI   DDLINKSW,C'N'       NEW INS?                                     
         BNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSADD)                                           
         CLI   DDLINKSW,C'F'       DRAFT INS?                                   
         BNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSDER)                                           
         CLI   DDLINKSW,C'C'       CHANGE INS?                                  
         BNE   *+10                                                             
         MVC   3(2,R3),=AL2(E#INSCHA)                                           
         LH    RE,WRKHALF1                                                      
         SHI   RE,5                RECALCULATE RETURNED DATE HDR EL LEN         
         STH   RE,WRKHALF1                                                      
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDDD35   TM    WRKSW,DRFTELBQ      DRAFT CHANGE ELEM BUILD?                     
         BO    BDDD40              YES, NO NEED TO BUILD ANOTHER                
         CLI   LKDRFTSW,C'F'       DRAFT CHANGE MODE?                           
         BNE   BDDD40                                                           
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY DRAFT CHANGE ACTION                    
         MVI   1(R3),0                                                          
         MVI   2(R3),7             LENGTH                                       
         MVC   3(2,R3),=AL2(D#DACTN)                                            
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVI   6(R3),C'D'                                                       
         BRAS  RE,NXTWFELM                                                      
         LH    RF,WRKHALF1                                                      
         SHI   RF,6+1              RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,WRKHALF1                                                      
*                                                                               
         OC    WRKABKEY,WRKABKEY   NEED TO REPLY "ADBUYER ONLY" KEY?            
         BZ    BDDD40                                                           
         MVI   0(R3),LQ_RAWDQ      REPLY "ADBUYER ONLY" KEY                     
         SR    RE,RE                                                            
         IC    RE,WRKABKEY                                                      
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,WRKHALF1                                                      
         SR    RF,RE               RECALCULATE RETURNED DATA HDR EL LEN         
         STH   RF,WRKHALF1                                                      
         MVC   3(2,R3),=AL2(D#ADBKEY)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         SHI   RE,6+7              6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),WRKABKEY+1  INSERTION KEY                                
         BRAS  RE,NXTWFELM                                                      
*                                                                               
* NOTE THAT REPLY INS KEY ELEM FOR ADD INS UPLOAD IS NOT BUILD HERE             
* BECAUSE ON A SUCCESSFUL ADD, REPLY INS KEY ELEM WILL BE BUILD IN              
* T41117.  REPLY INS KEY ELEM CANNOT BE BUILT IF ERROR OCCURED                  
* WHEN DOING AN ADD (I.E. THERE'S NO SERIAL NUMBER!)                            
*                                                                               
BDDD40   TM    WRKSW,INSKEYBQ      REPLY INS. KEY ELEM BUILD?                   
         BO    BDDD80              YES, NO NEED TO BUILD ANOTHER                
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BE    BDDD80                                                           
*                                                                               
* NEED TO REPLY INS. KEY FOR CHANGE UPLOAD MODES                                
*                                                                               
BDDD50   MVI   0(R3),LQ_RAWDQ      REPLY INSERTION KEY                          
         SR    RE,RE                                                            
         IC    RE,WRKWORK1                                                      
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,WRKHALF1                                                      
         SR    RF,RE               RECALCULATE RETURNED DATE HDR EL LEN         
         STH   RF,WRKHALF1                                                      
         MVC   3(2,R3),=AL2(D#INSKEY)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         SHI   RE,6+1              6 FOR OVERHEAD AND 1 FOR EX                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),WRKWORK1+1  INSERTION KEY                                
         BRAS  RE,NXTWFELM                                                      
*                                                                               
BDDD80   MVI   0(R3),LQ_RDATQ      REBUILD RETURNED DATA HEADER ELEM            
         MVC   1(2,R3),WRKHALF1                                                 
         OI    WRKSWTCH,DLELBLDQ   SET SWITCH TO ALREADY BUILT                  
*                                                                               
BDDDX    DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FULL = ERROR MSG NUMBER (MAX IS HALF WORD)                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDERREL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
*                                                                               
BDERR20  CLI   0(R3),0             ELEM CODES EXIST?                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    *+12                                                             
         BRAS  RE,NXTWFELM                                                      
         B     BDERR20                                                          
*                                                                               
         MVC   WRKHALF1,1(R3)      ELEM LENGTH                                  
*                                                                               
         CLI   PCVERSN#,X'04'      HIGHER THAN 4.X.X.X?                         
         JL    BDERR22                                                          
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         JNE   BDERR22                                                          
         CLC   REQTOKEN,SPACES     HAVE REQUEST TOKEN?                          
         JNH   BDERR22                                                          
         MVI   0(R3),LQ_RAWDQ      REPLY TOKEN                                  
         MVI   1(R3),0                                                          
         MVI   2(R3),6+L'REQTOKEN  LENGTH                                       
         MVC   3(2,R3),=AL2(D#QTOKEN)                                           
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
         MVC   6(L'REQTOKEN,R3),REQTOKEN                                        
         BRAS  RE,NXTWFELM                                                      
         LH    RE,WRKHALF1                                                      
         SHI   RE,L'REQTOKEN                                                    
         STH   RE,WRKHALF1         NEW RETURNED DATA HEADER ELEM LENGTH         
*                                                                               
BDERR22  MVI   0(R3),LQ_RAWDQ      REPLY ERROR FIELD NUMBER                     
         LHI   RE,8                                                             
         STCM  RE,3,1(R3)          ELEM LENGTH                                  
         LH    RF,WRKHALF1                                                      
         SHI   RF,8                                                             
         STH   RF,WRKHALF1         NEW RETURNED DATA HEADER ELEM LENGTH         
         LHI   RE,D#ERRNUM                                                      
         STCM  RE,3,3(R3)                                                       
         MVI   5(R3),LD_UBINQ      DATA TYPE - BINARY                           
         LHI   RE,D#AD_ID                                                       
         STCM  RE,3,6(R3)          AD-ID MAP CODE                               
         BRAS  RE,NXTWFELM                                                      
*                                                                               
         BRAS  RE,GET_ETXT         GET ERROR MSG IN BUYMSG                      
         OC    BUYMSG,SPACES       MAKE SURE NO TRAILING NULLS                  
*                                                                               
         MVI   0(R3),LQ_RAWDQ      REPLY ERROR MESSGE (TEXT)                    
         LHI   RE,D#ERRDSC                                                      
         STCM  RE,3,3(R3)                                                       
         MVI   5(R3),LD_CHARQ      DATA TYPE - EBCDIC CHARACTERS                
*                                                                               
         LHI   R4,L'BUYMSG                                                      
         LA    RF,BUYMSG+60-1      POINT TO LAST CHAR OF MSG                    
         CLI   0(RF),C' '                                                       
         BNE   *+12                                                             
         BCTR  R4,0                TRAILING SPACES ARE STRIPPED                 
         BCTR  RF,0                                                             
         B     *-12                                                             
         BCTR  R4,0                                                             
         CHI   R4,0                                                             
         BH    *+6                                                              
         DC    H'0'                INVALID MSG LENGTH                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),BUYMSG                                                   
         LH    RE,WRKHALF1                                                      
         AHI   R4,6+1              6 FOR OVERHEAD AND 1 FOR EX                  
         STCM  R4,3,1(R3)          LENGTH                                       
         SR    RE,R4                                                            
         STH   RE,WRKHALF1         NEW RETURNED DATA HEADER ELEM LENGTH         
*                                                                               
         MVI   0(R3),LQ_RDATQ      RETURNED DATA HEADER ELEM CODE               
         MVC   1(2,R3),WRKHALF1    NEW LENGTH                                   
*                                                                               
         MVI   ERRAREA,X'FF'       NO ADD/CHG WHEN ERROR ENCOUNTERED            
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP                                                                   
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   NEWREC              MAP BUY RECORD TO NEWREC                     
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WORK21D  DSECT                                                                  
*                                                                               
SPACES   DS    CL(L'BUYMSG)        C' '                                         
*                                                                               
RELO21   DS    F                   RELOACTION FACTOR                            
*                                                                               
A_ADCDEL DS    F                   ADDRESS OF AD CODE ELEM                      
A_ADIDEL DS    F                   ADDRESS OF AD-ID ELEM                        
*                                                                               
SAVADCOD DS    XL(L'PJOBKJOB)      SAVE AD CODE                                 
SAVAD_ID DS    XL(L'PJOBADID)      SAVE AD-ID                                   
WRKAD_ID DS    XL(L'PJOBADID)      WORK AD-ID                                   
*                                                                               
WRKSWTCH DS    X                   WORKING ERROR SWITCH                         
DLELBLDQ EQU   X'80'               DOWNLOAD ELEM IS BUILT                       
*                                                                               
WRKSW    DS    X                                                                
DLDATABQ EQU   X'80'               DOWNLOAD DATA ELEM IS BUILT                  
INSKEYBQ EQU   X'40'               REPLY KEY ELEM IS BUILT                      
DRFTELBQ EQU   X'20'               DRAFT CHANGE ELEM IS BUILT                   
*                                                                               
WRKSVKEY DS    XL(L'KEY)                                                        
WRKWORK1 DS    XL50                                                             
WRKABKEY DS    XL20                                                             
WRKELEM1 DS    XL256                                                            
WRKBYTE1 DS    X                                                                
WRKBYTE2 DS    X                                                                
WRKHALF1 DS    H                                                                
WRKHALF2 DS    H                                                                
WRKFULL1 DS    F                                                                
WRKFULL2 DS    F                                                                
WRKDUB_1 DS    D                                                                
WRKDUB_2 DS    D                                                                
*                                                                               
LWRKREC  DS    XL4096              LOCAL WORKING REC AREA                       
*                                                                               
WORK21X  EQU   *                   END OF LOCAL WORKING STORAGE AREA            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029PPBUY21   07/07/14'                                      
         END                                                                    
