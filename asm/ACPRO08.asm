*          DATA SET ACPRO08    AT LEVEL 032 AS OF 02/21/06                      
*PHASE T60B08A                                                                  
         TITLE 'T60B08 - MEDIA MAINT'                                           
T60B08   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B08**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         GOTO1 VMODPTRS,DMCB,(X'80',POINTERS)                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC                                                          
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY                                                          
         B     XIT                                                              
*                                                                               
MODE6    CLI   MODE,DISPREC                                                     
         BNE   MODE8                                                            
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE8    GOTO1 CANWEDEL                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
VKEY     NTR1                      MEDIA RECORD                                 
         LA    R2,PROMEDH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BH    ERREND                                                           
         BAS   RE,TSTAN            MAKE SURE ALPHA-NUMERIC                      
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),CUL                                                     
         MVC   KEY+2(1),WORK                                                    
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
*                                                                               
VREC     NTR1                                                                   
         MVI   ELCODE,ACMDELQ      CHECK IF EXISTING ELEMENT                    
         LA    R6,ELEMENT                                                       
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         USING ACMEDIAD,R6                                                      
         MVI   ACMDEL,ACMDELQ                                                   
         MVI   ACMDLEN,ACMDLNQ3                                                 
         MVC   ACMDCODE,PROMED                                                  
*                                                                               
         MVI   ACMDGRP,0                                                        
         LA    R2,PROMGRH          GROUP IS OPTIONAL                            
         CLI   5(R2),0                                                          
         BE    VREC02                                                           
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY1,KEY                                                     
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALMG                                                            
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
         MVI   OPTION,0                                                         
         MVC   ACMDGRP,PROMGR                                                   
*                                                                               
VREC02   LA    R2,PROCOMH          COMMISSION ACCOUNT                           
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   WORK(2),=C'SI'      MAKE SURE 'SI' IS SPECIFIED                  
         BNE   ERREND                                                           
         CLI   5(R2),3                                                          
         BL    ERREND              MUST BE 'SI' PLUS CODE                       
         MVC   ACMDCOMM(1),CUL                                                  
         MVC   ACMDCOMM+1(14),WORK                                              
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY1,KEY                                                     
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALAC14                                                          
         BAS   RE,BALEL                                                         
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
         MVI   OPTION,0                                                         
*                                                                               
         MVC   ACMDDESC,SPACES                                                  
         LA    R2,PRODESCH                                                      
         CLI   5(R2),0             IF DESCRIPTION IS INPUT                      
         BE    VREC04                                                           
         GOTO1 ANY                                                              
         MVC   ACMDDESC+3(12),WORK    USE IT                                    
         B     VREC06                                                           
*                                                                               
VREC04   DS    0H                  ELSE CHOP IT FROM COMM A/C                   
         GOTO1 CHOPPER,DMCB,(36,PROCOMN),(12,ACMDDESC+3),(0,1)                  
*                                                                               
VREC06   CLC   COUNTRY,=C'UK'      VAT IF UK                                    
         BNE   VREC08                                                           
*&&UK                                                                           
         LA    R2,PROVATH                                                       
         GOTO1 ANY                                                              
         MVC   ACMDVTAC(1),CUL                                                  
         MVC   ACMDVTAC+1(14),WORK                                              
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY1,KEY                                                     
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALAC14                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
         MVI   OPTION,0                                                         
*&&                                                                             
*                                                                               
VREC08   LA    R2,PROCSDH          PROD CASH DISCOUNT ACCOUNT                   
         CLI   5(R2),0                                                          
         BE    VREC10                                                           
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   WORK(2),=C'SI'                                                   
         BNE   ERREND                                                           
         MVC   AIO,AIO2            SAVE KEY AND BUFFER                          
         MVC   SAVEKEY1,KEY                                                     
         MVI   OPTION,C'Y'                                                      
         MVC   KEY(1),CUL                                                       
         MVC   KEY+1(14),WORK                                                   
         MVC   ACMDCSHD,KEY        SAVE ACCOUNT IN ELEMENT                      
         MVC   PROCSDN,SPACES                                                   
         OI    PROCSDNH+6,X'80'                                                 
         GOTO1 READ                                                             
         BAS   RE,BALEL            MAKE SURE IT HAS A BALANCE ELEMENT           
         LA    R2,PROCSDNH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1            RESTORE KEY AND BUFFER                       
         MVC   KEY,SAVEKEY1                                                     
         MVI   OPTION,0                                                         
*                                                                               
VREC10   LA    R2,PROPCDH          PRINT CASH DISCOUNT ACCOUNT                  
         CLI   5(R2),0                                                          
         BE    VREC12                                                           
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   WORK(2),=C'SI'                                                   
         BNE   ERREND                                                           
         MVC   AIO,AIO2            SAVE KEY AND BUFFER                          
         MVC   SAVEKEY1,KEY                                                     
         MVI   OPTION,C'Y'                                                      
         MVC   KEY(1),CUL                                                       
         MVC   KEY+1(14),WORK                                                   
         MVC   ACMDPRCD,KEY        SAVE ACCOUNT IN ELEMENT                      
         MVC   PROPCDN,SPACES                                                   
         OI    PROPCDNH+6,X'80'                                                 
         GOTO1 READ                                                             
         BAS   RE,BALEL            MAKE SURE IT HAS A BALANCE ELEMENT           
         LA    R2,PROPCDNH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1            RESTORE KEY AND BUFFER                       
         MVC   KEY,SAVEKEY1                                                     
         MVI   OPTION,0                                                         
*                                                                               
VREC12   LA    R2,PROTWOH          TIME WRITE-OFF ACCOUNT                       
         CLI   5(R2),0                                                          
         BE    VREC14                                                           
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   WORK(2),=C'SI'                                                   
         BNE   ERREND                                                           
         MVC   AIO,AIO2            SAVE KEY AND BUFFER                          
         MVC   SAVEKEY1,KEY                                                     
         MVI   OPTION,C'Y'                                                      
         MVC   KEY(1),CUL                                                       
         MVC   KEY+1(14),WORK                                                   
         MVC   ACMDTWO,KEY        SAVE ACCOUNT IN ELEMENT                       
         MVC   PROTWON,SPACES                                                   
         OI    PROTWONH+6,X'80'                                                 
         GOTO1 READ                                                             
         BAS   RE,BALEL            MAKE SURE IT HAS A BALANCE ELEMENT           
         LA    R2,PROTWONH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1            RESTORE KEY AND BUFFER                       
         MVC   KEY,SAVEKEY1                                                     
         MVI   OPTION,0                                                         
*                                                                               
VREC14   LA    R2,PROTINH          TIME INCOME ACCOUNT                          
         CLI   5(R2),0                                                          
         BE    VREC16                                                           
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   WORK(2),=C'SK'                                                   
         BE    VREC15                                                           
         CLC   WORK(2),=C'SI'                                                   
         BNE   ERREND                                                           
VREC15   MVC   AIO,AIO2            SAVE KEY AND BUFFER                          
         MVC   SAVEKEY1,KEY                                                     
         MVI   OPTION,C'Y'                                                      
         MVC   KEY(1),CUL                                                       
         MVC   KEY+1(14),WORK                                                   
         MVC   ACMDTIN,KEY        SAVE ACCOUNT IN ELEMENT                       
         MVC   PROTINN,SPACES                                                   
         OI    PROTINNH+6,X'80'                                                 
         GOTO1 READ                                                             
         BAS   RE,BALEL            MAKE SURE IT HAS A BALANCE ELEMENT           
         LA    R2,PROTINNH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1            RESTORE KEY AND BUFFER                       
         MVC   KEY,SAVEKEY1                                                     
         MVI   OPTION,0                                                         
*                                                                               
VREC16   MVC   ACMDANAL,SPACES     OPTIONAL ANALYSIS CODE                       
         OC    PROANAL,SPACES                                                   
         LA    R2,PROANALH                                                      
         CLI   5(R2),0                                                          
         BE    VREC18                                                           
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY1,KEY                                                     
         MVI   OPTION,C'Y'                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),CUL                                                       
         MVC   KEY+1(2),=C'11'                                                  
         MVC   KEY+3(12),PROANAL                                                
         GOTO1 READ                                                             
         BAS   RE,BALEL                                                         
         MVC   KEY+1(2),=C'12'                                                  
         GOTO1 READ                                                             
         BAS   RE,BALEL                                                         
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
         MVI   OPTION,0                                                         
         MVC   ACMDANAL,PROANAL                                                 
         MVC   ACMDCOST,PROANAL                                                 
*                                                                               
VREC18   LA    R2,PRONEXTH                                                      
         CLI   5(R2),6             NEXT BILL MUST BE 6 BYTE                     
         BNE   INVEND                                                           
         GOTO1 NUMERIC             NUMERIC                                      
         MVC   ACMDFBIL,PRONEXT                                                 
         MVC   ACMDLBIL,PRONEXT                                                 
*                                                                               
         LA    R2,PRORESH                                                       
         CLI   5(R2),4             RESET MUST BE 4 BYTE                         
         BNE   INVEND                                                           
         GOTO1 NUMERIC             NUMERIC                                      
         MVC   ACMDRSET,PRORES                                                  
*                                                                               
         XC    ACMDSTAT,ACMDSTAT   CLEAR MEDIA STATUS                           
         CLI   PROTIT,C'Y'         PRINT IN BILLING TITLE                       
         BNE   *+8                                                              
         OI    ACMDSTAT,X'80'      TURN IT ON                                   
*                                                                               
         LA    R2,PROTMEH                                                       
         CLI   5(R2),0                                                          
         BE    VREC20                                                           
         CLI   PROTME,C'T'         TALENT MEDIA TV                              
         BNE   VREC19                                                           
         OI    ACMDSTAT,ACMDSTV    TURN IT ON                                   
         B     VREC20                                                           
*                                                                               
VREC19   CLI   PROTME,C'R'         TALENT MEDIA RADIO                           
         BNE   INVEND                                                           
         OI    ACMDSTAT,ACMDSRAD   TURN IT ON                                   
*                                                                               
VREC20   GOTO1 ADDELEM                                                          
*                                                                               
VREC22   MVC   HALF(1),ACMDCODE                                                 
         MVI   HALF+1,ACMNEST                                                   
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('ACMNELQ',AIO),(2,HALF),0              
         LA    R2,PROENAMH                                                      
         CLI   5(R2),0                                                          
         BE    VREC24                                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEM                                                          
         USING ACMND,R6                                                         
         GOTO1 ANY                                                              
         MVI   ACMNEL,ACMNELQ                                                   
         MVC   ACMNCODE,PROMED     MEDIA CODE                                   
         MVI   ACMNTYPE,ACMNEST                                                 
         MVC   ACMNAME,WORK                                                     
         ZIC   R1,5(R2)                                                         
         LA    R1,ACMNAME-ACMND(R1)                                             
         STC   R1,ACMNLEN                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
VREC24   GOTO1 PERSIN                                                           
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY KEY                                                      
*                                                                               
DKEY     NTR1                                                                   
         L     R4,AIO                                                           
         MVC   PROMED,2(R4)                                                     
         LA    R2,PROMEDH                                                       
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
*                                                                               
DREC     NTR1                                                                   
         MVI   ELCODE,ACMDELQ                                                   
         BAS   RE,GETELIO                                                       
         USING ACMEDIAD,R6                                                      
         LA    R2,PRODESCH                                                      
         MVC   8(12,R2),ACMDDESC+3                                              
         OI    6(R2),X'80'                                                      
         GOTO1 PERSOUT                                                          
         MVC   PROLACT,SPACES                                                   
         MVC   PROLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PROLACT+15(20),WORK+20                                           
         OI    PROLACTH+6,X'80'                                                 
*                                                                               
         MVC   PROMGR,SPACES       OPTIONAL WORK GROUP                          
         MVC   PROMGRN,SPACES                                                   
         OI    PROMGRH+6,X'80'                                                  
         OI    PROMGRNH+6,X'80'                                                 
         LA    R2,PROMGRH                                                       
         CLI   ACMDGRP,X'41'                                                    
         BL    DREC02                                                           
*                                                                               
         MVC   8(1,R2),ACMDGRP     IF ACTIVE, SHOW IT                           
         MVC   SAVEKEY1,KEY                                                     
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING ACMGKEY,R4                                                       
         MVI   ACMGRTYP,ACMGEQU                                                 
         MVI   ACMGSREC,ACMGSEQU                                                
         MVC   ACMGCUL,CUL                                                      
         MVC   ACMGCODE,ACMDGRP                                                 
         GOTO1 READ                                                             
         LA    R2,PROMGRNH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
         DROP  R4                                                               
*                                                                               
DREC02   LA    R2,PROCOMH          COMMISSION A/C                               
         OI    6(R2),X'80'                                                      
         MVC   8(14,R2),ACMDCOMM+1                                              
         MVC   SAVEKEY1,KEY                                                     
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACMDCOMM                                                 
         GOTO1 READ                                                             
         LA    R2,PROCOMNH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
*                                                                               
DREC04   CLC   COUNTRY,=C'UK'      VAT FOR UK                                   
         BNE   DREC06                                                           
*&&UK                                                                           
         LA    R2,PROVATH                                                       
         OI    6(R2),X'80'                                                      
         MVC   8(14,R2),ACMDVTAC+1                                              
         MVC   SAVEKEY1,KEY                                                     
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACMDVTAC                                                 
         GOTO1 READ                                                             
         LA    R2,PROVATNH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
*&&                                                                             
*                                                                               
DREC06   MVC   PROCSD,SPACES                                                    
         MVC   PROCSDN,SPACES                                                   
         OI    PROCSDH+6,X'80'                                                  
         OI    PROCSDNH+6,X'80'                                                 
         CLI   ACMDCSHD,X'41'                                                   
         BL    DREC08                                                           
         LA    R2,PROCSDH                                                       
         MVC   8(L'ACMDCSHD-1,R2),ACMDCSHD+1                                    
         MVC   SAVEKEY1,KEY                                                     
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACMDCSHD),ACMDCSHD                                         
         GOTO1 READ                                                             
         LA    R2,PROCSDNH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
*                                                                               
DREC08   MVC   PROPCD,SPACES                                                    
         MVC   PROPCDN,SPACES                                                   
         OI    PROPCDH+6,X'80'                                                  
         OI    PROPCDNH+6,X'80'                                                 
         MVC   PROANAL,SPACES                                                   
         OI    PROANALH+6,X'80'                                                 
         CLI   ACMDLEN,ACMDLNQ2                                                 
         BL    DREC12                                                           
         MVC   PROANAL,ACMDCOST                                                 
*                                                                               
         CLI   ACMDPRCD,X'41'                                                   
         BL    DREC10                                                           
         LA    R2,PROPCDH                                                       
         MVC   8(L'ACMDPRCD-1,R2),ACMDPRCD+1                                    
         MVC   SAVEKEY1,KEY                                                     
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACMDPRCD),ACMDPRCD                                         
         GOTO1 READ                                                             
         LA    R2,PROPCDNH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
*                                                                               
DREC10   MVC   PROTWO,SPACES                                                    
         MVC   PROTWON,SPACES                                                   
         OI    PROTWOH+6,X'80'                                                  
         OI    PROTWONH+6,X'80'                                                 
         CLI   ACMDTWO,X'41'                                                    
         BL    DREC12                                                           
         LA    R2,PROTWOH                                                       
         MVC   8(L'ACMDTWO-1,R2),ACMDTWO+1                                      
         MVC   SAVEKEY1,KEY                                                     
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACMDTWO),ACMDTWO                                           
         GOTO1 READ                                                             
         LA    R2,PROTWONH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
*                                                                               
DREC12   MVC   PROTIN,SPACES                                                    
         MVC   PROTINN,SPACES                                                   
         OI    PROTINH+6,X'80'                                                  
         OI    PROTINNH+6,X'80'                                                 
         CLI   ACMDLEN,ACMDLNQ3                                                 
         BL    DREC14                                                           
         CLI   ACMDTIN,X'41'                                                    
         BL    DREC14                                                           
         LA    R2,PROTINH                                                       
         MVC   8(L'ACMDTIN-1,R2),ACMDTIN+1                                      
         MVC   SAVEKEY1,KEY                                                     
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACMDTIN),ACMDTIN                                           
         GOTO1 READ                                                             
         LA    R2,PROTINNH                                                      
         GOTO1 NAMEOUT                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
*                                                                               
DREC14   MVC   PROANAL(1),ACMDANAL                                              
         OI    PROANALH+6,X'80'                                                 
         MVC   PRONEXT,ACMDLBIL                                                 
         OI    PRONEXTH+6,X'80'                                                 
         MVC   PRORES,ACMDRSET                                                  
         OI    PRORESH+6,X'80'                                                  
*                                                                               
         LA    R2,PROTITH          PRINT MEDIA NAME ON BILLS                    
         OI    6(R2),X'80'                                                      
         MVI   8(R2),C'N'                                                       
         TM    ACMDSTAT,X'80'                                                   
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
         LA    R2,PROTMEH          TALENT MEDIA                                 
         OI    6(R2),X'80'                                                      
         MVI   8(R2),C' '                                                       
         TM    ACMDSTAT,ACMDSTV                                                 
         BNO   *+8                                                              
         MVI   8(R2),C'T'                                                       
         TM    ACMDSTAT,ACMDSRAD                                                
         BNO   *+8                                                              
         MVI   8(R2),C'R'                                                       
*                                                                               
DREC16   MVC   PROENAM,SPACES                                                   
         LA    R2,PROENAMH                                                      
         OI    6(R2),X'80'                                                      
         MVC   HALF(1),ACMDCODE    FILTER ON MEDIA CODE/NAME TYPE               
         MVI   HALF+1,ACMNEST                                                   
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACMNELQ',AIO),(2,HALF),0              
         CLI   12(R1),0                                                         
         BNE   DRECX                                                            
         L     R6,12(R1)                                                        
         USING ACMND,R6                                                         
         MVC   LISTAR,SPACES                                                    
         ZIC   R1,ACMNLEN                                                       
         SH    R1,=Y(ACMNAME-ACMND+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTAR(0),ACMNAME                                                
         BAS   RE,MOVEFLD                                                       
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
*                                                                               
         USING ACKEYD,R3                                                        
BALEL    L     R3,AIO                                                           
         LA    R3,ACRECORD                                                      
         SR    R1,R1                                                            
*                                                                               
BAL2     CLI   0(R3),X'32'                                                      
         BER   RE                  FOUND BALANCE ELEMENT - OK TO RETURN         
         CLI   0(R3),0                                                          
         BE    BAL3                NO BALANCE - ERROR                           
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     BAL2                                                             
*                                                                               
BAL3     MVI   ERROR,INVPOST                                                    
         B     ERREND                                                           
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
MOVEFLD  ST    RE,FULL                                                          
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
TSTAN    ST    RE,FULL                                                          
         CLI   8(R2),C'0'                                                       
         BL    *+16                                                             
         CLI   8(R2),C'9'                                                       
         BH    MEDERR                                                           
         B     TSTANX                                                           
         CLI   8(R2),C'A'                                                       
         BL    MEDERR                                                           
         CLI   8(R2),C'I'                                                       
         BNH   TSTANX                                                           
         CLI   8(R2),C'J'                                                       
         BL    MEDERR                                                           
         CLI   8(R2),C'R'                                                       
         BNH   TSTANX                                                           
         CLI   8(R2),C'S'                                                       
         BL    MEDERR                                                           
         CLI   8(R2),C'Z'                                                       
         BH    MEDERR                                                           
*                                                                               
TSTANX   L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
MYEND    MVI   ERROR,SUPPLIED      USING MY OWN ERROR MSG                       
         B     ERREND                                                           
*                                                                               
MEDERR   MVI   ERROR,BADMEDIA                                                   
         B     ERREND                                                           
*                                                                               
INVEND   MVI   ERROR,INVALID                                                    
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROF8D                                                       
*                                                                               
POINTERS DS    CL(8*54+1)          PASSIVE POINTER BLOCK                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032ACPRO08   02/21/06'                                      
         END                                                                    
